# ########################################################################################
# WORKSHOP IN GEOSPATIAL ANALYSIS
#
# Purpose: Detecting spatial clusters in R.
#
# This script was written using R version 3.6.1 (2019-07-05) -- "Action of the Toes"
#
# Note: The R code below can be collapsed or expanded into sections for easy navigation:
#    collapse code - Alt+O
#    expand code - Shift+Alt+O
#
# Author: Michael Otterstatter
#
# Created: Aug 26, 2019
#
# Last modified: 
# 
########################################################################################




#### Resources #### 

# Further information on the Kulldorff cluster detection method used here can be found
#     in the documentation for the SpatialEpi package at
#     http://faculty.washington.edu/jonno/SISMIDmaterial/SpatialEpiVignette.pdf
#
# At any time, more information on R commands and function can be found in the built-in 
#     R help files.  To access help files on a specific command use '?'  e.g., type '?count' 
#     to find more information on the count() command



#### Setup ####

# When using this script for the first time, certain R functions (organized into 'packages')
#   may need to be installed on your computer.  Provided you have an internet connection, the
#   following line will retrieve and install the necessary functions:
install.packages("checkpoint") # functions to ensure reproducibility


# set working directory
my_workshop_R_folder <- "U:/Personal/CPAC geospatial analysis training/workshop materials/R"
setwd(my_workshop_R_folder)


# Set checkpoint to ensure reproducibility:
library(checkpoint) # Load checkpoint library 
checkpoint("2019-07-01") # set checkpoint date


# Load the installed packages ('libraries') for working with spatial data
library(sf)             # functions for working with spatial data
library(tmap)           # functions for generating maps
library(SpatialEpi)     # functions for statistical testing of spatial clusters 
library(tidyverse)      # functions for efficient data manipulation




#### 1. IMPORT DATA ####

source_url_chsa <- "https://catalogue.data.gov.bc.ca/dataset/68f2f577-28a7-46b4-bca9-7e9770f2f357/resource/f89f99b0-ca68-41e2-afc4-63fdc0edb666/download"
source_file_chsa <- "chsa_2018.zip"
download_dir_bc <- paste(my_workshop_R_folder, "data/bc_2018", sep = "/")


# NOTE these files have already been downloaded and unzipped - skip this section
# ***********************************************************************************************

download.file(url = paste(source_url_chsa, source_file_chsa, sep = "/"),
              destfile = paste(download_dir_bc, source_file_chsa, sep = "/"))
unzip(zipfile = paste(download_dir_bc, source_file_chsa, sep = "/"), exdir = download_dir_bc)

# ***********************************************************************************************


# read unzipped shapefile
shape_file_chsa <- "CHSA_2018.shp"
bc_chsa_bounds_2018 <- st_read(dsn = paste(download_dir_bc, shape_file_chsa, sep = "/"), 
                               stringsAsFactors = FALSE)

# simplify file (reduces from 14MB to 0.8MB) for faster processing
chsa_simple <- st_simplify(bc_chsa_bounds_2018, d = 500)





#### 2. PREPARE DATA FOR ANALYSIS ####

# Event counts

# For illustrative purposes, we simulate event counts for CHSAs rather than use real data
#   We generate normally distributed counts, with variation, based on a fixed rate of 5% of population size,
#   with elevated counts (cluster) in CHSAs in the central Okanagan region 

sim_counts <- chsa_simple %>% 
  select(chsa = CHSA_Name, lha = LHA_Name, population = CHSA_Pop16) %>%
  arrange(chsa) %>%
  mutate(location = row_number(), time = 1,
         count = rnorm(n = nrow(chsa_simple), mean = population * 0.05, sd = 50), 
         count = ifelse(lha == "Central Okanagan", count*1.5, abs(count)),
         rate = count / population)


# generate quick maps of simulated rates
tm_shape(sim_counts) +
  tm_polygons(col = "rate") +
  tm_legend(outside = TRUE)


# for further processing, we drop geometry variables from count data 
sim_counts <- sim_counts %>% st_drop_geometry() 



## Geometry data

# We use the centroid of each region (CHSA) to define a location in our cluster analysis
chsa_simple <- cbind(chsa_simple, st_coordinates(st_centroid(chsa_simple))) %>%
  rename(cent_x = "X", cent_y = "Y") %>% arrange(CHSA_Name)

# keep coordinates of centroids for each CHSA
chsa_geo <- chsa_simple %>% select(cent_x, cent_y) %>% st_drop_geometry()




#### 3. RUN CLUSTER ANALYSIS ####

# For the Kulldoff cluster detection method, we can use either a binomial model (does not use
#   expected event counts) or a Poisson model (uses expected event counts)

# For both methods we need 
#   1) the geometry data (coordinates of each region's centroid),
#   2) the observed event counts by region, and 
#   3) the population size for each region

# For both methods we will also define
#   the upper bound of population size that a cluster can contain, 
#   the number of simulations used to calculate a p-value, and 
#   the alpha level for determining statistical significance.


## Get observed counts, expected counts and population sizes by CHSA for use in cluster detection
chsa_cases <- sim_counts$count
chsa_pop <- chsa_simple$CHSA_Pop16
expected.cases <- expected(chsa_pop, chsa_cases, n.strata = 1) # needed for Poisson model


## 1. Kulldorff Binomial method (does not use expected event counts)

binomial <- kulldorff(geo = chsa_geo, 
                      cases = chsa_cases, 
                      population = chsa_pop, 
                      expected.cases = NULL, 
                      pop.upper.bound = 0.1, 
                      n.simulations = 999, 
                      alpha.level = 0.05, 
                      plot = FALSE)

# print statistics associated with most likely cluster
binomial$most.likely.cluster

# which regions (CHSA's) are in the most likely cluster? 
sim_counts[binomial$most.likely.cluster$location.IDs.included,]




## 2. Kulldorff Poisson method (uses expected event counts)

poisson <- kulldorff(geo = chsa_geo, 
                     cases = chsa_cases, 
                     population = chsa_pop, 
                     expected.cases = expected.cases, 
                     pop.upper.bound = 0.1, 
                     n.simulations = 999, 
                     alpha.level = 0.05, 
                     plot = FALSE)

# print statistics associated with most likely cluster
poisson$most.likely.cluster

# which regions (CHSA's) are in the most likely cluster? 
sim_counts[poisson$most.likely.cluster$location.IDs.included,]



#### 4. MAP RESULTS OF CLUSTER ANALYSIS ####

# Here, we add a two new flag variables in our CHSA data to indicate if a region was or 
#   was not in the most likely cluster identified by the binomial (cluster_b) or 
#   poisson model (cluster_p)

chsa_plot <- chsa_simple %>% 
  mutate(cluster_b = ifelse(row_number() %in% 
                              binomial$most.likely.cluster$location.IDs.included, "Yes", "No"),
         cluster_p = ifelse(row_number() %in% 
                              poisson$most.likely.cluster$location.IDs.included, "Yes", "No"))


# generate map showing CHSA's in most likely cluster from Binomail model
tm_shape(chsa_plot) +
  tm_polygons(col = "cluster_b", title = "Significant cluster")

# generate map showing CHSA's in most likely cluster from Poisson model
tm_shape(chsa_plot) +
  tm_polygons(col = "cluster_p")


