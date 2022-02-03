# ########################################################################################
# WORKSHOP IN GEOSPATIAL ANALYSIS - MODULE 5B - ADDING TIME TO CLUSTER DETECTION
#
# Purpose: Detecting space-time clusters in R.
# Example following: Introduction to scanstatistics Benjamin Allévius
#   https://cran.r-project.org/web/packages/scanstatistics/vignettes/introduction.html
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
# 
########################################################################################




#### Resources #### 

# Concepts and functions related to the freely available SaTScan software are discussed
#    in detail at the website https://www.satscan.org/
#
# Further information on spatial scan statistics available in R can be found at
#   https://cran.r-project.org/web/packages/scanstatistics/vignettes/introduction.html
#
# Here, we interface with the SaTScan software using the R package rsatscan; further
#   examples of the package can be found at https://www.satscan.org/rsatscan/rsatscan.html
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
library(scanstatistics) # functions for statistical testing of spatial clusters
library(tidyverse)      # functions for efficient data manipulation




#### 1. IMPORT DATA ####

# Here, we will use Health Service Delivery Areas (HSDA) in BC as the geographic unit for analysis,
#   and health indicator data at the same level from the Canadian Community Health Survey (CCHS)
#   The CCHS file also contains population sizes at the HSDA level

source_url_hsda <- "https://catalogue.data.gov.bc.ca/dataset/71c930b9-563a-46da-a10f-ead49ccbc390/resource/a4792ee7-ba88-458e-a1a5-597926033a9e/download"
source_file_hsda <- "hsda_2018.zip"
source_url_cchs <- "https://www150.statcan.gc.ca/n1/en/tbl/csv/13100086-eng.zip?st=iOE67370"
source_file_cchs <- "13100086-eng.zip"
download_dir_bc <- paste(my_workshop_R_folder, "data/bc_2018", sep = "/")
download_dir_cchs <- paste(my_workshop_R_folder, "data/cchs_2005", sep = "/")


# NOTE these files have already been downloaded and unzipped - skip this section
# ***********************************************************************************************

# download and unzip HSDA shape file and CCHS data file
download.file(url = paste(source_url_hsda, source_file_hsda, sep = "/"),
              destfile = paste(download_dir_bc, source_file_hsda, sep = "/"))
unzip(zipfile = paste(download_dir_bc, source_file_hsda, sep = "/"), exdir = download_dir_bc)


download.file(url = paste(source_url_cchs, source_file_cchs, sep = "/"),
              destfile = paste(download_dir_cchs, source_file_cchs, sep = "/"))
unzip(zipfile = paste(download_dir_cchs, source_file_cchs, sep = "/"), exdir = download_dir_cchs)

# ***********************************************************************************************


# read unzipped shapefile
shape_file_hsda <- "HSDA_2018.shp"
bc_hsda_bounds_2018 <- st_read(dsn = paste(download_dir_bc, shape_file_hsda, sep = "/"), 
                                 stringsAsFactors = FALSE)



# read unzipped CCHS data file and bc stats population size file with read_csv()
cchs_data_file <- "13100086.csv"
bc_pop_file <- "bc_population_2003_2005.csv"
cchs_2005 <- read_csv(paste(download_dir_cchs, cchs_data_file, sep = "/"), guess_max = 10000)
bc_pop_2003_05 <- read_csv(paste(download_dir_cchs, bc_pop_file, sep = "/"))




#### 2. EXPLORE AND CLEAN DATA ####

# HSDA boundaries

# simplify HSDA shapefile to speed processing time (if detailed boundaries are not needed)
#   and select only those columns needed for analysis
hsda <- st_simplify(bc_hsda_bounds_2018, d=500) %>%
  select(HSDA_Name, HSDA_Pop16, HSDA_Area, Latitude, Longitude, geometry) %>%
  arrange(HSDA_Name)


# generate quick map of HSDA boundaries
tm_shape(hsda) +
  tm_polygons(col = "HSDA_Name", title = "") +
  tm_legend(outside = TRUE)



# Population size data 
#   recode as needed to match with other datasets
bc_pop <- bc_pop_2003_05 %>%
  mutate(location = recode(location, "Thompson Cariboo" = "Thompson Cariboo Shuswap"))



# CCHS Health data
# clean cchs data andn subset for BC
cchs_2005 <- cchs_2005 %>%
  filter(str_detect(GEO, "British Columbia"),
         Characteristics == 'Number of persons') %>%
  mutate(GEO = str_replace(GEO, " Health Service Delivery Area, British Columbia",  "")) %>%
  mutate(GEO = recode(GEO, "Kootenay-Boundary" = "Kootenay Boundary", 
                      "Thompson/Cariboo" = "Thompson Cariboo Shuswap"),
         Sex = recode(Sex, "Both sexes" = "T", "Females" = "F", "Males" = "M")) %>%
  filter(!GEO == 'British Columbia') %>%
  select(year = REF_DATE, location = GEO, indicator = `Health profile`, sex = Sex,
         count = VALUE)

#join on population size data
cchs_2005 <- cchs_2005 %>%
  left_join(bc_pop, by = c("year", "location", "sex")) %>%
  mutate(Prevalence = count / population)

View(cchs_2005)




#### 3. PREPARE DATA FOR CLUSTER ANALYSIS ####

# generate counts and population sizes for indicator, years and sexes of interest

health_indicator <- "Received routine screening mammogram within the last 2 years (50 to 69 years)"
sexes <- "F"  # chose a single sex classification (M for males, F for females or T for Total)
years <- c("2003","2005")  # choose cchs year(s) (2003 and/or 2005)

# also note that we create a 'time' variable, as required by scanstatistics, 
#   with values number from 1 (oldest) n time points, and a 'location' variable, 
#   with values from 1 to n locations

indicator <- cchs_2005 %>%
  filter(indicator == health_indicator, sex == sexes, year %in% years) %>%
  mutate(year = as.character(year)) %>%
  arrange(location, year) %>%
  rename("hsda" = "location") %>%
  group_by(hsda) %>%
  mutate(time = row_number()) %>% # time variable
  group_by(time) %>%
  mutate(location = row_number()) %>% # location variable
  arrange(location, time)

glimpse(indicator)


# create plotting dataset of indicator prevalence by HSDA
plot_data <- hsda %>%
  left_join(indicator, by = c("HSDA_Name" = "hsda"))


# generate facetted plot of indicator over time
tm_shape(plot_data) +
  tm_polygons(col = "Prevalence") +
  tm_facets(by = "year", ncol = 1, nrow = 2) +
  tm_layout(main.title = "Mammogram within last 2 years (50 to 69 years)",  
            main.title.size = 0.95, frame = TRUE)




#### 4. RUN CLUSTER ANALYSIS ####


# create zones of neighbouring geographic units for cluster detection

# add centroid coordinates for each HSDA to shapefile
hsda <- cbind(hsda, st_coordinates(st_centroid(hsda))) %>%
  rename(cent_x = "X", cent_y = "Y") %>% arrange(HSDA_Name)


# define zones for cluster anaysis, based on centroids and k nearest neighbours
zones <- hsda %>%
  select(cent_x, cent_y) %>%
  st_distance() %>%
  dist_to_knn(k = 4) %>% # here, we set zones to include a maximum of 4 nearest HSDAs
  knn_zones()

zones



# Here, we use the population-based space-time scan statistic of Kulldorff, 2001

cluster_pois <- scan_pb_poisson(counts = indicator, 
                          zones = zones, 
                          population = NULL,
                          n_mcsim = 999, 
                          max_only = FALSE)
print(cluster_pois)


# Map cluster scores from Poisson model

# Extract HSDA scores from Poisson cluster analysis 
hsda_scores <- score_locations(cluster_pois, zones) %>%
  left_join(indicator, by = "location")

# Create a table for plotting
score_map <- hsda %>% select(HSDA_Name, geometry) %>%
  left_join(hsda_scores, by = c("HSDA_Name" = "hsda"))

tm_shape(score_map) +
  tm_polygons(col = "relative_score", title = "Cluster score")




# We see that HSDAs are rather large areas for cluster detection (this was a limitation of
#   the availability of CCHS data) -- what if we use a smaller administrative health region like BC's 
#   Community Health Service Areas (CHSAs)?


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

# simplifiy shapefile for faster processing
chsa_simple <- st_simplify(bc_chsa_bounds_2018, d = 500)

tm_shape(chsa_simple) +
  tm_polygons(col = "CHSA_Name") + 
  tm_legend(show = FALSE)




#### 2. PREPARE DATA FOR ANALYSIS ####


# Create event counts

# simulate counts at two time points for each CHSA 
#   (we generate mock data here as CCHS survey data is not publically available below HSDA level

# counts for time 1
sim_counts1 <- chsa_simple %>% 
  select(chsa = CHSA_Name, lha = LHA_Name, population = CHSA_Pop16) %>%
  arrange(chsa) %>%
  mutate(location = row_number(), time = 1,
         count = runif(n = nrow(chsa_simple), min = floor(population * 0.05), max = floor(population * 0.15)),
         count = ifelse(lha %in% c("Prince George","Nechako"), count*1.5, count),
         rate = count / population)

# counts for time 2
sim_counts2 <- chsa_simple %>% 
  select(chsa = CHSA_Name, lha = LHA_Name, population = CHSA_Pop16) %>%
  arrange(chsa) %>%
  mutate(location = row_number(), time = 2,
         count = runif(n = nrow(chsa_simple), min = floor(population * 0.05), max = floor(population * 0.15)),
         count = ifelse(lha %in% c("Prince George","Quesnel"), count*1.5, count),
         rate = count / population)

# bind counts from both time points together
sim_counts <- rbind(sim_counts1, sim_counts2)


# generate quick multi-panel map to see patterns
tm_shape(sim_counts) +
  tm_polygons(col = "rate", palette="YlGn") +
  tm_facets(by = "time", ncol = 1, nrow = 2)


sim_counts <- sim_counts %>% st_drop_geometry()




#### 3. RUN CLUSTER ANALYSIS ####

# Create zones for cluster analysis

# add centroid coordinates for each CHSA to shapefile
chsa_simple <- cbind(chsa_simple, st_coordinates(st_centroid(chsa_simple))) %>%
  rename(cent_x = "X", cent_y = "Y") %>% arrange(CHSA_Name)


# define zones for cluster anaysis, based on centroids and k nearest neighbours
zones_chsa <- chsa_simple %>%
  select(cent_x, cent_y) %>%
  st_distance() %>%
  dist_to_knn(k = 8) %>% # here, we set zones to include a maximum of 4 nearest HSDAs
  knn_zones()

zones

# Now we run the cluster analysis as before

counts <- sim_counts %>% select(time, location, count, population) %>%
  mutate(count = as.integer(count))

# Use Poisson model for this analysis
cluster_pois_chsa <- scan_pb_poisson(counts = counts, 
                                zones = zones_chsa, 
                                population = NULL,
                                n_mcsim = 999, 
                                max_only = FALSE)
print(cluster_pois_chsa)




# Map cluster scores from Poisson model

# Extract HSDA scores from Poisson cluster analysis 
chsa_scores <- score_locations(cluster_pois_chsa, zones_chsa) %>%
  left_join(sim_counts, by = "location")

# Create a table for plotting
score_map_chsa <- chsa_simple %>% select(CHSA_Name, geometry) %>%
  left_join(chsa_scores, by = c("CHSA_Name" = "chsa"))

tm_shape(score_map_chsa) +
  tm_polygons(col = "relative_score", title = "Cluster score")



