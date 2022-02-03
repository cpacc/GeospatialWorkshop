# #########################################################################################
# WORKSHOP IN GEOSPATIAL ANALYSIS - MODULE 5A - ADDING TIME TO SPATIAL MAPPING AND ANALYSIS
#
# Purpose: Illustrate selected approaches to adding time to spatial mapping and analysis.
#
# This script was written using R version 3.6.1 (2019-07-05) -- "Action of the Toes"
#
# Note: The R code below can be collapsed or expanded into sections for easy navigation:
#    collapse code - Alt+O
#    expand code - Shift+Alt+O
#
# Author: Michael Otterstatter
#
# Created: Aug 30, 2019
#
########################################################################################



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
library(tidyverse)      # functions for efficient data manipulation



#### 1. IMPORT DATA ####

# For these examples, we will use BC community health service areas (CHSA) as our geography of interest
#   CHSAs (n=218) are used for community-level analysis of health needs and service provision
#   and were built using 2016 Canada census geographies, with Dissemination Block as the foundation

source_url_chsa <- "https://catalogue.data.gov.bc.ca/dataset/68f2f577-28a7-46b4-bca9-7e9770f2f357/resource/f89f99b0-ca68-41e2-afc4-63fdc0edb666/download"
source_file_chsa <- "chsa_2018.zip"
download_dir_bc <- paste(my_workshop_R_folder, "data/bc_2018", sep="/")


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

# simplify CHSA file for easier processing
chsa_simple <- st_simplify(bc_chsa_bounds_2018, d = 500)



#### 2. PREPARE DATA FOR ANALYSIS ####

# Create example event counts, by region, by time

# simulate counts for CHSAs
names <- levels(as.factor(chsa_simple$CHSA_Name)) # region names (CHSAs) in our geography file
locations <- length(names) # number of regions
time.points <- 9 # choose number of time points

counts <- data.frame(name = names, location = rep(1:locations, time.points)) %>%
  group_by(location) %>%
  mutate(time = row_number(),
         count = runif(1, min = 1, max = 20)) %>% # each region assigned random event count
  group_by(location, time) %>%
  mutate(count = count * (1 + runif(1, min = 0, max = time/2))) %>% # counts set to increase over time
  arrange(location, time)
 

# join geography file with simulated count data 
map_data <- chsa_simple %>% full_join(counts, by = c("CHSA_Name" = "name"))

map_data



#### 3. GENERATE MAPS WITH TIME COMPONENTS ####

# (a) Static maps, faceted for time

# easiest approach is to generate a map with panels (facets) for each time point
#   showing how spatial relationships change over time (or any other variable that 
#   coult be used in panels)

tm_shape(map_data) +
  tm_polygons(col = "count", palette="YlGn") + # choose mapping variable and colour palette
  tm_facets(by = "time", nrow = 3, ncol = 3) + # define facet variable, time, and row x col layout
  tm_legend(show = FALSE) # optionally can drop legend if not needed


# As another example, let's take a subset of the BC map for just Vancouver Island
subset_map <- chsa_simple %>%
  filter(HA_Name == "Vancouver Island")

# An subset the event data as well to include only Vancouver Island, and for only 3 time points
subset_events <- map_data %>% 
  filter(HA_Name == "Vancouver Island") %>%
  filter(time %in% c(1,5,9))

# We can generate a facetted map with proprortional symbols to show the change over time
tm_shape(subset_map) +
  tm_polygons() +
  tm_shape(subset_events) +
  tm_symbols(col = "darkgreen", border.col = "white", size = "count") +
  tm_facets(by = "time", nrow = 1, ncol = 3) +
  tm_legend(show = FALSE)


# (b) Animated maps, for visualizing patterns over time
#     Note: the free tool "ImageMagick" must be installed on your computer for this section 
#     (download from: https://imagemagick.org/index.php)

# A simple way to animate maps is to string together a series of panels into a short 'movie'
#   Here we essentially take a multi-panel (facetted) map and turing it into a gif 

# subset data to Vancouver Island, but keep all time points
subset_events2 = map_data %>% 
  filter(HA_Name == "Vancouver Island") %>%
  filter(time %in% c(1,2,3,4,5,6,7,8,9))

# create multi-panel proportional symbol plot, with facets 'along' the time dimension
map_animated <- tm_shape(subset_map) +
  tm_polygons() +
  tm_shape(subset_events2) +
  tm_dots(size = "count", col = "darkgreen") +
  tm_facets(along = "time") + 
  tm_layout(legend.position = c("left","bottom"))

# animate map using tmap_animation function (creates gif and saves to specified directory)
outdir <- paste(my_workshop_R_folder, "outputs/animated_maps", sep = "/")

tmap_animation(map_animated,
               filename = paste(outdir, "Van_Is_map_animated.gif", sep = "/"),
               width = 800, delay = 40)



# Interactive maps

# In the previous maps of Vancouver Island we could see a temporal pattern, but it was difficult
#   to visualize the details.  Creating an interactive map with flexible base layers provides
#   useful context (e.g., roadmap details) and allows zooming in/out with synchronization across
#   time period

# create vancouver island map as object
van_island_map <- tm_shape(subset_map) +
  tm_polygons() +
  tm_shape(subset_events) +
  tm_symbols(col = "red", border.col = "white", size = "count", alpha = 0.6) +
  tm_facets(by = "time", nrow = 1, ncol = 3)

# switch to viewer mode in tmap
tmap_mode("view")

# now generate map in interactive mode
van_island_map

# we can also specify different base maps, available from
#   http://leaflet-extras.github.io/leaflet-providers/preview/
van_island_map + tm_basemap(server = "OpenTopoMap")

# switch back to regular (non-interactive) plotting mode
tmap_mode("plot")


# It is also possible to generate simple interactive maps using the mapview package
#   note that you can click on a region and see the values contained in the underlying
#   dataset for that region
library(mapview)

mapview(chsa_simple)
