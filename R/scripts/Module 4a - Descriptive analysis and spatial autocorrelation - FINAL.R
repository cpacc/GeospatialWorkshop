########################################################################################
# WORKSHOP IN GEOSPATIAL ANALYSIS - Descriptive analysis and spatial autocorrelation
#
# Purpose: To map census based data and describe spatial patterns using measures of
#           spatial autocorrelation.
#
# This script was written using R version 3.6.1 (2019-07-05) -- "Action of the Toes"
#
# Note: The R code below can be collapsed or expanded into sections for easy navigation:
#    collapse code - Alt+O
#    expand code - Shift+Alt+O
#
# Author: Michael Otterstatter
#
# Created: Aug 27, 2019
#
# 
########################################################################################



#### Setup ####

# If using this script for the first time, certain R functions (organized into 'packages')
#   may need to be installed on your computer.  Provided you have an internet connection, the
#   following setup will retrieve and install the necessary functions:

install.packages("checkpoint") # functions to ensure reproducibility


# set working directory
my_workshop_R_folder <- "U:/Personal/CPAC geospatial analysis training/workshop materials/R"
setwd(my_workshop_R_folder)


# Set checkpoint to ensure reproducibility:
library(checkpoint) # Load checkpoint library 
checkpoint("2019-07-01") # set checkpoint date


# Load the installed packages ('libraries') for working with spatial data
library(tidyverse) 
library(spdep) # tools for calculating spatial dependency (spatial correlations)
library(maptools)
library(tmap) 



#### 1. IMPORT DATA ####

# define URL for source file, source filename, and path for saving downloaded files
source_url_da <- "http://www12.statcan.gc.ca/census-recensement/2011/geo/bound-limit/files-fichiers/2016/"
source_url_pop <- "https://www12.statcan.gc.ca/census-recensement/2016/dp-pd/hlt-fst/pd-pl/Tables/CompFile.cfm?Lang=Eng&T=1901&OFT=FULLCSV"
source_file_da <- "lda_000b16a_e.zip"
source_file_pop <- "pop_da.csv"

download_dir <- paste(my_workshop_R_folder, "data/census_2016", sep = "/")


# NOTE these files have already been downloaded and unzipped - skip this section
# ***********************************************************************************************
# download and unzip shape file
download.file(url = paste(source_url_da, source_file_da, sep = "/"), 
              destfile = paste(download_dir, source_file_da, sep = "/"))
unzip(zipfile = paste(download_dir, source_file_da, sep = "/"), exdir = download_dir)

# download population file
download.file(url = source_url_pop, destfile = paste(download_dir, source_file_pop, sep = "/"))

# ***********************************************************************************************


# read unzipped shapefile with st_read() function
shape_file_da <- "lda_000b16a_e.shp"
census_da_bounds_2016 <- st_read(dsn = paste(download_dir, shape_file_da, sep = "/"), stringsAsFactors = FALSE)

# read population size file with read_csv()
census_da_pop_2016 <- read_csv(paste(download_dir, source_file_pop, sep = "/"))

glimpse(census_da_pop_2016)



#### 2. CLEAN DATA ####

# subset census boundary file (aggregate dissemination areas) to include only Vancouver area
da_subset <- census_da_bounds_2016 %>% 
  filter(CDNAME == "Greater Vancouver") %>%
  dplyr::select(DAUID, CDNAME, CSDNAME, geometry)


# select variables of interest from population file and clean-up names
pop_subset <- census_da_pop_2016 %>% 
  dplyr::select(`Geographic code`,`Population, 2016`,`Total private dwellings, 2016`,
         `Population density per square kilometre, 2016`) %>%
  rename(DAUID = `Geographic code`, pop = `Population, 2016`, dwellings = `Total private dwellings, 2016`,
         density = `Population density per square kilometre, 2016`) %>%
  mutate(DAUID = as.character(DAUID))


# link shape file and population information by DA
da_pop <- left_join(da_subset, pop_subset, by = "DAUID")




#### 3. GENERATE MAP ####

# as an example, choose a region (here, Richmond) and create a map

da_pop %>%
  filter(CSDNAME == "Richmond") %>%
  tm_shape() +
  tm_polygons("pop", convert2density = TRUE)


# as examples, try formatting options to customize map 
da_pop %>%
  filter(CSDNAME == "Richmond") %>%
  tm_shape(unit = "km") +
  tm_polygons(col = "density", 
              style = "quantile", palette = "Reds", 
              border.alpha = 0.5, title = "") +
  tm_scale_bar(breaks = 0:5, size = 0.7) +
  tm_compass(type = "4star", position = c("left", "top"), size = 1) + 
  tm_layout(main.title = "Population Density, Richmond, BC (2016 Census)",  
            main.title.size = 0.95, frame = TRUE)




#### 4. CALCULATE SPATIAL AUTOCORRELATION ####

# A common element of descriptive analysis for maps is the calculation of spatial autocorrelation
#   Here, we calculate both global and local measures of spatial autocorrelation

# Using a subset of our data (Richmond CSD only), we convert the sf data object to class sp
#   for calculating autocorrelation
da_pop_richmond <- da_pop %>% filter(CSDNAME == 'Richmond')
da_pop_sp <- as(da_pop_richmond, "Spatial")


## Spatial neighbours and weights ##

#   need to define which DAs are considered 'neighbours' in our data and 
#   assign weights based on neighbouring locations 

#   simply put, neighbours could be those units sharing a border ('rook adjacency'), or
#   those units sharing either a border or a vertex ('queen adjacency')

# define spatial neighbours using poly2nb() function and rook adjacency rules
da_nb <- poly2nb(da_pop_sp, queen = TRUE)

summary(da_nb)


# define spatial weights for neighbouring location (to be used in autocorrelation statistic)
#   using nb2listw() function we specify that neighbour weights must sum to 1.0 for 
#   each DA (style = "W") and that we ignore DAs with no neighbours (zero.policy = TRUE) 
da_wt <- nb2listw(da_nb, style="W", zero.policy = TRUE)


# In order to illustrate spatial autocorrelation, we could examine spatial patterns
#   in population size.  We could ask, does population density cluster?  That is, do 
#   DAs with high population density also tend to have neighbours with high population density?
da_sp_lag_density = lag.listw(da_wt, da_pop_sp$density)


# Looking only at Richmond, visualize the relation between neighbouring population densities 
#   using a Moran scatterplot, which shows DA pop density by average neighbouring pop density
#   (so called 'spatial lag')

ggplot() +
  geom_point(aes(x = da_pop_sp$density, y = da_sp_lag_density)) +
  xlab("DA population density") + ylab("Average neighbouring population density") +
  ggtitle("Moran Scatterplot of DA population density, Richmond BC") 



## Calculate global spatial autocorrelation (Moran's I) ##

# Looking at population density, we can calculate an overall (within Richmond) measure of 
#   spatial autocorrelation called Moran's I.  Moran's I values may be negative or positive, 
#   indicating negative or positive spatial autocorrelation (i.e., dissimilar values tend to be
#   near to one another, or similar values tend to be near to one another, respectively).
moran.test(da_pop_sp$density, da_wt, zero.policy = TRUE)


# similarly, we could generate a Moran's I statistic and p value using Monte Carlo simulation,
#   which is preferable given that standard assumptions of normality are unlikely to hold
moran.mc(da_pop_sp$density, da_wt, zero.policy = TRUE, nsim = 9999)


# we can visualize the global autocorrelation statistic using the Moran's I scatterplot 
moran.plot(da_pop_sp$density, da_wt, zero.policy = TRUE, 
           xlab = "Population density", ylab = "Average neighbouring population density")


# *Note* that the interpretability of Moran's I would change if we were analyzing event counts
#   That is, if larger regional populations tend to be clustered in space, and larger populations
#   have more events, we might expect that event counts will appear clustered as well 
#   (significant Moran's I) simply due to the underlying populations.  One option would be to
#   calculate Moran's I using the regional incidence rates rather than the counts, but this
#   should also be considered carefully (see discussion of these issues in Waller and Gotway, 2004 (Ch 7).



## Calculate local spatial autocorrelation (Getis-Ord Gi*) ##

# Whereas global spatial autocorrelation indicates the presence (or absence) of clustering
#   in the study area, it does not detect where those clusters may be.  Local spatial auto-
#   correlation indicates for each region the similarity in its value (e.g., event count) 
#   to that of its neighbouring regions

# To begin, we need to include each region in the sets of neighbouring regions
da_nb_self <- include.self(da_nb)

# And generate the spatial weights matrix as we did for global spatial autocorrelation
da_nb_self_wt <- nb2listw(da_nb_self, style = "W", zero.policy = TRUE)

# Then calculate our local spatial autocorrelation statistic (Getis-Ord Gi*)
local_g <- localG(da_pop_sp$density, da_nb_self_wt)


# In order to map the local spatial autocorrelation, we add the calculated statistcs 
#   to our spatial data
da_pop_g <- da_pop_richmond %>%
  mutate(localg = as.numeric(local_g))


# And to make these statistics more informative, we can define breaks points in the distribution
#   according to standard z-scores for levels of significance (99%, 95%, 90% CIs)
breaks <- c(min(da_pop_g$localg), -2.58, -1.96, -1.65, 1.65, 1.96, 2.58, max(da_pop_g$localg))

# Now we generate a plot of local spatial autocorrelation defined according to the break points
tm_shape(da_pop_g, unit = "km") +
  tm_polygons(col = "localg", title = "Gi* value", palette = "-RdBu",
              breaks = breaks, border.alpha = 0.3) +
  tm_scale_bar(breaks = 0:5, size = 0.7) +
  tm_compass(type = "4star", position = c("left", "top"), size = 1) + 
  tm_layout(main.title = "Population Density, Richmond, BC (2016 Census)",  main.title.size = 0.95, frame = TRUE)


# We could further improve this by assigning more informative labels to our G statistic, 
#   to indicate hot spots and cold spots in local spatial autocorrelation by level of confidence
da_pop_g <- da_pop_richmond %>% 
  mutate(cluster = cut(local_g, breaks = breaks, include.lowest = TRUE, 
                       labels=c("Cold spot: 99% CI", "Cold spot: 95% CI", "Cold spot: 90% CI", 
                                "Not significant", "Hot spot: 90% CI", "Hot spot: 95% CI", "Hot spot:  99% CI"))) 


# Now we generate a map using these hot and cold spot labels
da_pop_hot_cold <- tm_shape(da_pop_g, unit = "km") +
  tm_polygons(col = "cluster", title = "", palette = "-RdBu",
              breaks = breaks, border.alpha = 0.4) +
  tm_scale_bar(breaks = 0:5, size = 0.7) +
  tm_compass(type = "4star", position = c("left", "top"), size = 1) + 
  tm_layout(main.title = "Population Density, Richmond, BC (2016 Census)",  
            main.title.size = 0.95, frame = TRUE)


da_pop_hot_cold


# As one final way to improve our map, we may wish to view it in the context of an 
#   interactive background map
tmap_mode("view")

da_pop_hot_cold + tm_view(basemaps = "OpenStreetMap")


# *Note* that there are a number of challanges in determining the statistical significance 
#   of local measures of spatial autocorrelation, including the appropriate test distribution
#   and the problems associated with multiple (regional) testing.  Further discussion of these 
#   issues can be found in Waller and Gotway, 2004 (Ch 7).