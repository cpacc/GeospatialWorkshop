########################################################################################
# WORKSHOP IN GEOSPATIAL ANALYSIS - MODULE 1 - INTRODUCTORY EXAMPLE
#
# Purpose: Brief example of mapping public health data in R
#
# Example based on Displaying Time Series, Spatial, and Space-Time Data with R by 
#               Oscar Perpinan Lamigueiro (2014)
#
# This script was written using R version 3.6.1 (2019-07-05) -- "Action of the Toes"
#
# Note: The R code below can be collapsed or expanded into sections for easy navigation:
#    collapse code - Alt+O
#    expand code - Shift+Alt+O
#
# Author: Michael Otterstatter
#
# Created: September, 14 2019
#
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
#   By defining a 'checkpoint' date, R will use only those versions of R functions available
#   at the specified date, thus ensuring subsequent updates to R will not cause the code to 
#   break.  For more information on checkpoint, see:
#   https://cran.r-project.org/web/packages/checkpoint/vignettes/checkpoint.html

library(checkpoint) # Load checkpoint library 
checkpoint("2019-07-01") # set checkpoint date

# packages for working with spatial data and mapping
library(sp)
library(sf)
library(tmap)
library(gstat)



#### Import data ####

# here we use example air quality (NO2 concentration) data from Madrid
#   from Lamigueiro (2014): https://github.com/oscarperpinan/spacetime-vis
load('data/no2_data/NO2sp.RData')

# data are stored as a SpatialPointsDataFrame using the sp package
class(NO2sp)

# we convert this sp data object to the more modern sf data object for mapping
NO2sf <- st_as_sf(NO2sp)

# examine spatial dataset
NO2sf



#### Create map ####

# define a colour palette (5 colours interpolated along a gradient) for plotting
airPal <- colorRampPalette(c('springgreen1', 'sienna3', 'gray5'))(5)

# for this example we will generate a map in the view mode of tmap, which 
#   automatically provides use background layers (street, topology) 
tmap_mode("view")

# generate a proportional symbol map with symbol colour and size representing
#   differences in NO2 concentration
air_qual_map <- tm_shape(NO2sf) +
  tm_dots(col = "mean", size = "mean", title = "NO2 Concentration",
          alpha = 0.75, palette = airPal, legend.size.show = FALSE) 

air_qual_map



#### Save output ####

# specify output file path and filename
outdir <- paste(my_workshop_R_folder, "outputs/intro_example", sep = "/")
outfile <- "air_quality_madrid.html"

# save interactive map as html file
tmap_save(tm = air_qual_map, 
          filename = paste(outdir, outfile, sep = "/"))


  
