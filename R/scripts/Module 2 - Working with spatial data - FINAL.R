########################################################################################
# WORKSHOP IN GEOSPATIAL ANALYSIS - MODULE 2 - WORKING WITH SPATIAL DATA
#
# Purpose: To illustrate key apsects of working with spatial data in R.
#
# This script was written using R version 3.6.1 (2019-07-05) -- "Action of the Toes"
#
# Note: The R code below can be collapsed or expanded into sections for easy navigation:
#    collapse code - Alt+O
#    expand code - Shift+Alt+O
#
# Author: Michael Otterstatter
#
# Created: Aug 13, 2019
# 
########################################################################################




#### Resources #### 

# Concepts and functions used here are discussed in detail in the online book
#    'Geocomputation with R' by Lovelace, Nowosad and Muenchow (2019)
#    https://geocompr.robinlovelace.net/
#
# Several R packages are available for working with spatial data.  Here, the
#     focus is on the sf (simple features) package.  For further information 
#     type vignette(package = "sf") in the console to see which vignettes are 
#     available, and type vignette("sf1") for example, see a particular vignette.
#     A significant advantage of the sf package is that it is compatible with the
#     essential data manipulation package 'tidyverse', which we use extensively.
#
# 1. If you are new to R and RStudio, you can review the basics in the online book
#     R for Data Science at:
#    https://r4ds.had.co.nz/workflow-basics.html
#    https://r4ds.had.co.nz/workflow-scripts.html
#
# 2. Throughout, data manipulation is done using functions from the tidyverse package.
#    If you are new to the tidyverse, you can review core functions at:
#    https://r4ds.had.co.nz/transform.html and
#    https://www.rstudio.com/wp-content/uploads/2015/02/data-wrangling-cheatsheet.pdf
#
# 3. At any time, more information on R commands and function can be found in the built-in 
#     R help files.  To access help files on a specific command use '?'  e.g., type '?count' 
#     to find more information on the count() command



#### Setup ####

# When using this script for the first time, certain R functions (organized into 'packages')
#   may need to be installed on your computer.  Provided you have an internet connection, the
#   following lines will retrieve and install the necessary functions:

install.packages("checkpoint") # functions to ensure reproducibility

# We will also make use of certain spatial data, by installing the spDataLarge package 
install.packages('spDataLarge', repos='https://nowosad.github.io/drat/', type='source')


# set working directory
my_workshop_R_folder <- "C:/Users/Michael/Google Drive/CPAC geospatial analysis training/workshop materials/R"
setwd(my_workshop_R_folder)


# Set checkpoint to ensure reproducibility:
#   By defining a 'checkpoint' date, R will use only those versions of R functions available
#   at the specified date, thus ensuring subsequent updates to R will not cause the code to 
#   break.  For more information on checkpoint, see:
#   https://cran.r-project.org/web/packages/checkpoint/vignettes/checkpoint.html

library(checkpoint) # Load checkpoint library 
checkpoint("2019-07-01") # set checkpoint date


# Load the installed packages ('libraries') for working with spatial data
library(sf)          # functions for working with vector data (i.e., points, lines, polygons)
library(raster)      # functions for working with raster data (i.e., pixels or grid cells)
library(spData)        # load geographic data
library(spDataLarge)   # load larger geographic data
library(tidyverse)     # data manipulation tools


#### Notes ####

# Please note the following point related to this particular script

# 1. When we load the sf package, note that it links to external resources used in geospatial analysis 
#     GDAL: Geospatial Data Abstraction Library
#     GEOS: Geometry Engine Open Source
#     PROJ: Cartographic Projections library


# 2. When we load packages in R, any conflicts between packages (i.e., two functions with the 
#    same name) are identified.  For example, R identifies the following conflicts in this script:

#     x tidyr::extract() masks raster::extract()
#     x dplyr::filter()  masks stats::filter()
#     x dplyr::lag()     masks stats::lag()
#     x dplyr::select()  masks raster::select()

#   For these functions, we should always specify the originating package so there is no 
#   confusion about what we are telling R to do.  For example, given the conflict with 
#   the select() function (which exists in both the raster package and the tidyverse's
#   'dplyr' package), we should remember to specify either raster::select() or 
#   dplyr::select() in our code according to which one we wish to use in a particular situation.


# 3. Throughout these scripts we frequently import, download or save files to and
#   ` from various locations.  To make this process easier and more re-usable, the filenames
#     and folder pathnames are separately assigned to R objects and combined/re-used as needed.
#     In this way, if ever we need to change/update filenames and folder paths, we only need to 
#     do so once at the beginning of our code. Also note that R pathnames must use forward slashes
#     "/", not backslashes "\".

#   For example:

#   assign source pathname and source filename for data import:
#     some_source_location <- "C:/Users/Michael/Google Drive" 
#     some_source_filename <- "interesting_shapefile.shp"

#   assign destination pathname and destination filename for data export:
#     my_destination_location <- "C:/Users/Michael/Documents"
#     my_destination_filename <- "my_new_shapfile.shp"

#   import the source file by combining the source pathname and source filename using paste():
#     my_shape_file <- st_read(dsn = paste(some_source_location, some_source_filename, sep = "/"))

#   when finished working with file, write it to the desination location (again, using paste()):
#     st_write(my_shape_file, dsn = paste(my_destination_location, my_destination_filename, sep = "/"))



#### 1. Importing vector data ####

# Here, we use the st_read() function from the sf package to read-in our spatial data 
#   There are several file formats for spatial data, but most can be read by st_read()
#   utilizing GDAL. To see a list of supported file formats, type st_drivers() in the R console 


# Import from a website using a fixed URL

#   As an example, the Statistics Canada census boundaries
#   See: https://www12.statcan.gc.ca/census-recensement/2011/geo/bound-limit/bound-limit-eng.cfm

# define URL for source file, source filename, and path for saving downloaded files
source_url <- "http://www12.statcan.gc.ca/census-recensement/2011/geo/bound-limit/files-fichiers/2016"
source_file_da <- "lda_000b16a_e.zip"
download_dir <- paste(my_workshop_R_folder, "data/census_2016", sep = "/")


# NOTE: skip to this section - the following files have already been saved to our workshop folder
# ***********************************************************************************************

# download and unzip file
download.file(url = paste(source_url, source_file_da, sep = "/"), 
              destfile = paste(download_dir, source_file_da, sep = "/"))
unzip(zipfile = paste(download_dir, source_file_da, sep = "/"), exdir = download_dir)

# ***********************************************************************************************


# read unzipped shapefile with st_read() function
shape_file_da <- "lda_000b16a_e.shp"
census_da_bounds_2016 <- st_read(dsn = paste(download_dir, shape_file_da, sep = "/"), 
                                 stringsAsFactors = FALSE)


# look at contents of census boundary file
#   note the files contains a variety of variables (fields) for each DA unit (features) in Canada
#   the actual geographic information is stored in the 'geometry' variable as a list of 
census_da_bounds_2016
glimpse(census_da_bounds_2016)


# we can see what (if any) coordinate reference system has been used to generate these data with the 
#   st_crs() function.  Key elements of the CRS description include the EPSG code (none for this example),
#   the projection system (Lambert conformal conic projection, "+proj=lcc"), the datum (North American datum, 
#   from 1983, referenced to the center of Earth, "+datum=NAD83"), the origin ("lat_0=63.391 +lon_0=-91.867"),
#   first and second standard parallels (+lat_1=49 +lat_2=77) and units (meter "+units=m"). 
st_crs(census_da_bounds_2016) 


# generate quick plot of DAs for Alberta using the plot() function in base R
census_da_bounds_2016 %>%
  filter(PRNAME == 'British Columbia / Colombie-Britannique') %>%
  select(DAUID, geometry) %>%
  plot()



#### 2. Working With Vector Data ####

# Here we use the previously imported Statistics Canada 2016 Census boundary file,
#   which is a 'simple feature collection' with 56589 features (rows) and 
#   22 fields (variables)
census_da_bounds_2016

# Note the variable definitions given at 
#   https://www150.statcan.gc.ca/n1/pub/92-160-g/2011002/tbl/tbl4.12-eng.htm
#   https://www150.statcan.gc.ca/n1/en/pub/92-160-g/92-160-g2016001-eng.pdf


# (a) Spatial subsetting

# Two methods for generating spatial subsets

# i) using filter() to retain only those rows of interest
#   For example, subset for BC only
bc <- census_da_bounds_2016 %>% 
  filter(PRNAME == "British Columbia / Colombie-Britannique")

bc

#   Or, subset for towns only
towns <- census_da_bounds_2016 %>% 
  filter(CSDTYPE == 'T')

towns

#   Note that mulitple conditions can be specified when filtering, e.g., only
#   BC towns
bc_towns1 <- census_da_bounds_2016 %>% 
  filter(PRNAME == "British Columbia / Colombie-Britannique" & CSDTYPE == 'T')

bc_towns1


# ii) given two spatial (sf) datasets, use the 'target' dataset x 
#   to extract corresponding rows from the 'source' dataset y, 
#   using the notation x[y, ]
bc_towns2 <- towns[bc, ]

bc_towns2


#   Note that this notation also allows one to specify an 'operator', to
#   allow subsetting based on whether features of the two datasets intersect,
#   touch, or lie within one another
bc_towns3 <- towns[bc, , op = st_within]

bc_towns3


# (b) topological relationships

# A number of functions exist for identifying the relationship between spatial objects
#   Generally, one object may occupy the same space as another object ('intersect') or 
#   not ('disjoint), it may have contact with the border of another object ('touches'),
#   it may lie inside the borders of another object ('within'), or be within a certain
#   distance of the borders of another object.


# download 2016 Census Provinces and Designated Places from Statistics Canada
# (Small communities or settlements not otherwise defined as census subdivisions or population centres)
source_url <- "http://www12.statcan.gc.ca/census-recensement/2011/geo/bound-limit/files-fichiers/2016"
source_file_dpl <- "ldpl000b16a_e.zip"
source_file_pr <- "lpr_000b16a_e.zip"

shape_file_dpl <- "ldpl000b16a_e.shp"
shape_file_pr <- "lpr_000b16a_e.shp"


# NOTE - skip this section: these files have already been downloaded and unzipped
# ***********************************************************************************************

download.file(url = paste(source_url, source_file_dpl, sep = "/"), 
              destfile = paste(download_dir, source_file_dpl, sep = "/"))
download.file(url = paste(source_url, source_file_pr, sep = "/"), 
              destfile = paste(download_dir, source_file_pr, sep = "/"))

unzip(zipfile = paste(download_dir, source_file_dpl, sep = "/"), exdir = download_dir)
unzip(zipfile = paste(download_dir, source_file_pr, sep = "/"), exdir = download_dir)

# ***********************************************************************************************


# read boundary files for provinces and designated places
census_provinces_2016 <- st_read(dsn = paste(download_dir, shape_file_pr, sep = "/"))
census_designated_places_2016 <- st_read(dsn = paste(download_dir, shape_file_dpl, sep = "/"))


# generate a quick map of the provincial boundaries
census_provinces_2016 %>%
  select(PRNAME, geometry) %>%
  plot()


# subset provinces to include only BC and subset designated places to select two BC locations
bc <- filter(census_provinces_2016, PRNAME == "British Columbia / Colombie-Britannique")
fanny_bay <- filter(census_designated_places_2016, DPLNAME == "Fanny Bay") 
qualicum_bay <- filter(census_designated_places_2016, DPLNAME == "Qualicum Bay")


# compare locations based on topological relationships

#   - does Fanny Bay 'intersect' (occupy same space) with BC?
st_intersects(fanny_bay, bc, sparse = FALSE) # sparse = FALSE returns logical value

#   - does Fanny Bay NOT 'intersect' (not occupy same space) with BC?
st_disjoint(fanny_bay, bc, sparse = FALSE)

#   - does Fanny Bay 'touch' (have contact with border of) BC?
st_touches(fanny_bay, bc, sparse = FALSE)

#   - does Fanny Bay lie 'within' (lie completely inside borders of) BC?
st_within(fanny_bay, bc, sparse = FALSE)

#   - is Fanny Bay within a given distance of Qualicum Bay (e.g., 10 km)? 
st_is_within_distance(fanny_bay, qualicum_bay, dist = 10000.0, sparse = FALSE)

