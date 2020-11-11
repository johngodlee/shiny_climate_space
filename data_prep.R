# Prepare data for use in Shiny miombo app
# John Godlee (johngodlee@gmail.com)
# 2020-11-11

# Packages
library(raster)
library(dplyr)
library(ggplot2)
library(rgdal)
library(ncdf4)
library(seosawr)

# Define input directories
spat_dir <- "/Volumes/seosaw_spat"
dat_dir <- "~/git_proj/seosaw_data/data_out"

# Import data

# Plots 
plots_spatial <- read.csv(file.path(dat_dir, "plots_spatial.csv"))
plots <- read.csv(file.path(dat_dir, "plots.csv"))

# SEOSW region outline 
seosaw_region <- get(data(seosaw_region))

# Bioclim
bioclim <- stack(file.path(spat_dir, "bioclim.vrt"))
mat <- bioclim$bioclim.1
map <- bioclim$bioclim.12

# Population density
pop_density <- raster(file.path(spat_dir, "ppp_2020_1km_Aggregated.tif"))
pop_density[pop_density < 0 | pop_density > 5000] <- NA

# Travel time to city
travel_time <- raster(file.path(spat_dir, "2015_accessibility_to_cities_v1.0.tif"))
travel_time[travel_time < 0] <- NA

# Herbivore total biomass
herbivory <- stack(file.path(spat_dir, "historic.gri"))
total_herbiv <- herbivory$Total
total_herbiv[total_herbiv < 0] <- NA

# Fire frequency
fire_count <- raster(file.path(spat_dir, "AFfrpmean_2001_2018.tif"))
fire_count[fire_count < 0] <- NA

# Frost days
frost_days <- readRDS(file.path(spat_dir, "frs_mean.rds"))
frost_days[frost_days < 0] <- NA

# Make list of raster layers
rast_list <- list(mat, map, pop_density, travel_time, 
  total_herbiv, fire_count, frost_days)

# Crop all rasters to SEOSAW region
crop_list <- lapply(rast_list, function(x) {
  x <- crop(x, seosaw_region)
  mask(x, seosaw_region)
})

# Match extent and resolution of rasters
agg_list <- lapply(crop_list, function(x) { 
  x <- resample(x, fire_count, method = "bilinear")
  aggregate(x, fact=10)
})

sapply(agg_list, res)
plot(agg_list[[1]])
##' 0.01deg

# Stack all rasters
crop_stack <- stack(agg_list)

# Clean names
names(crop_stack) <- c("mat", "map", "pop_density", "travel_time", 
  "total_herbiv", "fire_count", "frost_days")

# Export rasters
saveRDS(crop_stack, "app/data/rast.rds")
rast <- readRDS("app/data/rast.rds")

# Get latitude and longitude for plots
plots_spatial <- left_join(plots_spatial[,c("plot_id", "bio1", "bio12", 
    "pop_density", "travel_time_city", "herbiv_total", "fire_count", "frost_days")],
  plots[,c("plot_id", "longitude_of_centre", "latitude_of_centre")], 
  by = "plot_id") %>% 
  rename(mat = bio1, map = bio12, travel_time = travel_time_city, 
    total_herbiv = herbiv_total, 
    lon = longitude_of_centre, lat = latitude_of_centre)

# Write plot spatial data
saveRDS(plots_spatial, "app/data/plots.rds")

# Get values from all rasters
vals <- as.data.frame(values(rast)) %>% 
  filter(across(everything(), ~!is.na(.x)))

# Write values
saveRDS(vals, "app/data/val.rds")
