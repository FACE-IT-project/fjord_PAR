# code/0_functions.R
# Functions created for the fjord_PAR project
# This file is meant to be source() loaded in the other scripts
# It contains all of the common packages used throughout the project


# Setup -------------------------------------------------------------------

# devtools::install_github("FACE-IT-project/FjordLight")
library(tidyverse)
library(tidync)
library(FjordLight)
library(doParallel); registerDoParallel(cores = 15)


# Metadata ----------------------------------------------------------------

# Bounding boxes
bbox_EU <- c(-60, 60, 60, 90) # Note that this is intentionally different from the Copernicus definition of c(-25, 60, 66, 90)
bbox_sval <- c(9, 30, 76, 81)
bbox_kong <- c(11, 12.69, 78.86, 79.1); bbox_kong_wide <- c(9.5, 14.0, 78.0, 79.5)
bbox_is <- c(12.97, 17.50, 77.95, 78.90); bbox_is_wide <- c(10.0, 18.0, 77.0, 79.0)
bbox_stor <- c(17.35, 21.60, 77.33, 78.13); bbox_stor_wide <- c(17.0, 22.0, 77.0, 78.5)
bbox_young <- c(-22.367917, -19.907644, 74.210137, 74.624304); bbox_young_wide <- c(-22.5, -17.5, 73.0, 75.5)
bbox_disko <- c(-55.56, -49.55, 68.22, 70.5); bbox_disko_wide <- c(-56.0, -49.0, 68.0, 71.0)
bbox_nuup <- c(-53.32, -48.93, 64.01, 64.8); bbox_nuup_wide <- c(-53.5, -48.5, 63.5, 65.0)
bbox_por <- c(24.5, 27, 70, 71.2); bbox_por_wide <- c(23.5, 28, 69, 72.0)
bbox_trom <- c(17.5, 21.0, 69.0, 70.5)

# Single site bbox for use with points_in_region()
bbox_kong_df <- data.frame(region = "kong", lon = bbox_kong[1:2], lat = bbox_kong[3:4])
bbox_is_df <- data.frame(region = "is", lon = bbox_is[1:2], lat = bbox_is[3:4])
bbox_stor_df <- data.frame(region = "stor", lon = bbox_stor[1:2], lat = bbox_stor[3:4])
bbox_young_df <- data.frame(region = "young", lon = bbox_young[1:2], lat = bbox_young[3:4])
bbox_disko_df <- data.frame(region = "disko", lon = bbox_disko[1:2], lat = bbox_disko[3:4])
bbox_nuup_df <- data.frame(region = "nuup", lon = bbox_nuup[1:2], lat = bbox_nuup[3:4])
bbox_por_df <- data.frame(region = "por", lon = bbox_por[1:2], lat = bbox_por[3:4])
bbox_ALL_df <- rbind(bbox_kong_df, bbox_is_df, bbox_stor_df,
                     bbox_young_df, bbox_disko_df, bbox_nuup_df, bbox_por_df)

# Colour palette for sites
site_colours <- c(
  "Kongsfjorden" = "chocolate4", 
  "Isfjorden" = "chocolate3", 
  "Storfjorden" = "chocolate1", 
  "Young Sound" = "springgreen4", 
  "Qeqertarsuup Tunua" = "springgreen3", 
  "Nuup Kangerlua" = "springgreen1", 
  "Porsangerfjorden" = "plum4"
)

# Long names for merging
long_site_names <- data.frame(site = c("kong", "is", "stor", "young", "disko", "nuup", "por"),
                              site_long = c("Kongsfjorden", "Isfjorden", "Storfjorden",
                                            "Young Sound", "Qeqertarsuup Tunua", "Nuup Kangerlua",
                                            "Porsangerfjorden")) %>% 
  mutate(site_long = factor(site_long,
                            levels = c("Kongsfjorden", "Isfjorden", "Storfjorden",
                                       "Young Sound", "Qeqertarsuup Tunua", "Nuup Kangerlua",
                                       "Porsangerfjorden")))

## Mapping metadata files from other projects
# The base global map - adapted from borders()
map_base <- readRDS("metadata/map_base.Rda")
# Hi-res coastlines - adapted from GSHHG/GSHHS_shp/f/GSHHS_f_L1.shp
# https://www.soest.hawaii.edu/pwessel/gshhg/
# coastline_full <- sf::read_sf("~/pCloudDrive/FACE-IT_data/maps/GSHHG/GSHHS_shp/f/GSHHS_f_L1.shp")
# coastline_full_df <- sfheaders::sf_to_df(coastline_full, fill = TRUE)
# save(coastline_full_df, file = "metadata/coastline_full_df.RData")
if(!exists("coastline_full_df")) load("metadata/coastline_full_df.RData")


# Functions ---------------------------------------------------------------

# Multi-core-able function to extract 3D cubes from PAR NetCDF to combine into 4D
# E.g. Extract a stack of monthly data by year, and then bind all yearly cubes together
# NB: There are 8 months of data (3-10), and 20 years (2003-2022)
monthly_cube <- function(year_val, file_name, var_name, depth_mask){
  coord_mask <- depth_mask |> dplyr::select(lon, lat)
  PAR_brick <- brick(file_name, values = TRUE, varname = var_name, lvar = 4, level = year_val)
  PAR_df <- raster::extract(PAR_brick, coord_mask) |> cbind(depth_mask) |>
    pivot_longer(cols = X3:X10, names_to = "month") |> 
    # NB: Years start at 2003, hence 'year_val+2002'
    mutate(date = as.Date(paste0(year_val+2002,"-",gsub("X", "", month),"-01"))) |> 
    dplyr::select(lon, lat, depth, area, date, value)
  colnames(PAR_df)[6] <- var_name
  return(PAR_df)
}

# Convenience wrapper to load global surface, clim values, and monthly  values
load_PAR <- function(file_name, depth_limit = -50){
  
  # Load global surface data
  PAR_global <- tidync(file_name) |> activate("D0,D1") |>  
    hyper_tibble() |> dplyr::rename(lon = longitude, lat = latitude, depth = bathymetry) |> 
    dplyr::select(lon, lat, depth, area, everything())

  # Depth mask
  depth_mask <- PAR_global |> filter(depth >= depth_limit) |> dplyr::select(lon, lat, depth, area)
  
  # Load clim monthly values
  PAR_clim <- tidync(file_name) |> activate("D0,D1,D2") |>
    tidync::hyper_tibble() |> dplyr::rename(lon = longitude, lat = latitude) |> 
    right_join(depth_mask, by = c("lon", "lat")) |> 
    dplyr::rename(month = Months) |> dplyr::select(lon, lat, depth, area, month, everything())
  
  # Load monthly bottom data
  PAR_bottom <- plyr::ldply(1:20, monthly_cube, .parallel = T,
                            file_name = file_name, var_name = "PARbottom", depth_mask = depth_mask)
    
  # Merge all
  PAR_list <- list(PAR_global = PAR_global,
                   PAR_clim = PAR_clim,
                   PAR_bottom = PAR_bottom)
  
  # Exit
  return(PAR_list)
}

# Convenience wrapper for desired PAR linear model
lm_tidy <- function(df){
  broom::tidy(lm(value ~ Years, data = df))[2,] |> 
    dplyr::select(estimate, std.error, p.value) |> 
    dplyr::rename(slope = estimate)
}

# Function for finding and cleaning up points within a given region polygon
points_in_region <- function(region_in, bbox_df, data_df){
  region_sub <- bbox_df %>% 
    filter(region == region_in)
  distinct_df <- data_df %>%
    dplyr::select(lon, lat) %>%
    distinct()
  coords_in <- distinct_df %>%
    mutate(in_grid = sp::point.in.polygon(point.x = distinct_df[["lon"]], point.y = distinct_df[["lat"]],
                                          pol.x = region_sub[["lon"]], pol.y = region_sub[["lat"]])) %>%
    filter(in_grid >= 1) %>%
    mutate(region = region_in) %>%
    dplyr::select(lon, lat, region)
  return(coords_in)
}

# Convenience wrapper for Figure 1 subplots
# TODO: Constrain colour scale to be the same across all sites
# TODO: Add 50 m isobath as a red line
fig_1_subplot <- function(PAR_df, site_name){
  ggplot(data = PAR_df, aes(x = lon, y = lat)) +
    geom_raster(aes(fill = GlobalPAR0m)) + scale_fill_viridis_c() + coord_quickmap(expand = FALSE) + 
    labs(x = NULL, y = NULL, fill = "PAR\n(mol m-2 d-1)", title = site_name) +
    theme(legend.position = "bottom", 
          panel.border = element_rect(colour = site_colours[site_name], fill  = NA, linewidth = 3))
}
