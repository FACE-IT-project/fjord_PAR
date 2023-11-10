# code/0_functions.R
# Functions created for the fjord_PAR project
# This file is meant to be source() loaded in the other scripts
# It contains all of the common packages used throughout the project


# Setup -------------------------------------------------------------------

# devtools::install_github("FACE-IT-project/FjordLight")
library(tidyverse)
library(tidync)
library(raster)
library(FjordLight)
library(doParallel); registerDoParallel(cores = 15)

# Remove scientific notation
options(scipen = 9999)


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
  "Kongsfjorden" = "chocolate3", 
  "Isfjorden" = "goldenrod", 
  "Storfjorden" = "burlywood3", 
  "Young Sound" = "chartreuse3", 
  "Qeqertarsuup Tunua" = "springgreen3", 
  "Nuup Kangerlua" = "palegreen1", 
  "Porsangerfjorden" = "plum3"
)

# Colour palette for sites
site_letters <- c(
  "Kongsfjorden" = "B)", 
  "Isfjorden" = "C)", 
  "Storfjorden" = "D)", 
  "Young Sound" = "E)", 
  "Qeqertarsuup Tunua" = "F)", 
  "Nuup Kangerlua" = "G)", 
  "Porsangerfjorden" = "H)"
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


# Multi-core-able function to extract layers from PAR NetCDF to combine into 3D
filter_3D_cube <- function(var_name, file_name, depth_mask){
  coord_mask <- depth_mask |> dplyr::select(lon, lat)
  PAR_brick <- raster::brick(file_name, values = TRUE, varname = var_name)
  PAR_df <- raster::extract(PAR_brick, coord_mask) |> cbind(depth_mask) |>
    pivot_longer(cols = c(-lon, -lat, -depth, -area), names_to = "date") |> 
    filter(!is.na(value)) |> 
    mutate(date = as.numeric(gsub("X", "", date))) |> 
    dplyr::select(lon, lat, depth, area, date, value)
  colnames(PAR_df)[6] <- var_name
  return(PAR_df)
}

# Multi-core-able function to extract 3D cubes from PAR NetCDF to combine into 4D
# E.g. Extract a stack of monthly data by year, and then bind all yearly cubes together
# NB: There are 8 months of data (3-10), and 20 years (2003-2022)
filter_4D_cube <- function(year_val, file_name, var_name, depth_mask){
  coord_mask <- depth_mask |> dplyr::select(lon, lat)
  PAR_brick <- raster::brick(file_name, values = TRUE, varname = var_name, lvar = 4, level = year_val)
  PAR_df <- raster::extract(PAR_brick, coord_mask) |> cbind(depth_mask) |>
    pivot_longer(cols = X3:X10, names_to = "month") |> 
    filter(!is.na(value)) |> 
    # NB: Years start at 2003, hence 'year_val+2002'
    mutate(date = as.Date(paste0(year_val+2002,"-",gsub("X", "", month),"-01"))) |> 
    dplyr::select(lon, lat, depth, area, date, value)
  colnames(PAR_df)[6] <- var_name
  return(PAR_df)
}

# Convenience wrapper to load global surface, clim values, and monthly  values
load_PAR <- function(site_name_short, depth_limit = -50){
  
  # Find file name
  file_name <- file_name_search(site_name_short)
  
  # Load global surface data
  PAR_global <- tidync(file_name) |> activate("D0,D1") |>  
    hyper_tibble() |> dplyr::rename(lon = longitude, lat = latitude) |> 
    dplyr::select(lon, lat, depth, area, everything()) |> 
    filter(!is.na(depth))
  
  # Depth mask
  depth_limit_mask <- PAR_global |> filter(depth >= depth_limit) |> dplyr::select(lon, lat, depth, area)
  depth_no_mask <- PAR_global |> dplyr::select(lon, lat, depth, area)
  
  # Load annual values
  PAR_YearlyPAR0m <- filter_3D_cube("YearlyPAR0m", file_name, depth_no_mask)
  PAR_YearlyKpar <- filter_3D_cube("YearlyKpar", file_name, depth_no_mask)
  PAR_YearlyPARbottom <- filter_3D_cube("YearlyPARbottom", file_name, depth_no_mask)
  PAR_annual <- left_join(PAR_YearlyPAR0m, PAR_YearlyKpar, by = c("lon", "lat", "depth", "area", "date")) |> 
    left_join(PAR_YearlyPARbottom, by = c("lon", "lat", "depth", "area", "date")) |> rename(year = date)
  rm(PAR_YearlyPAR0m, PAR_YearlyKpar, PAR_YearlyPARbottom); gc()
  
  # Load clim monthly values
  PAR_ClimPAR0m <- filter_3D_cube("ClimPAR0m", file_name, depth_no_mask)
  PAR_ClimKpar <- filter_3D_cube("ClimKpar", file_name, depth_no_mask)
  PAR_ClimPARbottom <- filter_3D_cube("ClimPARbottom", file_name, depth_no_mask)
  PAR_clim <- left_join(PAR_ClimPAR0m, PAR_ClimKpar, by = c("lon", "lat", "depth", "area", "date")) |> 
    left_join(PAR_ClimPARbottom, by = c("lon", "lat", "depth", "area", "date")) |> rename(month = date)
  rm(PAR_ClimPAR0m, PAR_ClimKpar, PAR_ClimPARbottom); gc()
  
  # Load monthly bottom data
  # NB: Some datasets are too beefy to load on many cores in parallel
  registerDoParallel(cores = 5)
  PAR_monthly <- plyr::ldply(1:20, filter_4D_cube, .parallel = T,
                            file_name = file_name, var_name = "MonthlyPARbottom", depth_mask = depth_limit_mask)
    
  # Merge all
  PAR_list <- list(PAR_global = PAR_global,
                   PAR_annual = PAR_annual,
                   PAR_clim = PAR_clim,
                   PAR_monthly = PAR_monthly)
  
  # Exit
  return(PAR_list)
  # rm(file_name, depth_limit_mask, depth_no_mask, PAR_global, PAR_annual, PAR_clim, PAR_monthly, PAR_list)
}

# Takes site short name and returns the local or pCloud location of the PAR NetCDF
file_name_search <- function(site_name_short){
  if(file.exists(paste0("data/PAR/",site_name_short,".nc"))) {
    file_name <- paste0("data/PAR/",site_name_short,".nc")
  } else if(file.exists(paste0("~/pCloudDrive/FACE-IT_data/PAR/",site_name_short,".nc"))) {
    file_name <- paste0("~/pCloudDrive/FACE-IT_data/PAR/",site_name_short,".nc")
  }
  return(file_name)
}

# Multi-core this to quickly load+merge global p functions for all sites
load_p_global <- function(site_name_short){
  
  # Find file name
  file_name <- file_name_search(site_name_short)
  
  # Load data
  tidync::tidync(file_name) |> tidync::activate("D4") |> tidync::hyper_tibble() |> 
    mutate(site = long_site_names$site_long[long_site_names$site == site_name_short], .before = 1)
}

# Multi-core this to quickly load+merge monthly clim p functions for all sites
load_p_clim <- function(site_name_short){
  
  # Find file name
  file_name <- file_name_search(site_name_short)
  
  # Load data
  tidync::tidync(file_name) |> tidync::activate("D4,D2") |> tidync::hyper_tibble() |> 
    mutate(site = long_site_names$site_long[long_site_names$site == site_name_short], .before = 1)
}

# Multi-core this to quickly load+merge monthly clim p functions for all sites
load_p_annual <- function(site_name_short){
  
  # Find file name
  file_name <- file_name_search(site_name_short)

  # Load data
  tidync::tidync(file_name) |> tidync::activate("D4,D3") |> tidync::hyper_tibble() |> 
    mutate(site = long_site_names$site_long[long_site_names$site == site_name_short], .before = 1)
}

# Run this to quickly load and merge all annual values
load_all_annual <- function(){
  
  # Find file name
  file_name <- file_name_search(site_name_short)
  
  # Load data
  tidync::tidync(file_name) |> tidync::activate("D4,D3") |> tidync::hyper_tibble() |> 
    mutate(site = long_site_names$site_long[long_site_names$site == site_name_short], .before = 1)
}

# Summaries of PAR data
PAR_summarise <- function(PAR_df, site_name = NULL){
  
  # Determine grouping column(s)
  if("year" %in% colnames(PAR_df)) {
    group_col <- "year"
  } else if("month" %in% colnames(PAR_df)) {
    group_col <- "month"
  } else if("date" %in% colnames(PAR_df)) {
    group_col <- c("year", "month")
    PAR_df <- mutate(PAR_df, month = month(date), year = year(date)) |> dplyr::select(-date)
  }
  
  # Summarise data
  PAR_summary <- PAR_df |> 
    dplyr::select(-lon, -lat, -depth, -area) |> 
    pivot_longer(cols = -all_of(group_col), names_to = "variable") |> 
    summarise(min = min(value, na.rm = T),
              q10 = quantile(value, 0.1, na.rm = T),
              q50 = quantile(value, 0.5, na.rm = T),
              mean = mean(value, na.rm = T),
              q90 = quantile(value, 0.9, na.rm = T),
              max = max(value, na.rm = T),
              .by = c(variable, all_of(group_col))) |> 
    mutate(across(min:max, \(x) round(x, 4)))
  
  # Add site name
  if(!is.null(site_name)){
    PAR_summary <- PAR_summary |> 
      mutate(site = site_name, .before = variable)
  }
  return(PAR_summary)
  # rm(PAR_df, group_col, PAR_summary)
}

# Summarise the spatial area of pixels with a given PAR threshold
# NB: depth_limit is only applied to global values
# It is assumed that the depth limit was applied to the other layers upon loading
PAR_spat_sum <- function(PAR_list, site_name = NULL, PAR_thresh = 0.13, depth_limit = -50){

  # Are of fjord >= depth_limit
  bottom_area <- PAR_list$PAR_global |> 
    filter(depth >= depth_limit) |> 
    summarise(total = sum(area))
  
  # Global
  PAR_spat_global <- PAR_list$PAR_global |> 
    filter(depth >= depth_limit, GlobalPARbottom >= PAR_thresh) |> 
    summarise(global_area = sum(area, na.rm = T))
  
  # Annual
  PAR_spatial <- PAR_list$PAR_annual |> 
    filter(YearlyPARbottom >= PAR_thresh) |> 
    summarise(annual_area = sum(area, na.rm = T), .by = "year") |> 
    mutate(global_area = PAR_spat_global$global_area,
           bottom_area = bottom_area$total,
           annual_perc = annual_area/bottom_area,
           global_perc = global_area/bottom_area) |> 
    arrange(year)
  
  # Add site name
  if(!is.null(site_name)){
    PAR_spatial <- PAR_spatial |> 
      mutate(site = site_name, .before = year)
  }
  return(PAR_spatial)
  # rm(PAR_list, site_name, PAR_thresh, depth_limit, bottom_area, PAR_spat_global, PAR_spatial); gc()
}

# Convenience wrapper for desired PAR linear model
# NB: Requires time variable column to be labeled 't'
lm_tidy <- function(df){
  broom::tidy(lm(value ~ t, data = df))[2,] |> 
    dplyr::select(estimate, std.error, p.value) |> 
    dplyr::rename(slope = estimate)
}

# Convenience function for 'calc_p_function'
calc_p_step <- function(PAR_limit, PAR_df, bot_area){
  
  # Determine grouping column(s)
  if("year" %in% colnames(PAR_df)) {
    group_col <- "year"
  } else if("month" %in% colnames(PAR_df)) {
    group_col <- "month"
  } else if("date" %in% colnames(PAR_df)) {
    group_col <- c("year", "month")
    PAR_df <- mutate(PAR_df, month = month(date), year = year(date)) |> dplyr::select(-date)
  } else {
    group_col <- c("global")
    PAR_df <- mutate(PAR_df, global = "global")
  }
  
  PAR_df |> 
    filter(value >= PAR_limit) |> 
    summarise(limit_area = sum(area, na.rm = T),
              .by = all_of(group_col)) |> 
    mutate(limit_perc = limit_area/bot_area,
           PAR_limit = PAR_limit)
}

# Calculate P-functions based on a given depth limit
calc_p_function <- function(PAR_list, depth_limit = -50, site_name){
  
  # Get the area of the fjord >= depth limit
  bottom_area <- PAR_list$PAR_global |> 
    filter(depth >= depth_limit) |> 
    summarise(total = sum(area))
  
  # Set the sequence to use for p-functions
  p_seq <- c(seq(0.001, 0.009, 0.001), seq(0.01, 0.09, 0.01), seq(0.1, 0.9, 0.1), seq(1, 10, 1))
  
  # Calculate global P-functions
  registerDoParallel(cores = 15)
  PAR_global <- PAR_list$PAR_global |> 
    pivot_longer(GlobalPARbottom:GlobalKpar) |> 
    filter(name == "GlobalPARbottom", depth >= depth_limit)
  PAR_global_p <- plyr::ldply(p_seq, calc_p_step, .parallel = T,
                              PAR_df = PAR_global, bot_area = bottom_area$total) |> 
    dplyr::rename(global_area = limit_area, global_perc = limit_perc) |> 
    dplyr::select(-global)
 
  # Calculate annual P-functions
  registerDoParallel(cores = 15)
  PAR_annual <- PAR_list$PAR_annual |> 
    pivot_longer(YearlyPAR0m:YearlyPARbottom) |> 
    filter(name == "YearlyPARbottom", depth >= depth_limit)
  PAR_annual_p <- plyr::ldply(p_seq, calc_p_step, .parallel = T,
                              PAR_df = PAR_annual, bot_area = bottom_area$total) |> 
    dplyr::rename(yearly_area = limit_area, yearly_perc = limit_perc)
  
  # Calculate clim P-functions
  registerDoParallel(cores = 15)
  PAR_clim <- PAR_list$PAR_clim |> 
    pivot_longer(ClimPAR0m:ClimPARbottom) |> 
    filter(name == "ClimPARbottom", depth >= depth_limit)
  PAR_clim_p <- plyr::ldply(p_seq, calc_p_step, .parallel = T,
                            PAR_df = PAR_clim, bot_area = bottom_area$total) |> 
    dplyr::rename(clim_area = limit_area, clim_perc = limit_perc)
  
  # Calculate monthly P-functions
  registerDoParallel(cores = 5)
  PAR_monthly <- PAR_list$PAR_monthly |> 
    pivot_longer(MonthlyPARbottom) |> 
    filter(name == "MonthlyPARbottom", depth >= depth_limit)
  PAR_monthly_p <- plyr::ldply(p_seq, calc_p_step, .parallel = T,
                               PAR_df = PAR_monthly, bot_area = bottom_area$total) |> 
    dplyr::rename(monthly_area = limit_area, monthly_perc = limit_perc)
  
  # Combine and exit
  PAR_p <- left_join(PAR_monthly_p, PAR_annual_p, by = c("PAR_limit", "year")) |> 
    left_join(PAR_clim_p, by = c("PAR_limit", "month")) |> 
    left_join(PAR_global_p, by = "PAR_limit") |> 
    mutate(site = site_name) |>
    dplyr::select(site, PAR_limit, year, month, everything()) |> 
    mutate(across(monthly_area:global_perc, \(x) round(x, 4)))
  return(PAR_p)
  # rm(PAR_df)
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

# Plots global surface, monthly clim, and annual surface PAR values with full range to highlight artefacts
plot_surface <- function(site_short, bathy_opt = "c"){
  
  # Load all data except monthly bottom PAR
  file_name <- paste0("data/PAR/",site_short,".nc")
  PAR_list <- fl_LoadFjord(site_short, "data/PAR")
  
  # Get long site name
  # site_label <- filter(long_site_names, site == site_short)
  
  # Get bathymetry
  PAR_area <- flget_area(PAR_list, mode = "df") |> 
    dplyr::rename(lon = longitude, lat = latitude, area = PixArea_km2)
  PAR_bathy <- flget_bathymetry(PAR_list, what = bathy_opt, mode = "df") |> 
    dplyr::rename(lon = longitude, lat = latitude) |> filter(!is.na(depth)) |> 
    left_join(PAR_area, by = c("lon", "lat"))
  
  # Global surface
  PAR_global <- flget_climatology(PAR_list, optics = "PAR0m", period = "Global", mode = "df") |> 
    dplyr::rename(lon = longitude, lat = latitude) |> filter(!is.na(PAR0m_Global))
  surf_global <- ggplot(data = PAR_global, aes(x = lon, y = lat)) +
    geom_raster(aes(fill = PAR0m_Global)) + scale_fill_viridis_c() +
    coord_quickmap(expand = FALSE) + 
    labs(x = NULL, y = NULL, fill = "PAR\n[mol m-2 d-1]", title = "Global Surface PAR") +
    theme(panel.border = element_rect(color = "black", fill = NA))
  
  # Monthly clim surface
  PAR_ClimPAR0m <- filter_3D_cube("ClimPAR0m", file_name, PAR_bathy)
  surf_clim <- ggplot(data = PAR_ClimPAR0m, aes(x = lon, y = lat)) +
    geom_raster(aes(fill = ClimPAR0m)) + scale_fill_viridis_c() +
    coord_quickmap(expand = FALSE) + 
    labs(x = NULL, y = NULL, fill = "PAR\n[mol m-2 d-1]", title = "Monthly Clim Surface PAR") +
    facet_wrap(~date) +
    theme(panel.border = element_rect(color = "black", fill = NA))
  
  # Annual surface
  PAR_YearlyPAR0m <- filter_3D_cube("YearlyPAR0m", file_name, PAR_bathy)
  surf_ann <- ggplot(data = PAR_YearlyPAR0m, aes(x = lon, y = lat)) +
    geom_raster(aes(fill = YearlyPAR0m)) + scale_fill_viridis_c() +
    coord_quickmap(expand = FALSE) + 
    labs(x = NULL, y = NULL, fill = "PAR\n[mol m-2 d-1]", title = "Annual Surface PAR") +
    facet_wrap(~date) +
    theme(panel.border = element_rect(color = "black", fill = NA))
  
  # Save .CSV files of the lon/lat for suspicious pixels
  if(site_short %in% c("disko", "nuup", "por")) {
    filter_months <- 5:7
  } else {
    filter_months <- 6:7
  }
  ex_PAR_global <- filter(PAR_global, PAR0m_Global > 30) |> 
    dplyr::rename(value = PAR0m_Global) |> 
    mutate(var = "PAR0m_Global", date = as.numeric(NA))
  ex_PAR_Clim <- filter(PAR_ClimPAR0m, ClimPAR0m > 30, !date %in% filter_months) |> 
    dplyr::rename(value = ClimPAR0m) |> mutate(var = "YearlyPAR0m")
  ex_PAR_Yearly <- filter(PAR_YearlyPAR0m, YearlyPAR0m > 30) |> 
    dplyr::rename(value = YearlyPAR0m) |> mutate(var = "YearlyPAR0m")
  ex_PAR <- bind_rows(ex_PAR_global, ex_PAR_Clim, ex_PAR_Yearly) |> 
    dplyr::select(lon, lat, depth, date, var, value)
  write_csv(ex_PAR, paste0("metadata/ex_PAR_",site_short,".csv"))
  
  # Combine and save
  surf_ALL <- ggpubr::ggarrange(surf_global, surf_clim, surf_ann,
                                ncol = 1, nrow = 3, heights = c(1.1, 1.17, 0.97))
  ggsave(paste0("metadata/surface_PAR_",site_short,".png"), surf_ALL,  width = 17, height = 40)
  # rm(site_short, file_name, PAR_list, PAR_area, PAR_bathy,
  #    PAR_global, PAR_ClimPAR0m, PAR_YearlyPAR0m, surf_global, surf_clim, surf_ann, surf_ALL)
}

# Convenience wrapper for Figure 1 subplots
fig_1_subplot <- function(PAR_df, site_name, PAR_limits){
  # Prep data
  PAR_df <- PAR_df |> 
    filter(!is.na(PAR0m_Global)) |> 
    mutate(PAR0m_Global = case_when(PAR0m_Global > max(PAR_limits) ~ max(PAR_limits),
                                    PAR0m_Global < min(PAR_limits) ~ min(PAR_limits),
                                    TRUE ~ PAR0m_Global))
  # Get title
  panel_title <- paste0(site_letters[site_name]," ",site_name)
  
  ggplot(data = PAR_df, aes(x = longitude, y = latitude)) +
    # NB: Ignore geom_raster warning because geom_tile looks bad
    geom_raster(aes(fill = PAR0m_Global)) + 
    # scale_fill_viridis_c() +
    scale_fill_viridis_c(limits = PAR_limits) +
    coord_quickmap(expand = FALSE) + 
    labs(x = NULL, y = NULL, fill = latex2exp::TeX("PAR($0^-$)\n[mol photons $m^{-2}$ $d^{-1}$]"), title = panel_title) +
    theme(legend.position = "none", # Remove legend
          axis.text = element_blank(), axis.ticks = element_blank(), # Remove coords
          panel.background = element_rect(fill = "grey40"), # Background colour
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(), # Remove axis lines
          panel.border = element_rect(colour = site_colours[site_name], fill  = NA, linewidth = 5))
}
