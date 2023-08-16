# code/2_analyse.R
# Run the various analyses of the spatial data


# Setup -------------------------------------------------------------------

source("code/0_functions.R")


# Load data ---------------------------------------------------------------

# This loads bathymetry, pixel area, and all forms of PAR and Kd
# NB: This takes +15 minutes
PAR_kong <- load_PAR("kong")
PAR_is <- load_PAR("is")
PAR_stor <- load_PAR("stor")
PAR_young <- load_PAR("young")
PAR_disko <- load_PAR("disko")
PAR_nuup <- load_PAR("nuup")
PAR_por <- load_PAR("por")


# Extract bathymetry ------------------------------------------------------

# NB: Not currently necessary because all data already have bathy joined to them
# bathy_kong <- PAR_kong$PAR_global |> dplyr::select(lon, lat, depth, area) |> distinct()
# bathy_is <- PAR_is$PAR_global |> dplyr::select(lon, lat, depth, area) |> distinct()
# bathy_stor <- PAR_stor$PAR_global |> dplyr::select(lon, lat, depth, area) |> distinct()
# bathy_young <- PAR_young$PAR_global |> dplyr::select(lon, lat, depth, area) |> distinct()
# bathy_disko <- PAR_disko$PAR_global |> dplyr::select(lon, lat, depth, area) |> distinct()
# bathy_nuup <- PAR_nuup$PAR_global |> dplyr::select(lon, lat, depth, area) |> distinct()
# bathy_por <- PAR_por$PAR_global |> dplyr::select(lon, lat, depth, area) |> distinct()


# Annual analyses ---------------------------------------------------------

# Get range of summary states per time step
PAR_kong_annual_summary <- PAR_summarise(PAR_kong$PAR_annual, "kong")
PAR_is_annual_summary <- PAR_summarise(PAR_is$PAR_annual, "is")
PAR_stor_annual_summary <- PAR_summarise(PAR_stor$PAR_annual, "stor")
PAR_young_annual_summary <- PAR_summarise(PAR_young$PAR_annual, "young")
PAR_disko_annual_summary <- PAR_summarise(PAR_disko$PAR_annual, "disko")
PAR_nuup_annual_summary <- PAR_summarise(PAR_nuup$PAR_annual, "nuup")
PAR_por_annual_summary <- PAR_summarise(PAR_por$PAR_annual, "por")

# Combine and save
PAR_annual_summary <- rbind(PAR_kong_annual_summary, PAR_is_annual_summary, PAR_stor_annual_summary,
                            PAR_young_annual_summary, PAR_disko_annual_summary, PAR_nuup_annual_summary,
                            PAR_por_annual_summary)
save(PAR_annual_summary, file = "data/PAR_annual_summary.RData")
rm(PAR_kong_annual_summary, PAR_is_annual_summary, PAR_stor_annual_summary,
   PAR_young_annual_summary, PAR_disko_annual_summary, PAR_nuup_annual_summary,
   PAR_por_annual_summary); gc()


# Monthly clim analyses ---------------------------------------------------

# Get range of summary states per time step
PAR_kong_clim_summary <- PAR_summarise(PAR_kong$PAR_clim, "kong")
PAR_is_clim_summary <- PAR_summarise(PAR_is$PAR_clim, "is")
PAR_stor_clim_summary <- PAR_summarise(PAR_stor$PAR_clim, "stor")
PAR_young_clim_summary <- PAR_summarise(PAR_young$PAR_clim, "young")
PAR_disko_clim_summary <- PAR_summarise(PAR_disko$PAR_clim, "disko")
PAR_nuup_clim_summary <- PAR_summarise(PAR_nuup$PAR_clim, "nuup")
PAR_por_clim_summary <- PAR_summarise(PAR_por$PAR_clim, "por")

# Combine and save
PAR_clim_summary <- rbind(PAR_kong_clim_summary, PAR_is_clim_summary, PAR_stor_clim_summary,
                          PAR_young_clim_summary, PAR_disko_clim_summary, PAR_nuup_clim_summary,
                          PAR_por_clim_summary)
save(PAR_clim_summary, file = "data/PAR_clim_summary.RData")
rm(PAR_kong_clim_summary, PAR_is_clim_summary, PAR_stor_clim_summary,
   PAR_young_clim_summary, PAR_disko_clim_summary, PAR_nuup_clim_summary,
   PAR_por_clim_summary); gc()


# Monthly analyses --------------------------------------------------------

# Get range of summary states per time step
PAR_kong_monthly_summary <- PAR_summarise(PAR_kong$PAR_monthly, "kong")
PAR_is_monthly_summary <- PAR_summarise(PAR_is$PAR_monthly, "is")
PAR_stor_monthly_summary <- PAR_summarise(PAR_stor$PAR_monthly, "stor")
PAR_young_monthly_summary <- PAR_summarise(PAR_young$PAR_monthly, "young")
PAR_disko_monthly_summary <- PAR_summarise(PAR_disko$PAR_monthly, "disko")
PAR_nuup_monthly_summary <- PAR_summarise(PAR_nuup$PAR_monthly, "nuup")
PAR_por_monthly_summary <- PAR_summarise(PAR_por$PAR_monthly, "por")

# Combine and save
PAR_monthly_summary <- rbind(PAR_kong_monthly_summary, PAR_is_monthly_summary, PAR_stor_monthly_summary,
                             PAR_young_monthly_summary, PAR_disko_monthly_summary, PAR_nuup_monthly_summary,
                             PAR_por_monthly_summary)
save(PAR_monthly_summary, file = "data/PAR_monthly_summary.RData")
rm(PAR_kong_monthly_summary, PAR_is_monthly_summary, PAR_stor_monthly_summary,
   PAR_young_monthly_summary, PAR_disko_monthly_summary, PAR_nuup_monthly_summary,
   PAR_por_monthly_summary); gc()


# Spatial analyses --------------------------------------------------------

# Get area summary per time step
PAR_kong_spatial_summary <- PAR_spat_sum(PAR_kong, "kong")
PAR_is_spatial_summary <- PAR_spat_sum(PAR_is, "is")
PAR_stor_spatial_summary <- PAR_spat_sum(PAR_stor, "stor")
PAR_young_spatial_summary <- PAR_spat_sum(PAR_young, "young")
PAR_disko_spatial_summary <- PAR_spat_sum(PAR_disko, "disko")
PAR_nuup_spatial_summary <- PAR_spat_sum(PAR_nuup, "nuup")
PAR_por_spatial_summary <- PAR_spat_sum(PAR_por, "por")

# Combine and save
PAR_spatial_summary <- rbind(PAR_kong_spatial_summary, PAR_is_spatial_summary, PAR_stor_spatial_summary,
                             PAR_young_spatial_summary, PAR_disko_spatial_summary, PAR_nuup_spatial_summary,
                             PAR_por_spatial_summary)
save(PAR_spatial_summary, file = "data/PAR_spatial_summary.RData")
rm(PAR_kong_spatial_summary, PAR_is_spatial_summary, PAR_stor_spatial_summary,
   PAR_young_spatial_summary, PAR_disko_spatial_summary, PAR_nuup_spatial_summary,
   PAR_por_spatial_summary); gc()

# Calculate p-values of linear models on annual bottom PAR as a full time series
PAR_lm_annual <- PAR_spatial_summary |> 
  dplyr::select(site, year, annual_perc) |>  distinct() |> 
  dplyr::rename(t = year, value = annual_perc) |> 
  plyr::ddply(c("site"), lm_tidy, .parallel = T)
save(PAR_lm_annual, file = "data/PAR_lm_annual.RData")


# P-functions -------------------------------------------------------------

# TODO: Need to calculate p-functions based on a depth limit of 50 m

# Get surface areas from FjordLight package
# as a function
fG <- flget_Pfunction(fjorddata, "Global", plot = FALSE)
# then you can use it; for instance :
irradiance_levels <- c(0.1, 1, 10)
fG(irradiance_levels)

# As a 2 column data.frame
f2012 <- flget_Pfunction(fjorddata, "Yearly", year = 2012, mode = "2col")
fglobal <- flget_Pfunction(fjorddata, "Global", mode = "2col")

# Plot
flget_Pfunction(fjorddata, "Global", PLOT = TRUE, lty = 1, col = 1, lwd = 2, Main = paste(fjord, "P-functions"), ylim = c(0, 50))

# Manual calculation
kong_surf_total <- sum(kong_res$sq_area)
kong_surf_sea <- kong_res |> 
  filter(depth <= 0) |> 
  summarise(sq_area = sum(sq_area, na.rm = T))
kong_surf_sea
kong_surf_coast <- kong_res |> 
  filter(depth >= -200, depth <= 0) |> 
  summarise(sq_area = sum(sq_area, na.rm = T))
kong_surf_coast

# Manual proportions of surface > 10 PAR
kong_global_coast_PAR10 <- PBglobal |> 
  dplyr::rename(lon = longitude, lat = latitude) |> 
  left_join(kong_res) |> 
  filter(depth >= -200, depth <= 0) |> 
  filter(PARbottom_Global >= 10) |>
  summarise(sq_area = sum(sq_area, na.rm = T))
kong_global_coast_PAR10/kong_surf_coast

# Manual proportions of surface > 0.1 PAR
kong_global_coast_PAR0.1 <- PBglobal |> 
  dplyr::rename(lon = longitude, lat = latitude) |> 
  left_join(kong_res) |> 
  filter(depth >= -200, depth <= 0) |> 
  filter(PARbottom_Global >= 0.1) |> 
  summarise(sq_area = sum(sq_area, na.rm = T))
kong_global_coast_PAR0.1/kong_surf_coast


# Get pixels in regions ---------------------------------------------------

# Subset high-res coastline
coastline_kong_wide <- coastline_full_df %>% 
  filter(x >= bbox_kong[1]-1, x <= bbox_kong[2]+1,
         y >= bbox_kong[3]-1, y <= bbox_kong[4]+1) %>% 
  dplyr::select(x, y, polygon_id) %>% 
  dplyr::rename(lon = x, lat = y)
coastline_kong <- coastline_full_df %>% 
  filter(x >= bbox_kong[1], x <= bbox_kong[2],
         y >= bbox_kong[3], y <= bbox_kong[4]) %>% 
  dplyr::select(x, y) %>% 
  dplyr::rename(lon = x, lat = y)

# Manually create regions
kong_inner <- coastline_kong[270,] %>% 
  rbind(data.frame(lon = c(12.36, 12.65, 12.65), lat = c(78.86, 78.86, 79.01958))) %>% 
  rbind(coastline_kong[c(560:570, 536, 420),]) %>% 
  rbind(data.frame(lon = 12.36003, lat = 78.945)) %>% mutate(region = "inner")
kong_trans <- coastline_kong[c(157:270),] %>% 
  rbind(data.frame(lon = 12.36003, lat = 78.945)) %>% 
  rbind(coastline_kong[c(420, 536, 570:589, 500:470),]) %>% mutate(region = "transition")
kong_middle <- coastline_kong[c(76:157, 470:500, 589:666),] %>% mutate(region = "middle")
kong_outer <- coastline_kong[c(76, 666),] %>% rbind(data.frame(lon = 11.178, lat = 79.115)) %>% mutate(region = "outer")
kong_shelf <- coastline_kong[1:76,] %>% 
  rbind(data.frame(lon = c(11.178, 11.178, 11, 11, 11.72653), lat = c(79.115, 79.2, 79.2, 78.85, 78.85))) %>%  mutate(region = "shelf")
kong_regions <- rbind(kong_inner, kong_trans, kong_middle, kong_outer, kong_shelf) %>% 
  mutate(region = factor(region, levels = c("inner", "transition", "middle", "outer", "shelf")))

# Find regions for hi-res pixels
kong_hires_region <- plyr::ldply(unique(kong_regions$region), points_in_region, .parallel = F, 
                                 bbox_df = kong_regions, data_df = sea_df)

# Merge and get averages
PBglobal_regions <- PBglobal |> 
  dplyr::rename(lon = longitude, lat = latitude) |> 
  left_join(kong_hires_region, by = c("lon", "lat")) |> 
  filter(!is.na(region)) |> 
  summarise(PARbottom_Global = mean(PARbottom_Global, na.rm = T), .by = region)

