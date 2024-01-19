# code/2_analyse.R
# Run the various analyses of the spatial data


# Setup -------------------------------------------------------------------

source("code/0_functions.R")


# Load data ---------------------------------------------------------------

# This loads bathymetry, pixel area, and all forms of PAR and Kd
# NB: Only run one line at a time, this takes ~10 minutes and ~50 GB of RAM
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

# Calculate p-values of linear models on annual PAR values as full time series
# NB: This also calculates the values for bottom PAR
PAR_annual_lm <- PAR_annual_summary |> 
   dplyr::select(site, year, variable, q50) |>  distinct() |> 
   dplyr::rename(t = year, value = q50) |> 
   plyr::ddply(c("site", "variable"), lm_tidy, .parallel = T)
save(PAR_annual_lm, file = "data/PAR_annual_lm.RData")


# Clim analyses -----------------------------------------------------------

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

# Calculate p-values of linear models on monthly bottom PAR as a full time series
PAR_monthly_lm <- PAR_monthly_summary |> 
   dplyr::select(site, year, month, variable, q50) |>  distinct() |> 
   dplyr::rename(t = year, value = q50) |> 
   plyr::ddply(c("site", "variable", "month"), lm_tidy, .parallel = T)
save(PAR_monthly_lm, file = "data/PAR_monthly_lm.RData")


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
PAR_spatial_lm <- PAR_spatial_summary |> 
  dplyr::select(site, year, annual_perc) |>  distinct() |> 
  dplyr::rename(t = year, value = annual_perc) |> 
  plyr::ddply(c("site"), lm_tidy, .parallel = T)
save(PAR_spatial_lm, file = "data/PAR_spatial_lm.RData")


# P-functions -------------------------------------------------------------

# NB: Not re-run on new data as this appears to be deprecated

# Get base for P-functions
## NB: Run one at a time
# P_kong <- calc_p_function(PAR_kong, site_name = "kong")
# P_is <- calc_p_function(PAR_is, site_name = "is")
# P_stor <- calc_p_function(PAR_stor, site_name = "stor")
# P_young <- calc_p_function(PAR_young, site_name = "young")
# P_disko <- calc_p_function(PAR_disko, site_name = "disko")
# P_nuup <- calc_p_function(PAR_nuup, site_name = "nuup")
# P_por <- calc_p_function(PAR_por, site_name = "por")

# Combine and save
# P_all <- rbind(P_kong, P_is, P_stor, P_young, P_disko, P_nuup, P_por)
# save(P_all, file = "data/P_all.RData")
# rm(P_kong, P_is, P_stor, P_young, P_disko, P_nuup, P_por); gc()


# Monthly K_PAR -----------------------------------------------------------

kong_K_PAR <- fl_LoadFjord("kong", "K_PAR", "data/PAR", TS = TRUE)

