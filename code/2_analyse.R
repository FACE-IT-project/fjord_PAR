# code/2_analyse.R
# Run the various analyses of the spatial data


# Setup -------------------------------------------------------------------

source("code/0_functions.R")


# Load data ---------------------------------------------------------------

# Test file structures
ncdump::NetCDF("data/PAR/kong.nc")
PAR_test <- tidync::tidync("data/PAR/kong.nc") |> 
  tidync::hyper_tibble()

# Kongsfjorden
PAR_kong <- fl_LoadFjord("kong", dirdata = "data/PAR", TS = TRUE)


# Extract bathymetry ------------------------------------------------------

# as raster (mode = "raster")
# as 3 columns data frame (mode = "3col" : longitude, latitude, depth)
# all depths (what = "s" ; s for Sea), as raster
# coastal zone [0-200m] (what = "c" ; c for Coastal), as raster
bathy_kong_rast <- flget_bathymetry(PAR_kong, what = "s", mode = "raster", PLOT = TRUE)
bathy_kong_df <- flget_bathymetry(PAR_kong, what = "s", mode = "3col", PLOT = TRUE) |> 
  dplyr::rename(lon = longitude, lat = latitude)


# Extract data ------------------------------------------------------------

# Get time series data
P0ts <- flget_PARbottomMonthlyTS(PAR_kong, mode = "3col")

# PAR as 3 columns data frame
P02012 <- flget_optics(fjorddata, "PAR0m", "Yearly", year = 2012, mode = "3col")
P0June <- flget_optics(fjorddata, optics = "PAR0m", period = "Monthly", month = 6, mode = "3col")
PBJune <- flget_optics(fjorddata, optics = "PARbottom", period = "Monthly", month = 6, mode = "3col")
PB2012 <- flget_optics(fjorddata, "PARbottom", "Yearly", year = 2012, mode = "3col")
P0global <- flget_optics(fjorddata, "PAR0m", "Global", mode = "3col")
PBglobal <- flget_optics(fjorddata, "PARbottom", "Global", mode = "3col")
kdglobal <- flget_optics(fjorddata, "kdpar", "Global", mode = "3col")

# Get full annual time series
# TODO: Turn loading code into a wrapper function for use with seven sites
PAR_kong_yearly <- tidync::tidync("data/PAR/kong.nc") |> 
  tidync::activate("D0,D1,D3") |> 
  tidync::hyper_tibble() |> 
  na.omit() |> 
  dplyr::rename(lon = longitude, lat = latitude) |> 
  pivot_longer(cols = YearlyPARbottom:Yearlykdpar)


# Annual analyses ---------------------------------------------------------

# Run linear models per pixel
PAR_kong_yearly_lm <- plyr::ddply(PAR_kong_yearly, c("lon", "lat", "name"), lm_tidy, .parallel = T)

# Test plot
unique(PAR_kong_yearly_lm$name)
PAR_kong_yearly_lm |> 
  filter(name == "YearlyPAR0m") |> 
  left_join(bathy_kong_df, by = c("lon", "lat")) |> 
  filter(depth >= -200) |> 
  ggplot(aes( x = lon, y = lat)) +
  geom_raster(aes(fill = slope)) +
  scale_fill_gradient2()


# p functions -------------------------------------------------------------

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

