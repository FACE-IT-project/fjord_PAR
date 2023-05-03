# code/3_plot
# Create the figures/tables used in the manuscript
# Also contans code for exploratory visualisations


# Setup -------------------------------------------------------------------

# Load functions and packages
source("code/0_functions.R")

# Load data in FjordLight format
# Kongsfjorden
PAR_kong <- fl_LoadFjord("kong", dirdata = "data/PAR", TS = TRUE)

# Load global values
PAR_kong_global <- tidync::tidync("data/PAR/kong.nc") |> tidync::activate("D0,D1") |>  
  tidync::hyper_tibble() |> dplyr::rename(lon = longitude, lat = latitude, depth = bathymetry)

# Load monthly bottom data
PAR_kong_bottom <- tidync::tidync("data/PAR/kong.nc") |> tidync::hyper_tibble() |> 
  dplyr::rename(lon = longitude, lat = latitude) |> 
  left_join(PAR_kong_global[,c("lon", "lat", "depth", "area")], by = c("lon", "lat"))

# Load global monthly values
PAR_kong_global_monthly <- tidync::tidync("data/PAR/kong.nc") |> tidync::activate("D0,D1,D2") |>  
  tidync::hyper_tibble() |> dplyr::rename(lon = longitude, lat = latitude) |> 
  left_join(PAR_kong_global[,c("lon", "lat", "depth", "area")], by = c("lon", "lat"))

# Load trend data
load("data/PAR_kong_bottom_lm.RData")
PAR_kong_bottom_lm <- PAR_kong_bottom_lm |> 
  left_join(PAR_kong_global[,c("lon", "lat", "depth", "area")], by = c("lon", "lat"))
load("data/PAR_kong_yearly_lm.RData")
PAR_kong_yearly_lm <- PAR_kong_yearly_lm |> 
  left_join(PAR_kong_global[,c("lon", "lat", "depth", "area")], by = c("lon", "lat"))

# Load p function data
PAR_kong_p_monthly <- tidync::tidync("data/PAR/kong.nc") |> tidync::activate("D4,D2") |> tidync::hyper_tibble()
PAR_kong_p_yearly <- tidync::tidync("data/PAR/kong.nc") |> tidync::activate("D4,D3") |> tidync::hyper_tibble()


# Prep --------------------------------------------------------------------

# Address outliers for better plotting
PAR_kong_bottom_lm <- PAR_kong_bottom_lm |> 
  filter(!is.na(slope)) |>
  mutate(perc_01 = stats::quantile(slope, 0.01),
         perc_99 = stats::quantile(slope, 0.99),
         slope_fix = case_when(slope > perc_99 ~ perc_99,
                               slope < perc_01 ~ perc_01,
                               TRUE ~ slope))
PAR_kong_yearly_lm <- PAR_kong_yearly_lm |> 
  filter(!is.na(slope)) |>
  group_by(name) |> 
  mutate(perc_01 = stats::quantile(slope, 0.01),
         perc_99 = stats::quantile(slope, 0.99),
         slope_fix = case_when(slope > perc_99 ~ perc_99,
                               slope < perc_01 ~ perc_01,
                               TRUE ~ slope)) |> 
  ungroup()


# Regions -----------------------------------------------------------------

# Merge and plot
kong_regions |> 
  left_join(PBglobal_regions, by = c("region")) |> 
  ggplot(aes(x = lon, y = lat)) +
  geom_polygon(aes(group = region, colour = region, fill = PARbottom_Global)) +
  geom_polygon(data = coastline_kong_wide, aes(group = polygon_id), colour = "grey20") +
  coord_quickmap(expand = F,
                 xlim = c(bbox_kong[1]-0.3, bbox_kong[2]+0.3), 
                 ylim = c(bbox_kong[3]-0.05, bbox_kong[4]+0.05))


# Demo plots --------------------------------------------------------------

# Global surface
kong_surface_global_plot <- ggplot(data = PAR_kong_global, aes(x = lon, y = lat)) +
  geom_raster(aes(fill = GlobalPAR0m)) + scale_fill_viridis_c() + coord_quickmap(expand = FALSE) + 
  labs(x = NULL, y = NULL, fill = "PAR\n(mmol m-2 d-1)", title = "Kongsfjorden global surface PAR") +
  theme(legend.position = "bottom", panel.background = element_rect(colour = "black", fill  = "grey"))
ggsave("figures/kong_surface_global.png", kong_surface_global_plot, width = 8, height = 8)

# Global bottom
kong_bottom_global_plot <- ggplot(data = filter(PAR_kong_global, depth >= -200), aes(x = lon, y = lat)) +
  geom_raster(aes(fill = GlobalPARbottom)) + geom_contour(aes(z = GlobalPARbottom), breaks = 12.5, colour = "red") +
  scale_fill_viridis_c() + coord_quickmap(expand = FALSE) + 
  labs(x = NULL, y = NULL, fill = "PAR\n(mmol m-2 d-1)", title = "Kongsfjorden global bottom PAR (200 m isobath)",
       subtitle = "Red contour shows 12.5 mmol m-2 d-1") +
  theme(legend.position = "bottom", panel.background = element_rect(colour = "black", fill  = "grey"))
ggsave("figures/kong_bottom_global.png", kong_bottom_global_plot, width = 8, height = 8)

# Global kd
kong_kd_global_plot <- ggplot(data = filter(PAR_kong_global, depth >= -200), aes(x = lon, y = lat)) +
  geom_raster(aes(fill = Globalkdpar)) + scale_fill_viridis_c(option = "A") + coord_quickmap(expand = FALSE) + 
  labs(x = NULL, y = NULL, fill = "kd", title = "Kongsfjorden global kd PAR (200 m isobath)") +
  theme(legend.position = "bottom", panel.background = element_rect(colour = "black", fill  = "grey"))
ggsave("figures/kong_kd_global.png", kong_kd_global_plot, width = 8, height = 8)

# Global monthly surface
# I don't think this is of interest

# Global monthly bottom
kong_bottom_global_monthly_plot <- ggplot(data = filter(PAR_kong_global_monthly, depth >= -200), aes(x = lon, y = lat)) +
  geom_raster(aes(fill = MonthlyPARbottom)) + geom_contour(aes(z = MonthlyPARbottom), breaks = 12.5, colour = "red") +
  scale_fill_viridis_c() + coord_quickmap(expand = FALSE) + 
  facet_wrap(~Months, nrow = 2) +
  labs(x = NULL, y = NULL, fill = "PAR\n(mmol m-2 d-1)", title = "Kongsfjorden global monthly bottom PAR (200 m isobath)",
       subtitle = "Red contour shows 12.5 mmol m-2 d-1") +
  theme(legend.position = "bottom", panel.background = element_rect(colour = "black", fill  = "grey"))
ggsave("figures/kong_bottom_global_monthly.png", kong_bottom_global_monthly_plot, width = 14, height = 8)

# Yearly surface trends
kong_surface_yearly_trend_plot <- ggplot(data = filter(PAR_kong_yearly_lm, name == "YearlyPAR0m"), aes(x = lon, y = lat)) +
  geom_raster(aes(fill = slope)) + scale_fill_gradient2() + coord_quickmap(expand = FALSE) + 
  labs(x = NULL, y = NULL, fill = "PAR/year\n(mmol m-2 d-1)", title = "Kongsfjorden yearly surface PAR trend") +
  theme(legend.position = "bottom", panel.background = element_rect(colour = "black", fill  = "grey"))
ggsave("figures/kong_surface_yearly_trend.png", kong_surface_yearly_trend_plot, width = 8, height = 8)

# Yearly bottom trends
kong_bottom_yearly_trend_plot <- ggplot(data = filter(PAR_kong_yearly_lm, name == "YearlyPARbottom", depth >= -200), 
                                        aes(x = lon, y = lat)) +
  geom_raster(aes(fill = slope_fix)) + scale_fill_gradient2() + coord_quickmap(expand = FALSE) + 
  labs(x = NULL, y = NULL, fill = "PAR/year\n(mmol m-2 d-1)", title = "Kongsfjorden yearly bottom PAR trend (200 m isobath)",
       subtitle = "Rounded to 1st and 99th percentiles") +
  theme(legend.position = "bottom", panel.background = element_rect(colour = "black", fill  = "grey"))
ggsave("figures/kong_bottom_yearly_trend.png", kong_bottom_yearly_trend_plot, width = 8, height = 8)

# Monthly bottom trends
kong_bottom_monthly_trend_plot <- ggplot(data = filter(PAR_kong_bottom_lm, depth >= -200), aes(x = lon, y = lat)) +
  geom_raster(aes(fill = slope_fix)) + scale_fill_gradient2() + coord_quickmap(expand = FALSE) + 
  facet_wrap(~Months, nrow = 2) +
  labs(x = NULL, y = NULL, fill = "PAR/year\n(mmol m-2 d-1)", title = "Kongsfjorden monthly bottom PAR trend (200 m isobath)",
       subtitle = "Rounded to 1st and 99th percentiles") +
  theme(legend.position = "bottom", panel.background = element_rect(colour = "black", fill  = "grey"))
ggsave("figures/kong_bottom_monthly_trend.png", kong_bottom_monthly_trend_plot, width = 14, height = 8)

# Bottom that receives 12.5 PAR
kong_bottom_monthly_area_plot <- PAR_kong_bottom |> 
  filter(PARbottom >= 12.5) |> 
  mutate(date = as.Date(paste0(Years,"-",Months,"-01"))) |> 
  summarise(total_area = sum(area), .by = c("Years", "Months", "date")) |> 
  # Manually add a blank value for Month 3 to maintain grid shape
  rbind(data.frame(Years = 2003, Months = 3, date = as.Date("2003-03-01"), total_area = NA)) |> 
  ggplot(aes(x = date, y = total_area)) +
  geom_point() + geom_line() + geom_smooth(method = "lm") +
  facet_wrap(~ Months, nrow = 2) +
  labs(x = "Date", y = "Total area (km^2)", title = "Monthly bottom area receiving >= 12.5 mmol m-2 d-1",
       subtitle = "Note that March is always 0 km^2") +
  theme(panel.background = element_rect(colour = "black", fill  = "grey"))
ggsave("figures/kong_bottom_monthly_area.png", kong_bottom_monthly_area_plot, width = 14, height = 8)

# Monthly p functions
kong_p_monthly_plot <- ggplot(PAR_kong_p_monthly, aes(x = irradianceLevel, y = MonthlyPfunction)) +
  geom_line(aes(colour = as.factor(Months)), linewidth = 3) +
  scale_x_continuous(trans = ggforce::trans_reverser("log10"), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 40), expand = c(0, 0), breaks = c(10, 20, 30)) +
  scale_colour_viridis_d(option = "F") +
  labs(x = "E mol photons m-2 day-1", y = "% of surface receiving more than E", colour = "Month") +
  theme(legend.position = "bottom", panel.background = element_rect(colour = "black", fill  = "grey"))
ggsave("figures/kong_p_monthly.png", kong_p_monthly_plot, width = 8, height = 8)

# Yearly p functions
kong_p_yearly_plot <- ggplot(PAR_kong_p_yearly, aes(x = irradianceLevel, y = YearlyPfunction)) +
  geom_line(aes(colour = Years, group = Years), linewidth = 2) +
  scale_x_continuous(trans = ggforce::trans_reverser("log10"), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 40), expand = c(0, 0), breaks = c(10, 20, 30)) +
  scale_colour_viridis_c() +
  labs(x = "E mol photons m-2 day-1", y = "% of surface receiving more than E", colour = "Year") +
  theme(legend.position = "bottom", panel.background = element_rect(colour = "black", fill  = "grey"))
ggsave("figures/kong_p_yearly.png", kong_p_yearly_plot, width = 8, height = 8)

# Combine p function plots
kong_p_plot <- ggpubr::ggarrange(kong_p_monthly_plot, kong_p_yearly_plot, 
                                 ncol = 2, nrow = 1, labels = c("A)", "B)"), align = "hv")
ggsave("figures/kong_p.png", kong_p_plot, width = 14, height = 8)


# Figure 1 ----------------------------------------------------------------
# Map of the study area + seven sites showing PAR in some way
# Probably global average surface values


# Figure 2 ----------------------------------------------------------------
# p functions per site
# Similar to how they are shown in the other two publications that came before


# Figure 3 ----------------------------------------------------------------
# Maps + time series that show which regions are inhabitable, and how they have changed over time
# My thinking is to show maps of global bottom PAR (surface PAR shown in Figure 1)
# With an extra facet attached to each map showing the annual time series of change for the three PAR variables
# The overall average would be shown as a thick black line, with the values for each pixel shown as thin grey
# It may be ideal to break these up into inner, middle, and outer fjord values, too


# Table 1 -----------------------------------------------------------------
# Light requirements by species


# Table 2 -----------------------------------------------------------------
# Changes to inhabitable area over time by site

