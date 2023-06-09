# code/3_plot
# Create the figures/tables used in the manuscript
# Also contains code for exploratory visualisations


# Setup -------------------------------------------------------------------

# Load functions and packages
source("code/0_functions.R")
library(ggOceanMaps)

# Load processed data from '2_analyse.R'
load("data/PAR_annual_summary.RData")
load("data/PAR_clim_summary.RData")
load("data/PAR_monthly_summary.RData")
load("data/PAR_spatial_summary.RData")


# Prep --------------------------------------------------------------------

# Load trend data
load("data/PAR_kong_bottom_lm.RData")
PAR_kong_bottom_lm <- PAR_kong_bottom_lm |> 
  left_join(PAR_kong_global[,c("lon", "lat", "depth", "area")], by = c("lon", "lat"))
load("data/PAR_kong_yearly_lm.RData")
PAR_kong_yearly_lm <- PAR_kong_yearly_lm |> 
  left_join(PAR_kong_global[,c("lon", "lat", "depth", "area")], by = c("lon", "lat"))

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
  labs(x = NULL, y = NULL, fill = "PAR\n(mol m-2 d-1)", title = "Kongsfjorden global surface PAR") +
  theme(legend.position = "bottom", panel.background = element_rect(colour = "black", fill  = "grey"))
ggsave("figures/kong_surface_global.png", kong_surface_global_plot, width = 8, height = 8)

# Global bottom
kong_bottom_global_plot <- ggplot(data = filter(PAR_kong_global, depth >= -200), aes(x = lon, y = lat)) +
  geom_raster(aes(fill = GlobalPARbottom)) + geom_contour(aes(z = GlobalPARbottom), breaks = 0.3, colour = "red") +
  scale_fill_viridis_c() + coord_quickmap(expand = FALSE) + 
  labs(x = NULL, y = NULL, fill = "PAR\n(mol m-2 d-1)", title = "Kongsfjorden global bottom PAR (200 m isobath)",
       subtitle = "Red contour shows 0.3 mol m-2 d-1") +
  theme(legend.position = "bottom", panel.background = element_rect(colour = "black", fill  = "grey"))
ggsave("figures/kong_bottom_global.png", kong_bottom_global_plot, width = 8, height = 8)

# Global kd
kong_kd_global_plot <- ggplot(data = filter(PAR_kong_global, depth >= -200), aes(x = lon, y = lat)) +
  geom_raster(aes(fill = Globalkdpar)) + scale_fill_viridis_c(option = "A") + coord_quickmap(expand = FALSE) + 
  labs(x = NULL, y = NULL, fill = "kd", title = "Kongsfjorden global kd PAR (200 m isobath)") +
  theme(legend.position = "bottom", panel.background = element_rect(colour = "black", fill  = "grey"))
ggsave("figures/kong_kd_global.png", kong_kd_global_plot, width = 8, height = 8)

# Global monthly bottom
kong_bottom_global_monthly_plot <- ggplot(data = filter(PAR_kong_global_monthly, depth >= -200), aes(x = lon, y = lat)) +
  geom_raster(aes(fill = MonthlyPARbottom)) + geom_contour(aes(z = MonthlyPARbottom), breaks = 0.3, colour = "red") +
  scale_fill_viridis_c() + coord_quickmap(expand = FALSE) + 
  facet_wrap(~Months, nrow = 2) +
  labs(x = NULL, y = NULL, fill = "PAR\n(mol m-2 d-1)", title = "Kongsfjorden global monthly bottom PAR (200 m isobath)",
       subtitle = "Red contour shows 0.3 mol m-2 d-1") +
  theme(legend.position = "bottom", panel.background = element_rect(colour = "black", fill  = "grey"))
ggsave("figures/kong_bottom_global_monthly.png", kong_bottom_global_monthly_plot, width = 14, height = 8)

# Yearly surface trends
kong_surface_yearly_trend_plot <- ggplot(data = filter(PAR_kong_yearly_lm, name == "YearlyPAR0m"), aes(x = lon, y = lat)) +
  geom_raster(aes(fill = slope)) + scale_fill_gradient2() + coord_quickmap(expand = FALSE) + 
  labs(x = NULL, y = NULL, fill = "PAR/year\n(mol m-2 d-1)", title = "Kongsfjorden yearly surface PAR trend") +
  theme(legend.position = "bottom", panel.background = element_rect(colour = "black", fill  = "grey"))
ggsave("figures/kong_surface_yearly_trend.png", kong_surface_yearly_trend_plot, width = 8, height = 8)

# Yearly bottom trends
kong_bottom_yearly_trend_plot <- ggplot(data = filter(PAR_kong_yearly_lm, name == "YearlyPARbottom", depth >= -200), 
                                        aes(x = lon, y = lat)) +
  geom_raster(aes(fill = slope_fix)) + scale_fill_gradient2() + coord_quickmap(expand = FALSE) + 
  labs(x = NULL, y = NULL, fill = "PAR/year\n(mol m-2 d-1)", title = "Kongsfjorden yearly bottom PAR trend (200 m isobath)",
       subtitle = "Rounded to 1st and 99th percentiles") +
  theme(legend.position = "bottom", panel.background = element_rect(colour = "black", fill  = "grey"))
ggsave("figures/kong_bottom_yearly_trend.png", kong_bottom_yearly_trend_plot, width = 8, height = 8)

# Monthly bottom trends
kong_bottom_monthly_trend_plot <- ggplot(data = filter(PAR_kong_bottom_lm, depth >= -200), aes(x = lon, y = lat)) +
  geom_raster(aes(fill = slope_fix)) + scale_fill_gradient2() + coord_quickmap(expand = FALSE) + 
  facet_wrap(~Months, nrow = 2) +
  labs(x = NULL, y = NULL, fill = "PAR/year\n(mol m-2 d-1)", title = "Kongsfjorden monthly bottom PAR trend (200 m isobath)",
       subtitle = "Rounded to 1st and 99th percentiles") +
  theme(legend.position = "bottom", panel.background = element_rect(colour = "black", fill  = "grey"))
ggsave("figures/kong_bottom_monthly_trend.png", kong_bottom_monthly_trend_plot, width = 14, height = 8)

# Bottom that receives 0.3 PAR
kong_bottom_monthly_area_plot <- PAR_kong_bottom |> 
  filter(PARbottom >= 0.3) |> 
  mutate(date = as.Date(paste0(Years,"-",Months,"-01"))) |> 
  summarise(total_area = sum(area), .by = c("Years", "Months", "date")) |> 
  # Manually add a blank value for Month 3 to maintain grid shape
  rbind(data.frame(Years = 2003, Months = 3, date = as.Date("2003-03-01"), total_area = NA)) |> 
  ggplot(aes(x = date, y = total_area)) +
  geom_point() + geom_line() + geom_smooth(method = "lm") +
  facet_wrap(~ Months, nrow = 2) +
  labs(x = "Date", y = "Total area (km^2)", title = "Monthly bottom area receiving >= 0.3 mol m-2 d-1") +
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

# Create plots that help investigate artefacts
plyr::l_ply(long_site_names$site, plot_surface, .parallel = F)


# Figure 1 ----------------------------------------------------------------
# Map of the study area + seven sites showing PAR in some way
# Probably global average surface values

# Range of surface PAR values
PAR_global_surface <- c(PAR_kong$PAR_global$GlobalPAR0m, PAR_is$PAR_global$GlobalPAR0m,
                        PAR_stor$PAR_global$GlobalPAR0m, PAR_young$PAR_global$GlobalPAR0m,
                        PAR_disko$PAR_global$GlobalPAR0m, PAR_nuup$PAR_global$GlobalPAR0m,
                        PAR_por$PAR_global$GlobalPAR0m)
PAR_range <- range(PAR_global_surface, na.rm = T)
PAR_quant <- c(quantile(PAR_global_surface, 0.01, na.rm = T),
               quantile(PAR_global_surface, 0.99, na.rm = T))

# Study sites
site_points <- data.frame(site = factor(x = c("Kongsfjorden", "Isfjorden", "Storfjorden", 
                                              "Young Sound", "Qeqertarsuup Tunua", "Nuup Kangerlua", 
                                              "Porsangerfjorden"),
                                        levels = c("Kongsfjorden", "Isfjorden","Storfjorden", 
                                                   "Young Sound", "Qeqertarsuup Tunua", "Nuup Kangerlua", 
                                                   "Porsangerfjorden")),
                          lon = c(11.845, 14.365, 19.88, -21.237, -52.555, -50.625, 25.75),
                          lat = c(78.98, 78.235, 77.78, 74.517, 69.36, 64.405, 70.6))

# Full study area
fig_1_base <- basemap(limits = c(-50, 50, 61, 90), bathymetry = T) +
  # Other labels
  geom_spatial_label(aes(x = 0, y = 78, label = "Fram\nStrait"), 
                     colour = "black", crs = 4326, size = 4, alpha = 0.5) +
  geom_spatial_label(aes(x = 27, y = 79, label = "Svalbard"), 
                     colour = "black", crs = 4326, size = 4, alpha = 0.5) +
  geom_spatial_label(aes(x = 40, y = 74, label = "Barents Sea"), 
                     colour = "black", crs = 4326, size = 4, alpha = 0.5) +
  geom_spatial_label(aes(x = 0, y = 73.5, label = "Greenland\nSea"), 
                     colour = "black", crs = 4326, size = 4, alpha = 0.5) +
  geom_spatial_label(aes(x = 0, y = 68, label = "Norwegian\nSea"), 
                     colour = "black", crs = 4326, size = 4, alpha = 0.5) +
  geom_spatial_label(aes(x = -40, y = 76, label = "Greenland"), 
                     colour = "black", crs = 4326, size = 4, alpha = 0.5) +
  geom_spatial_label(aes(x = 19, y = 70.5, label = "Northern\nNorway"), 
                     colour = "black", crs = 4326, size = 4, alpha = 0.5) +
  # Site points
  geom_spatial_point(data = site_points, size = 6, crs = 4326,
                     aes(x = lon, y = lat), colour = "black") +
  geom_spatial_point(data = site_points, size = 5, crs = 4326,
                     aes(x = lon, y = lat, colour = site)) +
  scale_colour_manual("Site", values = site_colours) +
  # Other minutia
  labs(x = NULL, y = NULL) +
  theme(panel.border = element_rect(colour = "black", fill = NA),
        panel.background = element_rect(fill = NA, colour = "black"),
        plot.background = element_rect(fill = "white", colour = NA),
        axis.text = element_text(colour = "black", size = 10),
        legend.position = c(0.925, 0.31),
        legend.box.margin = margin(10, 10, 10, 10), 
        legend.box.background = element_rect(fill = "white", colour = "black"))
# fig_1_base

# Add Surface PAR site panels
fig_1_kong <- fig_1_subplot(PAR_kong$PAR_global, "Kongsfjorden", PAR_quant)
fig_1_is <- fig_1_subplot(PAR_is$PAR_global, "Isfjorden", PAR_quant)
fig_1_stor <- fig_1_subplot(PAR_stor$PAR_global, "Storfjorden", PAR_quant)
fig_1_young <- fig_1_subplot(PAR_young$PAR_global, "Young Sound", PAR_quant)
fig_1_disko <- fig_1_subplot(PAR_disko$PAR_global, "Qeqertarsuup Tunua", PAR_quant)
fig_1_nuup <- fig_1_subplot(PAR_nuup$PAR_global, "Nuup Kangerlua", PAR_quant)
fig_1_por <- fig_1_subplot(PAR_por$PAR_global, "Porsangerfjorden", PAR_quant)

# Get legend
PAR_legend_base <- filter(PAR_kong$PAR_global) %>% 
  mutate(x = 1:n(), y = 1) %>% 
  ggplot() + geom_point(aes(x = x, y = y, colour = GlobalPAR0m)) +
  scale_colour_viridis_c(limits = PAR_quant) +
  labs(colour = "Surface PAR\n[mol m-2 d-1]") +
  theme(legend.position = "right", 
        legend.key.height = unit(1, "cm"),
        legend.key.width = unit(1, "cm"),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.box.background = element_rect(fill = NA, colour = NA))
PAR_legend <- ggpubr::get_legend(PAR_legend_base)

# Combine and save
fig_1_sites <- ggpubr::ggarrange(fig_1_kong, fig_1_is, fig_1_stor, PAR_legend,
                                 fig_1_young, fig_1_disko, fig_1_nuup, fig_1_por,
                                 ncol = 4, nrow = 2, align = "hv")#, 
                                 # labels = c("B)", "C)", "D)", "E)", "F)", "G)", "H)", ""))
fig_1 <- ggpubr::ggarrange(fig_1_base, fig_1_sites, 
                           # labels = c("A)", ""), 
                           ncol = 1, nrow = 2, heights = c(1, 0.7)) +
  ggpubr::bgcolor("white") + ggpubr::border("white")
ggsave("figures/fig_1.png", fig_1, height = 14, width = 12)


# Figure 2 ----------------------------------------------------------------
# p functions per site
# Similar to how they are shown in the other two publications that came before

# Load global p function data 
PAR_p_global <- plyr::ldply(long_site_names$site, load_p_global, .parallel = T)

# Yearly p functions
fig_2 <- ggplot(PAR_p_global, aes(x = irradianceLevel, y = GlobalPfunction)) +
  geom_line(aes(group = site), colour = "black", linewidth = 2.5) +
  geom_line(aes(colour = site), linewidth = 2) +
  scale_x_continuous(trans = ggforce::trans_reverser("log10"), expand = c(0, 0), 
                     breaks = c(10, 1, 0.1, 0.01, 0.001),
                     labels = c(10, 1, 0.1, 0.01, 0.001)) + # Need to force these
  scale_y_continuous(limits = c(0, 50), expand = c(0, 0), breaks = c(10, 20, 30, 40)) +
  scale_colour_manual("Site", values = site_colours) +
  labs(x = "mol photons m-2 day-1", y = "% of surface receiving value [x-axis]", colour = "Site") +
  theme(legend.position = c(0.14, 0.8), 
        legend.title = element_text(colour = "black", size = 12),
        legend.text = element_text(colour = "black", size = 10),
        legend.box.background = element_rect(colour = "black", fill = "white"),
        plot.margin = margin(5, 20, 5, 5),
        axis.text = element_text(colour = "black", size = 10),
        axis.title = element_text(colour = "black", size = 12),
        panel.border = element_rect(colour = "black", fill  = NA))
# fig_2

# Save
ggsave("figures/fig_2.png", fig_2, width = 8, height = 6)


# Figure 3 ----------------------------------------------------------------
# Ribbon plots showing annual mean PAR variables

# Surface
fig_3a <- PAR_annual_summary |> 
  filter(name == "YearlyPAR0m") |> 
  left_join(long_site_names, by = "site") |> 
  ggplot(aes(x = year, y = mean)) +
  geom_ribbon(aes(ymin = min, ymax = max, fill = site_long)) +
  geom_ribbon(aes(ymin = q10, ymax = q90), colour = "grey", alpha = 0.2) +
  geom_line(colour = "black") +
  geom_line(aes(y = q50), colour = "grey") +
  geom_point(colour = "black") +
  geom_point(aes(y = q50), colour = "grey") +
  facet_wrap(~site_long) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_fill_manual("Site", values = site_colours) +
  labs(x = NULL, y = "Annual average surface PAR [mol m-2 d-1]", fill = "Site") +
  theme(legend.position = "none",
        # legend.position = c(0.7, 0.2), 
        legend.title = element_text(colour = "black", size = 12),
        legend.text = element_text(colour = "black", size = 10),
        legend.box.background = element_rect(colour = "black", fill = "white"),
        axis.text = element_text(colour = "black", size = 10),
        axis.title = element_text(colour = "black", size = 12),
        panel.border = element_rect(colour = "black", fill  = NA))
# fig_3a

# Kd
fig_3b <- PAR_annual_summary |> 
  filter(name == "Yearlykdpar") |> 
  left_join(long_site_names, by = "site") |> 
  ggplot(aes(x = year, y = mean)) +
  geom_ribbon(aes(ymin = min, ymax = max, fill = site_long)) +
  geom_ribbon(aes(ymin = q10, ymax = q90), colour = "grey", alpha = 0.2) +
  geom_line(colour = "black") +
  geom_line(aes(y = q50), colour = "grey") +
  geom_point(colour = "black") +
  geom_point(aes(y = q50), colour = "grey") +
  facet_wrap(~site_long) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_fill_manual("Site", values = site_colours) +
  labs(x = NULL, y = "Annual average Kd", fill = "Site") +
  theme(legend.position = "none",
        # legend.position = c(0.14, 0.8), 
        legend.title = element_text(colour = "black", size = 12),
        legend.text = element_text(colour = "black", size = 10),
        legend.box.background = element_rect(colour = "black", fill = "white"),
        axis.text = element_text(colour = "black", size = 10),
        axis.title = element_text(colour = "black", size = 12),
        panel.border = element_rect(colour = "black", fill  = NA))
# fig_3b

# Kd
fig_3c <- PAR_annual_summary |> 
  filter(name == "YearlyPARbottom") |> 
  left_join(long_site_names, by = "site") |> 
  ggplot(aes(x = year, y = mean)) +
  geom_ribbon(aes(ymin = min, ymax = max, fill = site_long)) +
  geom_ribbon(aes(ymin = q10, ymax = q90), colour = "grey", alpha = 0.2) +
  geom_line(colour = "black") +
  geom_line(aes(y = q50), colour = "grey") +
  geom_point(colour = "black") +
  geom_point(aes(y = q50), colour = "grey") +
  facet_wrap(~site_long) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_fill_manual("Site", values = site_colours) +
  labs(x = NULL, y = "Annual average bottom PAR [mol m-2 d-1]", fill = "Site") +
  theme(legend.position = "none",
        # legend.position = c(0.14, 0.8), 
        legend.title = element_text(colour = "black", size = 12),
        legend.text = element_text(colour = "black", size = 10),
        legend.box.background = element_rect(colour = "black", fill = "white"),
        axis.text = element_text(colour = "black", size = 10),
        axis.title = element_text(colour = "black", size = 12),
        panel.border = element_rect(colour = "black", fill  = NA))
# fig_3c

# Combine and save
fig_3 <- ggpubr::ggarrange(fig_3a, fig_3b, fig_3c, align = "v", 
                           # common.legend = T, legend = "bottom",
                           labels = c("A)", "B)", "C)"), ncol = 1, nrow = 3)  +
  ggpubr::bgcolor("white") + ggpubr::border("white")
ggsave(filename = "figures/fig_3.png", plot = fig_3, height = 16, width = 10)


# Figure 4 ----------------------------------------------------------------
# Ribbon plots showing change in bottom PAR per month over time

# Bottom PAR
fig_4 <- PAR_monthly_summary |> 
  left_join(long_site_names, by = "site") |> 
  ggplot(aes(x = year, y = mean)) +
  # geom_ribbon(aes(ymin = min, ymax = max, fill = as.factor(month)), alpha = 0.2) +
  geom_ribbon(aes(ymin = q10, ymax = q90, fill = as.factor(month)), alpha = 0.2) +
  # geom_line(aes(colour = as.factor(month))) +
  geom_line(aes(y = q50, colour = as.factor(month))) +
  # geom_point(aes(colour = as.factor(month))) +
  geom_point(aes(y = q50, colour = as.factor(month))) +
  facet_wrap(~site_long, scales = "free_y") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_fill_viridis_d("Month", option = "A", aesthetics = c("colour", "fill")) +
  labs(x = NULL, y = "Monthly average bottom PAR [mol m-2 d-1]") +
  theme(legend.position = c(0.65, 0.2), 
        legend.direction = "horizontal",
        legend.title = element_text(colour = "black", size = 12),
        legend.text = element_text(colour = "black", size = 10),
        legend.box.background = element_rect(colour = "black", fill = "white"),
        axis.text = element_text(colour = "black", size = 10),
        axis.title = element_text(colour = "black", size = 12),
        panel.border = element_rect(colour = "black", fill  = NA))
# fig_4
ggsave(filename = "figures/fig_4.png", plot = fig_4, height = 8, width = 10)


# Figure 5 ----------------------------------------------------------------
# Changes to inhabitable area over time

# Prep data for plotting
PAR_spat_sum_comp <- PAR_spatial_summary |> 
  left_join(long_site_names, by = "site") |> 
  mutate(date = as.Date(paste0(year,"-",month,"-01"))) |> 
  group_by(site, site_long) |> 
  tidyr::complete(date = seq(min(date), max(date), by = "month")) |> 
  ungroup()
PAR_spat_sum_comp_ann <- PAR_spat_sum_comp |> 
  dplyr::select(site_long, year, annual_area) |> 
  filter(!is.na(year)) |> distinct() |> 
  mutate(date = as.Date(paste0(year,"-03-01")))

# Bottom PAR per month with per month trends
fig_5a <- PAR_spat_sum_comp |> 
  ggplot(aes(x = date, y = monthly_area)) +
  geom_line(aes(colour = site_long)) +
  geom_point(aes(colour = site_long)) +
  geom_smooth(aes(colour = site_long), method = "lm") +
  geom_point(data = PAR_spat_sum_comp_ann, 
             aes(y = annual_area), size = 3) +
  geom_smooth(data = PAR_spat_sum_comp_ann,
              aes(y = annual_area), colour = "black", method = "lm") +
  facet_wrap(~site_long, scales = "free_y") +
  scale_x_date(expand = c(0, 0)) +
  scale_colour_manual("Site", values = site_colours) +
  labs(x = NULL, y = "Spatial availability [km^2]") +
  theme(legend.position = "none",
        # legend.position = c(0.65, 0.15), 
        legend.margin = margin(5, 20, 5, 5),
        legend.title = element_text(colour = "black", size = 12),
        legend.text = element_text(colour = "black", size = 10),
        legend.box.background = element_rect(colour = "black", fill = "white"),
        axis.text = element_text(colour = "black", size = 10),
        axis.title = element_text(colour = "black", size = 12),
        panel.border = element_rect(colour = "black", fill  = NA))
# fig_5a

# Bottom PAR per month with total trend
fig_5b <- PAR_spatial_summary |> 
  left_join(long_site_names, by = "site") |> 
  ggplot(aes(x = year, y = monthly_area)) +
  geom_line(aes(colour = as.factor(month))) +
  geom_point(aes(colour = as.factor(month))) +
  geom_smooth(aes(colour = as.factor(month)), method = "lm") +
  facet_wrap(~site_long, scales = "free_y") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_colour_viridis_d("Month", option = "A") +
  labs(x = NULL, y = "Spatial availability [km^2]") +
  theme(legend.position = c(0.65, 0.15),
        legend.direction = "horizontal",
        legend.title = element_text(colour = "black", size = 12),
        legend.text = element_text(colour = "black", size = 10),
        legend.box.background = element_rect(colour = "black", fill = "white"),
        axis.text = element_text(colour = "black", size = 10),
        axis.title = element_text(colour = "black", size = 12),
        panel.border = element_rect(colour = "black", fill  = NA))
# fig_5b

# Merge and save
fig_5 <- ggpubr::ggarrange(fig_5a, fig_5b, align = "v", 
                           labels = c("A)", "B)"), ncol = 1, nrow = 2)  +
  ggpubr::bgcolor("white") + ggpubr::border("white")
ggsave(filename = "figures/fig_5.png", plot = fig_5, height = 12, width = 10)


# Figure S1 ---------------------------------------------------------------
# p functions per site for monthly clims

# Load global p function data 
PAR_p_clim <- plyr::ldply(long_site_names$site, load_p_clim, .parallel = T)

# Yearly p functions
fig_S1 <- ggplot(PAR_p_clim, aes(x = irradianceLevel, y = MonthlyPfunction)) +
  geom_line(aes(group = as.factor(Months)), colour = "black", linewidth = 2.2) +
  geom_line(aes(colour = as.factor(Months)), linewidth = 1.7) +
  scale_x_continuous(trans = ggforce::trans_reverser("log10"), expand = c(0, 0), 
                     breaks = c(1, 0.1, 0.01), labels = c(1, 0.1, 0.01)) +
  scale_y_continuous(limits = c(0, 50), expand = c(0, 0), breaks = c(10, 20, 30, 40)) +
  scale_colour_viridis_d(option = "A") +
  facet_wrap(~site, nrow = 2, ncol = 4) +
  labs(x = "mol photons m-2 day-1", y = "% of surface receiving value [x-axis]", colour = "Month") +
  theme(legend.position = c(0.875, 0.23), 
        legend.title = element_text(colour = "black", size = 12),
        legend.text = element_text(colour = "black", size = 10),
        legend.box.background = element_rect(colour = "black", fill = "white"),
        axis.text = element_text(colour = "black", size = 10),
        axis.title = element_text(colour = "black", size = 12),
        panel.border = element_rect(colour = "black", fill  = NA))
# fig_S1

# Save
ggsave("figures/fig_S1.png", fig_S1, width = 8, height = 6)


# Figure S2 ---------------------------------------------------------------
# p functions per site for annual averages

# Load global p function data 
PAR_p_annual <- plyr::ldply(long_site_names$site, load_p_annual, .parallel = T)

# Yearly p functions
fig_S2 <- ggplot(PAR_p_annual, aes(x = irradianceLevel, y = YearlyPfunction)) +
  # geom_line(aes(group = Years), colour = "black", linewidth = 1.5) +
  geom_line(aes(colour = Years, group = Years), linewidth = 1) +
  scale_x_continuous(trans = ggforce::trans_reverser("log10"), expand = c(0, 0), 
                     breaks = c(1, 0.1, 0.01), labels = c(1, 0.1, 0.01)) +
  scale_y_continuous(limits = c(0, 50), expand = c(0, 0), breaks = c(10, 20, 30, 40)) +
  scale_colour_viridis_c(option = "F") +
  facet_wrap(~site, nrow = 2, ncol = 4) +
  labs(x = "mol photons m-2 day-1", y = "% of surface receiving value [x-axis]", colour = "Year") +
  theme(legend.position = c(0.875, 0.23),
        legend.title = element_text(colour = "black", size = 12),
        legend.text = element_text(colour = "black", size = 10),
        legend.box.background = element_rect(colour = "black", fill = "white"),
        axis.text = element_text(colour = "black", size = 10),
        axis.title = element_text(colour = "black", size = 12),
        panel.border = element_rect(colour = "black", fill  = NA))
# fig_S2

# Save
ggsave("figures/fig_S2.png", fig_S2, width = 8, height = 6)


# Figure S3 ---------------------------------------------------------------
# The monthly climatologies for surface PAR and Kd


# Table 1 -----------------------------------------------------------------
# Light requirements by species


# Table 2 -----------------------------------------------------------------
# Changes to inhabitable area over time by site

