# code/3_plot
# Create the figures/tables used in the manuscript
# Also contains code for exploratory visualisations


# Setup -------------------------------------------------------------------

# Load functions and packages
source("code/0_functions.R")
library(ggOceanMaps)
library(ggspatial)
library(ggblend) # allows drawing under lines

# Load processed data from '2_analyse.R'
# load("data/P_all.RData") # Deprecated
load("data/PAR_annual_summary.RData")
load("data/PAR_clim_summary.RData")
load("data/PAR_monthly_summary.RData")
load("data/PAR_spatial_summary.RData")
load("data/PAR_annual_lm.RData")
load("data/PAR_monthly_lm.RData")
load("data/PAR_spatial_lm.RData")


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

# Create plots (and csv files) that help investigate artefacts
plyr::l_ply(long_site_names$site, plot_surface, .parallel = F)

# Look at monthly K_PAR
kong_K_PAR <- fl_LoadFjord("kong", "K_PAR", "data/PAR", TS = TRUE)
kong_K_PAR_monthly_plot <- ggplot(PAR_kong_p_yearly, aes(x = irradianceLevel, y = YearlyPfunction)) +
  geom_line(aes(colour = Years, group = Years), linewidth = 2) +
  scale_x_continuous(trans = ggforce::trans_reverser("log10"), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 40), expand = c(0, 0), breaks = c(10, 20, 30)) +
  scale_colour_viridis_c() +
  labs(x = "E mol photons m-2 day-1", y = "% of surface receiving more than E", colour = "Year") +
  theme(legend.position = "bottom", panel.background = element_rect(colour = "black", fill  = "grey"))

KONG_PAR_B <- fl_LoadFjord("kong", dirdata = "data/PAR", TS = FALSE)
P07 <- flget_climatology(KONG_PAR_B, optics = "PAR0m", period = "Clim", month = 7, PLOT = TRUE)
print(P07)


# Figure 1 ----------------------------------------------------------------
# Map of the study area + seven sites showing global surface PAR

# Consider having a pop-out panel for Svalbard sites:
# https://cran.r-project.org/web/packages/ggautomap/vignettes/ggautomap.html
# This would allow for the points to have large number labels to associate between panels
# Unfortunately, how to get this to work with the output of ggoceanmaps is not readily clear

# Load base data
# NB: Only used for Figure 1
PAR_kong <- fl_LoadFjord("kong", dirdata = "data/PAR")
PAR_is <- fl_LoadFjord("is", dirdata = "data/PAR")
PAR_stor <- fl_LoadFjord("stor", dirdata = "data/PAR")
PAR_young <- fl_LoadFjord("young", dirdata = "data/PAR")
PAR_disko <- fl_LoadFjord("disko", dirdata = "data/PAR")
PAR_nuup <- fl_LoadFjord("nuup", dirdata = "data/PAR")
PAR_por <- fl_LoadFjord("por", dirdata = "data/PAR")

# Extract global surface values
PAR_global_kong <- flget_climatology(PAR_kong, optics = "PAR0m", period = "Global", mode = "df")
PAR_global_is <- flget_climatology(PAR_is, optics = "PAR0m", period = "Global", mode = "df")
PAR_global_stor <- flget_climatology(PAR_stor, optics = "PAR0m", period = "Global", mode = "df")
PAR_global_young <- flget_climatology(PAR_young, optics = "PAR0m", period = "Global", mode = "df")
PAR_global_disko <- flget_climatology(PAR_disko, optics = "PAR0m", period = "Global", mode = "df")
PAR_global_nuup <- flget_climatology(PAR_nuup, optics = "PAR0m", period = "Global", mode = "df")
PAR_global_por <- flget_climatology(PAR_por, optics = "PAR0m", period = "Global", mode = "df")

# Range of surface PAR values
PAR_global <- c(PAR_global_kong, PAR_global_is, PAR_global_stor, 
                PAR_global_young, PAR_global_disko, PAR_global_nuup, PAR_global_por)
PAR_range <- range(PAR_global$PAR0m_Global, na.rm = T)
PAR_quant <- c(quantile(PAR_global$PAR0m_Global, 0.01, na.rm = T),
               quantile(PAR_global$PAR0m_Global, 0.99, na.rm = T))

# Study sites
site_points <- data.frame(site = factor(x = c("Kongsfjorden", "Isfjorden", "Storfjorden", 
                                              "Young Sound", "Qeqertarsuup Tunua", "Nuup Kangerlua", 
                                              "Porsangerfjorden"),
                                        levels = c("Kongsfjorden", "Isfjorden","Storfjorden", 
                                                   "Young Sound", "Qeqertarsuup Tunua", "Nuup Kangerlua", 
                                                   "Porsangerfjorden")),
                          point_text = c("B", "C", "D", "E", "F", "G", "H"),
                          label = c("B)", "C)", "D)", "E)", "F)", "G)", "H)"),
                          lon = c(11.845, 14.365, 19.88, -21.237, -52.555, -50.625, 25.75),
                          lat = c(78.98, 78.235, 77.78, 74.517, 69.36, 64.405, 70.6))

# Full study area
fig_1_base <- basemap(limits = c(-60, 30, 65, 90), bathymetry = T) +
  # Other labels
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
  geom_spatial_label(aes(x = 24, y = 68, label = "Northern\nNorway"), 
                     colour = "black", crs = 4326, size = 4, alpha = 0.5) +
  # Site points
  geom_spatial_point(data = site_points, size = 7, crs = 4326,
                     aes(x = lon, y = lat), colour = "black") +
  geom_spatial_point(data = site_points, size = 6, crs = 4326,
                     aes(x = lon, y = lat, colour = site)) +
  geom_spatial_text(data = site_points, size = 5, crs = 4326,
                     aes(x = lon, y = lat, label = point_text)) +
  scale_colour_manual("Site", values = site_colours) +
  # Other minutia
  labs(x = NULL, y = NULL) +
  guides(colour = "none", fill = guide_legend("Depth [m]", title.hjust = 1)) + 
  theme(panel.border = element_rect(colour = "black", fill = NA),
        panel.background = element_rect(fill = NA, colour = "black"),
        plot.background = element_rect(fill = "white", colour = NA),
        axis.text = element_text(colour = "black", size = 10),
        legend.direction = "horizontal",
        legend.position = c(0.295, 0.932),
        legend.box.margin = margin(5, 20, 5, 10), 
        legend.box.background = element_rect(fill = "white", colour = "black"))
# fig_1_base

# Add Surface PAR site panels
fig_1_kong <- fig_1_subplot(PAR_kong, "Kongsfjorden", PAR_quant)
fig_1_is <- fig_1_subplot(PAR_is, "Isfjorden", PAR_quant)
fig_1_stor <- fig_1_subplot(PAR_stor, "Storfjorden", PAR_quant)
fig_1_young <- fig_1_subplot(PAR_young, "Young Sound", PAR_quant)
fig_1_disko <- fig_1_subplot(PAR_disko, "Qeqertarsuup Tunua", PAR_quant)
fig_1_nuup <- fig_1_subplot(PAR_nuup, "Nuup Kangerlua", PAR_quant)
fig_1_por <- fig_1_subplot(PAR_por, "Porsangerfjorden", PAR_quant)

# Get legend
PAR_legend_base <- PAR_global_kong %>% 
  mutate(x = 1:n(), y = 1) %>% 
  ggplot() + geom_point(aes(x = x, y = y, colour = PAR0m_Global)) +
  scale_colour_viridis_c(limits = PAR_quant,
                         guide = guide_colorbar(
                           direction = "horizontal",
                           title.position = "top")) +
  # labs(colour = expression(atop(PAR(0^"-"), [mol~photons~m]))) +
  labs(colour = latex2exp::TeX("PAR(0$^-$) [mol photons $m^{-2}$ $d^{-1}$]")) +
  theme(legend.title.align = 0.5,
        legend.key.height = unit(0.8, "cm"),
        legend.key.width = unit(2, "cm"),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.box.background = element_rect(fill = NA, colour = NA))

# PAR_legend_base
PAR_legend <- ggpubr::get_legend(PAR_legend_base)

# Placeholder
blank_plot <- ggplot() + geom_blank() + theme_void()

# Combine and save
fig_1_sites <- ggpubr::ggarrange(fig_1_kong, fig_1_is, fig_1_stor, blank_plot,
                                 fig_1_young, fig_1_disko, fig_1_nuup, fig_1_por,
                                 ncol = 4, nrow = 2)
fig_1 <- ggpubr::ggarrange(fig_1_base, PAR_legend, fig_1_sites, 
                                  labels = c("A)", "", ""), font.label = list(size = 10),
                                  ncol = 1, nrow = 3, heights = c(1.0, 0.15, 0.75)) +
  ggpubr::bgcolor("white") + ggpubr::border("white")
ggsave("figures/fig_1.png", fig_1, height = 12, width = 9)


# Figure 2 ----------------------------------------------------------------
# Line plot showing monthly climatological changes per site for surface values

# Surface clim
fig_2a <- PAR_clim_summary |> 
  filter(variable == "ClimPAR0m") |> 
  left_join(long_site_names, by = "site") |> 
  ggplot(aes(x = month, y = q50)) +
  # geom_line(aes(group = site_long), linewidth = 1.7) +
  geom_ribbon(aes(ymin = q50-sd, ymax = q50+sd, fill = site_long), alpha = 0.1) +
  geom_line(aes(colour = site_long), linewidth = 1.2) |> 
  copy_under(aes(group = site_long), color = "black", linewidth = 1.7) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_fill_manual("Site", values = site_colours, aesthetics = c("colour", "fill")) +
  labs(x = "Month",
       y = latex2exp::TeX("PAR($0^-$) [mol photons $m^{-2}$ $d^{-1}$]")) +
  theme(legend.title = element_text(colour = "black", size = 12),
        legend.text = element_text(colour = "black", size = 10, margin = margin(r = 10)),
        axis.text = element_text(colour = "black", size = 10),
        axis.title = element_text(colour = "black", size = 12),
        plot.margin = margin(5, 20, 10, 5),
        panel.border = element_rect(colour = "black", fill  = NA))
# fig_2a

# K_PAR clim
fig_2b <- PAR_clim_summary |> 
  filter(variable == "ClimKpar") |> 
  left_join(long_site_names, by = "site") |> 
  ggplot(aes(x = month, y = q50)) +
  geom_ribbon(aes(ymin = q50-sd, ymax = q50+sd, fill = site_long), alpha = 0.1) +
  geom_line(aes(group = site_long), linewidth = 1.7) +
  geom_line(aes(colour = site_long), linewidth = 1.2) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(breaks = c(0.1, 0.2, 0.3, 0.4)) +
  scale_fill_manual("Site", values = site_colours, aesthetics = c("colour", "fill")) +
  labs(x = "Month",
       y = latex2exp::TeX("K$_{PAR}$ [$m^{-1}$]")) +
  theme(legend.title = element_text(colour = "black", size = 12),
        legend.text = element_text(colour = "black", size = 10),
        axis.text = element_text(colour = "black", size = 10),
        axis.title = element_text(colour = "black", size = 12),
        panel.border = element_rect(colour = "black", fill  = NA))
# fig_2b

# Combine and save
fig_2 <- ggpubr::ggarrange(fig_2a, fig_2b, align = "hv", 
                           common.legend = T, legend = "bottom",
                           labels = c("A)", "B)"), ncol = 1, nrow = 2) +
  ggpubr::bgcolor("white") + ggpubr::border("white")
ggsave(filename = "figures/fig_2.png", plot = fig_2, height = 12, width = 8)


# Figure 3 ----------------------------------------------------------------
# Line plot showing annual changes per site for surface values

# p-values of slopes may be found here:
PAR_annual_lm

# Surface annual
fig_3a <- PAR_annual_summary |> 
  filter(variable == "YearlyPAR0m") |> 
  left_join(long_site_names, by = "site") |> 
  ggplot(aes(x = year, y = q50)) +
  geom_smooth(aes(colour = site_long), method = "lm", formula = "y ~ x", se = FALSE, linetype = "dashed") +
  geom_ribbon(aes(ymin = q50-sd, ymax = q50+sd, fill = site_long), alpha = 0.2) +
  geom_line(aes(group = site_long), linewidth = 1.7) +
  geom_line(aes(colour = site_long), linewidth = 1.2) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_fill_manual("Site", values = site_colours, aesthetics = c("colour", "fill")) +
  labs(x = "Year", 
       y = latex2exp::TeX("PAR($0^-$) [mol photons $m^{-2}$ $d^{-1}$]")) +
  theme(legend.title = element_text(colour = "black", size = 12),
        legend.text = element_text(colour = "black", size = 10, margin = margin(r = 10)),
        axis.text = element_text(colour = "black", size = 10),
        axis.title = element_text(colour = "black", size = 12),
        panel.border = element_rect(colour = "black", fill  = NA))
# fig_3a

# K_PAR annual
fig_3b <- PAR_annual_summary |> 
  filter(variable == "YearlyKpar") |> 
  left_join(long_site_names, by = "site") |> 
  ggplot(aes(x = year, y = q50)) +
  geom_smooth(aes(colour = site_long), method = "lm", formula = "y ~ x", se = FALSE, linetype = "dashed") +
  geom_ribbon(aes(ymin = q50-sd, ymax = q50+sd, fill = site_long), alpha = 0.2) +
  geom_line(aes(group = site_long), linewidth = 1.7) +
  geom_line(aes(colour = site_long), linewidth = 1.2) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_fill_manual("Site", values = site_colours, aesthetics = c("colour", "fill")) +
  labs(x = "Year",
       y = latex2exp::TeX("K$_{PAR}$ [$m^{-1}$]")) +
  theme(legend.title = element_text(colour = "black", size = 12),
        legend.text = element_text(colour = "black", size = 10, margin = margin(r = 10)),
        axis.text = element_text(colour = "black", size = 10),
        axis.title = element_text(colour = "black", size = 12),
        panel.border = element_rect(colour = "black", fill  = NA))
# fig_3b

# Combine and save
fig_3 <- ggpubr::ggarrange(fig_3a, fig_3b, align = "hv", 
                            common.legend = T, legend = "bottom",
                            labels = c("A)", "B)"), ncol = 1, nrow = 2)+
  ggpubr::bgcolor("white") + ggpubr::border("white")
ggsave(filename = "figures/fig_3.png", plot = fig_3, height = 12, width = 8)


# Figure 4 ----------------------------------------------------------------
# Line plots showing change in bottom PAR per month over time

# p-values of slopes may be found here:
PAR_monthly_lm

# Significant slopes
filter(PAR_monthly_lm, p.value <= 0.05)

# Bottom PAR
fig_4 <- PAR_monthly_summary |> 
  left_join(long_site_names, by = "site") |> 
  ggplot(aes(x = year, y = q50)) +
  geom_smooth(aes(colour = as.factor(month)), method = "lm", 
              formula = "y ~ x", se = FALSE, linetype = "dashed") +
  geom_line(aes(group = as.factor(month)), linewidth = 1.5) +
  geom_line(aes(colour = as.factor(month)), linewidth = 1.0) +
  facet_wrap(~site_long, ncol = 2, scales = "free_y") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_colour_viridis_d("Month", option = "A") +
  # guides(fill = guide_legend(override.aes = list(shape = 15))) + # Doesn't work...
  labs(x = "Year", y = latex2exp::TeX("PAR$_B$ [mol photons $m^{-2}$ d$^{-1}$]")) +
  theme(legend.position = c(0.75, 0.1), 
        legend.direction = "horizontal",
        legend.title = element_text(colour = "black", size = 12),
        legend.text = element_text(colour = "black", size = 10),
        legend.box.background = element_rect(colour = "black", fill = "white"),
        axis.text = element_text(colour = "black", size = 10),
        axis.title = element_text(colour = "black", size = 12),
        panel.border = element_rect(colour = "black", fill  = NA))
# fig_4

# Save
ggsave(filename = "figures/fig_4.png", plot = fig_4, height = 7, width = 7)


# Figure 5 ----------------------------------------------------------------
# P-functions per site
# Similar to how they are shown in the other two publications that came before

# Use built-in P-functions
PAR_p_global <- plyr::ldply(long_site_names$site, load_p_global, .parallel = T)

# Or those calculated manually - deprecated
# P_all_site <- left_join(P_all, long_site_names, by = "site") |>
#   dplyr::select(-site) |>  dplyr::rename(site = site_long)

# Yearly p functions
fig_5 <- ggplot(PAR_p_global, aes(x = irradianceLevel, y = GlobalPshallow)) +
  geom_line(aes(group = site), colour = "black", linewidth = 2.5) +
  geom_line(aes(colour = site), linewidth = 2) +
  scale_x_continuous(trans = ggforce::trans_reverser("log10"), expand = c(0, 0),
                     breaks = c(10, 1, 0.1, 0.01, 0.001),
                     labels = c(10, 1, 0.1, 0.01, 0.001)) + # Need to force these
  scale_y_continuous(limits = c(0, 100), expand = c(0, 0), breaks = c(10, 30, 60, 90)) +
  scale_colour_manual("Site", values = site_colours) +
  # guides(colour = guide_legend(override.aes = list(shape = 15, size = 5))) +
  # guides(colour = guide_legend(override.aes = list(shape = 18))) +
  labs(x = latex2exp::TeX("$PAR_B$ Threshold [T; mol photons $m^{-2}$ $d^{-1}$]"),
       y = latex2exp::TeX("Cumulative area receiving $PAR_{B}$ $\\geq$ T [%]", bold = FALSE)) +
  theme(legend.position = c(0.144, 0.805),
        legend.title = element_text(colour = "black", size = 12),
        legend.text = element_text(colour = "black", size = 10),
        legend.box.background = element_rect(colour = "black", fill = "white"),
        legend.margin = margin(3, 15, 3, 3),
        plot.margin = margin(5, 20, 5, 5),
        axis.text = element_text(colour = "black", size = 10),
        axis.title = element_text(colour = "black", size = 12),
        panel.border = element_rect(colour = "black", fill  = NA))
# fig_5

# Save
ggsave("figures/fig_5.png", fig_5, height = 6, width = 8)


# Figure S1 ---------------------------------------------------------------
# p functions per site for monthly clims

# Load global p function data 
PAR_p_clim <- plyr::ldply(long_site_names$site, load_p_clim, .parallel = T)

# Yearly p functions
fig_S1 <- ggplot(PAR_p_clim, aes(x = irradianceLevel, y = ClimPshallow)) +
  geom_line(aes(group = as.factor(Months)), colour = "black", linewidth = 1.2) +
  geom_line(aes(colour = as.factor(Months)), linewidth = 0.9) +
  scale_x_continuous(trans = ggforce::trans_reverser("log10"), expand = c(0, 0), 
                     breaks = c(1, 0.1, 0.01), labels = c(1, 0.1, 0.01)) +
  scale_y_continuous(limits = c(0, 100), expand = c(0, 0), breaks = c(10, 30, 60, 90)) +
  scale_colour_viridis_d("Month", option = "A") +
  facet_wrap(~site, nrow = 2, ncol = 4) +
  labs(x = latex2exp::TeX("$PAR_B$ Threshold [T; mol photons $m^{-2}$ $d^{-1}$]"),
       y = latex2exp::TeX("Cumulative area receiving $PAR_{B}$ $\\geq$ T [%]", bold = FALSE)) +
  theme(legend.position = c(0.88, 0.22), 
        legend.direction = "vertical",
        legend.title = element_text(colour = "black", size = 12),
        legend.text = element_text(colour = "black", size = 10),
        legend.box.background = element_rect(colour = "black", fill = "white"),
        axis.text = element_text(colour = "black", size = 10),
        axis.title = element_text(colour = "black", size = 12),
        panel.border = element_rect(colour = "black", fill  = NA))
# fig_S1

# Save
ggsave("figures/fig_S1.png", fig_S1, height = 6, width = 8)


# Figure S2 ---------------------------------------------------------------
# p functions per site for annual averages

# Load global p function data 
PAR_p_annual <- plyr::ldply(long_site_names$site, load_p_annual, .parallel = T)

# Yearly p functions
fig_S2 <- ggplot(PAR_p_annual, aes(x = irradianceLevel, y = YearlyPshallow)) +
  geom_line(aes(group = Years), colour = "black", linewidth = 1.0) +
  geom_line(aes(colour = Years, group = Years), linewidth = 0.8) +
  scale_x_continuous(trans = ggforce::trans_reverser("log10"), expand = c(0, 0), 
                     breaks = c(1, 0.1, 0.01), labels = c(1, 0.1, 0.01)) +
  scale_y_continuous(limits = c(0, 100), expand = c(0, 0), breaks = c(10, 30, 60, 90)) +
  scale_colour_viridis_c("Year", option = "D") +
  facet_wrap(~site, nrow = 2, ncol = 4) +
  labs(x = latex2exp::TeX("$PAR_B$ Threshold [T; mol photons $m^{-2}$ $d^{-1}$]"),
       y = latex2exp::TeX("Cumulative area receiving $PAR_{B}$ $\\geq$ T [%]", bold = FALSE)) +
  # guides(colour = guide_legend()) +
  theme(legend.position = c(0.88, 0.22),
        legend.direction = "vertical",
        legend.key.width = unit(1, "cm"),
        legend.title = element_text(colour = "black", size = 12),
        legend.text = element_text(colour = "black", size = 10),
        legend.box.background = element_rect(colour = "black", fill = "white"),
        axis.text = element_text(colour = "black", size = 10),
        axis.title = element_text(colour = "black", size = 12),
        panel.border = element_rect(colour = "black", fill  = NA))
# fig_S2

# Save
ggsave("figures/fig_S2.png", fig_S2, height = 6, width = 8)


# Table 1 -----------------------------------------------------------------
# Summary of cryosphere info per site
# This was manually compiled by the co-authors directly within the manuscript


# Table 2 -----------------------------------------------------------------
# Table with shallow and coastal pixels per site
table_2 <- plyr::ldply(list(PAR_kong, PAR_is, PAR_stor, PAR_young, PAR_disko, PAR_nuup, PAR_por), table_2_calc)

# Table 3 -----------------------------------------------------------------
# Light requirements by species
# This was manually compiled by the co-authors directly within the manuscript


# Table 4 -----------------------------------------------------------------
# Slope statistics for monthly PAR_B trends per site

# p-values of monthly slopes may be found here:
PAR_monthly_lm

# Process
PAR_monthly_lm_round <- PAR_monthly_lm |> 
  mutate(slope = round(slope, 4),
         p.value = round(p.value, 2)) |> 
  dplyr::select(site, month, slope, p.value)

# Pivot wider and combine columns
table_3 <- PAR_monthly_lm_round |> 
  mutate(stat = paste0(slope," (",p.value,")")) |> 
  dplyr::select(-slope, -p.value) |> 
  pivot_wider(names_from = month, values_from = stat)


# Table 5 -----------------------------------------------------------------
# Changes to inhabitable area over time by site

# Round for table
PAR_spatial_summary_round <- PAR_spatial_summary |> 
  mutate(bottom_area = round(bottom_area),
         global_area = round(global_area),
         global_perc = round(global_perc, 2),
         annual_area = round(annual_area),
         annual_perc = round(annual_perc, 2)) |> 
  dplyr::select(site, year, bottom_area, 
                global_area, global_perc, annual_area, annual_perc)

# Base values
PAR_spat_base <- PAR_spatial_summary_round |> 
  dplyr::select(site, bottom_area, global_perc) |> distinct()

# Create table of lowest values
PAR_spat_low <- PAR_spatial_summary_round |> 
  group_by(site) |> filter(annual_perc == min(annual_perc)) |>
  filter(year == min(year)) |> ungroup() |> 
  dplyr::select(site, year, annual_perc) |> 
  dplyr::rename(min_year = year, min_annual_perc = annual_perc)

# Create table of highest values
PAR_spat_high <- PAR_spatial_summary_round |> 
  group_by(site) |> filter(annual_perc == max(annual_perc)) |>
  filter(year == max(year)) |> ungroup() |> 
  dplyr::select(site, year, annual_perc) |> 
  dplyr::rename(max_year = year, max_annual_perc = annual_perc)

# Trends and p-values
PAR_spat_lm <- PAR_spatial_lm |> dplyr::select(-std.error) |> 
  mutate(slope = round(slope*100, 2),
         p.value = round(p.value, 2))

# Combine
table_4 <- left_join(PAR_spat_base, PAR_spat_low, by = "site") |> 
  left_join(PAR_spat_high, by = "site") |> left_join(PAR_spat_lm, by = "site")


# Table 6 -----------------------------------------------------------------
# List of variable names per value
# Compiled manually

# SD variable names
kong_PAR_B <- fl_LoadFjord("kong", "PAR_B", "data/PAR", TS = FALSE)
kong_K_PAR <- fl_LoadFjord("kong", "K_PAR", "data/PAR", TS = FALSE)
kong_YearlySD <- fl_LoadFjord("kong", "YearlySD", "data/PAR")
kong_ClimSD <- fl_LoadFjord("kong", "ClimSD", "data/PAR")


# Table 7 -----------------------------------------------------------------
# List of additional meta-data within each NetCDF
# Compiled manually


# Table 8 -----------------------------------------------------------------
# Count of good and bad pixels per site per month per year

