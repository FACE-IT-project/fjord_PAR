# code/3_plot
# Create the figures/tables used in the manuscript
# Also contans code for exploratory visualisations


# Setup -------------------------------------------------------------------

source("code/0_functions.R")


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

