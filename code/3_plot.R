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

