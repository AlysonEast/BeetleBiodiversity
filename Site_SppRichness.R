install.packages("neonDivData", repos = 'https://daijiang.r-universe.dev')
library(neonDivData)
library(dplyr)

setwd("/home/aly/Beetles/BeetleBiodiversity")

# df<-data_beetle
# df<-merge(data_beetle, neon_sites, by="siteID")

# df_plot <- df %>%
#   group_by(plotID) %>%
#   summarise(
#     median_richness = n_distinct(taxon_id),
#     latitude = mean(latitude, na.rm = TRUE),
#     mean_longitude = mean(longitude, na.rm = TRUE),
#     .groups = "drop"
#   )
# 
# df_site <- df %>%
#   group_by(siteID) %>%
#   summarise(
#     median_richness = n_distinct(taxon_id),
#     latitude = mean(latitude, na.rm = TRUE),
#     mean_longitude = mean(longitude, na.rm = TRUE),
#     .groups = "drop"
#   )
# 
# df_domain <- df %>%
#   group_by(domainID) %>%
#   summarise(
#     median_richness = n_distinct(taxon_id),
#     latitude = mean(latitude, na.rm = TRUE),
#     mean_longitude = mean(longitude, na.rm = TRUE),
#     .groups = "drop"
#   )

df_site<-read.csv("./Site_annualVarWeightedMean_EstimatedSppRichness.csv")
df_site<-merge(df_site, neon_sites, by.x="Assemblage", by.y="siteID")
df_plot<-read.csv("./plot_annualVarWeightedMean_EstimatedSppRichness.csv")
neon_location$type<-substr(neon_location$location_id, (nchar(neon_location$location_id)-2), nchar(neon_location$location_id))
neon_location<-subset(neon_location, type=="bet")
df_plot<-merge(df_plot, neon_location, by.x="PlotID", by="plotID")


library(ggplot2)
library(maps)

# basemaps
world  <- map_data("world")
states <- map_data("state")

# calculate data extent with a little padding
x_range <- range(df_site$Longitude, na.rm = TRUE)
y_range <- range(df_site$Latitude, na.rm = TRUE)
x_pad <- diff(x_range) * 0.1
y_pad <- diff(y_range) * 0.1

# Site
ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group),
               fill = "gray95", color = "gray80") +
  geom_polygon(data = states, aes(x = long, y = lat, group = group),
               fill = NA, color = "gray80", linewidth = 0.4) +
  geom_point(data = df_site,
             aes(x = Longitude, y = Latitude, color = median_richness),
             size = 3) +
  scale_color_viridis_c(option = "plasma") +
  coord_cartesian(
    xlim = c(x_range[1] - x_pad, x_range[2] + x_pad),
    ylim = c(y_range[1] - y_pad, y_range[2] + y_pad)
  ) +
  labs(x = "Longitude", y = "Latitude", color = "Species\nRichness") +
  theme_minimal()

# plot
ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group),
               fill = "gray95", color = "gray80") +
  geom_polygon(data = states, aes(x = long, y = lat, group = group),
               fill = NA, color = "gray80", linewidth = 0.4) +
  geom_point(data = df_plot,
             aes(x = longitude, y = latitude, color = median_richness),
             size = 3) +
  scale_color_viridis_c(option = "plasma") +
  coord_cartesian(
    xlim = c(x_range[1] - x_pad, x_range[2] + x_pad),
    ylim = c(y_range[1] - y_pad, y_range[2] + y_pad)
  ) +
  labs(x = "Longitude", y = "Latitude", color = "Species\nRichness") +
  theme_minimal()

min(df_site$median_richness)
max(df_site$median_richness)

plot(df_plot$median_richness~df_plot$latitude)
plot(df_site$median_richness~df_site$Latitude)
#plot(df_domain$median_richness~df_domain$latitude)

library(ggpubr)

png("./Figures/SppRichness_Layout.png", height = 8, width = 10, units = "in", res = 300)
ggarrange(
  ggplot() +
    geom_polygon(data = world, aes(x = long, y = lat, group = group),
                 fill = "gray95", color = "gray80") +
    geom_polygon(data = states, aes(x = long, y = lat, group = group),
                 fill = NA, color = "gray80", linewidth = 0.4) +
    geom_point(data = df_site,
               aes(x = Longitude, y = Latitude, color = median_richness),
               size = 3) +
    scale_color_viridis_c(option = "plasma") +
    coord_cartesian(
      xlim = c(x_range[1] - x_pad, x_range[2] + x_pad),
      ylim = c(y_range[1] - y_pad, y_range[2] + y_pad)
    ) +
    labs(x = "Longitude", y = "Latitude", color = "Site Species\nRichness") +
    theme_pubr() ,
  ggplot(df_plot, aes(x=latitude, y=median_richness)) +
    geom_point() +
    theme_pubr() +
    xlab("Latitude") +
    ylab("Species Richness") +
    ylim(0,max(df_site$median_richness)) +
    ggtitle("Plot Level Species Richness"),
  ggplot(df_site, aes(x=Latitude, y=median_richness)) +
    geom_point() +
    theme_pubr() +
    xlab("Latitude") +
    ylab("Species Richness") + 
    ylim(0,max(df_site$median_richness)) +
    ggtitle("Site Level Species Richness"),
  # ggplot(df_domain, aes(x=latitude, y=median_richness)) +
  #   geom_point() +
  #   theme_pubr() +
  #   xlab("Latitude") +
  #   ylab("Species Richness")+ 
  #   ggtitle("Domain Level Species Richness"),
  nrow = 1#, ncol =2
  )
dev.off()

ggplot(df_site, aes(x=Latitude, y=median_richness)) +
  geom_point() +
  geom_errorbar(aes(ymin = median_lcl, ymax = median_ucl), width = 0.1) +
  theme_pubr() +
  xlab("Latitude") +
  ylab("Species Richness") + 
  ggtitle("Site Level Species Richness")
ggplot(df_plot, aes(x=jitter(latitude, 5), y=median_richness)) +
  geom_point(alpha=0.5) +
  geom_errorbar(aes(ymin = median_lcl, ymax = median_ucl), width = 0.1, alpha=0.5) +
  theme_pubr() +
  xlab("Latitude") +
  ylab("Species Richness") + 
  ggtitle("Plot Level Species Richness")

ggplot(df_plot, aes(x=latitude, y=median_completness, colour = median_richness)) +
  geom_point() +
  theme_pubr() +
  xlab("Latitude") +
  ylab("Species Richness") + 
  # ylim(0,max(df_site$median_richness)) +
  ggtitle("Site Level Species Richness")

ggplot() +
    geom_polygon(data = world, aes(x = long, y = lat, group = group),
                 fill = "gray95", color = "gray80") +
    geom_polygon(data = states, aes(x = long, y = lat, group = group),
                 fill = NA, color = "gray80", linewidth = 0.4) +
    geom_point(data = df_plot,
               aes(x = jitter(longitude, 300), y = jitter(latitude, 300), color = median_completness),
               size = 3,
               shape = 0, 
               stroke = 2) +
    scale_color_viridis_c(option = "plasma") +
    coord_cartesian(
      xlim = c(x_range[1] - x_pad, x_range[2] + x_pad),
      ylim = c(y_range[1] - y_pad, y_range[2] + y_pad)
    ) +
    labs(x = "Longitude", y = "Latitude", color = "Site Rarefaction\nCompletness") +
    theme_pubr() 
