install.packages("neonDivData", repos = 'https://daijiang.r-universe.dev')
library(neonDivData)
library(dplyr)

df<-data_beetle
df<-merge(data_beetle, neon_sites, by="siteID")

df_plot <- df %>%
  group_by(plotID) %>%
  summarise(
    n_unique_taxa = n_distinct(taxon_id),
    mean_latitude = mean(latitude, na.rm = TRUE),
    mean_longitude = mean(longitude, na.rm = TRUE),
    .groups = "drop"
  )

df_site <- df %>%
  group_by(siteID) %>%
  summarise(
    n_unique_taxa = n_distinct(taxon_id),
    mean_latitude = mean(latitude, na.rm = TRUE),
    mean_longitude = mean(longitude, na.rm = TRUE),
    .groups = "drop"
  )

df_domain <- df %>%
  group_by(domainID) %>%
  summarise(
    n_unique_taxa = n_distinct(taxon_id),
    mean_latitude = mean(latitude, na.rm = TRUE),
    mean_longitude = mean(longitude, na.rm = TRUE),
    .groups = "drop"
  )

library(ggplot2)
library(maps)

# basemaps
world  <- map_data("world")
states <- map_data("state")

# calculate data extent with a little padding
x_range <- range(df_summary$mean_longitude, na.rm = TRUE)
y_range <- range(df_summary$mean_latitude, na.rm = TRUE)
x_pad <- diff(x_range) * 0.1
y_pad <- diff(y_range) * 0.1

# plot
ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group),
               fill = "gray95", color = "gray80") +
  geom_polygon(data = states, aes(x = long, y = lat, group = group),
               fill = NA, color = "gray80", linewidth = 0.4) +
  geom_point(data = df_summary,
             aes(x = mean_longitude, y = mean_latitude, color = n_unique_taxa),
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
  geom_point(data = df_domain,
             aes(x = mean_longitude, y = mean_latitude, color = n_unique_taxa),
             size = 3) +
  scale_color_viridis_c(option = "plasma") +
  coord_cartesian(
    xlim = c(x_range[1] - x_pad, x_range[2] + x_pad),
    ylim = c(y_range[1] - y_pad, y_range[2] + y_pad)
  ) +
  labs(x = "Longitude", y = "Latitude", color = "Species\nRichness") +
  theme_minimal()

min(df_site$n_unique_taxa)
max(df_site$n_unique_taxa)

plot(df_plot$n_unique_taxa~df_plot$mean_latitude)
plot(df_site$n_unique_taxa~df_site$mean_latitude)
plot(df_domain$n_unique_taxa~df_domain$mean_latitude)

library(ggpubr)

png("/home/aly/Beetles/figures/SppRichness_Layout.png", height = 8, width = 10, units = "in", res = 300)
ggarrange(
  ggplot(df_plot, aes(x=mean_latitude, y=n_unique_taxa)) +
    geom_point() +
    theme_pubr() +
    xlab("Latitude") +
    ylab("Species Richness") +
    ggtitle("Plot Level Species Richness"),
  ggplot(df_site, aes(x=mean_latitude, y=n_unique_taxa)) +
    geom_point() +
    theme_pubr() +
    xlab("Latitude") +
    ylab("Species Richness") + 
    ggtitle("Site Level Species Richness"),
  ggplot(df_domain, aes(x=mean_latitude, y=n_unique_taxa)) +
    geom_point() +
    theme_pubr() +
    xlab("Latitude") +
    ylab("Species Richness")+ 
    ggtitle("Domain Level Species Richness"),
  ggplot() +
    geom_polygon(data = world, aes(x = long, y = lat, group = group),
                 fill = "gray95", color = "gray80") +
    geom_polygon(data = states, aes(x = long, y = lat, group = group),
                 fill = NA, color = "gray80", linewidth = 0.4) +
    geom_point(data = df_summary,
               aes(x = mean_longitude, y = mean_latitude, color = n_unique_taxa),
               size = 3) +
    scale_color_viridis_c(option = "plasma") +
    coord_cartesian(
      xlim = c(x_range[1] - x_pad, x_range[2] + x_pad),
      ylim = c(y_range[1] - y_pad, y_range[2] + y_pad)
    ) +
    labs(x = "Longitude", y = "Latitude", color = "Site Species\nRichness") +
    theme_pubr() ,
  nrow = 2, ncol =2
  )
dev.off()
