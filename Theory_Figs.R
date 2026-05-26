# Load libraries
library(ggplot2)

#### Plots ####
# Simulate data with a strong negative relationship
set.seed(123)
n <- 50
overlap <- runif(n, 0, 1)  # Overlap between 0 and 1
species_richness <- 100 - 80*overlap + rnorm(n, sd = 5) # negative trend with noise

# Put into a data frame
df <- data.frame(
  Overlap = overlap,
  SpeciesRichness = species_richness
)

# Plot with trend line
p1<-ggplot(df, aes(x = Overlap, y = SpeciesRichness)) +
  #geom_point(color = "darkgreen", size = 2) +
  geom_smooth(method = "lm", color = "black", se = TRUE) +
  labs(
    x = "Overlap",
    y = "Species Richness",
    title = "Niche Theory Prediction"
  ) + 
  ylim(20,100)+
  theme_pubr(base_size = 14)

# Simulate data with a strong positive relationship
set.seed(456)
n <- 50
overlap <- runif(n, 0, 1)  # Overlap between 0 and 1
species_richness <- 20 + 80*overlap + rnorm(n, sd = 5) # positive trend with noise

# Put into a data frame
df <- data.frame(
  Overlap = overlap,
  SpeciesRichness = species_richness
)

# Plot with trend line
p2<-ggplot(df, aes(x = Overlap, y = SpeciesRichness)) +
  #geom_point(color = "darkgreen", size = 2) +
  geom_smooth(method = "lm", color = "black", se = TRUE) +
  labs(
    x = "Overlap",
    y = "Species Richness",
    title = "Individual Theory Prediction"
  ) +
  ylim(20,100)+
  theme_pubr(base_size = 14)


# Plot with trend line
p3<-ggplot(df, aes(x = Overlap, y = SpeciesRichness)) +
  #geom_point(color = "darkgreen", size = 2) +
  geom_smooth(method = "lm", color = "white", se = FALSE) +
  geom_abline(intercept = 65, slope = 0) +
  labs(
    x = "Overlap",
    y = "Species Richness",
    title = "Neutral Theory Prediction"
  ) +
  ylim(20,100)+
  theme_pubr(base_size = 14)

png("/home/aly/Beetles/figures/PlotTheories.png", height = 3, width = 11, units = "in", res = 300)
ggarrange(p1, p2, p3, nrow = 1)
dev.off()

#### Sites ####
# Simulate data with a strong negative relationship
set.seed(123)
n <- 50
overlap <- runif(n, 0, 1)  # Overlap between 0 and 1
species_richness <- 80 - 40*overlap + rnorm(n, sd = 20) # negative trend with noise

# Put into a data frame
df <- data.frame(
  Overlap = overlap,
  SpeciesRichness = species_richness
)

# Plot with trend line
p1<-ggplot(df, aes(x = Overlap, y = SpeciesRichness)) +
  #geom_point(color = "darkgreen", size = 2) +
  geom_smooth(method = "lm", color = "black", se = TRUE) +
  labs(
    x = "Overlap",
    y = "Species Richness",
    title = "Niche Theory Prediction"
  ) +
  ylim(20,100) +
  theme_pubr(base_size = 14)

# Simulate data with a strong positive relationship
set.seed(456)
n <- 50
overlap <- runif(n, 0, 1)  # Overlap between 0 and 1
species_richness <- 30 + 40*overlap + rnorm(n, sd = 20) # positive trend with noise

# Put into a data frame
df <- data.frame(
  Overlap = overlap,
  SpeciesRichness = species_richness
)

# Plot with trend line
p2<-ggplot(df, aes(x = Overlap, y = SpeciesRichness)) +
  #geom_point(color = "darkgreen", size = 2) +
  geom_smooth(method = "lm", color = "black", se = TRUE) +
  labs(
    x = "Overlap",
    y = "Species Richness",
    title = "Individual Theory Prediction"
  ) +
  ylim(20,100) +
  theme_pubr(base_size = 14)


# Plot with trend line
p3<-ggplot(df, aes(x = Overlap, y = SpeciesRichness)) +
  #geom_point(color = "darkgreen", size = 2) +
  geom_smooth(method = "lm", color = "white", se = FALSE) +
  geom_abline(intercept = 65, slope = 0) +
  labs(
    x = "Overlap",
    y = "Species Richness",
    title = "Neutral Theory Prediction"
  ) +
  ylim(20,100) +
  theme_pubr(base_size = 14)

png("/home/aly/Beetles/figures/SiteTheories.png", height = 3, width = 11, units = "in", res = 300)
ggarrange(p1, p2, p3, nrow = 1)
dev.off()

#### Domain ####
# Simulate data with a strong negative relationship
set.seed(123)
n <- 50
overlap <- runif(n, 0, 1)  # Overlap between 0 and 1
species_richness <- 90 - 40*overlap + rnorm(n, sd = 40) # negative trend with noise

# Put into a data frame
df <- data.frame(
  Overlap = overlap,
  SpeciesRichness = species_richness
)

# Plot with trend line
p1<-ggplot(df, aes(x = Overlap, y = SpeciesRichness)) +
  #geom_point(color = "darkgreen", size = 2) +
  geom_smooth(method = "lm", color = "black", se = TRUE) +
  labs(
    x = "Overlap",
    y = "Species Richness",
    title = "Niche Theory Prediction"
  ) +
  ylim(20,100) +
  theme_pubr(base_size = 14)
p1
# Simulate data with a strong positive relationship
set.seed(456)
n <- 50
overlap <- runif(n, 0, 1)  # Overlap between 0 and 1
species_richness <- 40 + 40*overlap + rnorm(n, sd = 40) # positive trend with noise

# Put into a data frame
df <- data.frame(
  Overlap = overlap,
  SpeciesRichness = species_richness
)

# Plot with trend line
p2<-ggplot(df, aes(x = Overlap, y = SpeciesRichness)) +
  #geom_point(color = "darkgreen", size = 2) +
  geom_smooth(method = "lm", color = "black", se = TRUE) +
  labs(
    x = "Overlap",
    y = "Species Richness",
    title = "Individual Theory Prediction"
  ) +
  ylim(20,100) +
  theme_pubr(base_size = 14)
p2

# Plot with trend line
p3<-ggplot(df, aes(x = Overlap, y = SpeciesRichness)) +
  #geom_point(color = "darkgreen", size = 2) +
  geom_smooth(method = "lm", color = "white", se = FALSE) +
  geom_abline(intercept = 65, slope = 0) +
  labs(
    x = "Overlap",
    y = "Species Richness",
    title = "Neutral Theory Prediction"
  ) +
  ylim(20,100) +
  theme_pubr(base_size = 14)

png("/home/aly/Beetles/figures/DomainTheories.png", height = 3, width = 11, units = "in", res = 300)
ggarrange(p1, p2, p3, nrow = 1)
dev.off()
