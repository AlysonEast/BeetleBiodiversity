library(neonDivData)
library(dplyr)
library(tidyr)
library(vegan)
library(iNEXT)
library(scales)
library(ggplot2)
library(ggpubr)

setwd("/home/aly/Beetles/Biodiversity")

data(data_beetle)

df <- data_beetle

#### Create Species Column ####
df$Species <- ifelse(
  df$taxon_rank == "species",
  paste0(df$taxon_name),
  ifelse(
    df$taxon_rank == "subspecies",
    sub("^(\\S+\\s+\\S+).*", "\\1", df$taxon_name),
    NA
  )
)

#### Empty objects to store outputs ####
all_completeness <- data.frame()
all_abundance <- data.frame()

#### Loop through years ####
for(i in 2018:2025){
  
  cat("Processing year:", i, "\n")
  
  #### Filter yearly data ####
  df_counts <- df %>%
    filter(
      !is.na(Species),
      variable_name == "abundance",
      observation_datetime >= paste0(i, "-01-01"),
      observation_datetime <= paste0(i, "-12-31")
    ) %>%
    mutate(
      trappingDays = as.numeric(trappingDays),
      count_est = round(value * trappingDays)
    )
  
  #### Skip years with no data ####
  if(nrow(df_counts) == 0){
    next
  }
  
  #### Build site list for iNEXT ####
  site_list <- df_counts %>%
    group_by(siteID, Species) %>%
    summarise(abundance = sum(count_est), .groups = "drop") %>%
    group_split(siteID) %>%
    setNames(unique(
      df_counts %>%
        arrange(siteID) %>%
        pull(siteID) %>%
        unique()
    )) %>%
    lapply(function(x) x$abundance[x$abundance > 0])
  
  #### Run iNEXT ####
  iNEXT_sites <- iNEXT(
    site_list,
    q = 0,
    datatype = "abundance"
  )
  
  #### Create rarefaction plot ####
  p <- ggiNEXT(iNEXT_sites, type = 1) +
    theme_pubr() +
    xlab("Number of Individuals") +
    scale_x_continuous(labels = scales::label_number(accuracy = 1)) +
    theme(legend.position = "none") +
    facet_wrap(. ~ Assemblage, nrow = 4, scales = "free")
  
  #### Save plot ####
  png(
    filename = paste0("./Figures/Rarefaction/Rarefaction_", i, ".png"),
    width = 20,
    height = 6,
    units = "in",
    res = 300
  )
  
  print(p)
  
  dev.off()
  
  #### Extract completeness ####
  site_completeness <- iNEXT_sites$AsyEst %>%
    filter(Diversity == "Species richness") %>%
    mutate(
      completeness = Observed / Estimator,
      Year = i
    )
  
  #### Append completeness ####
  all_completeness <- bind_rows(
    all_completeness,
    site_completeness
  )
  
  #### Calculate abundance summaries ####
  abundance_summary <- df_counts %>%
    group_by(siteID) %>%
    summarise(
      total_abundance = sum(count_est, na.rm = TRUE),
      richness = n_distinct(Species),
      Year = i,
      .groups = "drop"
    )
  
  #### Append abundance ####
  all_abundance <- bind_rows(
    all_abundance,
    abundance_summary
  )
  
}


df_counts <- df %>%
  filter(
    !is.na(Species),
    variable_name == "abundance",
    observation_datetime >= "2018-01-01",
    observation_datetime <= "2025-12-31"
  ) %>%
  mutate(
    trappingDays = as.numeric(trappingDays),
    count_est = round(value * trappingDays),
    Year = format(as.Date(observation_datetime), "%Y"),
    SiteYear = paste(siteID, Year, sep = "_")
  )

siteyear_list <- df_counts %>%
  group_by(SiteYear, Species) %>%
  summarise(abundance = sum(count_est), .groups = "drop") %>%
  group_split(SiteYear) %>%
  setNames(unique(
    df_counts %>%
      arrange(SiteYear) %>%
      pull(SiteYear) %>%
      unique()
  )) %>%
  lapply(function(x) x$abundance[x$abundance > 0])

iNEXT_sitesByYear <- iNEXT(
  siteyear_list,
  q = 0,
  datatype = "abundance"
)

png(filename = paste0("./Figures/Rarefaction/Rarefaction_yearsStacked.png"),
  width = 22,
  height = 10,
  units = "in",
  res = 300)
ggiNEXT(iNEXT_sitesByYear, type = 1)  + 
  aes(colour = substr(Assemblage, 6, 9), x=(x/100))+
  theme_pubr() +
  xlab("Number of Individuals (x100)") +
  scale_shape_manual(values=c(rep(4,length(siteyear_list)))) +
  scale_fill_manual(values = c(rep("grey",523)))+
  scale_x_continuous(labels = scales::label_number(accuracy = 1),
                     breaks = breaks_extended(n = 4)) +
  theme(legend.position = "none") +
  facet_wrap(.~substr(Assemblage, 1, 4), nrow=4, scale="free")
dev.off()

#### All Years ####
df_counts <- df %>%
  filter(!is.na(Species),
         variable_name == "abundance",
         observation_datetime >= paste0("2018-01-01"),
         observation_datetime <= paste0("2025-12-31")) %>%
  mutate(trappingDays = as.numeric(trappingDays),
         count_est = round(value * trappingDays))

site_list <- df_counts %>%
  group_by(siteID, Species) %>%
  summarise(abundance = sum(count_est), .groups = "drop") %>%
  group_split(siteID) %>%
  setNames(unique(df_counts$siteID)) %>%
  lapply(function(x) x$abundance[x$abundance > 0])

iNEXT_sites <- iNEXT(site_list,
                     q = 0,
                     datatype = "abundance")

#Saturation
unconstrained_completeness$saturated <- ifelse(
  unconstrained_completeness$Observed >= unconstrained_completeness$LCL,
  "Saturated",
  "Unsaturated")
all_completeness$saturated <- ifelse(
  all_completeness$Observed >= all_completeness$LCL,
  "Saturated",
  "Unsaturated")

#Plots
ggplot(all_completeness, aes(y = completeness, x = Year, colour = saturated)) +
  geom_point() +
  geom_hline(data = unconstrained_completeness, aes(yintercept = completeness, colour = saturated), linetype = "dashed") +
  facet_wrap(. ~ Assemblage) +
  theme_pubr()

ggplot(all_completeness,
  aes(x = Year, y = Estimator, color = completeness)) +
  geom_point(size = 2) +
  geom_hline(data = unconstrained_completeness, aes(yintercept = Estimator, colour = completeness), linetype = "dashed") +
  geom_errorbar(aes(ymin = LCL, ymax = UCL), width = 0.2) +
  facet_wrap(. ~ Assemblage, scales = "free") +
  theme_pubr()

plot_dat <- all_completeness %>%
  pivot_longer(cols = c(Observed, Estimator),
    names_to = "Metric",
    values_to = "Richness")

ggplot(all_completeness, aes(x = Year)) +
  geom_point(aes(y = Estimator, colour = completeness), size = 2) +
  geom_line(aes(y = Estimator, group = 1), colour = "black", alpha = 0.5) +
  geom_errorbar(aes(ymin = LCL, ymax = UCL, colour = completeness), width = 0.2) +
  geom_point(aes(y = Observed), shape = 4, colour = "black", size = 2) +
  geom_line(aes(y = Observed, group = 1), colour = "black", linetype = "dotted") +
  geom_hline(data = unconstrained_completeness, aes(yintercept = Estimator), linetype = "dashed") +
  facet_wrap(. ~ Assemblage, scales = "free") +
  theme_pubr() +
  ylab("Species Richness")

#### Export final outputs ####
write.csv(
  all_completeness,
  "./All_Years_Completeness.csv",
  row.names = FALSE
)

write.csv(
  all_abundance,
  "./All_Years_Abundance.csv",
  row.names = FALSE
)