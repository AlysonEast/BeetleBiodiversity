library(neonDivData)
library(dplyr)
library(tidyr)
library(vegan)
library(iNEXT)
library(scales)
library(ggplot2)
library(ggpubr)
library(viridis)

setwd("/home/aly/Beetles/BeetleBiodiversity")

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
    PlotYear = paste(plotID, Year, sep = "_")
  )

PlotYear_list <- df_counts %>%
  group_by(PlotYear, Species) %>%
  summarise(abundance = sum(count_est), .groups = "drop") %>%
  group_split(PlotYear) %>%
  setNames(unique(
    df_counts %>%
      arrange(PlotYear) %>%
      pull(PlotYear) %>%
      unique()
  )) %>%
  lapply(function(x) x$abundance[x$abundance > 0])

iNEXT_plotByYear <- iNEXT(
  PlotYear_list,
  q = 0,
  datatype = "abundance"
)

# png(filename = paste0("./Figures/Rarefaction/Rarefaction_yearsStacked.png"),
#   width = 22,
#   height = 10,
#   units = "in",
#   res = 300)
# ggiNEXT(subset(iNEXT_plotByYear, substr(AsyEst$Assemblage, 1, 4)=="ABBY"), type = 1)  + 
#   aes(colour = substr(Assemblage, 10, 13), x=(x/100))+
#   theme_pubr() +
#   xlab("Number of Individuals (x100)") +
#   scale_shape_manual(values=c(rep(4,length(PlotYear_list)))) +
#   scale_fill_manual(values = c(rep("grey",length(PlotYear_list))))+
#   scale_x_continuous(labels = scales::label_number(accuracy = 1),
#                      breaks = breaks_extended(n = 4)) +
#   theme(legend.position = "none") +
#   facet_wrap(.~substr(Assemblage, 1, 4), nrow=4, scale="free")
# dev.off()

all_completeness <- iNEXT_plotByYear$AsyEst %>%
  filter(Diversity == "Species richness") %>%
  mutate(completeness = Observed / Estimator)
all_completeness$SiteID<-substr(all_completeness$Assemblage, 1, 4)
all_completeness$PlotID<-substr(all_completeness$Assemblage, 1, 8)
all_completeness$Year<-substr(all_completeness$Assemblage, 10, 13)
head(all_completeness)

#Saturation
all_completeness$saturated <- ifelse(
  all_completeness$Observed >= all_completeness$LCL,
  "Saturated",
  "Unsaturated")

#Plots
ggplot(subset(all_completeness, SiteID=="ABBY"),
  aes(x = Year, y = Estimator, color = completeness)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = LCL, ymax = UCL), width = 0.2) +
  facet_wrap(. ~ PlotID, scales = "free") +
  theme_pubr()

plot_dat <- all_completeness %>%
  pivot_longer(cols = c(Observed, Estimator),
    names_to = "Metric",
    values_to = "Richness")

# png(filename = paste0("./Figures/Rarefaction/Rarefaction_yearsEstimates.png"),
#     width = 22,
#     height = 10,
#     units = "in",
#     res = 300)

siteList<-sort(unique(all_completeness$SiteID))

#Estimate median values across years
plot_year_summary <- all_completeness %>%
  group_by(SiteID, PlotID) %>%
  summarise(
    median_completness = median(completeness),
    median_richness = median(Estimator),
    median_lcl      = median(LCL),
    median_ucl      = median(UCL),
    n_years         = n(),
    .groups = "drop"
  )


for (i in 1:length(siteList)) {
  png(filename = paste0("./Figures/Rarefaction/PlotLevel/",siteList[i],"_yearsEstimates.png"),
     width = 10,
     height = 10,
     units = "in",
     res = 300)
  p<-ggplot(subset(all_completeness, SiteID==siteList[i]), aes(x = Year)) +
    geom_point(aes(y = Observed), shape = 4, colour = "black", size = 2) +
    geom_line(aes(y = Observed, group = 1), colour = "black", linetype = "dotted") +
    geom_point(aes(y = Estimator, colour = completeness), size = 3) +
    geom_line(aes(y = Estimator, group = 1), colour = "black", alpha = 0.5) +
    geom_errorbar(aes(ymin = LCL, ymax = UCL, colour = completeness), width = 0.2) +
    facet_wrap(. ~ PlotID, nrow = 2) +
    geom_hline(data = subset(plot_year_summary, SiteID==siteList[i]), 
               aes(yintercept = median_richness)) +
    geom_hline(data = subset(plot_year_summary, SiteID==siteList[i]), 
               aes(yintercept = median_lcl), lty="dashed") +
    geom_hline(data = subset(plot_year_summary, SiteID==siteList[i]), 
               aes(yintercept = median_ucl), lty="dashed") +
    theme_pubr() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
    ylab("Species Richness") +
    scale_color_viridis()
  print(p)
  dev.off()
}

#### Year over year
df_counts <- df %>%
  filter(!is.na(Species),
         variable_name == "abundance",
         observation_datetime >= paste0("2018-01-01"),
         observation_datetime <= paste0("2025-12-31")) %>%
  mutate(trappingDays = as.numeric(trappingDays),
         count_est = round(value * trappingDays))

site_list <- df_counts %>%
  group_by(plotID, Species) %>%
  summarise(abundance = sum(count_est), .groups = "drop") %>%
  group_split(plotID) %>%
  setNames(sort(unique(df_counts$plotID))) %>%
  lapply(function(x) x$abundance[x$abundance > 0])

iNEXT_sites <- iNEXT(site_list,
                     q = 0,
                     datatype = "abundance")

png(filename = paste0("./Figures/Rarefaction/Rarefaction_plotsYearOverYear.png"),
    width = 10,
    height = 10,
    units = "in",
    res = 300)
ggiNEXT(iNEXT_sites, type = 1)  + 
  aes(x=(x/100))+
  theme_pubr() +
  xlab("Number of Individuals (x100)") +
  scale_shape_manual(values=c(rep(4,length(site_list)))) +
  scale_x_continuous(labels = scales::label_number(accuracy = 1),
                     breaks = breaks_extended(n = 4),
                     expand = c(0,0)) +
  xlim(0,75) +
  # theme(legend.position = "none") +
  guides(shape = "none", size = "none", fill = "none", col = "none") 
dev.off()

#Completeness
unconstrained_completeness <- iNEXT_sites$AsyEst %>%
  filter(Diversity == "Species richness") %>%
  mutate(completeness = Observed / Estimator)

summary(all_completeness$completeness)

summary(unconstrained_completeness$completeness)

summary(plot_year_summary$median_completness)

ggarrange(
  ggplot(unconstrained_completeness, aes(x=completeness)) +
    geom_histogram() +
    xlim(0,1.1) + ylim(0,100),
  ggplot(plot_year_summary, aes(x=median_completness)) +
    geom_histogram() +
    xlim(0,1.1) + ylim(0,100),
  ncol = 1)

#### Export final outputs ####
write.csv(plot_year_summary, "./plot_annualVarWeightedMean_EstimatedSppRichness.csv")
write.csv(all_completeness, "./plot_annual_EstimatedSppRichness.csv")
