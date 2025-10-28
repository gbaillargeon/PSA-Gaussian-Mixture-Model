library(ggplot2)
library(reshape2)
library(cowplot)
library(dplyr)
library(tidyr)
library(viridis)

#create faactor score only file 
data_scores <- read.csv("Combined_AllSP_old306.csv")
data_scores

data_scores <- select(data_scores, max_size_score, trophic_level_score,breeding_strategy_score, fecundity_score, 
                      PLD_score, ecological_niche_distribution_score, cyanide_use_score, encounter_depth_score,
                      aquarium_suitability_score, volume_in_trade_score_productivity_scaled, LCSH_score, 
                      vulnerability)
head(data_scores)


#Renaming columns
data_scores <- data_scores %>%
  rename(
    "Maximum Size" = "max_size_score",
    "Trophic Level" = "trophic_level_score",
    "Breeding Strategy" = "breeding_strategy_score",
    "Fecundity" = "fecundity_score",
    "PLD" = "PLD_score",
    "Eco. Niche & Distribution" = "ecological_niche_distribution_score",
    "Cyanide Use" = "cyanide_use_score",
    "Encounterability Depth" = "encounter_depth_score",
    "Aquarium Suitability" = "aquarium_suitability_score",
    "Volume in Trade" = "volume_in_trade_score_productivity_scaled",
    "LCSH" = "LCSH_score", 
  )

#make the columns numeric
data_scores <- as.numeric(data_scores)

#highest V 
TopV <- data_scores %>% arrange(desc(data_scores$Vulnerability)) %>% slice(1:10)
TopV 

#lowest V 
BottomV <- data_scores %>% arrange(data_scores$Vulnerability) %>% slice(1:10)
BottomV 

#highest V mean and sd
#remove name column

TopV <- TopV %>%
  select(-`vulnerability`)

means_top <- sapply(TopV, mean, na.rm = TRUE)
sds_top <- sapply(TopV, sd, na.rm = TRUE)

#highest V mean and sd
#remove name column

BottomV <- BottomV %>%
  select(-`vulnerability`)

means_bot <- sapply(BottomV, mean, na.rm = TRUE)
sds_bot <- sapply(BottomV, sd, na.rm = TRUE)

#overall
OverallV <- data_scores


OverallV <- OverallV %>%
  select(-`vulnerability`)

means_all <- sapply(OverallV, mean, na.rm = TRUE)
sds_all <- sapply(OverallV, sd, na.rm = TRUE)

#Combine 
calculate_stats <- function(df, dataset_id) {
  data.frame(
    Variable = names(df),
    Dataset_ID = dataset_id,
    Mean = sapply(df, mean, na.rm = TRUE),
    SD = sapply(df, sd, na.rm = TRUE)
  )
}

# Calculate means and SDs for each dataset
top_stats <- calculate_stats(TopV, "TopV")
bottom_stats <- calculate_stats(BottomV, "BottomV")
overall_stats <- calculate_stats(OverallV, "OverallV")

# Combine all results into a single data frame
combined_stats <- bind_rows(top_stats, bottom_stats, overall_stats)

combined_stats$Variable <- as.factor(combined_stats$Variable)

combined_stats <- combined_stats %>%
  mutate( PS = case_when(
(Variable == "Maximum Size") ~ "Productivity",
 (Variable == "Trophic Level" ) ~ "Productivity",
   (Variable == "Breeding Strategy" ) ~ "Productivity",
     (Variable == "Fecundity" ) ~ "Productivity",
       (Variable == "PLD" ) ~ "Productivity",
         (Variable == "Eco. Niche & Distribution" ) ~ "Susceptibility",
           (Variable == "Cyanide Use" ) ~ "Susceptibility",
            (Variable == "Encounterability Depth" ) ~ "Susceptibility",
             (Variable == "Aquarium Suitability" ) ~ "Susceptibility",
              (Variable == "Volume in Trade" ) ~ "Susceptibility",
               (Variable == "LCSH"  ) ~ "Susceptibility"
))
               
#graph
combined_stats$Dataset_ID <- factor(combined_stats$Dataset_ID,levels = c("TopV", "BottomV", "OverallV"))
combined_stats$PS <- as.factor(combined_stats$PS)
viridis_colors <- viridis(3)

ggplot(combined_stats, aes(x = Variable, y = Mean, fill = Dataset_ID)) +
  geom_bar(stat = "identity", position = position_dodge(0.7), width = 0.6) +
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD), width = 0.2, position = position_dodge(0.7)) +
  labs(x = "Factors", y = "Average Factor Score", fill = "Species") +
  #scale_fill_viridis_d() +
  scale_fill_manual(values = c(
    "TopV" = viridis_colors[1],      # Use the first viridis color for "TopV"
    "BottomV" = viridis_colors[2],   # Use the second viridis color for "BottomV"
    "OverallV" = viridis_colors[3]   # Use the third viridis color for "OverallV"
  ), 
  labels = c(
    "TopV" = "Top 10 Most Vulnerable",    
    "BottomV" = "Top 10 Least Vulnerable", 
    "OverallV" = "All Species" 
  )) +
  theme_minimal() +
  facet_wrap(~ PS, scales = "free_y") +
  theme(legend.position = "bottom") +
  coord_flip()
