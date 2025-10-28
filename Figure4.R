#Stacked bar chart of level 
library(viridis)
library(hrbrthemes)
library(dplyr)
library(ggplot2)
library(tidyverse)

#read in data
Sp_data <- read.csv("Combined_AllSp_old306.csv")
Sp_data<-na.omit(Sp_data)

#Stacked bar chart 
#need to make dataset with factor (PLD, Fec, BS), Taxonomic Level, and % of species (value)

##Breeding Strategy dataframe of percent by tax group##
# Use the table() function to get the counts of each species
BSgroup_counts <- table(Sp_data$BS.level)
BStotal_count <- sum(BSgroup_counts)
# Divide each count by the total count and multiply by 100 to get the percentage
BSpercentage_by_group <- (BSgroup_counts / BStotal_count) * 100
# Combine group names and percentages into a data frame
BSresult <- data.frame(
#  Tax_level = names(percentage_by_group), 
  Percentage = BSpercentage_by_group
)
#rename second column 
BSresult <- BSresult %>% 
  rename(
    Taxonomic_Level=Percentage.Var1,
     Frequency=Percentage.Freq
  )
#label dataframe as BS
BSresult$Factor <- "BS"
# Print the final table
print(BSresult)

#make dataframe for fecundity 
# Use the table() function to get the counts of each species
Fgroup_counts <- table(Sp_data$Fecundity.level)
Ftotal_count <- sum(Fgroup_counts)
# Divide each count by the total count and multiply by 100 to get the percentage
Fpercentage_by_group <- (Fgroup_counts / Ftotal_count) * 100
# Combine group names and percentages into a data frame
Fresult <- data.frame(
  #  Tax_level = names(percentage_by_group), 
  Percentage = Fpercentage_by_group
)
Fresult
#label dataframe as BS
Fresult$Factor <- "F"
#rename second column 
Fresult <- Fresult%>% 
  rename(
    Taxonomic_Level=Percentage.Var1,
    Frequency=Percentage.Freq
  )

print(Fresult)

#make dataframe for PLD#
# Use the table() function to get the counts of each species
PLDgroup_counts <- table(Sp_data$PLD.level)
PLDtotal_count <- sum(PLDgroup_counts)
# Divide each count by the total count and multiply by 100 to get the percentage
PLDpercentage_by_group <- (PLDgroup_counts / PLDtotal_count) * 100
# Combine group names and percentages into a data frame
PLDresult <- data.frame(
  #  Tax_level = names(percentage_by_group), 
  Percentage = PLDpercentage_by_group
)
PLDresult
#label dataframe as BS
PLDresult$Factor <- "PLD"
#rename second column 
PLDresult <- PLDresult%>% 
  rename(
    Taxonomic_Level=Percentage.Var1,
    Frequency=Percentage.Freq
  )

print(PLDresult)

#combine datasets into 1 to plot 
All_Spdata <- bind_rows(PLDresult, Fresult, BSresult)
All_Spdata <- as.data.frame(All_Spdata)
str(All_Spdata)

#rename taxonomic levels and factors to full value 

All_Spdata$Taxonomic_Level <- recode_factor(All_Spdata$Taxonomic_Level, "FL" = "Family", "GL" = "Genus", "ND" = "No Data", "SL"="Species")
All_Spdata$Taxonomic_Level <- factor(All_Spdata$Taxonomic_Level, levels= c("Family", "Genus", "Species","No Data"))

newF <- c(PLD = "Pelagic Larval Duration", BS = "Breeding Strategy", F = "Fecundity")
All_Spdata$Factor <- as.factor(All_Spdata$Factor)
All_Spdata$Factor <- recode_factor(All_Spdata$Factor, "PLD" = "Pelagic Larval Duration", "BS" = "Breeding Strategy", "F" = "Fecundity")
    
# Stacked + percent
ggplot(All_Spdata, aes(fill=Taxonomic_Level, y=Frequency, x=Factor)) + 
  geom_bar(position="fill", stat="identity")

#updated stack plot with better colors and labels
graph<-ggplot(All_Spdata, aes(fill=Taxonomic_Level, y=Frequency, x=Factor)) + 
  geom_bar(position="stack", stat="identity") +
 # scale_fill_viridis(discrete = T) +
 # theme_ipsum() +
  labs(title = "Taxonomic Level of Data Available for Species",
       x = "Productivity Factor",
       y = "Percentage",
       fill = "Taxonomic Level")+
  theme(
    panel.background = element_rect(fill = "white"),   # inside the plotting area
    panel.grid.major = element_line(color = "grey90"), # optional: light grid
    axis.title.x = element_text(size = 14, hjust=0.5, margin = margin(t = 20, r = 0, b = 0, l = 0)),
    axis.text.x = element_text(size = 12, hjust=0.5, color = "black"),
    axis.title.y = element_text(size = 14, hjust = 0.5, margin = margin(t = 0, r = 15, b = 0, l = 0)),
    axis.text.y = element_text(size = 12, hjust=0.5, color = "black"),
    legend.text = element_text(size=14),
    legend.title = element_text(size=14),
    plot.title = element_text(size=16, hjust=0.5)
    ) 
graph + scale_fill_viridis_d(alpha=1)

