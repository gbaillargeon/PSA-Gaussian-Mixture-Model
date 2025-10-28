library(dplyr)
library(ggplot2)
library(plotly)

############################################################## All families
rm(list = ls())
data <- read.csv("Combined_AllSP_old306.csv")

# Calculate average values for vulnerability within each family
averaged_data <- data %>%
  group_by(family) %>%
  summarise(average_v = mean(vulnerability, na.rm = TRUE), sd=sd(vulnerability, na.rm = TRUE), species_count = n_distinct(species) )
averaged_data
#least V
TopN<-averaged_data %>%
  arrange(desc(averaged_data$species_count))%>% slice(1:11)
TopN

#top families by volume
TopFam <- TopN[TopN$species_count > 5, ] #gives top 10 sp
TopFam

c<-as.data.frame(TopFam$family)
c

#select data for only top families 
dataTop<- data[data$family %in% c("Pomacentridae",  "Labridae",       "Gobiidae",       "Pomacanthidae",  "Acanthuridae",   "Blenniidae",    
                "Chaetodontidae", "Anthiadidae",    "Balistidae",     "Microdesmidae",  "Apogonidae"), ]
dataTop
#dataTop <- dataTop %>% arrange(desc(dataTop$species_count))

write.csv(TopN, "/Users/gbaillargeon/Downloads/TopNFamilyV.csv", row.names=FALSE)

level_order <- c("Pomacentridae",  "Labridae",       "Gobiidae",       "Pomacanthidae",  "Acanthuridae",   "Blenniidae",    
                 "Chaetodontidae", "Anthiadidae",    "Balistidae",     "Microdesmidae",  "Scorpaenidae")
dataTop$v

#206 sp in dataTop dataset 
library(fishualize)
#boxplot
try1 <- ggplot(dataTop, aes(x = reorder(family,vulnerability), y=vulnerability, colour = family)) + 
  geom_boxplot(outlier.shape = NA, size=0.8)+
   theme(axis.title.x = element_text(hjust = 0.6),axis.line = element_line(color = "black"),
         legend.position="none", panel.background = element_rect(fill = "white"),
         panel.grid.major.x = element_line(colour = "grey", size = rel(0.5)))+
   geom_jitter(alpha=0.5, size =0.8) +
  labs(title = "Average Vulnerability Values for Top Families") +
  labs(title = "Vulnerability of Species in Most Frequently Traded Families", x = "Family", y = "Vulnerability Score") +
  coord_flip() 


try1
ggplotly(try1)



#plot top SP
p <- ggplot(TopN, aes(y = family, x = average_v, 
                               text = paste("Family: ", family, "<br>Average vulnerability: ", round(average_v, 2)))) +
  geom_bar(stat = "identity", fill = "virids") +
  labs(title = "Average Vulnerability Values for Top Families",
       x = "Average Vulnerability",
       y = "Family") +
  theme_minimal() +
  geom_errorbar(aes(xmin=average_v-sd, xmax=average_v+sd), width=.2,
                position=position_dodge(.9)) +
  theme(axis.text.y = element_text(hjust = 0))
p
