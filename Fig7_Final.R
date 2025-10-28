library(dplyr)
library(ggplot2)
install.packages("ggpubr")
library(ggpubr)
#read in data with species, FB vulnerability score, FB rating, and PSA vulnerability score
FBdata <- read.csv("FB_PSA_AllSp.csv")
FBdata<-na.omit(FBdata)

#order from high to low V
FBdata <- arrange(FBdata, PSA.V)

#Compare top 15 and bottom 15

#highest V scores
FB_top <- FBdata %>% slice(292:306) 
FB_top

#lowest V scores
FB_bottom <- FBdata %>% slice(1:15) 

#min PSA V score
print(min(FBdata$PSA.V))
#0.19 Chrysiptera unimaculata

#max PSA V score 
print(max(FBdata$PSA.V))
#2.03

#Correlation analysis for FB vs. PSA model trends in Vulnerability by Species
lm <- lm(FishbaseV~PSA.V, data = FBdata)
summary(lm)

correlation <- cor(FBdata$FishbaseV, FBdata$PSA.V, method = 'pearson')
correlation

nrow(FBdata[FBdata$FishbaseV == '10', ])

#Histogram comparing spread of Vulnerability data between FB and PSA model
FBhist <- ggplot(data = FBdata, aes(x = FishbaseV, fill = "blue")) +
  geom_histogram(bins=10) +
  xlim(0,NA)+
  theme_bw()+
  labs(x = "Vulnerability Bin",
       y = "Count") +
  theme(legend.position = "none")
FBhist 

PSAhist <- ggplot(data = FBdata, aes(x = PSA.V)) +
  geom_histogram(bins=10) +
  theme_bw()+
  ylim (0,250)+
  xlim(0, NA)+
  labs(x = "Vulnerability Bin",
       y = "Count"
  )
PSAhist

Fig7 <- ggarrange(FBhist, PSAhist, 
                  labels = c("A", "B"),
                  ncol = 2, nrow = 1)

###REVISED DATA###
set.seed(123)
FBdata <- read.csv("FB_PSA_AllSp.csv")
head(FBdata)
dataFBgmm <- select(FBdata, FishbaseV) 

FB_gmm <- Mclust(dataFBgmm, G=3) 
#Mclust() will fit all available covariance models (EEE, VVI, VVV, etc.) with exactly 3 clusters, then compare them using BIC (Bayesian Information Criterion).

#The “best” fit is the one with the highest BIC value.
summary(FB_gmm) 

# Clustering table:
#   1   2   3 
# 250  45 11 

# 1 = low vuln 81.7%
#2 = med 14.7%
#3 = high 3.6%

#COMPARE TO PSA VULN
# Clustering table:
# 1   2   3 
# 117 133  56

#Mclust E (univariate, equal variance) model with 3 components: 

plot(FB_gmm, what="classification")


FB_gmm$classification

FBdata$cluster <- factor(FB_gmm$classification)
FBdata <- as.data.frame(FBdata)


ggplot(FBdata, aes(x = 1:nrow(FBdata), 
                      y = FB_Score, 
                      color = factor(cluster))) +
  geom_point(size = 3) +
  labs(x = "Index", y = "Vulnerability Score", color = "Cluster") +
  theme_minimal()

ggplot(FBdata1, aes(x = 1, y = FB_Score, color = factor(cluster))) +
  geom_jitter(width = 0.2, size = 3) +   # jitter so dots don’t overlap
  labs(x = "", y = "Vulnerability Score", color = "Cluster") +
  theme_minimal() +
  scale_colour_viridis_d(alpha=1) +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) + theme_minimal()

#try as ridgeline type plot
library(ggridges)
FBdata$cluster <- factor(FBdata$cluster, levels = c("3", "1", "2"))

p1 <- ggplot(FBdata, aes(
  x = FishbaseV,
  y = reorder(factor(cluster), FishbaseV, FUN = mean),   # order by mean vulnerability
  fill = factor(cluster)
)) +
  geom_density_ridges(alpha = 0.5, scale = 1.2, rel_min_height = 0.01) +
  geom_jitter(
    aes(color = factor(cluster)),
    height = 0.2, width = 0, size = 2, alpha = 0.7
  ) +
  labs(x = "FishBase Vulnerability Score", y = "Cluster", color = "Cluster", fill = "Cluster") +
  scale_colour_viridis_d(alpha = 1) +
  scale_fill_viridis_d(alpha = 0.6) +
  theme_minimal() +
  theme(
    legend.position = "none"
  )+ theme(
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text.x  = element_text(size = 14),
    axis.text.y  = element_text(size = 14),
    legend.title = element_text(size = 14),
    legend.text  = element_text(size = 12)
  )

p1
#try similar plot type with V scores?
dataV <- select(FinalData, cluster, vulnerability)
head(dataV)
dataV$cluster <- factor(dataV$cluster)

# Swap levels 2 and 3
levels(dataV$cluster)[levels(dataV$cluster) == "2"] <- "tmp"
levels(dataV$cluster)[levels(dataV$cluster) == "3"] <- "2"
levels(dataV$cluster)[levels(dataV$cluster) == "tmp"] <- "3"

dataV$cluster <- factor(dataV$cluster, levels = c("3", "1", "2"))

p2 <- ggplot(dataV, aes(
  x = vulnerability,
  y = reorder(factor(cluster), vulnerability, FUN = mean),   # order by mean vulnerability
  fill = factor(cluster)
))  +
  geom_density_ridges(alpha = 0.5, scale = 1.2, rel_min_height = 0.01) +
  geom_jitter(
    aes(color = factor(cluster)),
    height = 0.2, width = 0, size = 2, alpha = 0.7
  ) +
  labs(x = "PSA Vulnerability Score", y = "Cluster", color = "Cluster", fill = "Cluster") +
  scale_colour_viridis_d(alpha = 1) +
  scale_fill_viridis_d(alpha = 0.6) +
  theme_minimal() +
  theme(
    legend.position = "right"
  ) + theme(
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text.x  = element_text(size = 14),
    axis.text.y  = element_text(size = 14),
    legend.title = element_text(size = 14),
    legend.text  = element_text(size = 12)
  )
    
    
p2

library(gridExtra)
grid.arrange(p1, p2, ncol = 2) 

#need better alignment of graphs

library(cowplot)

plot_grid(p1, p2, align = "hv", axis = "tb")  # align both horizontally & vertically

#now also plot alongside previous histogram comparing scores

plot_grid(p1, p2, FBhist, PSAhist, align = "hv", axis = "tb")  # align both horizontally & vertically


