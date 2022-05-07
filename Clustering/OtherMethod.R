library(caret)
library(tidyverse)
library(leaps)
library(ggplot2)
library(reshape2)
library(MASS)
library(ggcorrplot)
library(corrplot)
library(plotmo)
library(keras)
library(kableExtra)
library(modelr)
library(psych)
library(Rmisc)
library(gridExtra)
library(scales)
library(rpart)
library(yardstick)
library(cluster)
library(NbClust)
library(factoextra)

# Load the data.
whiteDat <- read_excel("Whitewine_v2.xlsx")

coutliers = as.numeric(rownames(whiteDat[cooksd > 4 * mean(cooksd, na.rm=T), ]))
outliers = c(outliers , coutliers[ !coutliers %in% outliers ] )

cleanWhiteDat = whiteDat[-outliers, ]
oldpar = par(mfrow=c(2,6))
for ( i in 1:12 ) {
  truehist(cleanWhiteDat[[i]], xlab = names(cleanWhiteDat)[i], col = 'lightgreen', 
           main = paste("Average =", signif(mean(cleanWhiteDat[[i]]),3)))
}
par(oldpar)

pairs(cleanWhiteDat[,c(7, 8, 10, 11)], col = cleanWhiteDat$quality, pch = cleanWhiteDat$quality)

ggcorrplot(cor(cleanWhiteDat), hc.order = TRUE, type = "lower", lab = TRUE, insig = "blank")

numericVars <- which(sapply(cleanWhiteDat, is.numeric))

all_numVar <- cleanWhiteDat[, numericVars]
cor_numVar <- cor(all_numVar, use = "pairwise.complete.obs")

#Sort on decreasing correlations with alcohol
cor_sorted <- as.matrix(sort(cor_numVar[,"alcohol"], decreasing = TRUE))

#Selecting high correlations 
Cor_High <- names(which(apply(cor_sorted, 1, function(x) abs(x) > 0.175)))
cor_numVar <- cor_numVar[Cor_High, Cor_High]

corrplot.mixed(cor_numVar, tl.col = "black", tl.pos = "lt")

#Making changes for converting data types

#Converting 2 interger columns too numeric
cleanWhiteDat$quality <- as.numeric(cleanWhiteDat$quality)

#for ease creating a data set without color column
wineData_no_color <- cleanWhiteDat[1:12]

norm_data <- sapply(cleanWhiteDat[,c(1,4,6,7,9,11,12)], function(x) (x - min(x))/(max(x) - min(x)))
norm_data <- data.frame(norm_data)    # norm_data is a 'matrix'

wineData_norm <- cbind(cleanWhiteDat[,c(2,3,5,8,10)],norm_data)
head(wineData_norm)

wineData_scaled <- scale(wineData_no_color)

head(wineData_scaled)

#Converting to data.frame
wineData_scaled_df <- as.data.frame(wineData_scaled)
class(wineData_scaled_df)

#Lets find correlation using cor()
corr_scaled <- round(cor(wineData_scaled_df),1)

##Lets plot same for Scaled data
ggcorrplot(corr_scaled, hc.order = TRUE, 
           type = "lower", 
           lab = TRUE, 
           lab_size = 3, 
           method="circle", 
           colors = c("tomato2", "white", "springgreen3"), 
           title="Correlogram of Wine Data", 
           ggtheme=theme_dark)

# Initialize total within sum of squares error: wss
wss <- 0

# For 1 to 20 cluster centers
for (i in 1:10) {
  km.out <- kmeans(wineData_norm, centers = i)
  # Save total within sum of squares to wss variable
  wss[i] <- km.out$tot.withinss
}
wss

# Plot total within sum of squares vs. number of clusters
plot(1:10, wss, type = "b", 
     xlab = "Number of Clusters", 
     ylab = "Within groups sum of squares")

wine_train_data <- sample_frac(wineData_norm, 0.65)

head(wine_train_data)

#Let us plot Silhouette Plot using NbCluster
fviz_nbclust(wine_train_data, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")

km <- kmeans(wineData_norm, 4, iter.max = 140 , algorithm="Lloyd", nstart=100)
km

fviz_cluster(list(data = wineData_norm, cluster = km$cluster),
             ellipse.type = "norm", geom = "point", stand = FALSE,
             palette = "jco", ggtheme = theme_classic())