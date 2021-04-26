# Getting the data 
#https://rstudio-pubs-static.s3.amazonaws.com/294570_7aea540aa6784e8488ccb247265eeaf7.html#hierarchical_clustering
library(readr)
cereals_data <- read.csv("Cereals.csv", header=T)
str(cereals_data)

summary(cereals_data)

head(cereals_data)
tail(cereals_data)


# Data PreProcessing

sum(is.na(cereals_data))

# comment: There are 4 NA values in dataset we shall remove those. 

cereals_data <- na.omit(cereals_data)
sum(is.na(cereals_data))
head(cereals_data)

# Setting the rownames of the breakfast cereals to the row names, as this will later help us in visualising the clusters.
rownames(cereals_data) <- cereals_data$name
head(cereals_data)

#cereal_data <- cereal_data[,-c(colnames(cereal_data) %in% ("name"))]

#cereals_data$name = NULL
#head(cereals_data)

# Data scaling
## Converting categorical variables into dummy variables 
library(fastDummies)
cereals_data_norm <- fastDummies::dummy_cols(cereals_data, select_columns = "mfr")
head(cereals_data_norm)
cereals_data_norm <- fastDummies::dummy_cols(cereals_data_norm[,(2:22)], select_columns = "type")[,(2:23)]
head(cereals_data_norm)

str(cereals_data_norm)

cereals_data_norm <- scale(cereals_data_norm, center = T, scale = T)
#preproc1 < preProcess(cereals_data[,c])
head(cereals_data_norm)


# DATA EXPLORATION

library(factoextra)

distance <- get_dist(cereals_data)

fviz_dist(distance, gradient = list(low= "#00AFBB", mid = "white", high = "#DC4E07"))

# Hierarchical Clustering
# We use the euclidean distance measure.

## Apply hierarchical clustering to the data using Euclidean distance to the normalized measurements.
dist <- dist(cereals_data, method="euclidean")

# hierarchical clustering to the data using ward linkage method.
hc_fit_wd <- hclust(dist, method = "ward.D2")
plot(hc_fit_wd)

# hierarchical clustering to the data using complete linkage method.
hc_fit_comp <- hclust(dist, method = "complete")
plot(hc_fit_comp)

# hierarchical clustering to the data using single linkage method.
hc_fit_single <- hclust(dist, method = "single")
plot(hc_fit_single)

# hierarchical clustering to the data using complete linkage method.
hc_fit_avg <- hclust(dist, method = "average")
plot(hc_fit_avg)


# Determining Optimal Clusters
fviz_nbclust(cereals_data, FUN = hcut, method = "wss")
fviz_nbclust(cereals_data, FUN = hcut, method = "silhouette")


# Cutting the tree to 3 cluster; chossing the ward linkage method.
points_hc <- cutree(hc_fit_wd, k=3)
cereals_clusts_hc <- cbind(points_hc, cereals_data)

colnames(cereals_clusts_hc)[1] <- "cluster_hc"
head(cereals_clusts_hc)

plot(hc_fit_wd)
ggdendrogram(hc_fit_wd)
rect.hclust(hc_fit_wd, k = 3, border = "blue")

plot(hc_fit_comp)
rect.hclust(hc_fit_comp, k = 3, border = "red")

plot(hc_fit_single)
rect.hclust(hc_fit_single, k = 3, border = "green")

plot(hc_fit_avg)
rect.hclust(hc_fit_avg, k = 3, border = "purple")

# first cluster from the wade link partition can be used as set of cereals in their daily breakfast


ggdendrogram(hc_fit_wd)

dend <- hc_fit_wd
dend_data <- dendro_data(dend, type = "rectangle")
names(dend_data)


library(ggplot2)
p <- ggplot(dend_data$segments) + 
  geom_segment(aes(x = x, y = y, xend = xend, yend = yend))+
  geom_text(data = dend_data$labels, aes(x, y, label = label),
            hjust = 1, angle = 90, size = 3)+
  ylim(-3, 15)
print(p)