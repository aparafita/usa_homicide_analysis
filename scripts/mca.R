library(tidyverse)
library(FactoMineR)

data <- read_csv('data/database_preprocessed.csv.gz')
data <- data %>% select(-X1) # index

# Change to factors the categorical variables
data <- data %>% 
  mutate(
    AgencyType = as.factor(data$AgencyType),
    State = as.factor(data$State),
    Month = as.factor(data$Month),
    CrimeType = as.factor(data$CrimeType),
    VictimSex = as.factor(data$VictimSex),
    VictimRace = as.factor(data$VictimRace),
    PerpetratorSex = as.factor(data$PerpetratorSex),
    PerpetratorRace = as.factor(data$PerpetratorRace),
    Relationship = as.factor(data$Relationship),
    Weapon = as.factor(data$Weapon),
    City = as.factor(data$City),
    State = as.factor(data$State),
    Weapon_Fire = as.factor(data$Weapon_Fire),
    Weapon_Violence = as.factor(data$Weapon_Violence),
    Relationship_Family = as.factor(data$Relationship_Family),
    Relationship_Romantic = as.factor(data$Relationship_Romantic),
    Sex_Same = as.factor(data$Sex_Same),
    Month_Winter = as.factor(data$Month_Winter),
    Month_Spring = as.factor(data$Month_Spring),
    Month_Summer = as.factor(data$Month_Summer),
    Month_Autumn = as.factor(data$Month_Autumn)
  )

# Cesc: Parece que la ultima row contiene muchos NA, supongo que al generar las new features
# no se han generado para ese individuo. Para ahorrarme volver a ejecutar todo el preprocessing, 
# simplemente la quito. También quito la primera columna, que sólo es un ID
data <- data %>% filter(!is.na(Month_Winter)) 

data
summary(data)

# Divide in train/test first
nrow(data) # 448172

set.seed(123)
train <- sample.int(nrow(data), 300000)
test <- (1:nrow(data))[-train]

train <- data[train, ]
test <- data[test, ]

# MCA defined with train, test transformed through that MCA.
# Clustering defined with train, test assigned to those clusters 
# through Euclidean distance (Deterministic assignment).

# Let's start by doing a Multiple Correspondence Analysis. We will use all the
# categorical variables available
data %>% names()

var_MCA <- c(1,2,5,6,8,9,11,12,13,16,17,18,19,20,21,22,23,24,25)

(mca <- MCA(
  train[,var_MCA], 
  ncp = 120, 
  quanti.sup = 10, # AgeDiff 
  quali.sup = 3, # CrimeType
  graph=FALSE)
)

plot(mca$eig$`cumulative percentage of variance`)
plot(mca$eig$eigenvalue)

# We have omited some of the categorical variables that had many levels (like City).
# The dimensions created by the MCA have a very low associated eigenvalues (as expected), so we 
# need to take a large number of dimensions to have enough variance explained.
# Taking a look at the screeplot, we see that the dimensions between 20 to 90 have a very similar
# eigenvalue, so they don't add much information. So one option could be taking the first 20 dimensions.
# On the other hand, we can also compute how many dimensions have an associated eigenvalue
# greater than the mean. In this case there are 56 dimensions with a greater eigenvalue than the mean.

sum(mca$eig$eigenvalue - mean(mca$eig$eigenvalue) > 0)

ndim1 <- 20
ndim2 <- 56

mca_ind_20 <- mca$ind$coord[,1:ndim1]
mca_ind_56 <- mca$ind$coord[,1:ndim2]

plot(x = mca_ind_20[,1], y = mca_ind_20[,2])

# Perform a hierarchical clustering with a subset of 10000 rows (the function does not
# allow more than that). In order to perform a hierarchical clustering we need first to compute
# the matrix of Euclidean distances (dist() function).

sample <- sample.int(nrow(mca_ind_20), 10000)
hc <- hclust(dist(mca_ind_20[sample,]), method = "ward.D2")

par(mfrow=c(1, 1))
plot(hc, cex=.1, xlab='', ylab='', yaxt='n', sub='', main='Dendogram')
barplot(hc$height, main='Accumulated height')

# If we take a look at the dendrogram,  9 clusters seems a reasonable choice.
nc <- 9

old_clust <- cutree(hc, nc)

data_clust <- as.data.frame(mca_ind_20[sample, 1:2])
data_clust$clust <- as.factor(old_clust)

# The clusters are the following
ggplot(data = data_clust,mapping = aes(x = data_clust$`Dim 1`, y = data_clust$`Dim 2`)) +
  geom_point(mapping = aes(col = clust))

# Now, we are going to do a consolidation operation using the k-Means algorithm. We will
# compute the centroids of the HC and we will pass them as the initial centroids of the k-Means
X <- as.data.frame(mca_ind_20[sample,])
cluster_centroids <- list()
for (i in 1:nc){
  cluster_centroids[[i]] <- X[old_clust == i, ] %>% colMeans() %>% as.vector()
}
(cluster_centroids <- t(matrix(unlist(cluster_centroids), ncol=nc)))

# K-Means with the initial centroids of HC
set.seed(123)
model <- kmeans(X, cluster_centroids)
new_clusters <- model$cluster

# Once we have obtained the final clusters, we are going to use the catdes() function to 
# see which are the 'properties' of each cluster.
results <- train[sample,] %>% mutate(cluster = new_clusters)

ct <- catdes(
  donnee = as.data.frame(results),
  num.var = 26
)
names(as.data.frame(results))
as.data.frame(results)
