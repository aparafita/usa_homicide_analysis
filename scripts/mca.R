library(tidyverse)
library(FactoMineR)

# Preprocessing -----------------------------------------------------------

data <- read_csv('data/database_preprocessed.csv.gz')
data <- data %>% select(-X1) # index

# Drop some columns from the analysis. 
# Some because they would increase the inertia unnecessarily,
# some because they are direcly associated with certain modalities 
# (Relationship_Family), which should not be considered in the MCA.
# Those are features for the final prediction model, not for the MCA.
data <- data %>% 
  select(
    -State, -Year, -Month, -City, -Date,
    -Weapon_Fire, -Weapon_Violence, 
    -Relationship_Family, -Relationship_Romantic
  )

data %>% colnames

# Change the categorical variables to factors
data <- data %>% 
  mutate(
    VictimAge = as.numeric(VictimAge),
    PerpetratorAge = as.numeric(PerpetratorAge),
    AgeDiff = as.numeric(AgeDiff),
    
    AgencyType = as.factor(AgencyType),
    CrimeType = as.factor(CrimeType),
    VictimSex = as.factor(VictimSex),
    VictimRace = as.factor(VictimRace),
    PerpetratorSex = as.factor(PerpetratorSex),
    PerpetratorRace = as.factor(PerpetratorRace),
    Relationship = as.factor(Relationship),
    Weapon = as.factor(Weapon),
    Sex_Same = as.factor(Sex_Same),
    Month_Winter = as.factor(Month_Winter),
    Month_Spring = as.factor(Month_Spring),
    Month_Summer = as.factor(Month_Summer),
    Month_Autumn = as.factor(Month_Autumn)
  )

data
summary(data)


# Train/Test split --------------------------------------------------------

# Divide in train/test first
nrow(data) # 448172

set.seed(123)

train_idx <- sample.int(nrow(data), 300000)
test_idx <- (1:nrow(data))[-train_idx]

train <- data[train_idx, ]
test <- data[test_idx, ]


# MCA ---------------------------------------------------------------------

# MCA defined with train, test transformed through that MCA.
# Clustering defined with train, test assigned to those clusters 
# through Euclidean distance (Deterministic assignment).

# Let's start by doing a Multiple Correspondence Analysis. We will use all the
# categorical variables available
data %>% names()

# Exclude numerical variables and those categories that we don't want to use for the MCA
# CrimeType needs to be excluded too because it's the target variable

# How many PCs are there?
train %>% 
  select(-AgeDiff, -PerpetratorAge, -VictimAge, -CrimeType) %>% 
  map(~length(unique(.))) %>% unlist() %>% sum # 75 different modalities

data %>% colnames
quanti.sup <- c(4, 7, 11)
quali.sup <- c(2)
ind.sup <- test_idx

mca <- MCA(
  data %>% as.data.frame, 
  ncp = 75, 
  quanti.sup = quanti.sup,
  quali.sup = quali.sup,
  ind.sup = ind.sup,
  level.ventil=.2,
  graph=FALSE
)

# Let's analyze the results of MCA

png('plots/mca_screeplot.png')
plot(mca$eig$eigenvalue, type='o', cex=.5, pch=16)
abline(h=mca$eig$eigenvalue %>% mean, lty='dashed', col='red')
dev.off()

png('plots/mca_cumulative_screeplot.png')
plot(mca$eig$`cumulative percentage of variance`, type='o', cex=.5, pch=16)
dev.off()

sum((mca$eig$eigenvalue - mean(mca$eig$eigenvalue)) > 0) # 15

mca$eig$`cumulative percentage of variance`[2] # 12.42322
mca$eig$`cumulative percentage of variance`[5] # 25.33834
mca$eig$`cumulative percentage of variance`[15] # 58.76992

# We have a dataset with some variables that have a high number of modalities,
# some of those rare. That increases the overall variance of the dataset
# without adding useful information. Accounting for a 25% of the variability
# might then be desirable and thus, in this case, it would be preferable
# to use the Last Elbow rule over the significant eigenvalues (the ones above the mean)
# than using the Kaiser rule directly just to get to the 58% of explained variance.

ncomp <- 5

X <- mca$ind$coord[, 1:ncomp]

X[, 1:2] %>% as_tibble() %>% bind_cols(train['CrimeType']) %>% 
  'colnames<-'(c('x', 'y', 'CrimeType')) %>% 
  ggplot(aes(x, y, color=CrimeType)) +
  geom_point(alpha=.5)

ggsave('plots/MCA_scatter.png')

# Latent factors ----------------------------------------------------------
tibble(
  x=mca$ind$coord[, 1],
  y=mca$ind$coord[, 2]
) %>% 
  sample_n(10000) %>% 
  ggplot(aes(x, y)) + 
  geom_point(color='black', alpha=.5) + 
  geom_point(
    aes(x, y), color='red', 
    data=tibble(
      x=mca$quali.sup$coord[, 1],
      y=mca$quali.sup$coord[, 2]
    )
  ) + 
  geom_text(
    aes(x, y, label=CrimeType), color='red', 
    data=tibble(
      x=mca$quali.sup$coord[, 1],
      y=mca$quali.sup$coord[, 2],
      CrimeType=rownames(mca$quali.sup$coord)
    )
  )

ggsave('plots/MCA_CrimeType_centroids.png')

# As we've seen in the previous plot, CrimeType can appear everywhere in the factorial plane.
# If we plot the centroids of its individuals, we don't notice a significant difference either.

# If we look at the variables plot from the MCA model:
png('plots/MCA_vars_plot.png')
plot(mca, choix='var')
dev.off()

# We can't see any other characteristics. 
# There aren't any relevant latent factors 
# (at least retrievable from this dataset) worth analyzing.


# Clustering --------------------------------------------------------------

# Since we have a huge dataset, if we want to use Hierarchical Clustering,
# we need to perform some previous steps.

# Run K-Means 3 times
nclust <- 14

set.seed(12345)
km1 <- kmeans(X, nclust)
km2 <- kmeans(X, nclust)

# Cross-table
table(km1$cluster, km2$cluster)

inds <- list()
members <- c()
for (c1 in 1:nclust){
  for (c2 in 1:nclust){
    inds_cell <- (km1$cluster == c1) & (km2$cluster == c2)
    
    if (any(inds_cell)) {
      centroid <- X[inds_cell, ]
      if (dim(centroid) %>% is.null){
        centroid <- matrix(centroid, ncol=ncomp, byrow=TRUE)
      }
      
      centroid <- colMeans(centroid)
      
      inds <- combine(inds, list(centroid))
      members <- c(members, sum(inds_cell))
    }
  }
}

centers <- combine(inds) %>% matrix(ncol=ncomp, byrow=TRUE)
centers %>% dim

hc <- hclust(dist(centers), method='ward.D2', members=members)

png('plots/dendogram.png')
plot(hc)
dev.off()

png('plots/cluster_heights_barplot.png')
barplot(hc$height, main='Dendogram Heights')
dev.off()

# By looking at the barplot, 
# the fifth bar still has enough significance in the difference in height.
# As a result, 6 (5+1) clusters would be considered.
# Looking at the dendogram, those clusters seem not to be outlying clusters.
# So, let's stick with 6 clusters.

nc <- 6
hc <- cutree(hc, nc)

centroids <- list()
for (i in 1:nc){
  x <- centers[hc == i, ]
  
  if (dim(x) %>% is.null){
    x <- matrix(x, ncol=ncomp, byrow=TRUE)
  }
  
  centroids <- combine(centroids, list(colMeans(x)))
}

centroids <- combine(centroids) %>% matrix(ncol=ncomp, byrow=TRUE)


# Final consolidation step
km <- kmeans(X, centroids)

table(km$cluster)
# 1     2     3     4     5     6 
# 58692 64538 58612 43595 49623 24940

cbind(X[, 1:2], km$cluster) %>% 
  as_tibble %>% 
  'colnames<-'(c('x', 'y', 'cluster')) %>% 
  mutate(cluster=factor(cluster)) %>% 
  ggplot(aes(x, y, color=cluster)) +
  geom_point(alpha=.5)

ggsave('plots/clusters.png')


# Cluster profiling -------------------------------------------------------

# Once we have obtained the final clusters, 
# we are going to use the catdes() function to see 
# which are the properties of each cluster.

train <- train %>% 
  mutate(cluster=factor(km$cluster))

train %>% colnames
cluster_profiling <- train %>% 
  as.data.frame %>% 
  catdes(num.var=17)


# Categorical variables:
cluster_profiling$category

# There isn't any significant description through the numerical variables 
# (because the corresponding object for the test on numerical variables
# in the cluster_profiling object isn't even filled, 
# which means there aren't any significant differences).


# Cluster assignment for test ---------------------------------------------
# We have computed the cluster for train, but not for test
# In order to assign the corresponding cluster, we compute the distance to their centers

Xtest <- mca$ind.sup$coord[, 1:ncomp]
centers <- km$centers %>% as.matrix()

test_assignment <- 1:nrow(Xtest) %>% 
  map(
    ~(matrix(rep(Xtest[., ], nc), nrow=nc, byrow=TRUE) - centers) ^ 2 %>% 
      rowSums %>% which.min
  ) %>% combine

table(test_assignment)

test <- test %>% mutate(cluster=factor(test_assignment))

# Add the index again (to recover all the remaining data later)
train$index <- train_idx
test$index <- test_idx

train <- bind_cols(train, as_tibble(X) %>% 'colnames<-'(paste('MCA', 1:ncomp, sep='')))
test <- bind_cols(test, as_tibble(Xtest) %>% 'colnames<-'(paste('MCA', 1:ncomp, sep='')))

write.csv(train, gzfile('data/train.csv.gz', 'wt'))
write.csv(test, gzfile('data/test.csv.gz', 'wt'))