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

# $`1`
# Cla/Mod   Mod/Cla    Global    p.value    v.test
# Weapon=Drugs                           22.64770 0.3526886 0.3046667 0.02053243  2.316473
# Relationship=Ex-Wife                   22.11246 0.4958086 0.4386667 0.02116154  2.305090
# PerpetratorRace=Asian/Pacific Islander 20.85427 1.4141621 1.3266667 0.04009540  2.052765
# Relationship=Stepfather                16.59243 0.2538676 0.2993333 0.02240805 -2.283379
# Relationship=Wife                      18.82861 4.9734206 5.1676667 0.01735918 -2.379010
# 
# $`2`
# Cla/Mod  Mod/Cla Global    p.value    v.test
# PerpetratorSex=Female 21.95122 11.04466 10.824 0.04202869  2.033236
# PerpetratorSex=Male   21.45944 88.95534 89.176 0.04202869 -2.033236
# 
# $`3`
# Cla/Mod    Mod/Cla     Global     p.value    v.test
# Relationship=Son                       20.81762  2.3544667  2.2096667 0.008280064  2.640431
# Weapon=Drowning                        23.44214  0.2695694  0.2246667 0.012162064  2.507407
# Relationship=Husband                   20.59075  2.0575991  1.9523333 0.041048818  2.043037
# CrimeType=Manslaughter by Negligence   20.56689  2.0303010  1.9286667 0.047169509  1.984775
# CrimeType=Murder or Manslaughter       19.51709 97.9696990 98.0713333 0.047169509 -1.984775
# PerpetratorRace=Asian/Pacific Islander 18.11558  1.2301235  1.3266667 0.021777917 -2.294219
# 
# $`4`
# Cla/Mod   Mod/Cla    Global     p.value    v.test
# Relationship=Stepdaughter 18.73805 0.2247964 0.1743333 0.008185626  2.644316
# Relationship=Father       13.07301 0.8831288 0.9816667 0.022449097 -2.282682
# 
# $`5`
# Cla/Mod   Mod/Cla    Global    p.value    v.test
# Relationship=In-Law 14.82874 0.7153941 0.7980000 0.02205838 -2.289361
# Weapon=Drugs        13.23851 0.2438385 0.3046667 0.00583820 -2.756733
# 
# $`6`
# Cla/Mod    Mod/Cla     Global      p.value    v.test
# VictimRace=Unknown                10.274542 0.99037690 0.80133333 0.0006944266  3.391770
# VictimRace=Asian/Pacific Islander  9.298813 1.72814755 1.54500000 0.0157724377  2.414139
# AgencyType=Regional Police        13.600000 0.06816359 0.04166667 0.0457269847  1.997905
# Weapon=Drugs                      10.175055 0.37289495 0.30466667 0.0464471683  1.991307
# Relationship=Stepmother            4.081633 0.02405774 0.04900000 0.0490703762 -1.967980
# Relationship=Ex-Wife               6.534954 0.34482759 0.43866667 0.0160629975 -2.407481

# Possible titles:
# Cluster 2: woman perpetrator
# Cluster 3: son murder by negligence
# Cluster 4: stepdaughter murder
# Cluster 6: unkwown race or asian victims

# There isn't any significant description through the numerical variables

# Finally, if we study which of the variables affect the classification for the clusters:
cluster_profiling$test.chi2
# only PerpetratorSex has a significant p-value (0.0496), 
# which already appeared in cluster 2, 
# depicting those cases where the perpetrator was a Woman.

# In conclusion, there are some meaningful differences between clusters, but not that many.

# Let's keep the cluster variable nevertheless, 
# in case it has more importance for the prediction of CrimeType.


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