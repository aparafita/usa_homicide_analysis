library(tidyverse)

data <- read_csv('data/database_preprocessed.csv.gz')
data <- data %>% select(-X1) # index

data

# Divide in train/test first
nrow(data) # 448172

set.seed(123)
train <- sample.int(nrow(data), 300000)
test <- (1:nrow(data))[-train]

data <- data[train, ]
data <- data[test, ]

# MCA defined with train, test transformed through that MCA.
# Clustering defined with train, test assigned to those clusters 
# through Euclidean distance (Deterministic assignment).