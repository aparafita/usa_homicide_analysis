library(randomForest)
library(ROCR)
library(tidyverse)

train <- read_csv('data/train.csv.gz')
test <- read_csv('data/test.csv.gz')

train <- select(train, -X1)
test <- select(test, -X1)

data <- bind_rows(
  train %>% mutate(train=TRUE),
  test %>% mutate(train=FALSE)
) %>% 
  mutate(
    VictimAge = as.numeric(VictimAge),
    PerpetratorAge = as.numeric(PerpetratorAge),
    AgeDiff = as.numeric(AgeDiff),
    
    CrimeType = as.factor(CrimeType),
    
    AgencyType = as.factor(AgencyType),
    VictimSex = as.factor(VictimSex),
    VictimRace = as.factor(VictimRace),
    PerpetratorSex = as.factor(PerpetratorSex),
    PerpetratorRace = as.factor(PerpetratorRace),
    Relationship = as.factor(Relationship),
    Weapon = as.factor(Weapon),
    
    cluster = as.factor(cluster)
  )

original_dataset <- read_csv('data/database_preprocessed.csv.gz')
original_dataset <- select(original_dataset, -X1)

original_dataset <- mutate(
  original_dataset,
  
  State = as.factor(State),
  Year = as.factor(Year),
  Month = as.factor(Month)
) %>% select(-City) # too many modalities; cannot use it

for (col in (setdiff(original_dataset %>% colnames, data %>% colnames))){
  if (col != 'Date'){
    data[col] <- original_dataset[[col]]
  }
}

rm(train)
rm(test)
rm(original_dataset)

train_idx <- data$train
test_idx <- !train_idx
data <- select(data, -train)


# Random Forest -----------------------------------------------------------

# There's high imbalance between the two classes, 
# so we need to stratify the sample for each tree in the forest accordinly.
data[train_idx, ]$CrimeType %>% table

# Before performing hyperparameter optimization, 
# let's select which features should be included.
# We could do this while optimizing (as another model to consider)
# but since this already takes too much time, we're forced to do it before

# Let's train then a model with all variables and look at their importances
max_depth <- 10
ntree <- 100

model <- randomForest(
  CrimeType ~ .,
  data=select(data, -index, -MCA1, -MCA2, -MCA3, -MCA4, -MCA5),
  subset=train_idx,
  maxnodes=2^(max_depth - 1),
  ntree=ntree,
  importance=TRUE,
  strata=data[train_idx, ]$CrimeType,
  sampsize=c(1000, 1000)
)

model$importance[,'Manslaughter by Negligence'] %>% 
  sort(decreasing=TRUE) %>% 
  tibble(
    feature=factor(
      names(.), 
      levels=names(.)[
        order(., decreasing=TRUE)
        ]
    ), 
    importance=.
  ) %>% 
  ggplot(aes(feature, importance)) +
  geom_bar(stat='identity') + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  ggtitle('Feature importance (before hyperparameter optimization)')

ggsave('plots/variable_importance_preopt.png')

# Starting from AgencyType, all the remaining variables seem to be irrelevant, even cluster
# We'll omit all of those except cluster, Agency Type and those related to Sex, 
# since those were from the original dataset and still could have some importance. 
# cluster is included since our initial objective was to use that variable in our analysis.

# Note that from all the extracted features, only AgeDiff and Sex_Same could be relevant.
# We'll analyze this again once we optimize the hyperparameters.
model$importance[,'Manslaughter by Negligence'] %>% 
  sort(decreasing=TRUE) %>% names %>% paste(collapse=', -')

data <- select(
  data,
  -index, -MCA1, -MCA2, -MCA3, -MCA4, -MCA5,
  -State, -Month, -Month_Autumn, -Month_Summer, 
  -Weapon_Fire, -Month_Spring, -Relationship_Romantic, 
  -Weapon_Violence, -Year, -Month_Winter, -Relationship_Family
)


# Train/validation set
set.seed(12345)
x <- which(train_idx)
t <- 1:length(x) %>% sample(200000)
t <- x[t]
v <- x[-t]

auc <- c()
hyperparameters <- list()

for (ntree in c(seq(25, 100, 25), 150, seq(200, 500, 100))){
  for (max_depth in seq(2, 10, 2)){
    print(paste(ntree, max_depth))
    
    model <- randomForest(
      CrimeType ~ .,
      data=data,
      subset=t,
      maxnodes=2^(max_depth - 1),
      ntree=ntree,
      strata=data[t, ]$CrimeType,
      sampsize=c(1000, 1000)
    )
    
    real <- data$CrimeType[v]
    pred <- predict(model, data[v, ], type='prob')
    
    pred_obj <- prediction(
      pred[, 'Manslaughter by Negligence'], 
      real == 'Manslaughter by Negligence'
    )
    roc <- performance(pred_obj, measure="tpr", x.measure="fpr")
    auc <- c(auc, as.numeric(performance(pred_obj, 'auc')@y.values))
    
    hyperparameters <- combine(
      hyperparameters,
      list(c(max_depth, ntree))
    )
  }
}

hyperparameter_optimization <- combine(hyperparameters) %>% 
  matrix(ncol=2, byrow=TRUE) %>% 
  'colnames<-'(c('max_depth', 'ntree')) %>% 
  as_tibble() %>% 
  bind_cols(data.frame(auc=auc))

write_csv(hyperparameter_optimization, 'data/hyperparameter_optimization.csv')

hyperparameter_optimization %>% 
  mutate(max_depth=factor(max_depth)) %>% 
  ggplot(aes(ntree, auc, color=max_depth, label=max_depth)) +
  geom_line() + geom_text() + ggtitle('Hyperparameter optimization')

ggsave('plots/hyperparameter_optimization.png')

# The model doesn't improve significantly starting from 200 trees.
# max_depth=10 is the clear winner in all ntrees, so we'll stick with that.
# We choose ntree=200 due to its apparent stability

max_depth <- 10
ntree <- 200

model <- randomForest(
  CrimeType ~ .,
  data=data,
  subset=train_idx,
  maxnodes=2^(max_depth - 1),
  ntree=ntree,
  importance=TRUE,
  strata=data[train_idx, ]$CrimeType,
  sampsize=c(1000, 1000)
)

# Confusion matrix and Perfomance metrics ---------------------------------

real <- data$CrimeType[test_idx]
pred <- predict(
  model, 
  data[test_idx, ],
  type='prob'
)

pred_obj <- prediction(
  pred[, 'Manslaughter by Negligence'], 
  real == 'Manslaughter by Negligence'
)
roc <- performance(pred_obj, measure="tpr", x.measure="fpr")

(auc <- as.numeric(performance(pred_obj, 'auc')@y.values)) # 0.871645

tibble(
  FPR=roc@x.values[[1]],
  TPR=roc@y.values[[1]]
) %>% 
  ggplot(aes(FPR, TPR)) + 
  geom_line() + 
  geom_abline(slope=1, lty='dashed') + 
  ggtitle('ROC curve')

ggsave('plots/roc.png')


pred <- colnames(pred)[
  apply(pred, 1, which.max) %>% combine
  ]

(conf <- table(real=real, pred=pred))
#                              pred
# real                         Manslaughter by Negligence Murder or Manslaughter
# Manslaughter by Negligence                       2171                    771
# Murder or Manslaughter                          21751                 123479

(accuracy <- sum(diag(conf)) / sum(conf)) # 0.848001

(precision_negl <- conf[[1, 1]] / sum(conf[, 1])) # 0.09075328
(sensitivity <- conf[[1, 1]] / sum(conf[1, ])) # 0.7379334

(f_score <- 2 * precision_negl * sensitivity / (precision_negl + sensitivity)) # 0.1616289

# Feature importance ------------------------------------------------------

feature_importance <- model$importance[,'Manslaughter by Negligence'] %>% 
  sort(decreasing=TRUE)

feature_importance
# Weapon    Relationship       VictimAge  PerpetratorAge PerpetratorRace         AgeDiff 
# 0.1266666734    0.1241922381    0.0847558701    0.0466412424    0.0313578027    0.0302220061 
# VictimRace      AgencyType  PerpetratorSex       VictimSex        Sex_Same         cluster 
# 0.0137042184    0.0077710294    0.0058374889    0.0030771813    0.0030129977    0.0009858836

tibble(
  feature=factor(
    names(feature_importance), 
    levels=names(feature_importance)[
      order(feature_importance, decreasing=TRUE)
    ]
  ), 
  importance=feature_importance
) %>% 
  ggplot(aes(feature, importance)) +
  geom_bar(stat='identity') + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  ggtitle('Feature importance')

ggsave('plots/variable_importance.png')
