library(doParallel)
registerDoParallel(cores=3)

library(randomForest)
library(FactoMineR)
library(tidyverse)
library(stringr)
library(lubridate)
library(forcats)

data <- read_csv('data/database.csv.gz')
data
dim(data)
# Change the column names so they don't include spaces
colnames(data) <- colnames(data) %>% map(str_replace, ' ', '')

names(data)
# 1. What is the objective of the data? -----------------------------------

# We'll try to predict several variables about the perpetrator or the type of incident.
# For that, we'll use the variables described by the victim. 
# Additionally, we'll define a cluster from the variables of the victim and use that cluster
# as an additional predictor for our models.


# 2. First exploration and preprocessing ----------------------------------

### Analyse (probably) useless columns ###

# Number of unique values for each column
data %>% map(~unique(.) %>% length()) %>% unlist()

# We notice that RecordID is useless.
# AgencyCode and AgencyName might be too.
# Let's see if there's an structure in their values in case they might be useful.

data$AgencyCode %>% unique()
# We notice that the first two letters are the state in which the Agency operates,
# which might be useful, but there's already a State column

data$State %>% unique() # We can use this variable directly

data$AgencyName %>% unique() 
# AgencyName can't be used either, because there's so many of them and their names 
# don't have any relevance in the problem at hand, either
# City follows the same rule; too many values and not that much relevance.

# Incident has too many values. What's its meaning?
data$Incident %>% unique() %>% sort()

# It might be the number of incident in that period Year-Month. 
# In fact, if we look at the set difference between [1, 999] 
# and that set, only 0 remains.
setdiff(1:999, data$Incident %>% unique())
setdiff(data$Incident %>% unique(), 1:999)

# In any case, the ordering between incidents can be very hard to analyse,
# since we don't know if two incidents are related. So we won't take that into account.

# RecordSource has only 2 different values: FBI or FOIA
data$RecordSource %>% unique()
# It might not have importance, so we remove it too

# City, although there are a lot of different values, might have importance,
# specially since some of them are highly populated. 
data$City %>% table() %>% sort(decreasing=TRUE) %>% head(50)

# Then, let's remove these columns
data <- select(data, -RecordID, -AgencyCode, -AgencyName, -Incident, -RecordSource)

### Categorical variables ###

# Transform categorical variables to factors
ch_cols <- colnames(data)[map(colnames(data), ~class(data[[.]]) == 'character') %>% unlist()]
for (col in ch_cols) {
  data[col] <- as.factor(data[[col]])
}
rm(ch_cols)

### Crime Variables ###
# The sizes of the other variables seem feasible, so let's study them in depth

# AgencyType might have some relevance on the type of crime
data$AgencyType %>% unique()
# There are also few levels, so it's worthy of being included in the analysis

# CrimeType has only two values
data$CrimeType %>% unique()
# This might be our target variable. Let's keep as it is

### CrimeSolved ###

# We notice that when the crime has not been solved, 
# the perpetrator information is obviously missing.
colMeans(filter(data, CrimeSolved == 'No') == 'Unknown')

# We'll remove these rows, since we want to predict features about the perpetrator
data <- data %>% filter(CrimeSolved == 'Yes') %>% select(-CrimeSolved)

# Possible types of Relationship
data$Relationship %>% table() %>% sort()
# Very useful for prediction, but obviously shouldn't be used 
# when the perpetrator variables are the target.
# This will only be used when trying to predict murder by negligence

# Weapon has a few values but they might be very good predictors too
data$Weapon %>% table %>% sort
# In this case, the variable can be used to predict the perpetrator without "cheating"

### Time variable ###

# Time might be useful, if we notice that some behaviours change over time
data$Year %>% unique() # From 1980 to 2014

# How many crimes per year?
data %>% 
  count(Year) %>% 
  ggplot() + 
  geom_line(aes(Year, n)) + 
  ylab("Number of murders")
# We notice a huge increase around 1990, and after 1993, the highest increase ever, 
# there's a considerable drop in the number of murders to a more stable rate.
# http://legal-dictionary.thefreedictionary.com/Violent+Crime+Control+and+Law+Enforcement+Act+of+1994

ggsave('plots/year_evolution.png')

months <- c(
  'January', 'February', 'March',
  'April', 'May', 'June',
  'July', 'August', 'September',
  'October', 'November', 'December'
)
months <- c(paste('0', 1:9, sep=''), 10:12) %>% 
  'names<-'(months)

# Add Date column for ease of use. 
# Since we don't have day, we'll assume the first day of the month
data <- data %>% 
  mutate(Date = paste(Year, months[Month], '01', sep='-') %>% as_date())
# Note that Date is only for exploration; this won't be used as a predictor

# Perform some analyses with time
data %>% 
  count(Date) %>% 
  ggplot(aes(Date, n)) +
  ggtitle('Homicides by month') + 
  geom_line() +
  geom_vline(
    xintercept=as.numeric(date(paste(1980:2014, '01-01', sep='-'))), 
    linetype='dashed', color='blue', alpha=.5
  ) +
  geom_vline(
    xintercept=as.numeric(date(paste(1980:2014, '07-01', sep='-'))),
    linetype='dashed', color='red', alpha=.25
  ) + 
  theme(plot.title = element_text(hjust = 0.5))

ggsave('plots/month_evolution.png')

# Types of crime over time
data %>% 
  mutate(CrimeType=fct_relevel(CrimeType, 'Murder or Manslaughter', 'Manslaughter by Negligence')) %>% 
  ggplot() +
  ggtitle('Homicides by Month-Type') + 
  geom_bar(aes(Year, fill=CrimeType)) + 
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position="bottom"
  )

ggsave('plots/crime_type_time.png')


### Perpetrator Variables Analysis ###

### Categorical variables: Sex, Race, Ethnicity. ###
select(data, PerpetratorSex, PerpetratorRace, PerpetratorEthnicity) %>% 
  summary()

# We have 'Unknowns' in:
#   Sex: 147
#   Race: 6049
#   Ethnicity: 256374

# There are a lot of NAs in Ethnicity, and maybe we should discard the entire column,
# as it does not carry much information: it is just a binary variable 
# 'Hispanic/Not hispanic' and it carries a lot of 'Unknowns'.
data <- select(data, -PerpetratorEthnicity)

### Perpetrator Sex and Race ###

# Take a look at the sex of the perpetrators, combined with their race
ggplot(data, mapping = aes(x = PerpetratorSex)) + 
  geom_bar(mapping = aes(fill = PerpetratorRace))
# The majority of the identified perpetrators are white or black males

# The following plot expresses the same information as before in a better way.
ggplot(data, mapping = aes(x = PerpetratorRace)) +
  geom_bar(mapping = aes(fill = PerpetratorSex))


### Quantitative variables: Age, Count. ###

# PerpetratorAge
summary(data$PerpetratorAge)

# We just have one NA, let's keep it in case we find more

# Create a histogram to see how the variable behaves
ggplot(data, mapping = aes(x = PerpetratorAge)) + 
  geom_histogram()

# We see that we have a lot of ages that have a value close to 0.
# Let's see how many individuals we have in that situation.
data %>% filter(PerpetratorAge <= 2) %>% count(PerpetratorAge)

# 0 years old: 26700 victims
# 1 years old: 16 victims
# 2 years old: 6 victims

# The 0 ones are obviously wrong. Let's change them for 'NA'
data <- data %>%
  mutate(PerpetratorAge = replace(PerpetratorAge, PerpetratorAge == 0, NA))

#Let's take a look again at the histogram
ggplot(data, mapping = aes(x = PerpetratorAge)) + 
  geom_histogram()

# Now it looks much better.
# We see that we have some ages lower than 10 years old, which is weird
data %>% 
  filter(PerpetratorAge < 10) %>% 
  ggplot(mapping = aes(x = PerpetratorAge)) +
  geom_bar(mapping = aes(fill = CrimeType))

data %>% 
  filter(PerpetratorAge < 10) %>% 
  count()

# There is an uncommon high rate of 'Manslaughter by Negligence' if we take only the perpetrators
# with an age lower than 10 years old (in the overall dataset it just represents the 1.5% of the cases,
# and here it accounts for more than the 50%). This means that there is probably a high correlation
# between these ages and the 'negligence' cases. However, they are just 338 cases out of 638454 and they
# will not affect much our analysis and models.


### Perpetrator Count ###

# Create an histogram to see how the variable behaves
ggplot(data, mapping = aes(x = PerpetratorCount)) + 
  geom_histogram(bins = 10, mapping = aes())

# It follows an skewed distribution, most of the values are concentred at 0. We assume it is just and 
# id for when there is more than 1 perpetrator in a case. Because of that, we will discard the
# variable, as it does not carry much information. 

data <- data %>% select(-PerpetratorCount)


# Victim Variables Analysis -----------------------------------------------

### Victim Variables Analysis ###

# We will do the same analysis as we did in the Perpetrators part. 
# In this case we will directly work with the solved cases. 

### Categorical variables: Sex, Race, Ethnicity. ###

# Again, we have some unknowns in the Sex and Race, but not many. There are some strange things in the
# age, so we will have to treat the 'weird' values. In the Ethnicity variable there are a lot of 'Unknowns',
# so again we have decided to discard the entire variable, as it does not carry a lot of information.

data <- data %>% select(-VictimEthnicity)

### Quantitative variables: Age and Count ###

### Victim Age ###

# Take a look at the histogram of the variable
data %>% 
  ggplot(mapping = aes(x = VictimAge)) +
  geom_histogram()

# There are some values greater than 120 that seem impossible. They must be wrong.
# Change to NA the values greater than 120.
data <- data %>% 
  mutate(VictimAge = replace(VictimAge, VictimAge >= 120, NA))

# Re-plot the histogram
data %>% 
  ggplot(mapping = aes(x = VictimAge)) +
  geom_histogram()

# We see that there are three 'weird' peaks. Two at '0' and '1' years old and another one close to 100.
# Let's examine the one close to 100
data %>% filter(VictimAge >= 98) %>% count(VictimAge)

# 98 years old: 24 victims
# 99 years old: 4451 victims

# It may be that 99 is a value used when the police does not know the age of the victim. 
# We will change these values to 'NA'. Let's take a look now at the other side of the histogram, close to 0.

data %>% filter(VictimAge <= 4) %>% count(VictimAge)

# 0 years old: 7419 victims
# 1 years old: 5005 victims
# 2 years old: 3485 victims 
# 3 years old: 2156 victims
# 4 years old: 1488 victims

# We are not sure about these ones. But the peak in '0 years old' does not seem normal. We will
# change these values by 'NA' as well.
data <- data %>% 
  mutate(VictimAge = replace(VictimAge, VictimAge == 0 | VictimAge == 99, NA))

# Take a final look at the histogram.
data %>% 
  ggplot(mapping = aes(x = VictimAge))+
  geom_histogram()

### Victim Count ###
# Create a histogram
data %>% 
  select(VictimCount) %>% 
  ggplot(mapping = aes(x=VictimCount))+
  geom_histogram()

# This variable represents an identificator for when there is more than one victim in the same case. 
# We believe that it does not bring important information for our analysis, so we will discard it. 
data <- data %>% select(-VictimCount)


# EDA Visualization -------------------------------------------------------
data2 <- data %>%
  select(VictimSex, PerpetratorSex) %>% 
  filter(VictimSex != 'Unknown' & PerpetratorSex != 'Unknown')
ggplot(data = data2, mapping = aes(x = VictimSex)) +
  geom_bar(mapping = aes(fill = PerpetratorSex)) +
  ylab('Number of murders') +
  xlab('Victim Sex')

ggsave('plots/sex_victim_perpetrator.png')

data %>% 
  mutate(AgeDiff = PerpetratorAge - VictimAge) %>% 
  filter(AgeDiff < 50 & AgeDiff > -50) %>% 
  na.omit() %>% 
  ggplot(mapping = aes(x = AgeDiff)) +
  geom_bar(mapping = aes(fill = Weapon)) +
  xlab('Age Difference = Perpetrator Age - Victim Age') +
  ylab('Number of murders')

data %>% 
  mutate(AgeDiff = PerpetratorAge - VictimAge) %>% 
  filter(AgeDiff < 50 & AgeDiff > -50) %>% 
  na.omit() %>% 
  ggplot(mapping = aes(x = AgeDiff)) +
  geom_bar(mapping = aes(fill = Weapon)) +
  xlab('Age Difference = Perpetrator Age - Victim Age') +
  ylab('Number of murders')

# Which variables should we impute? ------------------------------------------------------------------



# Let's now take a look at the final dataset
# The numerical variables are just these three: Year, VictimAge and PerpetratorAge
colnames(data)[map(data, ~class(.) == 'integer') %>% unlist()]

# Is there any linear relationship between the ages of the Victim and the Perpetrator?
# It doesn't seem like it for most of the ages
data %>% 
  sample_frac(.1) %>% 
  ggplot(aes(VictimAge, PerpetratorAge)) +
  geom_jitter(alpha=.25)

# Unknown frequency: notice that VictimAge and PerpetratorAge have NAs instead of Unknown
(select(data, -Date, -VictimAge, -PerpetratorAge) == 'Unknown') %>% 
  colMeans(na.rm=TRUE)

# Only Relationship has a significant amount of Unknowns, 
select(data, Relationship) %>% table()
# but it might actually be a different modality for this category.

data %>% 
  mutate(
    Relationship = ifelse(Relationship != 'Unknown', 'Known', 'Unknown') %>% factor,
    Year = factor(Year)
  ) %>%
  select(-Date) %>%
  as.data.frame() %>%
  catdes(num.var=13, proba=0.05) %>% 
  '$'('test.chi2')

# If we look at the test.chi2 table, we see that all variables characterize
# the Known/Unkwown factor for Relationship.
# We then know that these are MAR (Missings at Random),
# which means that they are affected by third variables.
# We decide to leave the Unknowns as a new modality,
# since they contain information related to the rest of the variables.

# Let's analyze Race and Weapon in comparison with the rest of variables.
data %>%
  mutate(
    VictimRace = ifelse(VictimRace != 'Unknown', 'Known', 'Unknown') %>% factor,
    Year = factor(Year)
  ) %>%
  select(-Date) %>%
  as.data.frame() %>%
  catdes(num.var=9, proba=0.05) %>% 
  '$'('test.chi2')

# There's relationship between VictimRace=Unknown and the rest of the variables

data %>%
  mutate(
    PerpetratorRace = ifelse(PerpetratorRace != 'Unknown', 'Known', 'Unknown') %>% factor,
    Year = factor(Year)
  ) %>%
  select(-Date) %>%
  as.data.frame() %>%
  catdes(num.var=12, proba=0.05) %>% 
  '$'('test.chi2')

# There's relationship between PerpetratorRace=Unknown and the rest of the variables

data %>%
  mutate(
    Weapon = ifelse(Weapon != 'Unknown', 'Known', 'Unknown') %>% factor,
    Year = factor(Year)
  ) %>%
  select(-Date) %>%
  as.data.frame() %>%
  catdes(num.var=14, proba=0.05) %>% 
  '$'('test.chi2')

# There's relationship between Weapon=Unknown and the rest of the variables

# So, we'll leave Unknown as a separate modality in these variables


# For Sex (Victim or Perpetrator), since it's a dichotomy, 
# it doesn't make sense to create a separate modality.
# We'll try to impute their values.

# We will use Random Forest imputation, because most of our variables are categorical
# and RFs should capture the imputation rules appropriately.
imputed_data <- data %>%
  select(-Date, -City) %>% # we can't consider as many modalities as City provides, so we need to exclude it
  mutate(
    VictimSex=levels(VictimSex)[
      ifelse(VictimSex == 'Unknown', NA, VictimSex)
      ] %>% factor,
    PerpetratorSex=levels(PerpetratorSex)[
      ifelse(PerpetratorSex == 'Unknown', NA, PerpetratorSex)
      ] %>% factor,
    Year=factor(Year)
  )

imputation_columns <- colnames(imputed_data)[imputed_data %>% is.na %>% colSums %>% '>'(0)]
missings <- imputed_data[imputation_columns] %>% is.na

# Random initialization
for (col in imputation_columns){
  if (class(imputed_data[[col]]) == 'factor'){
    imputed_data[missings[, col], col] <- imputed_data[[col]] %>% 
      levels %>% sample(missings[, col] %>% sum, replace=TRUE)  
  } else if (class(imputed_data[[col]]) == 'integer') {
    col_data <- imputed_data[[col]]
    
    imputed_data[missings[, col], col] <- sample(
      min(col_data, na.rm=TRUE):max(col_data, na.rm=TRUE), 
      missings[, col] %>% sum, replace=TRUE
    )
  } else {
    paste(class(col), 'class not implemented yet') %>% stop
  }
}

max_iterations <- 10
max_depth <- 5
ntree <- 50

for (iteration in 1:max_iterations) {
  prev_matrix <- imputed_data
  
  for (col in imputation_columns) {
    print(paste(iteration, col))
    
    train <- !missings[, col]
    test <- missings[, col]
    
    col_num <- (colnames(prev_matrix) == col) %>% which
    
    model <- randomForest(
      prev_matrix[train, -col_num],
      prev_matrix[[col_num]][train],
      maxnodes=2^(max_depth - 1),
      ntree=ntree
    )
    
    imputed_data[[col_num]][test] <- predict(
      model, prev_matrix[test, -col_num]
    )
  }
  
  rm(prev_matrix)
}

imputed_data$City <- data$City
imputed_data$Date <- data$Date

data <- imputed_data
rm(imputed_data)

data$VictimAge <- data$VictimAge %>% round %>% as.integer
data$PerpetratorAge <- data$PerpetratorAge %>% round %>% as.integer

### Feature Extraction ###
# Could we define any new variables based on the ones we already have?

data$AgeDiff <- data$PerpetratorAge - data$VictimAge

data$Weapon %>% table %>% sort

data$Weapon_Fire <- data$Weapon %in% c('Firearm', 'Gun', 'Handgun', 'Rifle', 'Shotgun')
data$Weapon_Violence <- data$Weapon %in% c('Fall', 'Drowning', 'Suffocation', 'Strangulation')

data$Relationship %>% table %>% sort

data$Relationship_Family <- data$Relationship %in% c(
  'Husband', 'Wife', 
  'Ex-Husband', 'Ex-Wife', 
  
  'Common-Law Husband', 'Common-Law Wife',
  
  'Father', 'Mother', 
  'Stepfather', 'Stepmother',
  
  'Son', 'Daughter',
  'Stepson', 'Stepdaughter',
  
  'Brother', 'Sister', 
  'Family', 'In-Law'
)

data$Relationship_Romantic <- data$Relationship %in% c(
  'Boyfriend', 'Girlfriend', 
  'Boyfriend/Girlfriend', 
  'Husband', 'Wife', 
  'Common-Law Husband', 'Common-Law Wife', 
  'Ex-Husband', 'Ex-Wife'
)

data$Sex_Same <- data$VictimSex == data$PerpetratorSex
data$Sex_PerpetratorMaleVictimFemale <- data$PerpetratorSex == 'Male' & data$VictimSex == 'Female'

data$Month %>% unique

data$Month_Winter <- data$Month %in% c('December', 'January', 'February')
data$Month_Spring <- data$Month %in% c('March', 'April', 'May')
data$Month_Summer <- data$Month %in% c('June', 'July', 'August')
data$Month_Autumn <- data$Month %in% c('September', 'October', 'November')

# Save final preprocessed file
write.csv(data, gzfile('data/database_preprocessed.csv.gz', 'wt'))