library(tidyverse)
library(stringr)
library(lubridate)
library(forcats)

data <- read_csv('database.csv')
data

# Change the column names so they don't include spaces
colnames(data) <- colnames(data) %>% map(str_replace, ' ', '')


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

### Crime Variables ###
# The sizes of the other variables seem feasible, so let's study them in depth

# AgencyType might have some relevance on the type of crime
data$AgencyType %>% unique()
# There are also few levels, so it's worthy of being included in the analysis

# CrimeType has only two values
data$CrimeType %>% unique()
# This might be our target variable. Let's keep as it is

#### CRIMESOLVED ####
# TODO: Fill this with Cesc's part
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
  geom_line(aes(Year, n))
# We notice a huge increase around 1990, and after 1993, the highest increase ever, 
# there's a considerable drop in the number of murders to a more stable rate.
# http://legal-dictionary.thefreedictionary.com/Violent+Crime+Control+and+Law+Enforcement+Act+of+1994

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


# Errors

# Outliers

# Missing Values

# Feature Selection

# Feature Extraction

data %>% map(~unlist(.) %>% is.na() %>% sum()) %>% unlist()
is_null

data$`Perpetrator Age` %>% unique()
data$`Perpetrator Sex` %>% unique()
data$`Crime Type` %>% unlist() %>% is_null() %>% sum()
