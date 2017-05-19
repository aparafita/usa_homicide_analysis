
# Initialising ------------------------------------------------------------

rm(list = ls())
library(tidyverse)
library(stringr)

setwd("/Users/cesc/Documents/UPC MIRI-FIB/2Q/Multivariate Analysis/Final Project/")
data <- read_csv("database.csv")

# Data Pre-processing

# We transform the column names, without spaces
colnames(data) <- colnames(data) %>% map(str_replace, ' ', '')

# First summary of the data
summary(data)


# Perpetrator Variables Analysis ------------------------------------------

### Perpetrator Variables Analysis ###

# Create a vector with just the perpetrator variables, for simplicity when working
perp <- c(16, 17, 18, 19, 23)
perp_factors <- c(16, 18, 19)
perp_quanti <- c(17, 23)


### Categorical variables: Sex, Race, Ethnicity. ###

head(data[,perp])

# We need to transform into factors the variables Sex, Race and Ethnicity
data <- data %>% 
  mutate(PerpetratorSex = as.factor(data$PerpetratorSex)) %>% 
  mutate(PerpetratorRace = as.factor(data$PerpetratorRace)) %>% 
  mutate(PerpetratorEthnicity = as.factor(data$PerpetratorEthnicity))

summary(data[,perp_factors])

# We have 'Unknowns' in:
#   Sex: 190365
#   Race: 196047
#   Ethnicity: 446410
# There are many unknowns, but probably concentrated inside the unsolved cases, let's check it

solved <- data %>% filter(CrimeSolved == "Yes")
summary(solved[,perp_factors])

# After applying the 'Solved' filter, the 'Unknowns' that we obtain in these 3 columns are:
#   Sex: 17
#   Race: 6049
#   Ethnicity: 256374

# This means that the majority of 'Unknowns' in Sex and Race are due the fact that the case has not been
# solved. For this reason, from this point on we will only work with the 'Solved' cases. 
# Apart from that, in Ethnicity there are a lot of NA, and maybe we should discard the entire column,
# as it does not carry much information. It is just a binary variable 'Hispanic/Not hispanic' and it carries a lot
# of 'Unknowns', so we will just discard the variable.

data <- solved

### Perpetrator Sex ###

# Take a look at the sex of the perpetrators, combined with their race
ggplot(data, mapping = aes(x = PerpetratorSex)) + 
  geom_bar(mapping = aes(fill = PerpetratorRace))
# The majority of the identified perpetrators are white or black males

### Perpetrator Race ###

# The following plot expresses the same information as before in a better way.
ggplot(data, mapping = aes(x = PerpetratorRace)) +
  geom_bar(mapping = aes(fill = PerpetratorSex))

### Perpetrator Ethnicity ###

ggplot(data, mapping = aes(x = PerpetratorEthnicity)) + 
  geom_bar()

# As we knew before, a lot of 'Unknowns', even with the subset of the solved cases. 
# For this, we will omit the variable for our study.

data <- data %>% select(-PerpetratorEthnicity)


### Quantitative variables: Age, Count. ###

### Perpetrator Age ###

summary(data$PerpetratorAge)

# We just have one 'predeclared' NA, let's keep it in case we find more

# Create a histogram to see how the variable behaves
ggplot(data, mapping = aes(x = PerpetratorAge)) + 
  geom_histogram()

# We see that we have a lot of ages that have a value close to 0.
# Let's see how many individuals we have in that situation.
data %>% filter(PerpetratorAge == 0) %>% count()
data %>% filter(PerpetratorAge == 1) %>% count()
data %>% filter(PerpetratorAge == 2) %>% count()

# 0 years old: 26700 victims
# 1 years old: 16 victims
# 2 years old: 6 victims

# The 0 ones are obviously wrong. Let's change them for 'NA'
data <- data %>%
  mutate(PerpetratorAge = replace(data$PerpetratorAge, data$PerpetratorAge == 0, NA))

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
# will not affect our analysis and models.

### Perpetrator Count ###

# Create an histogram to see how the variable behaves
ggplot(data, mapping = aes(x = PerpetratorCount)) + 
  geom_histogram(bins = 10, mapping = aes())

# It follows an skewed distribution, most of the values are concentred at 0. We assume it is just and 
# identificator for when there is more than 1 perpetrator for a case. Because of that, we will discard the
# variable, as it does not carry much information. 

data <- data %>% select(-PerpetratorCount)


# Victim Variables Analysis -----------------------------------------------

### Victim Variables Analysis ###

# We will do the same analysis as we did in the Perpetrators part. In this case we will directly work
# with the solved cases. 
# Create a vector to take the victim variables
vic <- c(12, 13, 14, 15, 22)
summary(data[,vic])

### Categorical variables: Sex, Race, Ethnicity. ###

# We need to transform into factors the variables Sex, Race and Ethnicity
data <- solved %>% 
  mutate(VictimSex = as.factor(solved$VictimSex)) %>% 
  mutate(VictimRace = as.factor(solved$VictimRace)) %>% 
  mutate(VictimEthnicity = as.factor(solved$VictimEthnicity))

summary(data[,vic])

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
data %>% filter(VictimAge == 98) %>% count()
data %>% filter(VictimAge == 99) %>% count()
data %>% filter(VictimAge == 100) %>% count()

# 98 years old: 24 victims
# 99 years old: 4451 victims
# 100 years old: 0 victims 

# It may be that 99 is a value used when the police does not know the age of the victim. We will change this
# values to 'NA'. Let's take a look now at the other side of the histogram, close to 0.

data %>% filter(VictimAge == 0) %>% count()
data %>% filter(VictimAge == 1) %>% count()
data %>% filter(VictimAge == 2) %>% count()
data %>% filter(VictimAge == 3) %>% count()
data %>% filter(VictimAge == 4) %>% count()

# 0 years old: 7419 victims
# 1 years old: 5005 victims
# 2 years old: 3485 victims 
# 3 years old: 2156 victims
# 4 years old: 1488 victims

# We are not sure about this ones. But the peak in '0 years old' does not seem normal. We will
# change this values by 'NA' as well.
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
# We believe that it does not bring important information, so we will discard it. 

data <- data %>% select(-VictimCount)

head(data)
summary(data)
