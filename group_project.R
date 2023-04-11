library (tidyverse)
library (repr)
library (dplyr)

# read data
bikes<-read.csv("train_bikes.csv")
head(bikes)

#factorize categorical variables
bikes$season <- as.factor(bikes$season)
bikes$holiday <- as.factor(bikes$holiday)
bikes$workingday <- as.factor(bikes$workingday)
bikes$weather <- as.factor(bikes$weather)

# Extra EDA
ggplot(data = bikes, aes(x = temp, y = atemp)) + 
  geom_point() +
  ggtitle("Temperature vs. ATemperature") +
  xlab("Temperature (C)") +
  ylab("aTemperature")

ggplot(data = bikes, aes(x = windspeed, y = count)) + 
  geom_point() +
  ggtitle("Windspeed vs. Count") +
  xlab("Windspeed (km/h)") +
  ylab("Count")

ggplot(data = bikes, aes(x = humidity, y = count)) + 
  geom_point() +
  ggtitle("Humidity vs. Count") +
  xlab("Humidity") +
  ylab("Count")

ggplot(data = bikes, aes(x = holiday, y = workingday)) + 
  geom_point() +
  ggtitle("Holiday vs. Workingday") +
  xlab("Holiday") +
  ylab("Workingday")

# Check for examples that are neither holidays or working days
num_examples <- bikes %>% 
  filter(holiday==0, workingday==0) %>% 
  nrow()
num_examples

# Add day variable combining holiday and working day
bikes <- bikes %>% 
  mutate(day = case_when(
    workingday == 1 ~ 0,
    holiday == 1 ~ 1,
    TRUE ~ 2
  ))
bikes$day <- as.factor(bikes$day)

# Add normalized counts variable (model ended up being the same even for normalized counts)
count_mean <- mean(bikes$count)
count_sd <- sd(bikes$count)
bikes$normalized_counts <- (bikes$count - count_mean) / count_sd


# Basic Linear Model
basic_add_model = lm(count ~ season + day + atemp + humidity + windspeed, data = bikes)
summary(basic_add_model)
AIC(basic_add_model)


# Poisson Regression Model (basic additive)
count_model <- glm(count ~ season + day + atemp + humidity + windspeed, data = bikes, family=poisson) 
summary(count_model)

registered_model <- glm(registered ~ season + day + atemp + humidity + windspeed, data = bikes, family=poisson) 
summary(registered_model)

casual_model <- glm(casual ~ season + day + atemp + humidity + windspeed, data = bikes, family=poisson) 
summary(casual_model)



# Add interactions between variables (one at a time)
full_interaction_model <- glm(count ~ season * day * atemp * humidity * windspeed, data = bikes, family=poisson) 
summary(full_interaction_model)

interaction_model_1 <- glm(count ~ season * day + atemp + humidity + windspeed, data = bikes, family=poisson) 
summary(interaction_model_1)

interaction_model_2 <- glm(count ~ season + day + atemp + humidity * windspeed, data = bikes, family=poisson) 
summary(interaction_model_2)

interaction_model_3 <- glm(count ~ season * day + atemp + humidity * windspeed, data = bikes, family=poisson) 
summary(interaction_model_3)

interaction_model_4 <- glm(count ~ season * day + atemp * humidity * windspeed, data = bikes, family=poisson) 
summary(interaction_model_4)

interaction_model_5 <- glm(count ~ season * day + atemp + humidity + windspeed, data = bikes, family=poisson) 
summary(interaction_model_5)



# What if i drop all categorical variables?
model_bs_1 = glm(count ~ atemp + humidity + windspeed, data = bikes, family=poisson) 
summary(model_bs_1)



# Supposedly this calculates R^2 for poisson regression models
null_model <- glm(formula = count ~ 1, family = poisson, data = bikes)
R2 <- 1 - (logLik(null_model) - logLik(model_bs_1)) / logLik(null_model)
R2