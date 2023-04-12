library (tidyverse)
library (repr)
library (dplyr)
library(ggplot2)
library(MASS)

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


#add variable 'time_group'
bikes['time']=substr(bikes$datetime,12,13)
df=data.frame(bikes$time,bikes$count)
ggplot(df)+geom_bar(aes(bikes$time,bikes$count),stat="identity")


group_0=c('00','01','02','03','04','05','06','23')
group_1=c('07','09','10','11','12','13','14','15','20','21','22')
group_2=c('08','16','17','18','19')


for (i in 1:nrow(bikes)) {
  bikes[i,'time_group']=ifelse(bikes[i,'time'] %in% group_0, 0,ifelse(bikes[i,'time'] %in% group_1 ,1,2))
}

bikes$time_group <- as.factor(bikes$time_group)


# Add normalized counts variable (model ended up being the same even for normalized counts)
count_mean <- mean(bikes$count)
count_sd <- sd(bikes$count)
bikes$normalized_counts <- (bikes$count - count_mean) / count_sd


# Basic Linear Model
basic_add_model = lm(count ~ season + day + atemp + humidity + windspeed, data = bikes)
summary(basic_add_model)
AIC(basic_add_model)

basic_add_model2 = lm(count ~ season + day + atemp + humidity + windspeed + time_group, data = bikes)
summary(basic_add_model2)
AIC(basic_add_model2)


#stepAIC
bikes2=bikes[,c('season','day','weather','atemp','humidity','time_group','windspeed','count')]
model=lm(count~.,bikes2)
stepReg=MASS::stepAIC(model, direction="both")
basic_add_model3=lm(count ~ season + weather + atemp + humidity + time_group + windspeed,data = bikes)



# Poisson Regression Model (basic additive)
count_model <- glm(count ~ season + day + atemp + humidity + windspeed, data = bikes, family=poisson) 
summary(count_model)

count_model2 <- glm(count ~ season + day + atemp + humidity + windspeed+time_group, data = bikes, family=poisson) 
summary(count_model2)

count_model3 <- glm(count ~  day + weather + season + atemp + time_group + humidity + humidity*atemp, data = bikes, family=poisson) 
summary(count_model3)

bikes2=bikes[,c('season','day','atemp','humidity','windspeed','count')]
mod.glm.poisson <- glm(count~., data = bikes2,family=poisson)
summary(mod.glm.poisson)


model.poisson.step <- stepAIC(mod.glm.poisson, direction = 'both')
model.poisson.step
stepAIC(mod.glm.poisson, direction = 'forward')
stepAIC(mod.glm.poisson, direction = 'backward')

model_p=glm(count~.,bikes2,family=poisson)
stepReg=MASS::stepAIC(model_p, direction="both")
stepReg$anova
summary(model_p)

registered_model <- glm(registered ~ season + day + atemp + humidity + windspeed, data = bikes, family=poisson) 
summary(registered_model)

registered_model2 <- glm(registered ~ season + day + atemp + humidity + windspeed + time_group, data = bikes, family=poisson) 
summary(registered_model2)

casual_model <- glm(casual ~ season + day + atemp + humidity + windspeed , data = bikes, family=poisson) 
summary(casual_model)

casual_model2 <- glm(casual ~ season + day + atemp + humidity + windspeed + time_group, data = bikes, family=poisson) 
summary(casual_model2)


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

#best_options
interaction_model_6 = lm(count ~  weather + season + atemp + time_group + humidity + humidity*atemp, data = bikes)
summary(interaction_model_6)
AIC(interaction_model_6)

interaction_model_7 = lm(count ~  weather + season+ I(atemp^3) + time_group + I(humidity^3) + humidity*atemp, data = bikes)
summary(interaction_model_7)
AIC(interaction_model_7)

#vif
car::vif(interaction_model_6)
#vif
car::vif(interaction_model_7)



# What if i drop all categorical variables?
model_bs_1 = glm(count ~ atemp + humidity + windspeed, data = bikes, family=poisson) 
summary(model_bs_1)






#residual plot
interaction_model_7=resid(interaction_model_7)
plot(bikes$count, interaction_model_7, 
     ylab="Residuals", xlab="Counts", 
     main="residual plot") 

