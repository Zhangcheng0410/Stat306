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
library(dplyr)
# Add day variable combining holiday and working day
bikes <- bikes %>% 
  mutate(day = case_when(
    workingday == 1 ~ 0,
    holiday == 1 ~ 1,
    TRUE ~ 1
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
basic_add_model = lm(count ~., data = bikes2)
summary(basic_add_model)
AIC(basic_add_model)
stepAIC(basic_add_model,direction='both')
stepAIC(basic_add_model,direction='forward')
stepAIC(basic_add_model,direction='backward')

#stepAIC
bikes2=bikes[,c('season','day','temp','humidity','time_group','count')]
bikes$day
model=lm(log(count)~.,bikes2)
stepReg=MASS::stepAIC(model, direction="both")

model.poisson.step=glm(count ~  ., data = bikes2, family=poisson) 
summary(model.poisson.step)
library(MASS)
stepAIC(model.poisson.step,direction='both')
stepAIC(model.poisson.step,direction='forward')
stepAIC(model.poisson.step,direction='backward')

#

2nd log.
basic_add_logmodel2=lm(log(count) ~ day +season + weather + temp + humidity + time_group ,data = bikes) #drop 'day' 
summary(basic_add_logmodel2)


3.interaction => add day*season & humidity*temp 
basic_add_logint1=lm(log(count) ~ day +season + weather + temp + humidity*time_group ,data = bikes) 
summary(basic_add_logint1)

basic_add_logint2=lm(log(count) ~ day +season + weather + temp*humidity + time_group ,data = bikes) 
summary(basic_add_logint2)

basic_add_logint3=lm(log(count) ~ day +season + weather*temp + humidity + time_group ,data = bikes) 
summary(basic_add_logint3)

basic_add_logint4=lm(log(count) ~ day +season* weather+ temp + humidity + time_group ,data = bikes) 
summary(basic_add_logint4)

basic_add_logint5=lm(log(count) ~ day*season+ weather+ temp + humidity + time_group ,data = bikes) 


exclude weather


4.final




interaction_model_9 = lm(log(count) ~  day + season + I(temp^2) + time_group + I(humidity^2) + humidity*temp, data = bikes)
summary(interaction_model_9)
AIC(interaction_model_9)

interaction_model_10 = lm(log(count) ~  day*season + I(temp^2) + time_group + I(humidity^2) + humidity*temp, data = bikes)
summary(interaction_model_10)
AIC(interaction_model_10)

stepAIC(interaction_model_10)



basic_add_logmodel2=lm(log(count) ~ day +season + weather + atemp + humidity + time_group ,data = bikes) #drop 'day' 
summary(basic_add_logmodel2)

bikes$humidity
cor(bikes$humidity,bikes$count)

# Poisson Regression Model (basic additive)
count_model1 <- glm(count ~  day + weather + season + atemp + time_group + humidity, data = bikes, family=poisson) 
summary(count_model1)
stepReg=MASS::stepAIC(count_model1, direction="both")





count_model2 <- glm(count ~  day + weather + season + atemp + time_group + humidity + humidity*atemp, data = bikes, family=poisson) 
summary(count_model2)

bikes2=bikes[,c('season','day','atemp','humidity','count')]
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

basic_add_model3=lm(count ~ season + weather + atemp + humidity + time_group ,data = bikes) #got from stepAIC process above.

interaction_model_6 = lm(count ~  weather + season + temp + time_group + humidity + humidity*temp, data = bikes)
summary(interaction_model_6)
AIC(interaction_model_6)

interaction_model_7 = lm(count ~  weather + season+ I(atemp^3) + time_group + I(humidity^3) + humidity*atemp, data = bikes)
summary(interaction_model_7)
AIC(interaction_model_7)

interaction_model_8 = lm(log(count) ~  season + I(temp^2) + time_group + I(humidity^2) + humidity*temp, data = bikes)
summary(interaction_model_8)
AIC(interaction_model_8)


sample_size <- 1000

# Generate a random sample of row indices
sample_indices <- sample(1:nrow(bikes), sample_size)

# Create the sampled dataset using the random indices
#sample_data <- bikes[sample_indices, ]


sample_data <- sample_data %>% 
  mutate(day = case_when(
    workingday == 1 ~ 0,
    holiday == 1 ~ 1,
    TRUE ~ 1
  ))
sample_data$day <- as.factor(sample_data$day)


#add variable 'time_group'
sample_data['time']=substr(sample_data$datetime,12,13)
df=data.frame(sample_data$time,sample_data$count)
ggplot(df)+geom_bar(aes(sample_data$time,sample_data$count),stat="identity")


group_0=c('00','01','02','03','04','05','06','23')
group_1=c('07','09','10','11','12','13','14','15','20','21','22')
group_2=c('08','16','17','18','19')


for (i in 1:nrow(sample_data)) {
  sample_data[i,'time_group']=ifelse(sample_data[i,'time'] %in% group_0, 0,ifelse(sample_data[i,'time'] %in% group_1 ,1,2))
}

sample_data$time_group <- as.factor(sample_data$time_group)

#sample_data <- select(- "datetime",data=sample_data)

head(sample_data)
bikes2=bikes[,c('season','day','temp','humidity','time_group','count')]
sample_data=sample_data[,c('season','day','temp','humidity','time_group','count')]
install.packages("MASS")
library(MASS)
library(dplyr)



library(leaps)


s<-regsubsets(log(count)~.,data=sample_data,method='exhaustive')
ss<-summary(s)
ss$which
which.max(ss$adjr2)
which.min(ss$cp)
which.min(ss$bic)
ss$bic
ss$aic


log_subset=lm(log(count)~season+temp+humidity+time_group,sample_data)
stepAIC(log_subset)
head(sample_data)
AIC(log_subset)
library(leaps)

s<-regsubsets(log(count)~.,data=sample_data,method='exhaustive')
ss<-summary(s)
ss$which
which.max(ss$adjr2)
which.min(ss$cp)
which.min(ss$bic)

# Fit a linear regression model
model1 <- lm(log(count) ~ season, data = sample_data)
model2 <- lm(log(count) ~ season + holiday, data = sample_data)
model3 <- lm(log(count) ~ season + ho

#vif
car::vif(basic_add_model3)
car::vif(interaction_model_6)
car::vif(interaction_model_7)

interaction_model_7 = lm(count ~  weather + season+ I(atemp^3) + time_group + I(humidity^3) + humidity*atemp, data = bikes)
summary(interaction_model_7)
AIC(interaction_model_7)

# What if i drop all categorical variables?
model_bs_1 = glm(count ~ atemp + humidity + windspeed, data = bikes, family=poisson) 
summary(model_bs_1)



interaction_model_8 = lm(log(count) ~  season + I(temp^2) + time_group + I(humidity^2) + humidity*temp, data = bikes)
summary(interaction_model_8)
AIC(interaction_model_8)


1. drop windspeed
2. drop datetime
3. drop day - in simple linear regression model, stepAIC
4. 



#residual plot
interaction_model_7=resid(interaction_model_7)
plot(bikes$count, interaction_model_7, 
     ylab="Residuals", xlab="Counts", 
     main="residual plot") 

