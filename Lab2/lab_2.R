####### Data Analytics Fall 2025 Lab 2 ######

library(ggplot2)

### set working directory
setwd("~/Downloads/Lab 2-selected")

### read in data
House.data <- read.csv("NY-House-Dataset.csv", header=TRUE)

House.data$log_PRICE = log10(House.data$PRICE)

House.data$log_PROPERTYSQFT = log10(House.data$PROPERTYSQFT)

House.data$log_BEDS = log10(House.data$BEDS)

House.data$log_BATH = log10(House.data$BATH)


#### Exploratory Analysis ####

price <- House.data$PRICE

## the indices where data has extreme value
oulier.indices <- (price>1e8) | (price<1e4) | ( House.data$BATH == 0 ) | (House.data$BATH != floor(House.data$BATH))

## drop ouliers
House.data.subset <- House.data[!oulier.indices,]


lin.mod1 <- lm(log_PRICE ~ log_PROPERTYSQFT + log_BEDS + log_BATH , data = House.data.subset)

print(summary(lin.mod1))


ggplot(House.data.subset, aes(x = log_BATH, y = log_PRICE))+geom_smooth(method = "lm") +
  geom_point()+
  labs(title='Model 1')

ggplot(lin.mod1, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  labs(title='Model 1 Residual vs. Fitted Values Plot', x='log_BATH', y='Residuals')

lin.mod2 <- lm(log_PRICE ~ log_PROPERTYSQFT + log_BEDS , data = House.data.subset)

print(summary(lin.mod2))



ggplot(House.data.subset, aes(x = log_PROPERTYSQFT, y = log_PRICE))+geom_smooth(method = "lm") +
  geom_point()+
  labs(title='Model 2')

ggplot(lin.mod2, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  labs(title='Model 2 Residual vs. Fitted Values Plot', x='Fitted Values', y='Residuals')

lin.mod3 <- lm(log_PRICE ~ log_PROPERTYSQFT + log_BATH , data = House.data.subset)

print(summary(lin.mod3))

ggplot(House.data.subset, aes(x = log_BATH, y = log_PRICE))+geom_smooth(method = "lm") +
  geom_point()+
  labs(title='Model 3')

ggplot(lin.mod2, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  labs(title='Model 3 Residual vs. Fitted Values Plot', x='log_BATH', y='Residuals')


