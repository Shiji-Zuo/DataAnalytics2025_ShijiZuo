library(readr)
library(EnvStats)
library(nortest)

# set working directory (relative path)
setwd("~/Downloads/Lab 1")

# read data~/Downloads/Lab 1
epi.data <- read_csv("epi_results_2024_pop_gdp.csv")

ECO <- epi.data$ECO.new
BDH <- epi.data$BDH.new
summary(ECO)
summary(BDH)

# boxplot of variable(s)
boxplot(ECO, BDH, names = c("ECO","BDH"))

### Histograms ###

# histogram (frequency distribution) over range
hist(ECO,  prob=TRUE)

# print estimated density curve for variable
lines(density(ECO,na.rm=TRUE,bw="SJ")) # or try bw=“SJ”

# histogram (frequency distribution) over range
hist(BDH,  prob=TRUE)

# print estimated density curve for variable
lines(density(BDH,na.rm=TRUE,bw="SJ")) # or try bw=“SJ”

#ecdf
plot(ecdf(ECO), do.points=FALSE, verticals=TRUE) 

plot(ecdf(BDH), do.points=FALSE, verticals=TRUE) 


### Quantile-quantile Plots ###

# print quantile-quantile plot for variable with theoretical normal distribuion
qqnorm(ECO); qqline(ECO) 

# print quantile-quantile plot for variable with theoretical normal distribuion
qqnorm(BDH); qqline(BDH) 

# print quantile-quantile plot for 2 variables
qqplot(ECO, BDH, xlab = "Q-Q plot for ECO vs BDH") 

## Statistical Tests

shapiro.test(ECO) 
shapiro.test(BDH) 

ad.test(ECO) 
ad.test(BDH) 

ks.test(ECO,BDH) 

wilcox.test(ECO,BDH) 

t.test(ECO,BDH) 

