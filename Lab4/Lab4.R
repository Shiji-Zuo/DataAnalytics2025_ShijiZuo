##########################################
### Principal Component Analysis (PCA) ###
##########################################

## load libraries
library(ggplot2)
library(ggfortify)
library(GGally)
library(e1071)
library(class)
library(psych)
library(readr)

## set working directory so that files can be referenced without the full path
setwd("/Users/Shiji/Library/CloudStorage/OneDrive-shanghaitech.edu.cn/rpi_first_term/data analytics/Lab 4-selected")

## read dataset
wine <- read_csv("wine.data", col_names = FALSE)

## set column names
names(wine) <- c("Type","Alcohol","Malic acid","Ash","Alcalinity of ash","Magnesium","Total phenols","Flavanoids","Nonflavanoid Phenols","Proanthocyanins","Color Intensity","Hue","Od280/od315 of diluted wines","Proline")

## inspect data frame
# head(wine)

## change the data type of the "Type" column from character to factor
####
# Factors look like regular strings (characters) but with factors R knows 
# that the column is a categorical variable with finite possible values
# e.g. "Type" in the Wine dataset can only be 1, 2, or 3
####

wine$Type <- as.factor(wine$Type)


## 1

X <- wine[,2:14]
Y <- wine[,1]

Xmat <- as.matrix(X)

Xc <- scale(Xmat, center = T, scale = T)


principal_components <- princomp(Xc)

summary(principal_components)

principal_components$loadings

## Comp 1 :Flavanoids: 0.423,,,  2:Color Intensity: 0.530

## using autoplot() function to plot the components
autoplot(principal_components, data = wine, colour = 'Type',
         loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 3, scale = 0)



train <- 1:150

k_pre <- 13

knn1 <- knn(wine[train, 2:14], wine[-train, 2:14], wine[train, ]$Type, k = k_pre )
tab1 <- table(knn1, wine[-train, ]$Type, dnn = c("predicted","actual"))
print(tab1)
acc1 <- sum(diag(tab1)) / sum(tab1)
precision <- mean(diag(tab1) / rowSums(tab1), na.rm = TRUE)
recall    <- mean(diag(tab1) / colSums(tab1), na.rm= TRUE)
f1        <- mean(2 * precision * recall / (precision + recall),na.rm = TRUE)

cat("Accuracy of all features : ", round(acc1, 4), "\n", sep = "")
cat("Mean precision of all features : ", round(precision, 4), "\n", sep = "")
cat("Mean recall of all features : ", round(recall, 4), "\n", sep = "")
cat("Mean f1 of all features : ", round(f1, 4), "\n", sep = "")

s <- principal_components$scores


knn2 <- knn(s[train,1:2 ], s[-train,1:2 ], wine[train, ]$Type, k = k_pre )

tab2 <- table(knn2, wine[-train, ]$Type, dnn = c("predicted","actual"))


print(tab2)

acc2 <- sum(diag(tab2)) / sum(tab2)
precision <- mean(diag(tab2) / rowSums(tab2), na.rm = TRUE)
recall    <- mean(diag(tab2) / colSums(tab2), na.rm= TRUE)
f1        <- mean(2 * precision * recall / (precision + recall),na.rm = TRUE)



cat("Accuracy of first two PCs : ", round(acc2, 4), "\n", sep = "")
cat("Mean precision of first two PCs : ", round(precision, 4), "\n", sep = "")
cat("Mean recall of first two PCs : ", round(recall, 4), "\n", sep = "")
cat("Mean f1 of first two PCs : ", round(f1, 4), "\n", sep = "")


# print(tab2)
# cat("Accuracy of fieature set 1: ", round(acc2, 4), "\n", sep = "")


## visualize variables
# pairs.panels(wine[,-1],gap = 0,bg = c("red", "yellow", "blue")[wine$Type],pch=21)
# 
# ggpairs(wine, ggplot2::aes(colour = Type))

###
