library(class)

abalone.data <- read.csv("/Users/Shiji/Library/CloudStorage/OneDrive-shanghaitech.edu.cn/rpi_first_term/data anlysis/Lab 3/abalone_dataset.csv")

features <- c("length", "diameter", "height", "whole_weight", "shucked_wieght", "viscera_wieght", "shell_weight")

abalone.data[, features] <- scale(abalone.data[, features])

abalone.data$age.group <- cut(
  abalone.data$rings,
  br = c(0, 8, 11, 35),
  labels = c("young", "adult", "old")
)
abalone.data$age.group <- factor(abalone.data$age.group)

# Clean rows if needed (like keeping code simple)
abalone.data <- na.omit(abalone.data)


ft1 <- c("length", "diameter", "height")
ft2 <- c("whole_weight", "shucked_wieght", "viscera_wieght", "shell_weight")

train <- sample(nrow(abalone.data), 3600)

abalone.train <- abalone.data[train, ]
abalone.test  <- abalone.data[-train, ]

k_pre <- 60
 
knn1 <- knn(
  abalone.train[, ft1],
  abalone.test[,  ft1],
  abalone.train$age.group,
  k = k_pre
)
tab1 <- table(knn1, abalone.test$age.group, dnn = c("predicted","actual"))
acc1 <- sum(diag(tab1)) / sum(tab1)

print(tab1)
cat("Accuracy of fieature set 1: ", round(acc1, 4), "\n", sep = "")

knn2 <- knn(
  abalone.train[, ft2],
  abalone.test[,  ft2],
  abalone.train$age.group,
  k = k_pre
)
tab2 <- table(knn2, abalone.test$age.group, dnn = c("predicted","actual"))
acc2 <- sum(diag(tab2)) / sum(tab2)


print(tab2)
cat("Accuracy of fieature set 2: ", round(acc2, 4), "\n", sep = "")


eval_subset <- function(feats, k.range = 1:100) {
  acc  <- numeric(length(k.range))
  for (i in seq_along(k.range)) {
    pred <- knn(abalone.train[, feats], abalone.test[, feats],
                abalone.train$age.group, k = k.range[i])
    tab  <- table(pred, abalone.test$age.group)
    acc[i]  <- sum(diag(tab)) / sum(tab)
  }
  list(best_k_acc  = k.range[which.max(acc)],
       best_acc    = max(acc))
}

k.range <- 1:100
acc1  <- numeric(length(k.range))
acc2  <- numeric(length(k.range))
for (i in seq_along(k.range)) {
  pred1 <- knn(abalone.train[, ft1], abalone.test[, ft1],
               abalone.train$age.group, k = k.range[i])
  pred2 <- knn(abalone.train[, ft2], abalone.test[, ft2],
               abalone.train$age.group, k = k.range[i])
  tab1  <- table(pred1, abalone.test$age.group)
  tab2  <- table(pred2, abalone.test$age.group)
  acc1[i]  <- sum(diag(tab1)) / sum(tab1)
  acc2[i]  <- sum(diag(tab2)) / sum(tab2)
}
res1 = list(best_k_acc  = k.range[which.max(acc1)],
     best_acc    = max(acc1))
res2 = list(best_k_acc  = k.range[which.max(acc2)],
            best_acc    = max(acc2))


cat("  Best accuracy of feature set 1:", round(res1$best_acc,4), " at k=", res1$best_k_acc,  "\n", sep="")


cat("  Best accuracy of feature set 2:", round(res2$best_acc,4), " at k=", res2$best_k_acc,  "\n", sep="")


if (res1$best_acc >= res2$best_acc) {
  best_subset <- "1"
  best_feats  <- ft1
  best_k      <- res1$best_k_acc
} else {
  best_subset <- "2"
  best_feats  <- ft2
  best_k      <- res2$best_k_acc
}
cat("Best subset:", best_subset, 
    "with k =", best_k)

