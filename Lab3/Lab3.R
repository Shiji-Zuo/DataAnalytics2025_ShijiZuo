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


abalone.data <- na.omit(abalone.data)


ft1 <- c("length", "diameter", "height")
ft2 <- c("whole_weight", "shucked_wieght", "viscera_wieght", "shell_weight")

train <- sample(nrow(abalone.data), 3600)

abalone.train <- abalone.data[train, ]
abalone.test  <- abalone.data[-train, ]

k_pre <- 60
 
knn1 <- knn(abalone.train[, ft1], abalone.test[,  ft1], abalone.train$age.group, k = k_pre )
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


cat("  Best accuracy of feature set 1:", max(acc1), " at k=", k.range[which.max(acc1)],  "\n")


cat("  Best accuracy of feature set 2:", max(acc2), " at k=", k.range[which.max(acc2)],  "\n")


if (max(acc1) >= max(acc2)){
  cat("Best subset: feature set 1 with k =", k.range[which.max(acc1)])
} else {
  cat("Best subset: feature set 2 with k =", k.range[which.max(acc2)])
}


# ---- Packages ----
suppressPackageStartupMessages({
  library(ggplot2)
  library(factoextra)
  library(cluster)
})


DATA_PATH <- "/Users/Shiji/Library/CloudStorage/OneDrive-shanghaitech.edu.cn/rpi_first_term/data anlysis/Lab 3/abalone_dataset.csv"


FEATURE_ALL <- c("length", "diameter", "height", "whole_weight",
                 "shucked_wieght", "viscera_wieght", "shell_weight")

FEATURES_SEL <- c("whole_weight", "shucked_wieght", "viscera_wieght", "shell_weight")

# ---- Functions ----

load_abalone <- function(path) {
  read.csv(path, stringsAsFactors = FALSE)
}

scale_columns <- function(df, cols) {
  if (length(cols) == 0) {
    stop("No columns provided to scale_columns().")
  } else {
    df[, cols] <- scale(df[, cols])
    df
  }
}

prep_matrix <- function(df, cols) {
  X <- df[, cols, drop = FALSE]
  scale(X)
}

pairwise_dist <- function(X) {
  dist(X)
}

silhouette_scores_kmeans_while <- function(X, D, k_values, seed_base = 1L) {
  nK <- length(k_values)
  scores <- numeric(nK)
  i <- 1L
  while (i <= nK) {
    k <- k_values[i]
    set.seed(seed_base + k)
    km_fit <- kmeans(X, centers = k)
    sil    <- silhouette(km_fit$cluster, D)
    scores[i] <- mean(sil[, 3])
    i <- i + 1L
  }
  scores
}

silhouette_scores_pam_while <- function(X, D, k_values) {
  nK <- length(k_values)
  scores <- numeric(nK)
  j <- 1L
  while (j <= nK) {
    k <- k_values[j]
    pam_fit <- pam(X, k)
    sil     <- silhouette(pam_fit$cluster, D)
    scores[j] <- mean(sil[, 3])
    j <- j + 1L
  }
  scores
}

best_k_from_scores <- function(k_values, scores) {
  k_values[which.max(scores)]
}

plot_sil_curve <- function(k_values, scores, title_txt) {
  plot(k_values, scores,
       xlab = "Number of clusters",
       ylab = "Average silhouette",
       main = title_txt, type = "b", pch = 19)
}

visualize_best_sil <- function(cluster_labels, D, title_txt = NULL) {
  sil_obj <- silhouette(cluster_labels, D)
  p <- fviz_silhouette(sil_obj)
  if (!is.null(title_txt)) p <- p + ggtitle(title_txt)
  print(p)
  invisible(sil_obj)
}
 

check_columns_exist_for <- function(df, cols) {
  missing_cols <- character(0)
  for (nm in cols) {
    if (!nm %in% names(df)) missing_cols <- c(missing_cols, nm)
  }
  if (length(missing_cols)) {
    stop(sprintf("Missing columns: %s", paste(missing_cols, collapse = ", ")))
  }
  TRUE
}


run_clustering_pipeline <- function(
    data_path        = DATA_PATH,
    all_cols         = FEATURE_ALL,
    selected_cols    = FEATURES_SEL,
    k_candidates     = 2:10
) {
  # Load and prep
  abalone_tbl <- load_abalone(data_path)
  check_columns_exist_for(abalone_tbl, all_cols)
  abalone_tbl <- scale_columns(abalone_tbl, all_cols)
  
  X <- prep_matrix(abalone_tbl, selected_cols)
  D <- pairwise_dist(X)
  
  # K-means sweep (for→while)
  km_scores <- silhouette_scores_kmeans_while(X, D, k_candidates, seed_base = 11L)
  k_km_best <- best_k_from_scores(k_candidates, km_scores)
  cat("Optimal number of clusters (K-Means, silhouette):", k_km_best, "\n")
  
  set.seed(k_km_best + 99L)
  km_best <- kmeans(X, centers = k_km_best)
  visualize_best_sil(km_best$cluster, D, "Silhouette - Best K for K-Means")
  plot_sil_curve(k_candidates, km_scores, "Silhouette vs K for K-Means")
  
  # PAM sweep (for→while)
  pam_scores <- silhouette_scores_pam_while(X, D, k_candidates)
  k_pam_best <- best_k_from_scores(k_candidates, pam_scores)
  cat("Optimal number of clusters (PAM, silhouette):", k_pam_best, "\n")
  
  pam_best <- pam(X, k_pam_best)
  visualize_best_sil(pam_best$cluster, D, "Silhouette - Best K for PAM")
  plot_sil_curve(k_candidates, pam_scores, "Silhouette vs K for PAM")
  
  
  list(
    data_scaled   = X,
    dist_matrix   = D,
    k_candidates  = k_candidates,
    kmeans = list(
      scores = km_scores,
      k_best = k_km_best,
      model  = km_best
    ),
    pam = list(
      scores = pam_scores,
      k_best = k_pam_best,
      model  = pam_best
    )
  )
}


results <- run_clustering_pipeline()

