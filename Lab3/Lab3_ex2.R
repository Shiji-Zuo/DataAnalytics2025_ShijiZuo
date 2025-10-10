library(ggplot2)
library(factoextra)
library(cluster)


abalone_df <- read.csv("/Users/Shiji/Library/CloudStorage/OneDrive-shanghaitech.edu.cn/rpi_first_term/data anlysis/Lab 3/abalone_dataset.csv")


all_features <- c("length", "diameter", "height", "whole_weight", 
                  "shucked_wieght", "viscera_wieght", "shell_weight")

abalone_df[, all_features] <- scale(abalone_df[, all_features])


selected_features <- c("whole_weight","shucked_wieght","viscera_wieght","shell_weight")

scaled_data <- abalone_df[, selected_features]
scaled_data <- scale(scaled_data)

dist_mat <- dist(scaled_data)

## ---------------- K-means ----------------
k_range <- 2:10              
sil_scores_km  <- numeric(length(k_range))           

for (i in seq_along(k_range)) { 
  k <- k_range[i]
  set.seed(k) 
  km_fit <- kmeans(scaled_data, centers = k)              
  sil <- silhouette(km_fit$cluster, dist_mat)
  sil_scores_km[i] <- mean(sil[, 3])
}

best_k_km <- k_range[which.max(sil_scores_km)]
cat("Optimal number of clusters (K-Means, silhouette method):", best_k_km, "\n")

set.seed(best_k_km)  
km_best <- kmeans(scaled_data, centers = best_k_km)
sil_km  <- silhouette(km_best$cluster, dist_mat)
fviz_silhouette(sil_km)

plot(k_range, sil_scores_km,
     xlab = "Number of clusters", ylab = "Average silhouette",
     main = " Silhouette vs K for K-Means")

## ---------------- PAM ----------------
sil_scores_pam <- numeric(length(k_range)) 

for (i in seq_along(k_range)) {
  k <- k_range[i]
  pam_fit <- pam(scaled_data, k)
  sil <- silhouette(pam_fit$cluster, dist_mat)
  sil_scores_pam[i] <- mean(sil[, 3]) 
}

best_k_pam <- k_range[which.max(sil_scores_pam)]
cat("Optimal number of clusters (PAM, silhouette method):", best_k_pam, "\n")

pam_best <- pam(scaled_data, best_k_pam)
sil_pam  <- silhouette(pam_best$cluster, dist_mat)
fviz_silhouette(sil_pam)

plot(k_range, sil_scores_pam, 
     xlab = "Number of clusters", ylab = "Average silhouette",
     main = "Silhouette vs K for PAM")

