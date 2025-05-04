## 1. Paramètres généraux
n <- 200         # Nombre d'observations
n_sim <- 500       # Nombre de simulations

## Variables pour stocker les résultats
somme_moy_X1 <- 0
somme_moy_B1 <- 0
somme_moy_X2 <- 0

somme_moy_impute_mcar <- 0
somme_moy_impute_mar <- 0
somme_moy_impute_mnar <- 0

somme_moy_impute_median_mcar <- 0
somme_moy_impute_median_mar <- 0
somme_moy_impute_median_mnar <- 0

somme_moy_complet_mcar <- 0
somme_moy_complet_mar <- 0
somme_moy_complet_mnar <- 0

# Moyennes théoriques
moy_theo_X1 <- 1
moy_theo_B1 <- 0.5
moy_theo_X2 <- 1

# Stockage pour boxplots
res_MCAR <- matrix(NA, nrow = n_sim, ncol = 4)
res_MAR  <- matrix(NA, nrow = n_sim, ncol = 4)
res_MNAR <- matrix(NA, nrow = n_sim, ncol = 4)

## 5. Boucle de simulation
for (i in 1:n_sim) {
  
  set.seed(i)
  
  # Génération des données
  B1 <- rbinom(n, 1, 0.5)
  B2 <- rbinom(n, 1, 0.7)
  X1 <- rnorm(n, mean = 1, sd = 0.7)
  X2 <- rnorm(n, mean = 1, sd = 0.5)
  
  data_complet <- data.frame(X1 = X1, X2 = X2, B1 = B1, B2 = B2)
  
  # --- MCAR ---
  data_MCAR <- data_complet
  missing_index <- sample(1:n, size = 0.1 * n)
  data_MCAR$X1[missing_index] <- NA
  moyenne_X1 <- mean(data_MCAR$X1, na.rm = TRUE)
  somme_moy_X1 <- somme_moy_X1 + moyenne_X1
  
  # Imputation moyenne
  data_MCAR_impute <- data_MCAR
  data_MCAR_impute$X1[is.na(data_MCAR_impute$X1)] <- mean(data_MCAR$X1, na.rm = TRUE)
  somme_moy_impute_mcar <- somme_moy_impute_mcar + mean(data_MCAR_impute$X1)
  
  # Imputation médiane
  data_MCAR_impute_median <- data_MCAR
  data_MCAR_impute_median$X1[is.na(data_MCAR_impute_median$X1)] <- median(data_MCAR$X1, na.rm = TRUE)
  somme_moy_impute_median_mcar <- somme_moy_impute_median_mcar + mean(data_MCAR_impute_median$X1)
  
  # Cas complets
  data_complet_mcar <- na.omit(data_MCAR)
  somme_moy_complet_mcar <- somme_moy_complet_mcar + mean(data_complet_mcar$X1)
  
  res_MCAR[i, ] <- c(moyenne_X1,
                     mean(data_MCAR_impute$X1),
                     mean(data_MCAR_impute_median$X1),
                     mean(data_complet_mcar$X1))
  
  # --- MAR ---
  data_MAR <- data_complet
  missing_index1 <- ifelse(data_MAR$B2 == 0 & data_MAR$X1 < 0, 1, 0)
  data_MAR$B1 <- ifelse(missing_index1 == 0, data_MAR$B1, NA)
  moyenne_B1 <- mean(data_MAR$B1, na.rm = TRUE)
  somme_moy_B1 <- somme_moy_B1 + moyenne_B1
  
  # Imputation moyenne
  data_MAR_impute <- data_MAR
  data_MAR_impute$B1[is.na(data_MAR_impute$B1)] <- mean(data_MAR$B1, na.rm = TRUE)
  somme_moy_impute_mar <- somme_moy_impute_mar + mean(data_MAR_impute$B1)
  
  # Imputation médiane
  data_MAR_impute_median <- data_MAR
  data_MAR_impute_median$B1[is.na(data_MAR_impute_median$B1)] <- median(data_MAR$B1, na.rm = TRUE)
  somme_moy_impute_median_mar <- somme_moy_impute_median_mar + mean(data_MAR_impute_median$B1)
  
  # Cas complets
  data_complet_mar <- na.omit(data_MAR)
  somme_moy_complet_mar <- somme_moy_complet_mar + mean(data_complet_mar$B1)
  
  res_MAR[i, ] <- c(moyenne_B1,
                    mean(data_MAR_impute$B1),
                    mean(data_MAR_impute_median$B1),
                    mean(data_complet_mar$B1))
  
  # --- MNAR ---
  data_MNAR <- data_complet
  missing_index2 <- ifelse(data_MNAR$X1 < 0.5 & data_MNAR$X2 < 1.1 & data_MNAR$B1 == 1, 1, 0)
  data_MNAR$X2 <- ifelse(missing_index2 == 0, data_MNAR$X2, NA)
  moyenne_X2 <- mean(data_MNAR$X2, na.rm = TRUE)
  somme_moy_X2 <- somme_moy_X2 + moyenne_X2
  
  # Imputation moyenne
  data_MNAR_impute <- data_MNAR
  data_MNAR_impute$X2[is.na(data_MNAR_impute$X2)] <- mean(data_MNAR$X2, na.rm = TRUE)
  somme_moy_impute_mnar <- somme_moy_impute_mnar + mean(data_MNAR_impute$X2)
  
  # Imputation médiane
  data_MNAR_impute_median <- data_MNAR
  data_MNAR_impute_median$X2[is.na(data_MNAR_impute_median$X2)] <- median(data_MNAR$X2, na.rm = TRUE)
  somme_moy_impute_median_mnar <- somme_moy_impute_median_mnar + mean(data_MNAR_impute_median$X2)
  
  # Cas complets
  data_complet_mnar <- na.omit(data_MNAR)
  somme_moy_complet_mnar <- somme_moy_complet_mnar + mean(data_complet_mnar$X2)
  
  res_MNAR[i, ] <- c(moyenne_X2,
                     mean(data_MNAR_impute$X2),
                     mean(data_MNAR_impute_median$X2),
                     mean(data_complet_mnar$X2))
}

## 6. Calcul des moyennes sur les 500 simulations
moyenne_des_X1 <- somme_moy_X1 / n_sim
moyenne_des_B1 <- somme_moy_B1 / n_sim
moyenne_des_X2 <- somme_moy_X2 / n_sim

moyenne_impute_mcar <- somme_moy_impute_mcar / n_sim
moyenne_impute_mar <- somme_moy_impute_mar / n_sim
moyenne_impute_mnar <- somme_moy_impute_mnar / n_sim

moyenne_impute_median_mcar <- somme_moy_impute_median_mcar / n_sim
moyenne_impute_median_mar <- somme_moy_impute_median_mar / n_sim
moyenne_impute_median_mnar <- somme_moy_impute_median_mnar / n_sim

moyenne_complet_mcar <- somme_moy_complet_mcar / n_sim
moyenne_complet_mar <- somme_moy_complet_mar / n_sim
moyenne_complet_mnar <- somme_moy_complet_mnar / n_sim

## 7. Calcul des biais
Biais_mcar <- moyenne_des_X1 - moy_theo_X1
Biais_mar <- moyenne_des_B1 - moy_theo_B1
Biais_mnar <- moyenne_des_X2 - moy_theo_X2

Biais_impute_mcar <- moyenne_impute_mcar - moy_theo_X1
Biais_impute_mar <- moyenne_impute_mar - moy_theo_B1
Biais_impute_mnar <- moyenne_impute_mnar - moy_theo_X2

Biais_impute_median_mcar <- moyenne_impute_median_mcar - moy_theo_X1
Biais_impute_median_mar <- moyenne_impute_median_mar - moy_theo_B1
Biais_impute_median_mnar <- moyenne_impute_median_mnar - moy_theo_X2

Biais_complet_mcar <- moyenne_complet_mcar - moy_theo_X1
Biais_complet_mar <- moyenne_complet_mar - moy_theo_B1
Biais_complet_mnar <- moyenne_complet_mnar - moy_theo_X2

# Calcul des écarts-types pour chaque méthode
ecart_type_mcar <- sqrt(var(res_MCAR[, 1]))
ecart_type_mar <- sqrt(var(res_MAR[, 1]))
ecart_type_mnar <- sqrt(var(res_MNAR[, 1]))


# Calcul des erreurs quadratiques moyennes (RMSE) pour chaque méthode
erreur_quadratique_mcar <- sqrt(mean((res_MCAR[, 1] - moy_theo_X1)^2))
erreur_quadratique_mar <- sqrt(mean((res_MAR[, 1] - moy_theo_B1)^2))
erreur_quadratique_mnar <- sqrt(mean((res_MNAR[, 1] - moy_theo_X2)^2))



## 8. Résultats
print(Biais_mcar)
print(Biais_mar)
print(Biais_mnar)

print(Biais_impute_mcar)
print(Biais_impute_mar)
print(Biais_impute_mnar)

print(Biais_impute_median_mcar)
print(Biais_impute_median_mar)
print(Biais_impute_median_mnar)

print(Biais_complet_mcar)
print(Biais_complet_mar)
print(Biais_complet_mnar)


print(paste("EQM MCAR (X1) : ", erreur_quadratique_mcar))
print(paste("EQM MAR (B1) : ", erreur_quadratique_mar))
print(paste("EQM MNAR (X2) : ", erreur_quadratique_mnar))

print(paste("Ecart-type MCAR : ", ecart_type_mcar))
print(paste("Ecart-type MAR : ", ecart_type_mar))
print(paste("Ecart-type MNAR : ", ecart_type_mnar))


## 9. Boxplots des moyennes simulées
par(mfrow = c(1, 3))
# Boxplot pour MCAR avec ligne de la moyenne théorique
boxplot(res_MCAR,
        main = "MCAR",
        names = c("Sans Imput.", "Moyenne", "Médiane", "Cas complets"),
        ylab = "Moyennes simulées")
abline(h = moy_theo_X1, col = "red")  # Ajouter la ligne théorique

# Boxplot pour MAR avec ligne de la moyenne théorique
boxplot(res_MAR,
        main = "MAR",
        names = c("Sans Imput.", "Moyenne", "Médiane", "Cas complets"),
        ylab = "Moyennes simulées")
abline(h = moy_theo_B1, col = "red")  # Ajouter la ligne théorique

# Boxplot pour MNAR avec ligne de la moyenne théorique
boxplot(res_MNAR,
        main = "MNAR",
        names = c("Sans Imput.", "Moyenne", "Médiane", "Cas complets"),
        ylab = "Moyennes simulées")
abline(h = moy_theo_X2, col = "red")  # Ajouter la ligne théorique


