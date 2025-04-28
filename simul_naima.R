
## 1. Paramètres généraux
n <- 1000          # Nombre d'observations
n_sim <- 500       # Nombre de simulations

## Variables pour stocker les résultats
somme_moy_X1 <- 0
somme_moy_B1 <- 0
somme_moy_X2 <- 0

moy_mcar <- vector("list", n_sim)
moy_mar <- vector("list", n_sim)
moy_mnar <- vector("list", n_sim)

# Moyennes théoriques
moy_theo_X1 <- 1
moy_theo_B1 <- 1
moy_theo_X2 <- 1

## 5. Boucle de simulation
for (i in 1:n_sim) {
  
  set.seed(i)
  
  # Génération des données
  B1 <- rbinom(n, 1, 0.5)
  B2 <- rbinom(n, 1, 0.5)
  X1 <- rnorm(n, mean = 1, sd = 0.5)
  X2 <- rnorm(n, mean = 1, sd = 0.5)
  
  data_complet <- data.frame(X1 = X1, X2 = X2, B1 = B1, B2 = B2)
  
  # --- MCAR (Manquant complètement aléatoire) ---
  data_MCAR <- data_complet
  missing_index <- sample(1:n, size = 0.1 * n)
  data_MCAR$X1[missing_index] <- NA
  moyenne_X1 <- mean(data_MCAR$X1 , na.rm = TRUE)
  somme_moy_X1 <- somme_moy_X1 + moyenne_X1
  
  # --- MAR (Manquant à cause de variables observées) ---
  data_MAR <- data_complet
  missing_index1 <- ifelse(data_MAR$B2 == 0 & data_MAR$X1 < 0 , 1 , 0)
  data_MAR$B1 <- ifelse(missing_index1 == 0 , data_MAR$B1 , NA)
  moyenne_B1 <- mean(data_MAR$B1 , na.rm = TRUE)
  somme_moy_B1 <- somme_moy_B1 + moyenne_B1
  
  # --- MNAR (Manquant à cause de variables non observées) ---
  data_MNAR <- data_complet
  missing_index2 <- ifelse(data_MNAR$X1 < 0.5 & data_MNAR$X2 < 1.1 & data_MNAR$B1 == 1, 1 , 0)
  data_MNAR$X2 <- ifelse(missing_index2 == 0 , data_MNAR$X2 , NA)
  moyenne_X2 <- mean(data_MNAR$X2 , na.rm = TRUE)
  somme_moy_X2 <- somme_moy_X2 + moyenne_X2
}

# Moyennes estimées sur toutes les simulations
moyenne_des_X1 <- somme_moy_X1 / n_sim
moyenne_des_B1 <- somme_moy_B1 / n_sim
moyenne_des_X2 <- somme_moy_X2 / n_sim

print(moyenne_des_X1)
print(moyenne_des_B1)
print(moyenne_des_X2)

# Biais pour chaque type de données manquantes
Biais_mcar <- moyenne_des_X1 - moy_theo_X1 
Biais_mar <- moyenne_des_B1 - moy_theo_B1 
Biais_mnar <- moyenne_des_X2 - moy_theo_X2

print(Biais_mcar)
print(Biais_mar)
print(Biais_mnar)


## 6. Imputation par la moyenne

# --- Imputation par la moyenne pour MCAR ---
data_MCAR_impute <- data_MCAR
data_MCAR_impute$X1[is.na(data_MCAR_impute$X1)] <- mean(data_MCAR_impute$X1, na.rm = TRUE)
moyenne_impute_mcar <- mean(data_MCAR_impute$X1, na.rm = TRUE)

# --- Imputation par la moyenne pour MAR ---
data_MAR_impute <- data_MAR
data_MAR_impute$B1[is.na(data_MAR_impute$B1)] <- mean(data_MAR_impute$B1, na.rm = TRUE)
moyenne_impute_mar <- mean(data_MAR_impute$B1, na.rm = TRUE)

# --- Imputation par la moyenne pour MNAR ---
data_MNAR_impute <- data_MNAR
data_MNAR_impute$X2[is.na(data_MNAR_impute$X2)] <- mean(data_MNAR_impute$X2, na.rm = TRUE)
moyenne_impute_mnar <- mean(data_MNAR_impute$X2, na.rm = TRUE)


# Biais pour l'imputation par la moyenne
Biais_impute_mcar <- moyenne_impute_mcar - moy_theo_X1
Biais_impute_mar <- moyenne_impute_mar - moy_theo_B1
Biais_impute_mnar <- moyenne_impute_mnar - moy_theo_X2

print(Biais_impute_mcar)
print(Biais_impute_mar)
print(Biais_impute_mnar)


## 7. Imputation par la médiane

# --- Imputation par la médiane pour MCAR ---
data_MCAR_impute_median <- data_MCAR
data_MCAR_impute_median$X1[is.na(data_MCAR_impute_median$X1)] <- median(data_MCAR_impute_median$X1, na.rm = TRUE)
moyenne_impute_median_mcar <- mean(data_MCAR_impute_median$X1, na.rm = TRUE)

# --- Imputation par la médiane pour MAR ---
data_MAR_impute_median <- data_MAR
data_MAR_impute_median$B1[is.na(data_MAR_impute_median$B1)] <- median(data_MAR_impute_median$B1, na.rm = TRUE)
moyenne_impute_median_mar <- mean(data_MAR_impute_median$B1, na.rm = TRUE)

# --- Imputation par la médiane pour MNAR ---
data_MNAR_impute_median <- data_MNAR
data_MNAR_impute_median$X2[is.na(data_MNAR_impute_median$X2)] <- median(data_MNAR_impute_median$X2, na.rm = TRUE)
moyenne_impute_median_mnar <- mean(data_MNAR_impute_median$X2, na.rm = TRUE)

# Biais pour l'imputation par la médiane
Biais_impute_median_mcar <- moyenne_impute_median_mcar - moy_theo_X1
Biais_impute_median_mar <- moyenne_impute_median_mar - moy_theo_B1
Biais_impute_median_mnar <- moyenne_impute_median_mnar - moy_theo_X2

print(Biais_impute_median_mcar)
print(Biais_impute_median_mar)
print(Biais_impute_median_mnar)


## 8. Méthode des cas complets

# --- Cas complets pour MCAR ---
data_complet_mcar <- na.omit(data_MCAR)
moyenne_complet_mcar <- mean(data_complet_mcar$X1)

# --- Cas complets pour MAR ---
data_complet_mar <- na.omit(data_MAR)
moyenne_complet_mar <- mean(data_complet_mar$B1)

# --- Cas complets pour MNAR ---
data_complet_mnar <- na.omit(data_MNAR)
moyenne_complet_mnar <- mean(data_complet_mnar$X2)

# Affichage des résultats pour la méthode des cas complets
print(moyenne_complet_mcar)
print(moyenne_complet_mar)
print(moyenne_complet_mnar)

# Biais pour la méthode des cas complets
Biais_complet_mcar <- moyenne_complet_mcar - moy_theo_X1
Biais_complet_mar <- moyenne_complet_mar - moy_theo_B1
Biais_complet_mnar <- moyenne_complet_mnar - moy_theo_X2

print(Biais_complet_mcar)
print(Biais_complet_mar)
print(Biais_complet_mnar)
