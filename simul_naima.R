## 1. Paramètres généraux
n <- 1000          # Nombre d'observations
n_sim <- 500       # Nombre de simulations

## 2. Déclaration des variables pour cumuler les résultats
somme_moy_X1 <- 0
somme_moy_B1 <- 0
somme_moy_X2 <- 0

## 3. Déclaration des variables pour stocker les résultats de chaque méthode
somme_moy_X1_impute_moy <- 0
somme_moy_X1_impute_med <- 0
somme_moy_X1_complet <- 0

## 4. Boucle de simulation
for (i in 1:n_sim) {
  # Initialisation des vecteurs de simulation
  set.seed(i)
  B1 <- rbinom(n, 1, 0.5)  # Variable binaire B1
  B2 <- rbinom(n, 1, 0.5)  # Variable binaire B2
  X1 <- rnorm(n, mean = 1, sd = 0.5)  # Variable continue X1
  X2 <- rnorm(n, mean = 1, sd = 0.5)  # Variable continue X2
  
  # Données complètes
  data_complet <- data.frame(X1 = X1, X2 = X2, B1 = B1, B2 = B2)
  
  
  View(data_complet)
  # --- MCAR (Manquant Complètement Aléatoire) ---
  data_MCAR <- data_complet
  missing_index <- sample(1:n, size = 0.1 * n)  # Indices pour les valeurs manquantes
  data_MCAR$X1[missing_index] <- NA  # Introduire des valeurs manquantes pour X1
  
  # Calcul de la moyenne de X1 pour les données MCAR
  moyenne_X1 <- mean(data_MCAR$X1 , na.rm = TRUE)
  somme_moy_X1 <- somme_moy_X1 + moyenne_X1
  
  
  # --- MAR (Manquant À Randomité Conditionnelle) ---
  data_MAR <- data_complet
  missing_index1 <- ifelse(data_MAR$B2 == 0 & data_MAR$X1 < 0 , 1 , 0)
  data_MAR$B1 <- ifelse(missing_index1 == 0 , data_MAR$B1 , NA)
  
  table(missing_index1)
  table(data_MAR$B1 ,exclude = FALSE)
  # Calcul de la moyenne de B1 pour les données MAR
  moyenne_B1 <- mean(data_MAR$B1 , na.rm = TRUE)
  somme_moy_B1 <- somme_moy_B1 + moyenne_B1
  
  
  # --- MNAR (Manquant dépendant de X2 elle-meme) ---
  data_MNAR <- data_complet
  missing_index2 <- ifelse(data_MNAR$X1 < 0.5 & data_MNAR$X2 < 1.1 & data_MNAR$B1 == 1, 1 , 0)
  
  table(missing_index2)
  data_MNAR$X2 <- ifelse(missing_index2 == 0 , data_MNAR$X2 , NA)
  table(data_MNAR$X2 ,exclude = FALSE)
  # Calcul de la moyenne de X2 pour les données MNAR
  moyenne_X2 <- mean(data_MNAR$X2 , na.rm = TRUE)
  somme_moy_X2 <- somme_moy_X2 + moyenne_X2
}

# Calcul des moyennes finales après les simulations
moyenne_des_X1 <- somme_moy_X1 / n_sim
moyenne_des_B1 <- somme_moy_B1 / n_sim
moyenne_des_X2 <- somme_moy_X2 / n_sim

# Affichage des moyennes obtenues pour chaque type de données manquantes
print(moyenne_des_X1)
print(moyenne_des_B1)
print(moyenne_des_X2)

# Moyennes théoriques (en supposant que les données sont issues de lois normales avec moyenne 1)
moy_theo_X1 <- 1
moy_theo_B1 <- 1
moy_theo_X2 <- 1

# Calcul des biais pour chaque type de données manquantes
Biais_mcar <- moyenne_des_X1 - moy_theo_X1
Biais_mar <- moyenne_des_B1 - moy_theo_B1
Biais_mnar <- moyenne_des_X2 - moy_theo_X2

# Affichage des biais
print(Biais_mcar)
print(Biais_mar)
print(Biais_mnar)

## 5. Imputation par la moyenne, médiane et cas complets
# Fonction d'imputation par la moyenne
imputer_moyenne <- function(data, variable) {
  data_impute <- data
  data_impute[[variable]][is.na(data_impute[[variable]])] <- mean(data_impute[[variable]], na.rm = TRUE)
  return(data_impute)
}

# Fonction d'imputation par la médiane
imputer_median <- function(data, variable) {
  data_impute <- data
  data_impute[[variable]][is.na(data_impute[[variable]])] <- median(data_impute[[variable]], na.rm = TRUE)
  return(data_impute)
}

# Fonction des cas complets
cas_complet <- function(data) {
  return(data[complete.cases(data), ])
}

# Boucle pour calculer l'impact de l'imputation sur les biais
for (i in 1:n_sim) {
  # --- Imputation par la moyenne ---
  data_MCAR_impute_moy <- imputer_moyenne(data_MCAR, "X1")
  moyenne_MCAR_impute_moy <- mean(data_MCAR_impute_moy$X1, na.rm = TRUE)
  
  # --- Imputation par la médiane ---
  data_MCAR_impute_med <- imputer_median(data_MCAR, "X1")
  moyenne_MCAR_impute_med <- mean(data_MCAR_impute_med$X1, na.rm = TRUE)
  
  # --- Cas complets ---
  data_MCAR_complet <- cas_complet(data_MCAR)
  moyenne_MCAR_complet <- mean(data_MCAR_complet$X1, na.rm = TRUE)
  
  # Ajout des moyennes pour calculer la moyenne des simulations
  somme_moy_X1_impute_moy <- somme_moy_X1_impute_moy + moyenne_MCAR_impute_moy
  somme_moy_X1_impute_med <- somme_moy_X1_impute_med + moyenne_MCAR_impute_med
  somme_moy_X1_complet <- somme_moy_X1_complet + moyenne_MCAR_complet
}

# Calcul des moyennes finales après imputation
moyenne_des_X1_impute_moy <- somme_moy_X1_impute_moy / n_sim
moyenne_des_X1_impute_med <- somme_moy_X1_impute_med / n_sim
moyenne_des_X1_complet <- somme_moy_X1_complet / n_sim

# Calcul des biais pour chaque méthode d'imputation
Biais_impute_moy <- moyenne_des_X1_impute_moy - moy_theo_X1
Biais_impute_med <- moyenne_des_X1_impute_med - moy_theo_X1
Biais_complet <- moyenne_des_X1_complet - moy_theo_X1

# Affichage des biais
print(Biais_impute_moy)
print(Biais_impute_med)
print(Biais_complet)
