## 1. Paramètres généraux
n <- 1000          # Nombre d'observations
n_sim <- 500       # Nombre de simulations



## 5. Boucle de simulation
for (i in 1:n_sim) {
  somme_moy_X1 <- 0
  somme_moy_B1 <- 0
  somme_moy_X2 <- 0
  
  
  moy_mcar <- vector("list", n_sim)
  moy_mar <- vector("list", n_sim)
  moy_mnar <- vector("list", n_sim)
  set.seed(i)
  
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
  #moyenne_X1
  #print(paste("Moyenne de X1 pour la simulation", i, ":", moyenne_X1))
  somme_moy_X1 <- somme_moy_X1 + moyenne_X1
  help("mean")
  
  
  data_MAR <- data_complet
  missing_index1 <- ifelse(data_MAR$B2 == 0 & data_MAR$X1 < 0 , 1 ,0)
  table(missing_index1)
  data_MAR$B1 <- ifelse(missing_index1 == 0 , data_MAR$B1 , NA)
  table(data_MAR$B1 ,exclude = FALSE)
  #View(data_MAR[is.na(data_MAR$B1) == 1,])
  moyenne_B1 <- mean(data_MAR$B1 , na.rm = TRUE)
  somme_moy_B1 <- somme_moy_B1 + moyenne_B1
  
  
  

  # --- MNAR (Manquant dépendant de X2 elle-même) ---
  data_MNAR <- data_complet
  missing_index2 <- ifelse(data_MNAR$X1 < 0.5 & data_MNAR$X2 < 1.1 & data_MNAR$B1 == 1, 1 ,0)
  table(missing_index2)
  data_MNAR$X2 <- ifelse(missing_index2 == 0 ,data_MNAR$X2 ,NA)
  table(data_MNAR$X2 ,exclude = FALSE)
  #View(data_MNAR[is.na(data_MNAR$X2) == 1,])
  moyenne_X2 <- mean(data_MNAR$X2 , na.rm = TRUE)
  somme_moy_X2 <- somme_moy_X2 + moyenne_X2
}


moyenne_des_X1 <- somme_moy_X1 / n_sim
moyenne_des_B1 <- somme_moy_B1 / n_sim
moyenne_des_X2 <- somme_moy_X2 / n_sim

print(moyenne_des_X1)
print(moyenne_des_B1)
print(moyenne_des_X2)


moy_theo_X1 <- 1
moy_theo_B1 <- 1
moy_theo_X2 <- 1

Biais_mcar <- moyenne_des_X1 - moy_theo_X1 
Biais_mar <- moyenne_des_B1 - moy_theo_B1 
Biais_mnar <- moyenne_des_X2 - moy_theo_X2

print(Biais_mcar)
print(Biais_mar)
print(Biais_mnar)


## Imputation par la moyenne 

























### Calculer la moyenne des moyennes de chaque simulation stockées dans une liste et calculer 
## la diff entre la moynne theo et ces moyennes 
## Methode des cas complets , imputation par le moyenne et mediane et ensuite methode de ponderation 
## des cas complets 
## trouver  la prOPORTION des donnees manquante(2% 10% 50%)





