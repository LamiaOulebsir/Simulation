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

var_complet_X1_mcar <- numeric(500)
var_moy_X1_mcar <- numeric(500)
var_med_X1_mcar <- numeric(500)

var_complet_X1_mar <- numeric(500)
var_moy_X1_mar <- numeric(500)
var_med_X1_mar <- numeric(500)

var_moy_X1_mnar <- numeric(500)
var_med_X1_mnar <- numeric(500)
var_complet_X1_mnar <- numeric(500)

med_complet_X1_mnar <- numeric(500)
med_moy_X1_mnar <- numeric(500)
med_med_X1_mnar <- numeric(500)

med_complet_X1_mcar <- numeric(500)
med_moy_X1_mcar <- numeric(500)
med_med_X1_mcar <- numeric(500)

med_complet_X1_mar <- numeric(500)
med_moy_X1_mar <- numeric(500)
med_med_X1_mar <- numeric(500)



# Moyennes théoriques
moy_theo_X1 <- 1
moy_theo_B1 <- 0.5
moy_theo_X2 <- 1
# mediane théorique 
med_theo_X1 <- 1

## Variances théoriques 

var_theo_X1 <- 0.7^2


# Stockage pour boxplots
res_MCAR <- matrix(NA, nrow = n_sim, ncol = 3)
res_MAR  <- matrix(NA, nrow = n_sim, ncol = 3)
res_MNAR <- matrix(NA, nrow = n_sim, ncol = 3)

# Stockage des variances par simulation
var_mcar <- matrix(NA, nrow = n_sim, ncol = 3)  # Moyenne, Médiane, Cas complets
var_mar <- matrix(NA, nrow = n_sim, ncol = 3)
var_mnar <- matrix(NA, nrow = n_sim, ncol = 3)

med_mcar <- matrix(NA, nrow = n_sim, ncol = 3)  # Moyenne, Médiane, Cas complets
med_mar <- matrix(NA, nrow = n_sim, ncol = 3)
med_mnar <- matrix(NA, nrow = n_sim, ncol = 3)

## 5. Boucle de simulation
for (i in 1:n_sim) {
  
  set.seed(i)
  
  # Génération des données
  B1 <- rbinom(n, 1, 0.5)
  B2 <- rbinom(n, 1, 0.7)
  X1 <- rnorm(n, mean = 1, sd = 0.7)
  X2 <- rnorm(n, mean = 1, sd = 0.5)
  
  data_complet <- data.frame(X1 = X1, X2 = X2, B2 = B2)
  
  # --- MCAR ---
  data_MCAR <- data_complet
  mean(data_complet$X1)
  missing_index <- sample(1:n, size = 0.1 * n)
  data_MCAR$X1[missing_index] <- NA

  
  # Imputation moyenne
  data_MCAR_impute <- data_MCAR
  data_MCAR_impute$X1[is.na(data_MCAR_impute$X1)] <- mean(data_MCAR$X1, na.rm = TRUE)
  somme_moy_impute_mcar <- somme_moy_impute_mcar + mean(data_MCAR_impute$X1)
  
  var_moy_X1_mcar[i] <- var(data_MCAR_impute$X1)
  med_moy_X1_mcar[i] <-  median(data_MCAR_impute$X1)
  
  # Imputation médiane
  data_MCAR_impute_median <- data_MCAR
  data_MCAR_impute_median$X1[is.na(data_MCAR_impute_median$X1)] <- median(data_MCAR$X1 ,na.rm =TRUE)
  somme_moy_impute_median_mcar <- somme_moy_impute_median_mcar + mean(data_MCAR_impute_median$X1)
  
  var_med_X1_mcar[i] <- var(data_MCAR_impute_median$X1)
  med_med_X1_mcar [i]<-  median(data_MCAR_impute_median$X1)
  
  # Cas complets
  data_complet_mcar <- na.omit(data_MCAR)
  somme_moy_complet_mcar <- somme_moy_complet_mcar + mean(data_complet_mcar$X1)
  
  
  res_MCAR[i, ] <- c(mean(data_MCAR_impute$X1),
                     mean(data_MCAR_impute_median$X1),
                     mean(data_complet_mcar$X1))
  
  var_mcar[i, ] <- c(var(data_MCAR_impute$X1),
                     var(data_MCAR_impute_median$X1),
                     var(data_complet_mcar$X1))
  
  med_mcar[i,] <- c(median(data_MCAR_impute$X1),
                    median(data_MCAR_impute_median$X1),
                    median(data_complet_mcar$X1))
  
 
  var_complet_X1_mcar[i] <- var(data_complet_mcar$X1)
  med_complet_X1_mcar [i]<- median(data_complet_mcar$X1)
  
  
  # --- MAR ---
  data_MAR <- data_complet
  missing_index1 <- ifelse(data_MAR$B2 == 0 & data_MAR$X2 < 0.7, 1, 0)
  table(missing_index1)
  data_MAR$X1 <- ifelse(missing_index1 == 0, data_MAR$X1, NA)
  
  
  # Imputation moyenne
  data_MAR_impute <- data_MAR
  data_MAR_impute$X1[is.na(data_MAR_impute$X1)] <- mean(data_MAR$X1, na.rm = TRUE)
  somme_moy_impute_mar <- somme_moy_impute_mar + mean(data_MAR_impute$X1)
  
  var_moy_X1_mar[i] <-  var(data_MAR_impute$X1)
  med_moy_X1_mar[i] <- median(data_MAR_impute$X1)
  
  # Imputation médiane
  data_MAR_impute_median <- data_MAR
  data_MAR_impute_median$X1[is.na(data_MAR_impute_median$X1)] <- median(data_MAR$X1, na.rm = TRUE)
  somme_moy_impute_median_mar <- somme_moy_impute_median_mar + mean(data_MAR_impute_median$X1)
   
 
  var_med_X1_mar[i] <- var(data_MAR_impute_median$X1)
  med_med_X1_mar[i] <- median(data_MAR_impute_median$X1)
  
  # Cas complets
  data_complet_mar <- na.omit(data_MAR)
  somme_moy_complet_mar <- somme_moy_complet_mar + mean(data_complet_mar$X1)
  
  res_MAR[i, ] <- c( mean(data_MAR_impute$X1),
                    mean(data_MAR_impute_median$X1),
                    mean(data_complet_mar$X1))
   
  var_mar[i, ] <- c(var(data_MAR_impute$X1),
                    var(data_MAR_impute_median$X1),
                    var(data_complet_mar$X1))
  
  med_mar[i,] <- c(median(data_MAR_impute$X1),
                   median(data_MAR_impute_median$X1),
                   median(data_complet_mar$X1))
  
  
  var_complet_X1_mar[i] <-  var(data_complet_mar$X1)
  med_complet_X1_mar [i]<-  median(data_complet_mar$X1)
  
  
  # --- MNAR ---
  data_MNAR <- data_complet
  missing_index2 <- ifelse(data_MNAR$X1 < 0.5 , 1, 0)
  table(missing_index2)
  data_MNAR$X1 <- ifelse(missing_index2 == 0, data_MNAR$X1, NA)
  
  
  # Imputation moyenne
  data_MNAR_impute <- data_MNAR
  data_MNAR_impute$X1[is.na(data_MNAR_impute$X1)] <- mean(data_MNAR$X1, na.rm = TRUE)
  somme_moy_impute_mnar <- somme_moy_impute_mnar + mean(data_MNAR_impute$X1)
 
  var_moy_X1_mnar[i] <- var(data_MNAR_impute$X1)
  med_moy_X1_mnar [i]<-  median(data_MNAR_impute$X1)
  
  # Imputation médiane
  data_MNAR_impute_median <- data_MNAR
  data_MNAR_impute_median$X1[is.na(data_MNAR_impute_median$X1)] <- median(data_MNAR$X1, na.rm = TRUE)
  somme_moy_impute_median_mnar <- somme_moy_impute_median_mnar + mean(data_MNAR_impute_median$X1)

  var_med_X1_mnar[i] <- var(data_MNAR_impute_median$X1)
  med_med_X1_mnar[i] <-  median(data_MNAR_impute_median$X1)
  
  
  # Cas complets
  data_complet_mnar <- na.omit(data_MNAR)
  somme_moy_complet_mnar <- somme_moy_complet_mnar + mean(data_complet_mnar$X1)
  med_complet_X1_mnar[i] <-  median(data_complet_mnar$X1)
  var_complet_X1_mnar[i] <-  var(data_complet_mnar$X1)
  
  res_MNAR[i, ] <- c(mean(data_MNAR_impute$X1),
                     mean(data_MNAR_impute_median$X1),
                     mean(data_complet_mnar$X1))
  
  var_mnar[i, ] <- c(var(data_MNAR_impute$X1),
                     var(data_MNAR_impute_median$X1),
                     var(data_complet_mnar$X1))
  
  med_mnar[i,] <- c(median(data_MNAR_impute$X1),
                   median(data_MNAR_impute_median$X1),
                   median(data_complet_mnar$X1))
  
  
  
}

## 6. Calcul des moyennes sur les 500 simulations

moyenne_impute_mcar <- somme_moy_impute_mcar / n_sim
moyenne_impute_mar <- somme_moy_impute_mar / n_sim
moyenne_impute_mnar <- somme_moy_impute_mnar / n_sim

moyenne_impute_median_mcar <- somme_moy_impute_median_mcar / n_sim
moyenne_impute_median_mar <- somme_moy_impute_median_mar / n_sim
moyenne_impute_median_mnar <- somme_moy_impute_median_mnar / n_sim

moyenne_complet_mcar <- somme_moy_complet_mcar / n_sim
moyenne_complet_mar <- somme_moy_complet_mar / n_sim
moyenne_complet_mnar <- somme_moy_complet_mnar / n_sim


## 7. Calcul des biais pour Moyenne

## imputation moyenne

Biais_impute_mcar <- moyenne_impute_mcar - moy_theo_X1
Biais_impute_mar <- moyenne_impute_mar - moy_theo_X1
Biais_impute_mnar <- moyenne_impute_mnar - moy_theo_X1

## imputation mediane

Biais_impute_median_mcar <- moyenne_impute_median_mcar - moy_theo_X1
Biais_impute_median_mar <- moyenne_impute_median_mar - moy_theo_X1
Biais_impute_median_mnar <- moyenne_impute_median_mnar - moy_theo_X1

## Cas complets

Biais_complet_mcar <- moyenne_complet_mcar - moy_theo_X1
Biais_complet_mar <- moyenne_complet_mar - moy_theo_X1
Biais_complet_mnar <- moyenne_complet_mnar - moy_theo_X1


## 7. calcul des biais pour variance 

## MCAR
var_mcar_moy <- mean(var_moy_X1_mcar)
var_mcar_med <- mean(var_med_X1_mcar)
var_mcar_complet <- mean(var_complet_X1_mcar)

## MAR
var_mar_complet <- mean(var_complet_X1_mar) 
var_mar_moy <- mean(var_moy_X1_mar) 
var_mar_med <- mean(var_med_X1_mar)

## MNAR
var_mnar_complet <- mean(var_complet_X1_mnar)
var_mnar_moy <- mean(var_moy_X1_mnar)
var_mnar_med <- mean(var_med_X1_mnar)

## Biais Variance 

## MCAR
Biais_var_complet_mcar <- var_mcar_complet - var_theo_X1
Biais_var_moy_mcar <- var_mcar_moy - var_theo_X1
Biais_var_med_mcar <- var_mcar_med - var_theo_X1


## MAR 
Biais_var_complet_mar <- var_mar_complet - var_theo_X1
Biais_var_moy_mar <- var_mar_moy - var_theo_X1
Biais_var_med_mar <- var_mar_med - var_theo_X1



## MNAR
Biais_var_complet_mnar <- var_mnar_complet - var_theo_X1
Biais_var_moy_mnar <- var_mnar_moy - var_theo_X1
Biais_var_med_mnar <- var_mnar_med - var_theo_X1

# Calcul des biais pour mediane
## MCAR
med_mcar_complet <- mean(med_complet_X1_mcar)
med_mcar_moy <- mean(med_moy_X1_mcar)
med_mcar_med <- mean(med_med_X1_mcar)

## MAR
med_mar_complet <- mean(med_complet_X1_mar)
med_mar_moy <- mean(med_moy_X1_mar)
med_mar_med <- mean(med_med_X1_mar )

## MNAR
med_mnar_complet <- mean(med_complet_X1_mnar) 
med_mnar_moy <- mean(med_moy_X1_mnar)
med_mnar_med <- mean(med_med_X1_mnar)

## Biais Mediane

## MCAR
Biais_med_complet_mcar <- med_mcar_complet - med_theo_X1
Biais_med_moy_mcar <- med_mcar_moy - med_theo_X1
Biais_med_med_mcar <- med_mcar_med - med_theo_X1


## MAR 
Biais_med_complet_mar <- med_mar_complet - med_theo_X1
Biais_med_moy_mar <- med_mar_moy - med_theo_X1
Biais_med_med_mar <- med_mar_med - med_theo_X1

## MNAR
Biais_med_complet_mnar <- med_mnar_complet - med_theo_X1
Biais_med_moy_mnar <- med_mnar_moy - med_theo_X1
Biais_med_med_mnar <- med_mnar_med - med_theo_X1


# --- EQM 

## MCAR
EQM_var_complet_mcar <- Biais_var_complet_mcar^2 + var(var_complet_X1_mcar)
EQM_var_moy_mcar     <- Biais_var_moy_mcar^2 + var(var_moy_X1_mcar)
EQM_var_med_mcar     <- Biais_var_med_mcar^2 + var(var_med_X1_mcar)

## MAR
EQM_var_complet_mar <- Biais_var_complet_mar^2 + var(var_complet_X1_mar)
EQM_var_moy_mar     <- Biais_var_moy_mar^2 + var(var_moy_X1_mar)
EQM_var_med_mar     <- Biais_var_med_mar^2 + var(var_med_X1_mcar)

## MNAR
EQM_var_complet_mnar <- Biais_var_complet_mnar^2 + var(var_complet_X1_mnar)
EQM_var_moy_mnar     <- Biais_var_moy_mnar^2 + var(var_moy_X1_mnar)
EQM_var_med_mnar     <- Biais_var_med_mnar^2 + var(var_med_X1_mnar)


library(ggplot2)
library(reshape2)
library(dplyr)


             ## Boxplots moyenne

# Supposons que res_MCAR, res_MAR et res_MNAR contiennent les moyennes de X1 pour chaque méthode
resultats <- data.frame(
  Method = rep(c("Imputation Moyenne", "Imputation Médiane", "Cas Complet"), each = n_sim, times = 3),
  Type = rep(c("MCAR", "MAR", "MNAR"), each = n_sim * 3),
  Mean_X1 = c(res_MCAR[,1], res_MCAR[,2], res_MCAR[,3],
              res_MAR[,1], res_MAR[,2], res_MAR[,3],
              res_MNAR[,1], res_MNAR[,2], res_MNAR[,3])
)


ggplot(resultats, aes(x = Type, y = Mean_X1, fill = Method)) +
  geom_boxplot() +
  geom_hline(yintercept = moy_theo_X1, color = "red") +
  labs(title = "Boxplots des Moyennes de X1 selon les Méthodes",
       x = "Type de Données Manquantes",
       y = "Moyenne de X1") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2") # Pour une palette de couleurs




    ## Boxplots variance 
resultats_var <- data.frame(
  Method = rep(c("Imputation Moyenne", "Imputation Médiane", "Cas Complet"), each = n_sim,times = 3),
  Type = rep(c("MCAR", "MAR", "MNAR"), each = n_sim * 3),
  Variance_X1 = c(var_mcar[,1], var_mcar[,2], var_mcar[,3],
                  var_mar[,1], var_mar[,2], var_mar[,3],
                  var_mnar[,1], var_mnar[,2], var_mnar[,3])
)


ggplot(resultats_var, aes(x = Type, y = Variance_X1, fill = Method)) +
  geom_boxplot() +
  labs(title = "Boxplots des Variances de X1 selon les Méthodes",
       x = "Type de Données Manquantes",
       y = "Variance de X1") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1") +  # Palette de couleurs
  geom_hline(yintercept = var_theo_X1, color = "red", size = 1) 

## Boxplots mediane

resultats_medianes <- data.frame(
  Method = rep(c("Imputation Moyenne", "Imputation Médiane", "Cas Complet"), each = n_sim, times = 3),
  Type = rep(c("MCAR", "MAR", "MNAR"), each = n_sim * 3),
  Median_X1 = c(med_mcar, med_mar, med_mnar)
)

# Tracer les boxplots avec les médianes
ggplot(resultats_medianes, aes(x = Type, y = Median_X1, fill = Method)) + 
  geom_boxplot() +
  labs(title = "Boxplots des medianes de X1 selon les méthodes",
       x = "Type de Données Manquantes",
       y = "Médiane de X1") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1")+
  # Ajouter les lignes médianes
  geom_hline(yintercept = med_theo_X1, color = "red" ,size =1)



























