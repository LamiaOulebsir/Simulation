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





# Biais des différentes hypothèses

# MCAR (Missing Completely at Random)
Biais_mcar  # Biais faible pour MCAR, indiquant que les données manquantes sont aléatoires.
# Le biais est de 0.00142, ce qui signifie que les données manquantes n'introduisent pas de biais significatif. 

# MAR (Missing at Random)
Biais_mar   # Biais faible pour MAR, suggérant que les données manquantes sont liées aux variables observées mais non aux valeurs manquantes.
# La valeur de 0.00054 est encore faible, ce qui montre que l'impact sur les résultats est faible.

# MNAR (Missing Not at Random)
Biais_mnar  # Le biais est plus élevé sous MNAR, ce qui est attendu puisque les données manquantes dépendent de leurs propres valeurs manquantes.
# Le biais de 0.02748 indique que les données manquantes introduisent un biais plus important dans l'analyse.

# Biais des méthodes d'imputation

# Imputation MCAR
Biais_impute_mcar   # L'imputation pour MCAR ne modifie pas le biais par rapport au cas sans imputation.
# Le biais reste à 0.00142, ce qui signifie que l'imputation n'a pas introduit de biais supplémentaire.

# Imputation MAR
Biais_impute_mar    # L'imputation pour MAR reste cohérente avec les résultats du biais initial MAR.
# Le biais est toujours faible (0.00054), ce qui montre que l'imputation a fonctionné correctement.

# Imputation MNAR
Biais_impute_mnar   # L'imputation ne réussit pas à résoudre le biais introduit par les données MNAR.
# Le biais reste élevé (0.02748), ce qui suggère que l'imputation ne peut pas éliminer les effets du biais lorsque les données manquantes dépendent de leurs propres valeurs.

# Biais avec imputation médiane

# Imputation médiane MCAR
Biais_impute_median_mcar  # L'imputation médiane réduit légèrement le biais par rapport à l'imputation classique.
# La valeur du biais de 0.00129 est légèrement plus faible, ce qui suggère une légère amélioration dans l'estimation.

# Imputation médiane MAR
Biais_impute_median_mar   # L'imputation médiane améliore légèrement le biais sous l'hypothèse MAR.
# La réduction du biais (0.00044) par rapport à l'imputation classique montre un petit gain en précision.

# Imputation médiane MNAR
Biais_impute_median_mnar  # L'imputation médiane ne parvient pas à résoudre le biais pour MNAR.
# Le biais reste relativement élevé (0.02815), ce qui suggère que cette méthode n'est pas efficace pour traiter les données MNAR.

# EQM (Erreur Quadratique Moyenne)

# EQM MCAR
erreur_quadratique_mcar  # L'EQM pour MCAR est de 0.0505, ce qui est faible et montre que l'imputation est précise dans le cas des données manquantes aléatoires.
# Un faible EQM indique que l'écart entre les valeurs imputées et les vraies valeurs est minimal.

# EQM MAR
erreur_quadratique_mar   # L'EQM pour MAR est plus faible (0.0351) que pour MCAR, ce qui montre que l'imputation fonctionne mieux lorsque les données manquantes sont liées aux variables observées.
# Cela montre que l'imputation est plus précise sous l'hypothèse MAR.

# EQM MNAR
erreur_quadratique_mnar  # L'EQM pour MNAR est de 0.0459, ce qui est plus élevé que pour MAR mais encore inférieur à MCAR.
# Cela suggère que, bien que l'imputation ne soit pas optimale pour MNAR, elle reste relativement précise par rapport aux autres cas.

# Écart-type

# Écart-type MCAR
ecart_type_mcar   # L'écart-type pour MCAR est de 0.0505, indiquant que la dispersion des erreurs est relativement faible dans le cas des données manquantes aléatoires.
# Un faible écart-type signifie que les erreurs sont homogènes et bien contrôlées.

# Écart-type MAR
ecart_type_mar    # L'écart-type pour MAR est de 0.0352, ce qui est inférieur à celui de MCAR et indique une plus grande précision dans les erreurs sous cette hypothèse.
# Un écart-type plus faible montre que l'imputation est plus stable sous l'hypothèse MAR.

# Écart-type MNAR
ecart_type_mnar   # L'écart-type pour MNAR est de 0.0368, légèrement plus élevé que pour MAR, mais reste relativement faible, ce qui montre une certaine dispersion autour des valeurs imputées.
# Bien que l'écart-type soit légèrement plus élevé, il reste faible, ce qui signifie que l'imputation ne génère pas de très grandes erreurs.


























#valeur theorique pour la mediane ?
