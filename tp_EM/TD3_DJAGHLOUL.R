
library(plyr)
library(mvtnorm)

# Functions
# ---------

# Fonction de visualisation

plotResult_DJAGHLOUL <- function(X, t_ik, CentersClass, K, toPlot = TRUE) {
  if (toPlot) {
    # Trouver la classe qui a la plus grande proba pour chaque observation 
    predicted_classes <- apply(t_ik, 1, which.max)
    
    # Créer un scatter plot avec des points colorée par la classe la plus probable
    plot(X[, 1], X[, 2], col = predicted_classes, pch = 16, 
         main = "EM Clustering", xlab = "Variable 1", ylab = "Variable 2")
    
    # Ajouter les centres des classes
    points(CentersClass[, 1], CentersClass[, 2], col = 1:K, pch = 8, cex = 2)
    
    # Ajouter une légende
    legend("topright", legend = 1:K, col = 1:K, pch = 16, title = "Classes")
  }
}



# Fonction d'initialisation des paramètres
initialize_parameters_DJAGHLOUL <- function(X, K) {
  n <- nrow(X)
  
  # Initialisation des probabilités de classe de manière équitable
  Pclass <- rep(1/K, K)
  
  # Sélection aléatoire de K observations comme centres de classe initiaux
  #CentersClass <- X[sample(1:n, K, replace = FALSE), c("alpha", "beta", "sigma2")]
  random_indices<-sample(1:n, K,replace = FALSE)
  CentersClass <- X[random_indices, , drop = FALSE]
  
  # Initialisation des matrices de covariance à partir de la variance empirique
  #variances <- var(X[, c("alpha", "beta", "sigma2")])
  variances <- var(X)
  variancesClass <- list()
  
  for (k in 1:K) {
    variancesClass[[k]] <- (1/K) * variances
  }
  
  return(list(Pclass = Pclass, CentersClass = CentersClass, variancesClass = variancesClass))
}



# Fonction de calcul de log vraissemblance
logLik_DJAGHLOUL <- function(data, Pclass, CentersClass, variancesClass) {
  n <- nrow(data)
  K <- ncol(Pclass)
  
  logLikelihood <- 0
  
  for (i in 1:n) {
    likelihood_i <- 0
    
    for (k in 1:K) {
      
      # Utilisez la fonction dmnorm du package mvtnorm pour calculer la densité
      likelihood_i <- likelihood_i + Pclass[k] * 
        mvtnorm::dmvnorm(data[i, ], mean = CentersClass[k, ], sigma = variancesClass[[k]], log = TRUE)
    }
    
    logLikelihood <- logLikelihood + log(likelihood_i)
  }
  
  return(logLikelihood)
}


# L'étape Estimation de l'algorithme EM
E_step_DJAGHLOUL <- function(data, Pclass, CentersClass, variancesClass) {
  n <- nrow(data)
  K <- length(Pclass)
  
  # Initialisation de la matrice des probabilités conditionnelles P(Y=k|X)
  conditional_probs <- matrix(0, n, K)
  
  for (i in 1:n) {
    for (k in 1:K) {
      
      # Check for missing values in the data row
      if (any(is.na(data[i, ]))) {
        # If missing, set conditional probability to 0
        conditional_probs[i, k] <- 0
      } else {
        # If not missing, calculate P(Y=k|X) using the formula de Bayes
        conditional_probs[i, k] <- Pclass[k] * 
          mvtnorm::dmvnorm(data[i, ], mean = CentersClass[k, ], sigma = variancesClass[[k]], log = FALSE)
      }
    }
    
    # Normalisation pour que la somme des probabilités soit égale à 1
    if (sum(conditional_probs[i, ], na.rm = TRUE) > 0) {
      conditional_probs[i, ] <- conditional_probs[i, ] / sum(conditional_probs[i, ], na.rm = TRUE)
    }
  }
  
  return(conditional_probs)
}



# Etape Maximisation de l'algoirthme EM
M_step_DJAGHLOUL <- function(data, t_ik) {
  n <- nrow(data)
  K <- ncol(t_ik)
  
  # Mise à jour des probabilités de classe
  Pclass <- colMeans(t_ik)
  
  # Mise à jour des centres de classe
  CentersClass <- matrix(0, nrow = K, ncol = ncol(data))
  
  for (k in 1:K) {
    CentersClass[k, ] <- colSums(data * t_ik[, k]) / sum(t_ik[, k])
  }
  
  # Mise à jour des matrices de covariance
  variancesClass <- vector("list", K)
  
  for (k in 1:K) {
    diff_centered <- data - CentersClass[k, ]
    weighted_diff <- t_ik[, k] * diff_centered
    variancesClass[[k]] <- cov(weighted_diff)
  }
  
  return(list(Pclass = Pclass, CentersClass = CentersClass, variancesClass = variancesClass))
}



EM_M2_SD_DJAGHLOUL <- function(X, K = 3, errorMin = 1e-1, maxIter = 30, toPlot = TRUE) {

  # Initialisation des paramètres
  params <- initialize_parameters_DJAGHLOUL(X, K)
  
  
  # Boucle EM
  iter <- 1
  test <- TRUE
  logLiks <- numeric(maxIter)
  t_ik <- matrix(0, nrow = nrow(X), ncol = K)
  
  while (test) {

    # Étape E
    t_ik <- E_step_DJAGHLOUL(X, params$Pclass, params$CentersClass, params$variancesClass)
    
    # Étape M
    params <- M_step_DJAGHLOUL(X, t_ik)
    
    # Calculer la log-vraisemblance
    logLiks[iter] <- logLik_DJAGHLOUL(X, params$Pclass, params$CentersClass, params$variancesClass)
    
    
    # Vérifier la convergence
    if (iter > 1) {
      error <- logLiks[iter] - logLiks[iter - 1]
      if (error < errorMin) {
        break
      }
    }
    
    iter <- iter + 1
  }
  
  
  # Renvoyer les résultats
  out <- list(logLikEnd = logLiks[iter], logLiks = logLiks,
              pis = params$Pclass,
              mus = params$CentersClass, Sigmas = params$variancesClass, t_ik = t_ik)

  # Visualisation si nécessaire
  if (toPlot) {
    
    # Trouver la classe la plus probable pour chaque observation
    predicted_classes <- apply(t_ik, 1, which.max)
    
    # Créer un nuage de points coloré en fonction des classes prédites
    plot(X[, 1], X[, 2], col = predicted_classes, pch = 16, main = "EM Clustering", xlab = "Variable 1", ylab = "Variable 2")
    
    # Ajouter les centres des classes
    points(params$CentersClass[, 1], params$CentersClass[, 2], col = 1:K, pch = 8, cex = 2)
    
    # Ajouter une légende
    legend("topright", legend = 1:K, col = 1:K, pch = 16, title = "Classes")
  }
  
  # Renvoyer les résultats
  out <- list(logLikEnd = logLiks[iter], logLiks = logLiks,
              pis = params$Pclass,
              mus = params$CentersClass, Sigmas = params$variancesClass, t_ik = t_ik)
  
  return(out)
}




# Application
data = read.csv("TD3.csv", header = T, sep = ";", dec = ",")
n = nrow(data)
p = ncol(data)



# Extract relevant columns for clustering
X <- data[, c("alpha", "beta", "sigma2")]


result =initialize_parameters_DJAGHLOUL(X,3)


r1 = logLik_DJAGHLOUL(X, result$Pclass, result$CentersClass, result$variancesClass)
r2=E_step_DJAGHLOUL(X, result$Pclass, result$CentersClass, result$variancesClass)
r3=M_step_DJAGHLOUL(X, t_ik)

result <- EM_M2_SD_DJAGHLOUL(X, K = 3, errorMin = 1e-1, maxIter = 30, toPlot = TRUE)
