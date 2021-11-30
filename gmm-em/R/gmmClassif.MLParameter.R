#' Estimation des paramètres par estimation du maximum de vraissemblance
#'
#' @param x Données
#' @param z Variable réponse ou à prédire
#' @param K Le nombre de cluster
#' @param tik La probabilité que les xi appartienne au cluster k
#' @param method La méthode utilisé Vraissemblance "classique" (ML) ou vraissemblance pondérée
#'
#' @return Paramètre proportion
#' @export
#'
#' @examples
#' require(rsample)
#'
#' # Données
#' split <- initial_split(iris, prop=.70, strata="Species")
#'
#' iris_train = training(split)
#' iris_test = testing(split)
#'
#' X_train = iris_train[,1:4]
#' y_train = iris_train[, 5]
#' X_test = iris_test[, 1:4]
#' y_test = iris_test[, 5]
#'
#' # Variables descriptives
#' X = X_train
#'
#' # Variable reponse
#' z = y_train
#'
#' model = gmmClassif.MLParameter(X = X, z = z, K = 3)
gmmClassif.MLParameter <- function (X, z, K, tik = NA, method = "ML") {
  # Information sur les données ####

  # Noms des classes
  clusterName <- levels(x = z)

  # Nombre de classes
  if (K > length(levels(x=z))) {
    K = length(
      x = levels(x = z)
    )
  }

  # Nombre d'observations
  n = nrow(x = X)

  # Nombre de variable
  p=ncol(x = X)

  # Initialisation des paramètres ####

  # Proportion de classes
  prop=rep(NA, K) # No se

  # Initialisation des aramètres de la gaussienne
  mu = matrix(
    data = NA, nrow = K, ncol = p
  )
  sigma=array(
    data = NA, dim = c(K,p,p)
  )



  # Calcul des paramètres par estimation du maximum de vraissemblance ####
  for (k in 1:K) {
    # On estime les paramaètres pour le maximum de vraissemblance
    if (method == "ML") {
      # Proportion de la classe
      prop[k] = mean(
        x = z == levels(x = z)[k]
      )

      # Moyenne de la gaussienne
      mu[k, ] = colMeans(
        x = X[which(x = z==levels(z)[k]), ]
      )

      # Covariance de la gaussinne
      sigma[k,,] = var(
        X[which(x = z==levels(z)[k]), ]
      )
    }

    # On estime les paramètres pour le maximum de vraissemblance complete
    else if (method == "CML") {
      # Proportion de la classe
      prop = colMeans(
        x = tik
      )

      # Moyenne de la gaussienne
      mu[k, ] = colSums(x = tik[, k] * X) / sum(tik[, k])
      # mu[k, ] = cov.wt(
      #   x = X, wt = tik[, k]
      # )$center

      # Covariance de la gaussienne
      sigma[k,,] = cov.wt(
        x = X, wt = tik[, k]
      )$cov
    }
  }

  # On retourne les parmamètres ####
  return(
    list(
      prop = prop, mu = mu, sigma = sigma, K = K
    )
  )
}

