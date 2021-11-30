#' Algorithme Expectation Maximization
#'
#' @param X Jeu de données (variables descriptives)
#' @param z Variable reponse
#' @param K Le nombre de cluster
#' @param eps Le critère de convergence
#'
#'
#' @return Nos paramètres maximisé ainsi que sa log-vraissemblance associé
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
#' gmmClassif.EM(X = X, z = z, K = 3)
gmmClassif.EM <- function(X, z, K, eps = 10**-3) {
  # Compteur pour savoir le nombre d'itération effectué
  i = 0

  # Intialisation des paramètres ####
  # Nombre de classes
  if (K > length(levels(x=z))) {
    K = length(
      x = levels(x = z)
    )
  }

  # Calcul de la matrice de covariance
  # On la suppose la même pour toutes les classes
  sigma <- array(
    data = NA, dim = c(K, ncol(x = X), ncol(x = X))
  )

  for (k in 1:K) {
    # Covariance de la gaussinne
    sigma[k,,] = var(x = X)
  }

  # Modèle initial
  model <- list(
    prop = rep(
      x = 1 / K, times = K
    ),
    mu = matrix(
      data = runif(n = K*ncol(x = X)), nrow = K, ncol = ncol(x = X)
    ),
    sigma = sigma,
    K = K
  )


  # Calcul de l'espérance de la vraissemblance complete ####
  expectation <- function(X, model) {
    # # Intialisation
    # gaussianMulti = matrix(
    #   data = NA, nrow = nrow(x), ncol = model$K
    # )
    #
    # tk_xi <- gmmClassif.predict(X = X, model = model)$tik
    #
    # for (k in 1:model$K) {
    #   gaussianMulti[, k] <- dmvnorm(
    #     data = x, mean = model$mu[k, ], sigma = model$sigma[k,, ], log = TRUE
    #   )
    # }
    #
    # return(
    #   tk_xi * (log(x = model$prop) * gaussianMulti)
    # )

    # On retourne les probabilités que les x appartienne au cluster k
    return(
      gmmClassif.predict(X = X, model = model)$tik
    )
  }


  # Maximisation de l'espérance ####
  maximisation <- function(X, z, model, eps, i) {
    # on augmente notre compteur d'iteration de l'algo
    i = i + 1

    # Modèle à l'iteration i + 1
    theta <- gmmClassif.MLParameter(
      X = X,
      z = z,
      K = K,
      tik = expectation(
        X = X, model = model
      ),
      method = "CML"
    )

    # Boucler jusqu'à la convergence de la log-vraissemblance (récurrence)
    if (abs(gmmClassif.logML(X = X, model = theta) - gmmClassif.logML(X = X, model = model)) < eps) {
      return(
        list(
          theta=theta,
          logML=gmmClassif.logML(X = X, model = theta),
          nbIter = i
        )
      )
    }

    else {
      # On reitere
      return(
        maximisation(
          X = X, z = z, model = theta, eps = eps, i=i
        )
      )
    }
  }

  # Lancement de l'algo ####
  return(
    maximisation(
      X = X, z = z, model = model, eps = eps, i=i
    )
  )
}
