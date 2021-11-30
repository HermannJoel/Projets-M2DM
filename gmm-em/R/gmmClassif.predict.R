#' Prédiction du cluster et calcul de la probabilité d'appartenance
#'
#' @param model les paramètres du modèle
#' @param X Les données
#'
#' @return La probabilité et la prédiction que les x_i appartiennent au cluster k
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
#'
#' pred = gmmClassif.predict(X = X_test, model = model)
#'
#' # Erreur de classification : Matrice de Confusion
#' table(pred$z, y_test)
gmmClassif.predict <- function (X, model) {
  # Acces à a la fonction qui permet de calculer de
  # densité gaussienne multivariée
  library(mclust)

  # Intialisation  ####
  tik = matrix(
    data = NA, nrow = nrow(X), ncol = model$K
  )

  # Probabilité que les x_i appartient aux différents clusters ####
  # Calul de la proba conditionnelle t_k(x_i)
  # p_k * f_k(x)
  for (k in 1:model$K){
    tik[, k] = model$prop[k] * dmvnorm(
      data = X, mean = model$mu[k,], sigma = model$sigma[k,,]
    )
  }

  # Marginal de X f_X(x
  tik = tik / rowSums(x = tik)

  # Classement (prédiction) des observations x_i dans un cluster k ####
  # Calcul du argmax de t_k(x_i) ####
  z = max.col(m = tik)

  # On retourne la proba et pred que x appartienne au cluster ####
  return(
    list(
      z = z, tik = tik
    )
  )
}
