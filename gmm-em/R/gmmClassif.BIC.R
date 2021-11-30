#' Critère BIC
#'
#' @param logML La log vraissemblance
#' @param v Le nombre de variable du modèle
#' @param n Le nombre d'observation
#'
#' @return BIC
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
#' model <- gmmClassif.EM(X = X, z = z, K = 3)
#' gmmClassif.BIC(model$logML, ncol(X), nrow(X))
gmmClassif.BIC <- function(logML, v, n) {
  return(
    2 * logML - v * log(n)
  )
}
