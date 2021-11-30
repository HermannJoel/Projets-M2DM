#' Log-Vraissemblance du Modèle de Mélange
#'
#' @param X Les données
#' @param model Les paramètres du modèle
#'
#' @return La log-vraissemblance
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
#' gmmClassif.logML(X = X, model = model)
gmmClassif.logML = function(X, model){
  # Initialisation ####
  logML <- 0

  # Calcul de la log-vraissemblance ####
  for (i in 1:nrow(X)) {
    logML <- logML + matrixStats::logSumExp(
      lx = log(x = model$prop) + sapply(
        X = 1:model$K, FUN = function(k) {
          mclust::dmvnorm(
            data = X[i,], mean = model$mu[k,], sigma = model$sigma[k,,], log=TRUE
          )
        }
      )
    )
  }

  # On retourne la log-vraissemblance ####
  return(logML)
}
