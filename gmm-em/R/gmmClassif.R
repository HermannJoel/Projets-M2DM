#' Modèle de Mélange Gaussien (GMM) en classification
#' Fonction de Classification
#'
#' @param Xtrain Variables descriptives des individus de l'échantillon d'apprentissage
#' @param Xtest Variables descriptives des individus de l'échantillon test
#' @param z Variable réponse à prédire pour les individus de l'échantillon d'apprentissage
#' @param K Le nombre de cluster
#' @param eps Le critère de convergence de l'algo EM
#'
#' @return
#' La variable réponse prédite dans le cluster k
#' Le nom du modèle
#' Les paramètres du modèles ainsi que le nombre de cluster, la proportion de mélange
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
#' gmm = gmmClassif(Xtrain = X_train, Xtest = X_test, z = y_train, K = 3)
#'
#' # Affichage graphique
#' require(cluster)
#' clusplot(X_test, gmm$pred$z, lines=0,
#'          shade=TRUE, color=TRUE,
#'          labels=2, plotchar=FALSE,
#'          span=TRUE, main=paste("Clusters of Iris"), dimens=c(1, 2, 3))
gmmClassif <- function(Xtrain, Xtest, z, K, eps = 10**-3) {
  # Attention au nombre de cluster choisi
  if (K > length(levels(x=z))) {
    K = length(
      x = levels(x = z)
    )
  }

  # On estime le modèle
  model <- gmmClassif.EM(
    X = Xtrain, z = z, K = K, eps = eps
  )

  # On retourne les prédictions, les paramètres du modèle ainsi que sa
  # vraissemblance
  return(
    list(
      pred = gmmClassif.predict(
        X = Xtest, model = model$theta
      ),
      modelName = "Classification",
      model = model$theta,
      logML = model$logML
    )
  )
}
