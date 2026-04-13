#' Compter le nombre de trajets
#'
#' @param trajet Un data.frame contenant les trajets
#' @return Le nombre de trajets
#' @export
compter_nombre_trajets <- function(trajet) {
  nrow(trajet)
}

#' Filtrer un jeu de données par numéro de boucle
#'
#' @param trajet Un data.frame contenant les trajets
#' @param boucle Un vecteur de numéros de boucle
#' @return Un data.frame filtré
#' @export
filtrer_trajet <- function(trajet, boucle) {
  if (is.null(boucle)) {
    return(trajet)
  }

  trajet[trajet$boucle %in% boucle, , drop = FALSE]
}
