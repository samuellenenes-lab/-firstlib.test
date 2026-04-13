#' Filtrer les lignes considérées comme anormales
#'
#' Cette fonction retire les lignes dont la probabilité de présence
#' d'anomalies dépasse un seuil donné.
#'
#' @param trajet Un data.frame contenant les données de trajets vélo.
#' @param seuil Un nombre entre 0 et 1. Les lignes avec une probabilité
#' d'anomalie strictement supérieure à ce seuil sont supprimées.
#'
#' @return Un data.frame filtré.
#' @export
filtre_anomalie <- function(trajet, seuil = 0.8) {
  if (!is.data.frame(trajet)) {
    stop("trajet doit être un data.frame")
  }

  if (!"Probabilité de présence d'anomalies" %in% names(trajet)) {
    stop("La colonne 'Probabilité de présence d'anomalies' est absente.")
  }

  if (!is.numeric(seuil) || length(seuil) != 1 || seuil < 0 || seuil > 1) {
    stop("seuil doit être un nombre compris entre 0 et 1.")
  }

  trajet[trajet[["Probabilité de présence d'anomalies"]] <= seuil, , drop = FALSE]
}


#' Compter le nombre de trajets
#'
#' Cette fonction calcule la somme de la colonne Total.
#'
#' @param trajet Un data.frame contenant les données de trajets vélo.
#'
#' @return Un nombre entier correspondant au total de trajets.
#' @export
compter_nombre_trajets <- function(trajet) {
  if (!is.data.frame(trajet)) {
    stop("trajet doit être un data.frame")
  }

  if (!"Total" %in% names(trajet)) {
    stop("La colonne 'Total' est absente.")
  }

  sum(trajet[["Total"]], na.rm = TRUE)
}


#' Compter le nombre de boucles distinctes
#'
#' Cette fonction compte le nombre de valeurs distinctes
#' dans la colonne Numéro de boucle.
#'
#' @param trajet Un data.frame contenant les données de trajets vélo.
#'
#' @return Un entier correspondant au nombre de boucles distinctes.
#' @export
compter_nombre_boucle <- function(trajet) {
  if (!is.data.frame(trajet)) {
    stop("trajet doit être un data.frame")
  }

  if (!"Numéro de boucle" %in% names(trajet)) {
    stop("La colonne 'Numéro de boucle' est absente.")
  }

  length(unique(trajet[["Numéro de boucle"]]))
}


#' Trouver la ligne correspondant au trajet maximal
#'
#' Cette fonction renvoie la ligne du data.frame ayant le Total maximal.
#'
#' @param trajet Un data.frame contenant les données de trajets vélo.
#'
#' @return Un data.frame d'une ligne correspondant au maximum de la colonne Total.
#' @export
trouver_trajet_max <- function(trajet) {
  if (!is.data.frame(trajet)) {
    stop("trajet doit être un data.frame")
  }

  if (!"Total" %in% names(trajet)) {
    stop("La colonne 'Total' est absente.")
  }

  indice_max <- which.max(trajet[["Total"]])
  trajet[indice_max, , drop = FALSE]
}


#' Calculer la distribution hebdomadaire des trajets
#'
#' Cette fonction calcule la somme des trajets par jour de la semaine.
#' Par défaut, elle filtre d'abord les anomalies.
#'
#' @param trajet Un data.frame contenant les données de trajets vélo.
#' @param filtre Booléen. Si TRUE, applique d'abord filtre_anomalie().
#'
#' @return Un data.frame avec deux colonnes :
#' \describe{
#'   \item{jour_semaine}{Le numéro du jour de la semaine}
#'   \item{nombre_trajets}{La somme des trajets pour ce jour}
#' }
#' @export
calcul_distribution_semaine <- function(trajet, filtre = TRUE) {
  if (!is.data.frame(trajet)) {
    stop("trajet doit être un data.frame")
  }

  if (!is.logical(filtre) || length(filtre) != 1) {
    stop("filtre doit être TRUE ou FALSE.")
  }

  if (!"Jour de la semaine" %in% names(trajet)) {
    stop("La colonne 'Jour de la semaine' est absente.")
  }

  if (!"Total" %in% names(trajet)) {
    stop("La colonne 'Total' est absente.")
  }

  if (filtre) {
    trajet <- filtre_anomalie(trajet)
  }

  res <- stats::aggregate(
    trajet[["Total"]],
    by = list(jour_semaine = trajet[["Jour de la semaine"]]),
    FUN = sum,
    na.rm = TRUE
  )

  names(res)[2] <- "nombre_trajets"
  res <- res[order(res$jour_semaine), , drop = FALSE]

  rownames(res) <- NULL
  res
}


#' Tracer la distribution hebdomadaire des trajets
#'
#' Cette fonction produit un graphique du nombre total de trajets
#' selon le jour de la semaine.
#'
#' @param trajet Un data.frame contenant les données de trajets vélo.
#' @param filtre Booléen. Si TRUE, applique d'abord filtre_anomalie().
#'
#' @return Un graphique ggplot2.
#' @export
plot_distribution_semaine <- function(trajet, filtre = TRUE) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Le package 'ggplot2' est nécessaire pour cette fonction.")
  }

  distribution <- calcul_distribution_semaine(trajet, filtre = filtre)

  ggplot2::ggplot(
    distribution,
    ggplot2::aes(x = factor(jour_semaine), y = nombre_trajets)
  ) +
    ggplot2::geom_col() +
    ggplot2::labs(
      title = "Distribution hebdomadaire des trajets vélo",
      x = "Jour de la semaine",
      y = "Nombre de trajets"
    )
}


#' Filtrer les trajets selon un ou plusieurs numéros de boucle
#'
#' Cette fonction conserve uniquement les lignes dont le numéro de boucle
#' appartient au vecteur fourni.
#'
#' @param trajet Un data.frame contenant les données de trajets vélo.
#' @param boucle Un vecteur des numéros de boucle à conserver.
#'
#' @return Un data.frame filtré.
#' @export
filtrer_trajet <- function(trajet, boucle) {
  if (!is.data.frame(trajet)) {
    stop("trajet doit être un data.frame")
  }

  if (!"Numéro de boucle" %in% names(trajet)) {
    stop("La colonne 'Numéro de boucle' est absente.")
  }

  if (is.null(boucle)) {
    return(trajet)
  }

  trajet[trajet[["Numéro de boucle"]] %in% boucle, , drop = FALSE]
}
