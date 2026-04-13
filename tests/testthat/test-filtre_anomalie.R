test_that("filtre_anomalie retire les lignes au-dessus du seuil", {
  trajet <- data.frame(
    `Probabilité de présence d'anomalies` = c(0.1, 0.9, 0.4),
    Total = c(10, 20, 30),
    check.names = FALSE
  )

  res <- filtre_anomalie(trajet, seuil = 0.8)

  expect_equal(nrow(res), 2)
  expect_equal(res$Total, c(10, 30))
})

test_that("filtre_anomalie renvoie une erreur si seuil est invalide", {
  trajet <- data.frame(
    `Probabilité de présence d'anomalies` = c(0.1, 0.2),
    check.names = FALSE
  )

  expect_error(
    filtre_anomalie(trajet, seuil = 2),
    "compris entre 0 et 1"
  )
})
