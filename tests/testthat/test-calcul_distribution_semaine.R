test_that("calcul_distribution_semaine calcule la somme par jour de semaine", {
  trajet <- data.frame(
    `Jour de la semaine` = c(1, 1, 2, 2, 3),
    Total = c(10, 20, 5, 15, 8),
    `Probabilité de présence d'anomalies` = c(0.1, 0.2, 0.1, 0.2, 0.1),
    check.names = FALSE
  )

  res <- calcul_distribution_semaine(trajet, filtre = FALSE)

  expect_equal(res$jour_semaine, c(1, 2, 3))
  expect_equal(res$nombre_trajets, c(30, 20, 8))
})

test_that("calcul_distribution_semaine refuse un parametre filtre invalide", {
  trajet <- data.frame(
    `Jour de la semaine` = c(1, 2),
    Total = c(10, 20),
    `Probabilité de présence d'anomalies` = c(0.1, 0.2),
    check.names = FALSE
  )

  expect_error(
    calcul_distribution_semaine(trajet, filtre = "oui"),
    "TRUE ou FALSE"
  )
})
