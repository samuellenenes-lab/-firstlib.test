test_that("plot_distribution_semaine retourne un objet ggplot", {
  trajet <- data.frame(
    `Jour de la semaine` = c(1, 1, 2),
    Total = c(10, 20, 15),
    `Probabilité de présence d'anomalies` = c(0.1, 0.2, 0.1),
    check.names = FALSE
  )

  res <- plot_distribution_semaine(trajet, filtre = FALSE)

  expect_s3_class(res, "ggplot")
})
