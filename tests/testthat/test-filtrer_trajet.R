test_that("filtrer_trajet renvoie les donnees non filtrees si boucle est NULL", {
  trajet <- data.frame(
    `Numéro de boucle` = c("880", "881", "882"),
    Total = c(10, 20, 30),
    check.names = FALSE
  )

  res <- filtrer_trajet(trajet, NULL)

  expect_equal(res, trajet)
})
