test_that("filtrer_trajet conserve les boucles demandees", {
  trajet <- data.frame(
    `Numéro de boucle` = c("880", "881", "882"),
    Total = c(10, 20, 30),
    check.names = FALSE
  )

  res <- filtrer_trajet(trajet, c("880", "881"))

  expect_equal(nrow(res), 2)
  expect_true(all(res$`Numéro de boucle` %in% c("880", "881")))
})

test_that("filtrer_trajet renvoie les donnees non filtrees si boucle est NULL", {
  trajet <- data.frame(
    `Numéro de boucle` = c("880", "881", "882"),
    Total = c(10, 20, 30),
    check.names = FALSE
  )

  res <- filtrer_trajet(trajet, NULL)

  expect_equal(res, trajet)
})
