test_that("compter_nombre_boucle compte les boucles distinctes", {
  trajet <- data.frame(
    `Numéro de boucle` = c("880", "881", "880", "882"),
    check.names = FALSE
  )

  res <- compter_nombre_boucle(trajet)

  expect_equal(res, 3)
})

test_that("compter_nombre_boucle renvoie une erreur si la colonne de boucle est absente", {
  trajet <- data.frame(Total = c(1, 2, 3))

  expect_error(
    compter_nombre_boucle(trajet),
    "Numéro de boucle"
  )
})
