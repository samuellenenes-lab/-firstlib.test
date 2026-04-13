test_that("compter_nombre_trajets calcule la somme de Total", {
  trajet <- data.frame(
    Total = c(10, 20, 30)
  )

  res <- compter_nombre_trajets(trajet)

  expect_equal(res, 60)
})

test_that("compter_nombre_trajets renvoie une erreur si trajet n'est pas un data.frame", {
  expect_error(
    compter_nombre_trajets(42),
    "data.frame"
  )
})

test_that("compter_nombre_trajets renvoie une erreur si la colonne Total est absente", {
  trajet <- data.frame(x = c(1, 2, 3))

  expect_error(
    compter_nombre_trajets(trajet),
    "Total"
  )
})
