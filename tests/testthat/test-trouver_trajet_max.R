test_that("trouver_trajet_max retourne la ligne avec le Total maximal", {
  trajet <- data.frame(
    `Numéro de boucle` = c("880", "881", "882"),
    Total = c(10, 50, 30),
    check.names = FALSE
  )

  res <- trouver_trajet_max(trajet)

  expect_equal(nrow(res), 1)
  expect_equal(res$Total, 50)
  expect_equal(res$`Numéro de boucle`, "881")
})
