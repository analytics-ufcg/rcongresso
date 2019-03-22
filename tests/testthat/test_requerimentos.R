context("Requerimentos")

# Constantes
prop_id_senado <- 91341

reqs_senado <<- fetch_related_requerimentos_senado(prop_id_senado)

# Testes

test_that("Is dataframe", {
  expect_true(is.data.frame(reqs_senado))
})

test_that("Not Empty", {
  expect_true(nrow(reqs_senado) != 0)
})

test_that("fetch_related_requerimentos_senado()", {
  expect_true(all(sapply(reqs_senado, class) %in% .COLNAMES_REQ_SENADO))
})
