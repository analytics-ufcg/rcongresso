context("Requerimentos")

# Constantes
prop_id_senado <- 91341
prop_id_camara <- 2121442

reqs_senado <<- fetch_related_requerimentos_senado(prop_id_senado)
pl_2121442 <- fetch_related_requerimentos_camara(prop_id_camara)
reqs_senado_normalized <<- fetch_related_requerimentos(prop_id_senado, 'senado')
reqs_camara_normalized <<- fetch_related_requerimentos(prop_id_camara, 'camara')

# Testes

test_that("Is dataframe", {
  expect_true(is.data.frame(reqs_senado))
  expect_true(is.data.frame(reqs_senado_normalized))
  expect_true(is.data.frame(reqs_camara_normalized))
})

test_that("Not Empty", {
  expect_true(nrow(reqs_senado) != 0)
  expect_true(nrow(reqs_senado_normalized) != 0)
  expect_true(nrow(reqs_camara_normalized) != 0)
})

test_that("fetch_related_requerimentos_senado()", {
  expect_true(all(sapply(reqs_senado, class) %in% .COLNAMES_REQ_SENADO))
  expect_true(all(sapply(reqs_senado_normalized, class) %in% .COLNAMES_REQUERIMENTOS))
  expect_true(all(sapply(reqs_camara_normalized, class) %in% .COLNAMES_REQUERIMENTOS))
})

test_that("fetch_related_requerimentos_camara()", {
  expect_true(all(sapply(pl_2121442, class) %in% .COLNAMES_REQUERIMENTOS_CAMARA))
})

test_that("fetch_related_requerimentos_camara() returns dataframe", {
  expect_true(is.data.frame(pl_2121442))
})
