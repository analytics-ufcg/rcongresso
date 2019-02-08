context("Emendas")

# Setup
emendas_unica <<- fetch_emendas_senado(91341)
emendas_ausentes <<- fetch_emendas_senado(126364)
emendas_variadas <<- fetch_emendas_senado(133943)
emendas_camara <<- fetch_emendas_camara('pl', 6726, 2016)

# Testa erros
test_that("GET proposição inexistente", {expect_error(fetch_emendas_senado(-1))})

# Testes
# Os nomes das colunas e os tipos estão definidos em colunas_constants.R
test_that("Is tibble", {
  expect_true(tibble::is_tibble(emendas_unica))
  expect_true(tibble::is_tibble(emendas_ausentes))
  expect_true(tibble::is_tibble(emendas_variadas))
})

test_that("Empty", {
  expect_true(nrow(emendas_ausentes) == 0)
})

test_that("Not Empty", {
  expect_true(nrow(emendas_unica) != 0)
  expect_true(nrow(emendas_variadas) != 0)
  expect_true(nrow(emendas_camara) != 0)
})

test_that("Várias - fetch_emendas_senado()", {
  expect_true(all(sapply(emendas_variadas, class) %in% .COLNAMES_EMENDAS_SENADO))
  })

test_that("Is dataframe", {
  expect_true(is.data.frame(emendas_camara))
})

test_that("fetch_emendas_camara()", {
  expect_true(all(sapply(emendas_camara, class) %in% .COLNAMES_EMENDAS_CAMARA))
})
