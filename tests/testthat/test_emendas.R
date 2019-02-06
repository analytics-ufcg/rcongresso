context("Emendas")

# Setup
emendas_unica <<- fetch_emendas(id = 91341, casa = 'senado')
emendas_ausentes <<- fetch_emendas(id = 126364, casa = 'senado')
emendas_variadas <<- fetch_emendas(id = 133943, casa = 'senado')
  
emendas_camara <<- fetch_emendas(id = 345311, casa = 'camara')
  

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
  expect_true(nrow(emendas_camara) == 0)
  expect_true(nrow(emendas_ausentes) == 0)
})

test_that("Not Empty", {
  expect_true(nrow(emendas_unica) != 0)
  expect_true(nrow(emendas_variadas) != 0)
})

test_that("Is invalid 'casa'", { expect_error(fetch_emendas(91341, "casa"))})

test_that("Várias - fetch_emendas_senado()", {
  expect_true(all(sapply(emendas_variadas, class) %in% .COLNAMES_EMENDAS))
  })

