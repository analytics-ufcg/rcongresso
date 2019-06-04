context("Emendas")

# Setup
emendas_unica <<- fetch_emendas_senado(91341)
emendas_ausentes <<- fetch_emendas_senado(126364)
emendas_variadas <<- fetch_emendas_senado(133943)
emendas_camara <<- fetch_emendas_camara('pl', 6726, 2016)
emendas_senado_geral <<- fetch_emendas(id = 91341, casa='senado')
emendas_camara_geral <<- fetch_emendas(id = 2121442, casa='camara',sigla='pl', numero = 6726, ano = 2016)
emenda_pec6 <<- .fetch_emendas_camara_auxiliar(2199624)
emenda_pl6299 <<- .fetch_emendas_camara_auxiliar(577691)


# Testa erros
test_that("GET proposição inexistente", {expect_error(fetch_emendas_senado(-1))})

# Testes
# Os nomes das colunas e os tipos estão definidos em colunas_constants.R
test_that("Is tibble", {
  expect_true(tibble::is_tibble(emendas_unica))
  expect_true(tibble::is_tibble(emendas_ausentes))
  expect_true(tibble::is_tibble(emendas_variadas))
})

test_that("Is dataframe", {
  expect_true(is.data.frame(emendas_senado_geral))
  expect_true(is.data.frame(emendas_camara_geral))
  expect_true(is.data.frame(emenda_pec6))
  expect_true(is.data.frame(emenda_pl6299))
})


test_that("Empty", {
  expect_true(nrow(emendas_ausentes) == 0)
})

test_that("Not Empty", {
  expect_true(nrow(emendas_unica) != 0)
  expect_true(nrow(emendas_variadas) != 0)
  expect_true(nrow(emendas_camara) != 0)
  expect_true(nrow(emendas_senado_geral) != 0)
  expect_true(nrow(emendas_camara_geral) != 0)
  expect_true(nrow(emenda_pec6) != 0)
  expect_true(nrow(emenda_pl6299) != 0)
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

test_that("fetch_emendas()", {
  expect_true(all(sapply(emendas_senado_geral, class) %in% .COLNAMES_EMENDAS_GERAL))
  expect_true(all(sapply(emendas_camara_geral, class) %in% .COLNAMES_EMENDAS_GERAL))
})

test_that(".fetch_emendas_camara_auxiliar()", {
  expect_true(emenda_pl6299$autor[[1]] == "MOREIRA MENDES PSD/RO")
  expect_true(emenda_pec6$autor[[1]] == "FAUSTO PINATO PP/SP e outros")
})
