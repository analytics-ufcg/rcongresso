context("Emendas")

# Setup
emendas_unica <<- fetch_emendas_senado(91341)
emendas_ausentes <<- fetch_emendas_senado(126364)
emendas_variadas <<- fetch_emendas_senado(133943)
emendas_reforma_setor <<- fetch_emendas_senado(126049)
emendas_camara <<- fetch_emendas_camara('pl', 6726, 2016)
emendas_senado_geral <<- fetch_emendas(id = 91341, casa='senado')
emendas_camara_geral <<- fetch_emendas(id = 2121442, casa='camara',sigla='pl', numero = 6726, ano = 2016)
emendas_pl_6621 <<- scrap_autores_from_website(2171808)
emendas_pec_6 <<- scrap_autores_from_website(2203549)
emendas_com_subemendas <<- fetch_emendas_senado(103831) 
emendas_pls_52 <<- fetch_emendas_senado(111048)

# Testa erros
test_that("GET proposição inexistente", {expect_error(fetch_emendas_senado(-1))})

# Testes
# Os nomes das colunas e os tipos estão definidos em colunas_constants.R
test_that("Is tibble", {
  expect_true(tibble::is_tibble(emendas_unica))
  expect_true(tibble::is_tibble(emendas_ausentes))
  expect_true(tibble::is_tibble(emendas_variadas))
  expect_true(tibble::is_tibble(emendas_reforma_setor))
  expect_true(tibble::is_tibble(emendas_com_subemendas))
  expect_true(tibble::is_tibble(emendas_pls_52))
})

test_that("Is dataframe", {
  expect_true(is.data.frame(emendas_senado_geral))
  expect_true(is.data.frame(emendas_camara_geral))
})


test_that("Empty", {
  expect_true(nrow(emendas_ausentes) == 0)
  expect_true(nrow(emendas_reforma_setor) == 0)
})

test_that("Not Empty", {
  expect_true(nrow(emendas_unica) != 0)
  expect_true(nrow(emendas_variadas) != 0)
  expect_true(nrow(emendas_camara) != 0)
  expect_true(nrow(emendas_senado_geral) != 0)
  expect_true(nrow(emendas_camara_geral) != 0)
  expect_true(nrow(emendas_com_subemendas) != 0)
  expect_true(nrow(emendas_pls_52) != 0)
})

test_that("Várias - fetch_emendas_senado()", {
  expect_true(all(sapply(emendas_variadas, class) %in% .COLNAMES_EMENDAS_SENADO))
  expect_true(all(sapply(emendas_com_subemendas, class) %in% .COLNAMES_EMENDAS_SENADO))
  expect_true(all(sapply(emendas_pls_52, class) %in% .COLNAMES_EMENDAS_SENADO))
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

test_that("scrap_autores_from_website()", {
  expect_true(emendas_pl_6621 == "Juscelino Filho - DEM/MA")
  expect_true(emendas_pec_6 ==
                "Delegado Marcelo Freitas - PSL/MG, Aluisio Mendes - PODE/MA, Carla Zambelli - PSL/SP, Delegado Antônio Furtado - PSL/RJ, Delegado Pablo - PSL/AM, Delegado Waldir - PSL/GO, Felício Laterça - PSL/RJ, José Medeiros - PODE/MT, Nicoletti - PSL/RR, Sanderson - PSL/RS, Felipe Francischini - PSL/PR, João Campos - PRB/GO")
})
