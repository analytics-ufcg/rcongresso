context("Relatorias")

# Setup
ID_PROP_SENADO <<- 91341
ID_PROP_CAMARA <<- 2121442
QUANT_IDS <<- 2
relatorias_senado_df <<- fetch_relatorias(ID_PROP_SENADO, "senado", QUANT_IDS)
relatorias_camara_df <<- fetch_relatorias(ID_PROP_CAMARA, "camara", QUANT_IDS)

# Testa erros
test_that("GET proposição inexistente", {
  expect_error(fetch_relatorias(-1, "senado"))
})

# Testes
# Os nomes das colunas e os tipos estão definidos em colunas_constants.R
test_that("Is dataframe", {
  expect_true(is.data.frame(relatorias_senado_df))
  expect_true(is.data.frame(relatorias_camara_df))
})

test_that("Quantidade de linhas retornadas - fetch_relatorias",{
  expect_equal(nrow(relatorias_senado_df), QUANT_IDS)
})

test_that("fetch_relatorias()", {
  expect_true(all(sapply(relatorias_senado_df, class) %in% .COLNAMES_RELATORIAS))
})
