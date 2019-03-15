context("Relatorias")

# Setup
ID_PROP_SENADO <<- 91341
ID_PROP_CAMARA <<- 257161
QUANT_RELATORES <<- 2
relatorias_senado_df <<- fetch_relatorias(ID_PROP_SENADO, "senado", QUANT_RELATORES)
relatorias_camara_df <<- fetch_relatorias(ID_PROP_CAMARA, "camara", QUANT_RELATORES)

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
  expect_equal(nrow(relatorias_senado_df), QUANT_RELATORES)
  expect_equal(nrow(relatorias_camara_df), QUANT_RELATORES)
})

test_that("fetch_relatorias() column names", {
  expect_true(all(sapply(relatorias_senado_df, class) %in% .COLNAMES_RELATORIAS_SENADO))
  expect_true(all(sapply(relatorias_camara_df, class) %in% .COLNAMES_RELATORIAS_CAMARA))
})

test_that("fetch_relatorias() when the casa param is invalid", {
  expect_error(fetch_relatorias(21312, "invalid"))
})