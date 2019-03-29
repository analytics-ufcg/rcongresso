context("Votações")

# Setup
proposicao_votacao_senado <<- fetch_votacoes_senado(91341)

# Testa erros
## test_that("GET votação inexistente", {expect_error(fetch_votacao(-1))})
## test_that("GET votos de uma votação inexistente", {expect_true(nrow(fetch_votos(-1)) == 0)})

# Testes
# Os nomes das colunas e os tipos estão definidos em colunas_constants.R
test_that("Is dataframe", {
  expect_true(is.data.frame(proposicao_votacao_senado))
})

test_that("Not Empty", {
  expect_true(nrow(proposicao_votacao_senado) != 0)
})

test_that("fetch_votos()", {
  expect_true(all(sapply(proposicao_votacao_senado, class) %in% .COLNAMES_VOT_SEN))
})
