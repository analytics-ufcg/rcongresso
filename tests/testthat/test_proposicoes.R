context("Proposições")

pec_241 <<- fetch_proposicao_camara(siglaTipo = "PEC", numero = 241, ano = 2016, dataInicio = "2016-01-01")
pec_241_id <<- fetch_id_proposicao_camara("PEC", 241, 2016)
pec_241_por_id <<- fetch_proposicao_camara(pec_241_id)
pls_91341 <<- fetch_proposicao_senado(91341)
pls_1489 <<- fetch_proposicao_senado(1489)
votacoes_pec_241 <<- fetch_votacoes(pec_241_id)
relacionadas_pec_241 <<- fetch_relacionadas(pec_241_id)
deferimento <- fetch_deferimento(c("102343", "109173", "115853"))

# Constantes
PEC241_ID <- 2088351
TIPO_PROP_PEC <- 136
TIPO_PROP_PL <- 139
TAM_DF_10REQ <- c(10, 7)
TAM_DF_100REQ <- c(100, 7)
TAM_DF_386REQ <- c(386, 7)
TAM_DF_DEFAULT <- c(15, 7)


# Testa erros
expect_that(fetch_id_proposicao_camara(tipo = "AAA", numero = -1, ano = 1870), gives_warning(.WARNING_PROPOSICAO_ID))
test_that("GET ID de proposição inexistente", {expect_equal(suppressWarnings(fetch_id_proposicao_camara(tipo = "AAA", numero = -1, ano = 1870)), NULL)})
test_that("GET proposição com ID inexistente", {expect_error(fetch_proposicao_camara(id = -1))})

# Testes
# Os nomes das colunas e os tipos estão definidos em colunas_constants.R
test_that("Is dataframe", {
  expect_true(is.data.frame(pec_241))
  expect_true(is.data.frame(pec_241_por_id))
  expect_true(is.data.frame(votacoes_pec_241))
  expect_true(is.data.frame(deferimento))
})

test_that("Not Empty", {
  expect_true(nrow(pec_241) != 0)
  expect_true(nrow(pec_241_por_id) != 0)
  expect_true(nrow(votacoes_pec_241) != 0)
  expect_true(nrow(deferimento) != 0)
})

test_that("fetch_proposicao_camara()", {
  expect_true(all(sapply(pec_241, class) %in% .COLNAMES_PROPOSICAO_CAMARA))
})

test_that("fetch_proposicao_camara() usando ID", {
  expect_true(all(sapply(pec_241_por_id, class) %in% .COLNAMES_PROPOSICAO_POR_ID_CAMARA))
})

test_that("fetch_proposicao_senado()", {
  expect_true(all(sapply(pls_91341, class) %in% .COLNAMES_PROPOSICAO_SENADO))
  expect_true(nrow(pls_91341) != 0)
  expect_true(all(sapply(pls_1489, class) %in% .COLNAMES_PROPOSICAO_SENADO))
  expect_true(nrow(pls_1489) != 0)
})

test_that("fetch_proposicao_senado() not empty", {
  expect_true(nrow(pls_91341) != 0)
  expect_true(nrow(pls_1489) != 0)
})

test_that("fetch_proposicao_senado() is dataframe", {
  expect_true(is.data.frame(pls_91341))
  expect_true(is.data.frame(pls_1489))
})

test_that("fetch_votacoes()", {
  expect_true(all(sapply(votacoes_pec_241, class) %in% .COLNAMES_VOTACOES))
})

test_that("fetch_relacionadas()", {
  expect_true(all(sapply(relacionadas_pec_241, class) %in% .COLNAMES_RELACIONADAS))
})

test_that("fetch_deferimento()", {
  expect_true(all(sapply(deferimento, class) %in% .COLNAMES_DEFRIMENTO))
})

test_that("ID Correto", {expect_equal(pec_241_id, fetch_id_proposicao_camara("PEC", 241, 2016))})

# Existe um idTipo que identifica os tipos de proposição em tramitação/votação na
#    Câmara. Nesse caso, 136 é PEC.
test_that("ID PEC241", {expect_equal(pec_241_id, PEC241_ID)})
test_that("Tipo PEC241=='PEC'", {expect_equal(pec_241$codTipo, TIPO_PROP_PEC)})
test_that("Numero PEC241", {expect_equal(pec_241$numero, 241)})
test_that("Ano PEC241", {expect_equal(pec_241$ano, 2016)})

# Testa tipos de proposições
r <- compare::compareEqual(fetch_tipo_proposicao(TIPO_PROP_PL)$sigla, "PL")
test_that("Tipo de proposição", {expect_true(r$result)})

# Testa quantidade de itens por requisição
test_that("Quantidade de itens por requisição",{
  expect_equal(dim(fetch_proposicao_camara(dataInicio = "2007-01-01", dataFim = "2017-01-01", itens = 10)), TAM_DF_10REQ)
  expect_equal(dim(fetch_proposicao_camara(dataInicio = "2007-01-01", dataFim = "2017-01-01", itens = 100)), TAM_DF_100REQ)
  expect_equal(dim(fetch_proposicao_camara(dataInicio = "2007-01-01", dataFim = "2017-01-01", itens = 386)), TAM_DF_386REQ)
})

test_that("Quantidade default por requisição, atualmente 15",{
  expect_equal(dim(fetch_proposicao_camara()), TAM_DF_DEFAULT)
})