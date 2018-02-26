# Testa erros
proposicao_inexistente <- fetch_id_proposicao(tipo = "AAA", numero = 55, ano = 1870)
test_that("GET ID de proposição inexistente", {expect_equal(proposicao_inexistente, NULL)})
test_that("GET proposição com ID inexistente", {expect_error(fetch_proposicao(id = 506))})

# Setup
pec_241 <- fetch_proposicao(siglaTipo = "PEC", numero = 241, ano = 2016, dataInicio = "2016-01-01")
pec_241_id <- fetch_id_proposicao("PEC", 241, 2016)
pec_241_por_id <- fetch_proposicao(pec_241_id)

# Constantes
PEC241_ID <- 2088351
TIPO_PROP_PEC <- 136
TIPO_PROP_PL <- 139
TAM_DF_10REQ <- c(10, 7)
TAM_DF_100REQ <- c(100, 7)
TAM_DF_386REQ <- c(386, 7)
TAM_DF_DEFAULT <- c(15, 7)

# Testes
# Os nomes das colunas e os tipos estão definidos em colunas_constants.R
test_that("Is dataframe", {
  expect_true(is.data.frame(pec_241))
  expect_true(is.data.frame(pec_241_por_id))
})

test_that("fetch_proposicao()", {
  expect_true(all(sapply(pec_241, class) %in% .COLNAMES_PROPOSICAO))
})

test_that("fetch_proposicao() usando ID", {
  expect_true(all(sapply(pec_241_por_id, class) %in% .COLNAMES_PROPOSICAO_POR_ID))
})

test_that("ID Correto", {expect_equal(pec_241_id, fetch_id_proposicao("PEC", 241, 2016))})

# Existe um idTipo que identifica os tipos de proposição em tramitação/votação na
#    Câmara. Nesse caso, 136 é PEC.
test_that("ID PEC241", {expect_equal(pec_241_id, PEC241_ID)})
test_that("Tipo PEC241=='PEC'", {expect_equal(pec_241$idTipo, TIPO_PROP_PEC)})
test_that("Numero PEC241", {expect_equal(pec_241$numero, 241)})
test_that("Ano PEC241", {expect_equal(pec_241$ano, 2016)})

# Verifica se as informações relacionadas às votações são pertinentes.
votacoes_pec_241 <- fetch_votacoes(pec_241_id)

ids <- c(7214, 7252, 7207, 7209, 7206, 7223, 7221, 7218, 7222, 7208, 7210,
         7210, 7220, 7212, 7211, 7213, 7215, 7216, 7219, 7245, 7249, 7256,
         7251, 7255, 7257, 7258, 7259, 7246, 7247, 7250, 7254, 7248, 7253)

ids_votacoes_pec241 <- votacoes_pec_241$id
r <- compare::compareEqual(ids, ids_votacoes_pec241)
test_that("IDs Votacoes PEC241", {expect_true(r$result)})

# Testa tipos de proposições
r <- compare::compareEqual(fetch_tipo_proposicao(TIPO_PROP_PL)$sigla, "PL")
test_that("Tipo de proposição", {expect_true(r$result)})

# Testa quantidade de itens por requisição
test_that("Quantidade de itens por requisição",{
  expect_equal(dim(fetch_proposicao(dataInicio = "2007-01-01", dataFim = "2017-01-01", itens = 10)), TAM_DF_10REQ)
  expect_equal(dim(fetch_proposicao(dataInicio = "2007-01-01", dataFim = "2017-01-01", itens = 100)), TAM_DF_100REQ)
  expect_equal(dim(fetch_proposicao(dataInicio = "2007-01-01", dataFim = "2017-01-01", itens = 386)), TAM_DF_386REQ)
})

test_that("Quantidade default por requisição, atualmente 15",{
  expect_equal(dim(fetch_proposicao()), TAM_DF_DEFAULT)
})
