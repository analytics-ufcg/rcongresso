# Testa erros
proposicao_inexistente <- fetch_id_proposicao(tipo = "AAA", numero = 55, ano = 1870)
test_that("GET ID de proposição inexistente", {expect_equal(proposicao_inexistente, NULL)})
test_that("GET proposição com ID inexistente", {expect_error(fetch_proposicao(id = 506))})

# Setup
pec_241 <- fetch_proposicao(siglaTipo = "PEC", numero = 241, ano = 2016, dataInicio = "2016-01-01")
pec_241_id <- fetch_id_proposicao("PEC", 241, 2016)
pec_241_por_id <- fetch_proposicao(pec_241_id)
status_pec241 <- fetch_status_proposicao(pec_241_id)

colnames_pec241 <- c("id","uri","siglaTipo","idTipo","numero","ano","ementa")

colnames_pec241_por_id <- c("id","uri","siglaTipo","idTipo","numero","ano","ementa",
                            "dataApresentacao","tipoAutor","idTipoAutor","descricaoTipo","keywords", "urlInteiroTeor")

colnames_status_pec241 <- c("id","dataHora","sequencia","siglaOrgao","uriOrgao","regime","descricaoTramitacao",
                            "idTipoTramitacao","descricaoSituacao","idSituacao","despacho","url")

tipos_pec241 <- c("integer","character","character","integer","integer","integer","character")

tipos_pec241_por_id <- c("integer","character","character","integer","integer","integer","character",
                  "character","character","integer","character","character","character")

tipos_status_pec241 <- c("integer","character","integer","character","character","character","character",
                         "character","character","integer","character","character")

names(tipos_pec241) <- colnames_pec241
names(tipos_status_pec241) <- colnames_status_pec241
names(tipos_pec241_por_id) <- colnames_pec241_por_id

# Testes

test_that("ID Correto", {expect_equal(pec_241_id, fetch_id_proposicao("PEC", 241, 2016))})

test_that("Is dataframe", {
  expect_true(is.data.frame(pec_241))
  expect_true(is.data.frame(status_pec241))
  expect_true(is.data.frame(pec_241_por_id))
})

test_that("Dimensoes do dataframe",{
  expect_equal(dim(pec_241), c(1, 7))
  expect_equal(dim(pec_241_por_id), c(1, 13))
  expect_equal(dim(status_pec241), c(1, 12))
})

test_that("Atributos do dataframe",{
  expect_equal(attributes(pec_241)$names, colnames_pec241)
  expect_equal(attributes(pec_241_por_id)$names, colnames_pec241_por_id)
  expect_equal(attributes(status_pec241)$names, colnames_status_pec241)
})

test_that("Campos do dataframe",{
  expect_equal(sapply(pec_241, class), tipos_pec241)
  expect_equal(sapply(pec_241_por_id, class), tipos_pec241_por_id)
  expect_equal(sapply(status_pec241, class), tipos_status_pec241)
})

# Existe um idTipo que identifica os tipos de proposição em tramitação/votação na
#    Câmara. Nesse caso, 136 é PEC.
test_that("ID PEC241", {expect_equal(pec_241_id, 2088351)})
test_that("Tipo PEC241=='PEC'", {expect_equal(pec_241$idTipo, 136)})
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
r <- compare::compareEqual(fetch_tipo_proposicao(139)$sigla, "PL")
test_that("Tipo de proposição", {expect_true(r$result)})

# Testa quantidade de itens por requisição
test_that("Quantidade de itens por requisição",{
  expect_equal(dim(fetch_proposicao(dataInicio = "2007-01-01", dataFim = "2017-01-01", itens = 10)), c(10, 7))
  expect_equal(dim(fetch_proposicao(dataInicio = "2007-01-01", dataFim = "2017-01-01", itens = 100)), c(100, 7))
  expect_equal(dim(fetch_proposicao(dataInicio = "2007-01-01", dataFim = "2017-01-01", itens = 386)), c(386, 7))
})

test_that("Quantidade default por requisição, atualmente 15",{
  expect_equal(dim(fetch_proposicao()), c(15, 7))
})
