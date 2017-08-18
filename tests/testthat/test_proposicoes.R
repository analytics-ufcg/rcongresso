# Verifica se o ID da PEC241 retornado está correto.
pec_241_id <- fetch_id_proposicao(tipo = "PEC", numero = 241, ano = 2016)
test_that("Mensagem default", {expect_equal(pec_241_id, 2088351)})

# Verifica se o ID da PEC287 retornado está correto.
pec_287_id <- fetch_id_proposicao(tipo = "PEC", numero = 287, ano = 2016)
test_that("Mensagem default", {expect_equal(pec_287_id, 2119881)})

# Verifica se o ID da PEC22 retornado está correto.
pec_22_id <- fetch_id_proposicao(tipo = "PEC", numero = 22, ano = 2011)
test_that("Mensagem default", {expect_equal(pec_22_id, 500843)})

# Verifica se as informações relacionadas às proposições são pertinentes.
# Existe um idTipo que identifica os tipos de proposição em tramitação/votação na
#    Câmara. Nesse caso, 136 é PEC.
pec_241 <- fetch_proposicao(pec_241_id)
test_that("Mensagem default", {expect_equal(pec_241$idTipo, 136)})
test_that("Mensagem default", {expect_equal(pec_241$numero, 241)})
test_that("Mensagem default", {expect_equal(pec_241$ano, 2016)})

pec_287 <- fetch_proposicao(pec_287_id)
test_that("Mensagem default", {expect_equal(pec_287$idTipo, 136)})
test_that("Mensagem default", {expect_equal(pec_287$numero, 287)})
test_that("Mensagem default", {expect_equal(pec_287$ano, 2016)})

pec_22 <- fetch_proposicao(pec_22_id)
test_that("Mensagem default", {expect_equal(pec_22$idTipo, 136)})
test_that("Mensagem default", {expect_equal(pec_22$numero, 22)})
test_that("Mensagem default", {expect_equal(pec_22$ano, 2011)})

# Verifica se as informações relacionadas às votações são pertinentes.
votacoes_pec_241 <- fetch_votacoes(pec_241_id)

ids <- c(7214, 7252, 7207, 7209, 7206, 7223, 7221, 7218, 7222, 7208, 7210,
         7210, 7220, 7212, 7211, 7213, 7215, 7216, 7219, 7245, 7249, 7256,
         7251, 7255, 7257, 7258, 7259, 7246, 7247, 7250, 7254, 7248, 7253)

ids_votacoes_pec241 <- votacoes_pec_241$id
r <- compare::compareEqual(ids, ids_votacoes_pec241)
test_that("Mensagem default", {expect_true(r$result)})


