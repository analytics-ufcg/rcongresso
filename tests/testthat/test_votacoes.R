# Testa erros
test_that("GET votação inexistente", {expect_error(fetch_votacao(1325))})
test_that("GET votos de uma votação inexistente", {expect_true(nrow(fetch_votos(1325)) == 0)})

# Setup
votos_pec241 <- fetch_votos(7252)
votacao_pec241 <- fetch_votacao(7252)
orientacoes_pec241 <- fetch_orientacoes(7252)
ultima_votacao_pec241 <- fetch_votacoes(2088351) %>%
  ultima_votacao()
votos_partidos_pec241 <- get_votos_partidos(7252)
proposicao_votacao7252 <- fetch_proposicao_from_votacao(7252)

# Novo modelo de teste:

colnames_orientacoes_pec241 <- c("uriBancada"="character","nomeBancada"="character",
                                 "voto"="character","id_votacao"="numeric")

colnames_votos_pec241 <- c("id_votacao"="numeric","voto"="character","parlamentar.id"="integer",
                           "parlamentar.uri"="character","parlamentar.nome"="character",
                           "parlamentar.siglaPartido"="character","parlamentar.uriPartido"="character",
                           "parlamentar.siglaUf"="character","parlamentar.idLegislatura"="integer",
                           "parlamentar.urlFoto"="character")

colnames_votacao_pec241 <- c("id"="numeric","uri"="character","titulo"="character","uriEvento"="character",
                             "proposicao.id"="numeric","proposicao.uri"="character","proposicao.ementa"="character",
                             "uriProposicaoPrincipal"="character","tipoVotacao"="character","aprovada"="character",
                             "placarSim"="numeric","placarNao"="numeric","placarAbstencao"="numeric","relator"="character",
                             "ementaParecer"="character","dataHoraInicio"="character","dataHoraFim"="character",
                             "numVotantes"="numeric","numPresentes"="numeric","despacho"="character")

colnames_votospartidos_pec241 <- c("partido"="character","orientacao_partido"="character","bancada_associada"="character",
                                   "id_votacao"="numeric")

colnames_ultimavotacao_pec241 <- c("id"="integer","uriProposicaoPrincipal"="character")

colnames_proposicao_votacao7252 <- c("id_votacao"="numeric","id_proposicao"="integer","uri"="character","siglaTipo"="character",
                                     "idTipo"="integer","numero"="integer","ano"="integer","ementa"="character")

test_that("Is dataframe", {
  expect_true(is.data.frame(votos_pec241))
  expect_true(is.data.frame(votacao_pec241))
  expect_true(is.data.frame(orientacoes_pec241))
  expect_true(is.data.frame(ultima_votacao_pec241))
  expect_true(is.data.frame(votos_partidos_pec241))
  expect_true(is.data.frame(proposicao_votacao7252))
})

test_that("fetch_votos()", {
  expect_true(all(sapply(votos_pec241, class) %in% colnames_votos_pec241))
})

test_that("fetch_votacao()", {
  expect_true(all(sapply(votacao_pec241, class) %in% colnames_votacao_pec241))
})

test_that("fetch_orientacoes()", {
  expect_true(all(sapply(orientacoes_pec241, class) %in% colnames_orientacoes_pec241))
})

test_that("get_votos_partidos()", {
  expect_true(all(sapply(votos_partidos_pec241, class) %in% colnames_votospartidos_pec241))
})

test_that("ultima_votacao()", {
  expect_true(all(sapply(ultima_votacao_pec241, class) %in% colnames_ultimavotacao_pec241))
})

test_that("fetch_proposicao_from_votacao()", {
  expect_true(all(sapply(proposicao_votacao7252, class) %in% colnames_proposicao_votacao7252))
})
