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

colnames_votos_pec241 <- c("id_votacao","voto","parlamentar.id","parlamentar.uri",
                          "parlamentar.nome","parlamentar.siglaPartido",
                          "parlamentar.uriPartido","parlamentar.siglaUf",
                          "parlamentar.idLegislatura","parlamentar.urlFoto")
colnames_votacao_pec241 <- c("id","uri","titulo","uriEvento","uriProposicaoPrincipal",
                            "tipoVotacao","placarSim","placarNao","placarAbstencao",
                            "dataHoraInicio","dataHoraFim")
colnames_orientacoes_pec241 <- c("uriBancada","nomeBancada","voto","id_votacao")
colnames_votospartidos_pec241 <- c("partido", "orientacao_partido", "bancada_associada", "id_votacao")
colnames_ultimavotacao_pec241 <- c("id","uriProposicaoPrincipal")
colnames_proposicao_votacao7252 <- c("id_votacao","uri","ementa","id_proposicao")

tipos_votos_pec241 <- c("numeric","character","integer","character","character","character",
                        "character","character","integer","character")
tipos_votacao_pec241 <- c("integer","character","character","character","character",
                        "character","integer","integer","integer","character","character")
tipos_orientacoes_pec241 <- c("character","character","character","numeric")
tipos_ultima_votacao_pec241 <- c("integer","character")
tipos_votos_partidos_pec241 <- c("character","character","character","numeric")
tipos_proposicao_votacao7252 <- c("numeric","character","character","integer")

names(tipos_votos_pec241) <- colnames_votos_pec241
names(tipos_votacao_pec241) <- colnames_votacao_pec241
names(tipos_orientacoes_pec241) <- colnames_orientacoes_pec241
names(tipos_ultima_votacao_pec241) <- colnames_ultimavotacao_pec241
names(tipos_votos_partidos_pec241) <- colnames_votospartidos_pec241
names(tipos_proposicao_votacao7252) <- colnames_proposicao_votacao7252

# Testes
test_that("Is dataframe", {
  expect_true(is.data.frame(votos_pec241))
  expect_true(is.data.frame(votacao_pec241))
  expect_true(is.data.frame(orientacoes_pec241))
  expect_true(is.data.frame(ultima_votacao_pec241))
  expect_true(is.data.frame(votos_partidos_pec241))
  expect_true(is.data.frame(proposicao_votacao7252))
})

# Nao necessario (remover depois)
test_that("Dimensoes do dataframe",{
  expect_equal(dim(votos_pec241), c(478, 10))
  expect_equal(dim(votacao_pec241), c(1, 11))
  expect_equal(dim(orientacoes_pec241), c(23, 4))
  expect_equal(dim(votos_partidos_pec241), c(38, 4))
  expect_equal(dim(ultima_votacao_pec241), c(1, 2))
  expect_equal(dim(proposicao_votacao7252), c(1, 4))
})

test_that("Atributos do dataframe",{
  expect_true(all(attributes(votos_pec241)$names %in% colnames_votos_pec241))
  expect_true(all(attributes(votacao_pec241)$names %in% colnames_votacao_pec241))
  expect_true(all(attributes(orientacoes_pec241)$names %in% colnames_orientacoes_pec241))
  expect_true(all(attributes(votos_partidos_pec241)$names %in% colnames_votospartidos_pec241))
  expect_true(all(attributes(ultima_votacao_pec241)$names %in% colnames_ultimavotacao_pec241))
  expect_true(all(attributes(proposicao_votacao7252)$names %in% colnames_proposicao_votacao7252))
})

test_that("Campos do dataframe",{
  expect_true(all(sapply(votos_pec241, class) %in% tipos_votos_pec241))
  expect_true(all(sapply(votacao_pec241, class) %in% tipos_votacao_pec241))
  expect_true(all(sapply(orientacoes_pec241, class) %in% tipos_orientacoes_pec241))
  expect_true(all(sapply(votos_partidos_pec241, class) %in% tipos_votos_partidos_pec241))
  expect_true(all(sapply(ultima_votacao_pec241, class) %in% tipos_ultima_votacao_pec241))
  expect_true(all(sapply(proposicao_votacao7252, class) %in% tipos_proposicao_votacao7252))
})
