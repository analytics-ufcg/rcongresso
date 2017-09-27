# Setup
pec241 <- fetch_proposicao(2088351)
votacao_segundoturno_pec241 <- fetch_votacao(7252)
dataframe_pec241 <- constroi_dataframe(pec241, votacao_segundoturno_pec241)

colnames_dataframe_pec241 <- c("id_votacao","voto","parlamentar.id","parlamentar.uri",
                               "parlamentar.nome","parlamentar.siglaPartido","parlamentar.uriPartido",
                               "parlamentar.siglaUf","parlamentar.idLegislatura","parlamentar.urlFoto",
                               "uri_votacao","titulo","uriEvento","uriProposicaoPrincipal","tipoVotacao",
                               "placarSim","placarNao","placarAbstencao","dataHoraInicio","dataHoraFim",
                               "id_proposicao","siglaTipo","idTipo","numero","ano","ementa","dataApresentacao",
                               "tipoAutor","idTipoAutor","descricaoTipo","keywords","tipo_prop",
                               "orientacao_partido","bancada_associada","orientacao_governo")

tipos_dataframe_pec241 <- c("integer","character","integer","character","character","character","character",
                            "character","integer","character","character","character","character","character",
                            "character","integer","integer","integer","character","character","integer",
                            "character","integer","integer","integer","character","character","character",
                            "integer","character","character","character","character","character","character")

names(tipos_dataframe_pec241) <- colnames_dataframe_pec241

# Testes
test_that("Is dataframe", {
  expect_true(is.data.frame(dataframe_pec241))
})

test_that("Dimensoes do dataframe",{
  expect_equal(dim(dataframe_pec241), c(478, 35))
})

test_that("Atributos do dataframe",{
  expect_equal(attributes(dataframe_pec241)$names, colnames_dataframe_pec241)
})

test_that("Campos do dataframe",{
  expect_equal(sapply(dataframe_pec241, class), tipos_dataframe_pec241)
})
