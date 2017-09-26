pec241 <- fetch_proposicao(2088351)
votacao_segundoturno_pec241 <- fetch_votacao(7252)
dataframe_pec241 <- constroi_dataframe(pec241, votacao_segundoturno_pec241)

test_that("Dataframe do QMR", {expect_true(is.data.frame(dataframe_pec241))})
