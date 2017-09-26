# Testa fetch_votacao()
test_that("GET votação inexistente", {expect_error(fetch_votacao(1325))})

# Testa fetch_votos()
test_that("GET votos de uma votação inexistente", {expect_error(fetch_votos(1325))})

# Testa se é data frame
votos_pec241 <- fetch_votos(7252)
votacao_pec241 <- fetch_votacao(7252)
orientacoes_pec241 <- fetch_orientacoes(7252)
ultima_votacao_pec241 <- fetch_votacoes(2088351) %>%
  ultima_votacao()
votos_partidos_pec241 <- get_votos_partidos(7252)

test_that("Dataframe de votos", {expect_true(is.data.frame(votos_pec241))})
test_that("Dataframe de votação", {expect_true(is.data.frame(votacao_pec241))})
test_that("Dataframe de orientações", {expect_true(is.data.frame(orientacoes_pec241))})
test_that("Dataframe de ultima votação", {expect_true(is.data.frame(ultima_votacao_pec241))})
test_that("Dataframe dos votos dos partidos", {expect_true(is.data.frame(votos_partidos_pec241))})

