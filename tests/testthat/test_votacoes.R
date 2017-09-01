# Testa fetch_votacao()
test_that("GET votação inexistente", {expect_error(fetch_votacao(1325))})

# Testa fetch_votos()
test_that("GET votos de uma votação inexistente", {expect_error(fetch_votos(1325))})
