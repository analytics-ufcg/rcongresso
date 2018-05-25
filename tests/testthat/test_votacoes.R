context("Votações")

# Setup
setup <- function(){
  votos_pec241 <<- fetch_votos(7252)
  votacao_pec241 <<- fetch_votacao(7252)
  orientacoes_pec241 <<- fetch_orientacoes(7252)
  ultima_votacao_pec241 <<- fetch_votacoes(2088351) %>%
    ultima_votacao()
  votos_partidos_pec241 <<- get_votos_partidos(7252)
  proposicao_votacao7252 <<- fetch_proposicao_from_votacao(7252)

  return(TRUE)
}

check_api <- function(){
  tryCatch(setup(), error = function(e){return(FALSE)})
}

test <- function(){
  # Testa erros
  test_that("GET votação inexistente", {expect_error(fetch_votacao(-1))})
  test_that("GET votos de uma votação inexistente", {expect_true(nrow(fetch_votos(-1)) == 0)})

  # Testes
  # Os nomes das colunas e os tipos estão definidos em colunas_constants.R
  test_that("Is dataframe", {
    expect_true(is.data.frame(votos_pec241))
    expect_true(is.data.frame(votacao_pec241))
    expect_true(is.data.frame(orientacoes_pec241))
    expect_true(is.data.frame(ultima_votacao_pec241))
    expect_true(is.data.frame(votos_partidos_pec241))
    expect_true(is.data.frame(proposicao_votacao7252))
  })

  test_that("Not Empty", {
    expect_true(nrow(votos_pec241) != 0)
    expect_true(nrow(votacao_pec241) != 0)
    expect_true(nrow(orientacoes_pec241) != 0)
    expect_true(nrow(ultima_votacao_pec241) != 0)
    expect_true(nrow(votos_partidos_pec241) != 0)
    expect_true(nrow(proposicao_votacao7252) != 0)
  })

  test_that("fetch_votos()", {
    expect_true(all(sapply(votos_pec241, class) %in% .COLNAMES_VOTOS))
  })

  test_that("fetch_votacao()", {
    expect_true(all(sapply(votacao_pec241, class) %in% .COLNAMES_VOTACAO))
  })

  test_that("fetch_orientacoes()", {
    expect_true(all(sapply(orientacoes_pec241, class) %in% .COLNAMES_ORIENTACOES))
  })

  test_that("get_votos_partidos()", {
    expect_true(all(sapply(votos_partidos_pec241, class) %in% .COLNAMES_VOTOSPARTIDOS))
  })

  test_that("ultima_votacao()", {
    expect_true(all(sapply(ultima_votacao_pec241, class) %in% .COLNAMES_ULTIMAVOTACAO))
  })

  test_that("fetch_proposicao_from_votacao()", {
    expect_true(all(sapply(proposicao_votacao7252, class) %in% .COLNAMES_PROP_VOTACAO))
  })
}

if(check_api()){
  test()
} else skip()
