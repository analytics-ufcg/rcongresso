context("Proposições")

setup <- function(){
  pec_241 <<- fetch_proposicao(siglaTipo = "PEC", numero = 241, ano = 2016, dataInicio = "2016-01-01")
  pec_241_id <<- fetch_id_proposicao("PEC", 241, 2016)
  pec_241_por_id <<- fetch_proposicao(pec_241_id)
  votacoes_pec_241 <<- fetch_votacoes(pec_241_id)
  relacionadas_pec_241 <<- fetch_relacionadas(pec_241_id)

  return(TRUE)
}

# Setup
check_api <- function(){
  tryCatch(setup(), error = function(e){
    message(e)
    return(FALSE)
    })
}

test <- function(){
  # Constantes
  PEC241_ID <- 2088351
  TIPO_PROP_PEC <- 136
  TIPO_PROP_PL <- 139
  TAM_DF_10REQ <- c(10, 7)
  TAM_DF_100REQ <- c(100, 7)
  TAM_DF_386REQ <- c(386, 7)
  TAM_DF_DEFAULT <- c(15, 7)

  # Testa erros
  expect_that(fetch_id_proposicao(tipo = "AAA", numero = -1, ano = 1870), gives_warning(.WARNING_PROPOSICAO_ID))
  test_that("GET ID de proposição inexistente", {expect_equal(suppressWarnings(fetch_id_proposicao(tipo = "AAA", numero = -1, ano = 1870)), NULL)})
  test_that("GET proposição com ID inexistente", {expect_error(fetch_proposicao(id = -1))})

  # Testes
  # Os nomes das colunas e os tipos estão definidos em colunas_constants.R
  test_that("Is dataframe", {
    expect_true(is.data.frame(pec_241))
    expect_true(is.data.frame(pec_241_por_id))
    expect_true(is.data.frame(votacoes_pec_241))
  })

  test_that("Not Empty", {
    expect_true(nrow(pec_241) != 0)
    expect_true(nrow(pec_241_por_id) != 0)
    expect_true(nrow(votacoes_pec_241) != 0)
  })

  test_that("fetch_proposicao()", {
    expect_true(all(sapply(pec_241, class) %in% .COLNAMES_PROPOSICAO))
  })

  test_that("fetch_proposicao() usando ID", {
    expect_true(all(sapply(pec_241_por_id, class) %in% .COLNAMES_PROPOSICAO_POR_ID))
  })

  test_that("fetch_votacoes()", {
    expect_true(all(sapply(votacoes_pec_241, class) %in% .COLNAMES_VOTACOES))
  })

  test_that("fetch_relacionadas()", {
    expect_true(all(sapply(relacionadas_pec_241, class) %in% .COLNAMES_RELACIONADAS))
  })

  test_that("ID Correto", {expect_equal(pec_241_id, fetch_id_proposicao("PEC", 241, 2016))})

  # Existe um idTipo que identifica os tipos de proposição em tramitação/votação na
  #    Câmara. Nesse caso, 136 é PEC.
  test_that("ID PEC241", {expect_equal(pec_241_id, PEC241_ID)})
  test_that("Tipo PEC241=='PEC'", {expect_equal(pec_241$idTipo, TIPO_PROP_PEC)})
  test_that("Numero PEC241", {expect_equal(pec_241$numero, 241)})
  test_that("Ano PEC241", {expect_equal(pec_241$ano, 2016)})

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
}

if(check_api()){
  test()
} else skip()
