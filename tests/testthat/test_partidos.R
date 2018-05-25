context("Partidos")

setup <- function(){
  # Setup
  PARTIDOS <<- c("PT", "PMDB", "PSDB", "DEM")
  PARTIDOS_ERRO <<- c("PT", "PMBD", "PS", "A")
  TAM_LISTA_PARTIDOS <<- 4
  TAM_LISTA_PAR_ERR <<- 1
  TAM_DF_DEFAULT <<- c(15, 4)

  partido_info <<- fetch_partido(sigla="PT")
  partidos_id <<- fetch_id_partido(PARTIDOS)
  partido_info_id <<- fetch_partido(id=36844)

  return(TRUE)
}

check_api <- function(){
  tryCatch(setup(), error = function(e){return(FALSE)})
}

test <- function(){
  # Testa erros
  test_that("GET partido inexistente", {
    expect_error(fetch_partido(-1))
  })
  test_that("Warnings de siglas incorretas",{
    expect_that(fetch_id_partido(PARTIDOS_ERRO), gives_warning(.WARNING_SIGLA_PARTIDO))
    expect_equal(suppressWarnings(length(fetch_id_partido(PARTIDOS_ERRO))), TAM_LISTA_PAR_ERR)
  })

  # Testes
  # Os nomes das colunas e os tipos estão definidos em colunas_constants.R
  test_that("Is dataframe", {
    expect_true(is.data.frame(partido_info))
    expect_true(is.data.frame(partido_info_id))
  })

  test_that("Is vector",{
    expect_true(is.vector(partidos_id))
  })

  test_that("Not Empty", {
    expect_true(nrow(partido_info) != 0)
    expect_true(nrow(partido_info_id) != 0)
    expect_true(length(partidos_id) != 0)
  })

  test_that("fetch_partido() usando ID", {
    expect_true(all(sapply(partido_info_id, class) %in% .COLNAMES_PARTIDOS_ID))
  })

  test_that("fetch_partido() usando queries", {
    expect_true(all(sapply(partido_info, class) %in% .COLNAMES_PARTIDOS))
  })

  test_that("Quantidade default por requisição, atualmente 15",{
    expect_equal(dim(fetch_partido()), TAM_DF_DEFAULT)
  })

  test_that("Quantidade de ids retornados - fetch_id_partido",{
    expect_equal(length(partidos_id), TAM_LISTA_PARTIDOS)
  })
}

if(check_api()){
  test()
} else skip()

