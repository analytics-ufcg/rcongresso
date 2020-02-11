context("Agenda")

# Setup
setup <- function(){
  agenda_camara <<- fetch_agenda_camara('2018-07-03', '2018-07-10')
  pauta_camara <<-
    fetch_pauta_camara('53184', '2018-07-03T10:00', '2018-07-03T12:37', 'CVT', 'Comissão de Viação e Transportes VIAÇÃO E TRANSPORTES')

  lista_agenda_senado <<- fetch_agenda_senado("2018-07-03")
  agenda_senado <<- lista_agenda_senado$agenda
  materias_senado <<- lista_agenda_senado$materias
  oradores_senado <<- lista_agenda_senado$oradores
  agenda_comissoes_senado <<- fetch_agenda_senado_comissoes("2016-05-01", "2016-05-25")
  agenda_comissoes_senado_1 <<- fetch_agenda_senado_comissoes("2019-01-10", "2019-01-17")
  agenda_comissoes_senado_2 <<- fetch_agenda_senado_comissoes("2018-08-13", "2018-08-17")
  agenda_comissoes_senado_3 <<- fetch_agenda_senado_comissoes("2018-10-22", "2018-10-26")

  return(TRUE)
}

check_api <- function(){
  tryCatch(setup(), error = function(e){
    message(e)
    return(FALSE)
  })
}

test <- function(){
  # Testes
  # Os nomes das colunas e os tipos estão definidos em colunas_constants.R
  test_that("Is dataframe", {
    expect_true(is.data.frame(agenda_camara))
    expect_true(is.data.frame(pauta_camara))
  })

  test_that("Have many rows", {
    expect_true(nrow(agenda_camara) >= 100)
    expect_true(nrow(pauta_camara) >= 80)
  })

  test_that("fetch_agenda_camara()", {
    expect_true(all(sapply(agenda_camara, class) %in% .COLNAMES_AGENDA_CAMARA))
    expect_true(all(sapply(pauta_camara, class) %in% .COLNAMES_PAUTA_CAMARA))
  })

  test_that("Is list", {
    expect_true(is.list(lista_agenda_senado))
  })

  test_that("Is dataframe", {
    expect_true(is.data.frame(agenda_senado))
    expect_true(is.data.frame(materias_senado))
    expect_true(is.data.frame(oradores_senado))
    expect_true(is.data.frame(agenda_comissoes_senado))
    expect_true(is.data.frame(agenda_comissoes_senado_1))
    expect_true(is.data.frame(agenda_comissoes_senado_2))
    expect_true(is.data.frame(agenda_comissoes_senado_3))
  })

  test_that("fetch_agenda_camara()", {
    expect_true(all(sapply(agenda_senado, class) %in% .COLNAMES_AGENDA_SENADO))
    expect_true(all(sapply(materias_senado, class) %in% .COLNAMES_AGENDA_MATERIA_SENADO))
    expect_true(all(sapply(oradores_senado, class) %in% .COLNAMES_AGENDA_ORADORES_SENADO))
  })

  test_that("Have many rows", {
    expect_true(nrow(agenda_comissoes_senado) >= 100)
  })

}

if(check_api()){
  test()
} else skip()
