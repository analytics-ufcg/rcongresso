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
    expect_true(nrow(agenda_camara) >= 480)
    expect_true(nrow(pauta_camara) >= 8000)
  })

  test_that("fetch_agenda_camara()", {
    expect_true(all(sapply(agenda_camara, class) %in% .COLNAMES_AGENDA))
    expect_true(all(sapply(pauta_camara, class) %in% .COLNAMES_PAUTA_CAMARA))
  })

  test_that("Is list", {
    expect_true(is.list(lista_agenda_senado))
  })

  test_that("Is dataframe", {
    expect_true(is.data.frame(agenda_senado))
    expect_true(is.data.frame(materias_senado))
    expect_true(is.data.frame(oradores_senado))
  })

  test_that("fetch_agenda_camara()", {
    expect_true(all(sapply(agenda_senado, class) %in% .COLNAMES_AGENDA_SENADO))
    expect_true(all(sapply(materias_senado, class) %in% .COLNAMES_AGENDA_MATERIA_SENADO))
    expect_true(all(sapply(oradores_senado, class) %in% .COLNAMES_AGENDA_ORADORES_SENADO))
  })
}

if(check_api()){
  test()
} else skip()
