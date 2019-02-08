context("Agenda")

# Setup
setup <- function(){
  agenda <<- fetch_agenda_camara('2018-07-03', '2018-07-10')
  pauta <<-
    fetch_pauta_camara('53184', '2018-07-03T10:00', '2018-07-03T12:37', 'CVT', 'Comissão de Viação e Transportes VIAÇÃO E TRANSPORTES')

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
    expect_true(is.data.frame(agenda))
    expect_true(is.data.frame(pauta))
  })

  test_that("Have many rows", {
    expect_true(nrow(agenda) >= 480)
    expect_true(nrow(pauta) >= 8000)
  })

  test_that("fetch_agenda_camara()", {
    expect_true(all(sapply(agenda, class) %in% .COLNAMES_AGENDA))
    expect_true(all(sapply(pauta, class) %in% .COLNAMES_PAUTA_CAMARA))
  })
}

if(check_api()){
  test()
} else skip()
