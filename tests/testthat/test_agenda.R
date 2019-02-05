context("Agenda")

# Setup
setup <- function(){
  agenda <<- fetch_agenda_camara('2018-07-03', '2018-07-10')

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
  })

  test_that("fetch_agenda_camara()", {
    expect_true(all(sapply(agenda, class) %in% .COLNAMES_AGENDA))
  })
}

if(check_api()){
  test()
} else skip()
