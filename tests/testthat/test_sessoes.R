context("Sessões")

# Setup
setup <- function(){
  sessoes_pec_241_2016 <<- fetch_sessoes(2088351, 'camara')
  sessoes_lei_qualidade_fiscal <<- fetch_sessoes(91341, 'senado')
  
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
    expect_true(is.data.frame(sessoes_pec_241_2016))
    expect_true(is.data.frame(sessoes_lei_qualidade_fiscal))
  })
  
  test_that("fetch_sessoes()", {
    expect_true(all(sapply(sessoes_pec_241_2016, class) %in% .COLNAMES_SESSOES_CAMARA))
    expect_true(all(sapply(sessoes_lei_qualidade_fiscal, class) %in% .COLNAMES_SESSOES_SENADO))
  })
}

if(check_api()){
  test()
} else skip()
