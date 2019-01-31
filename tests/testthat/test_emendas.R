context("Emendas")

# Setup
setup <- function(){
  emendas_unica <<- fetch_emendas(91341, 'senado')
  emendas_ausentes <<- fetch_emendas(126364, 'senado')
  emendas_variadas <<- fetch_emendas(133943, 'senado')
  
  emendas_camara <<- fetch_emendas(345311, 'camara')
  
  return(TRUE)
}

check_api <- function(){
  tryCatch(setup(), error = function(e){return(FALSE)})
}

test <- function(){
  # Testa erros
  test_that("GET proposição inexistente", {expect_error(fetch_emendas_senado(-1))})
  
  # Testes
  # Os nomes das colunas e os tipos estão definidos em colunas_constants.R
  test_that("Is tibble", {
    expect_true(tibble::is_tibble(emendas_unica))
    expect_true(tibble::is_tibble(emendas_ausentes))
    expect_true(tibble::is_tibble(emendas_variadas))
  })
  
  test_that("Empty", {
    expect_true(nrow(emendas_camara) == 0)
    expect_true(nrow(emendas_ausentes) == 0)
  })
  
  test_that("Not Empty", {
    expect_true(nrow(emendas_unica) != 0)
    expect_true(nrow(emendas_variadas) != 0)
  })
  
  test_that("Várias - fetch_emendas_senado()", {
    expect_true(all(sapply(emendas_variadas, class) %in% .COLNAMES_EMENDAS))
  })
}

if(check_api()){
  test()
} else skip()
