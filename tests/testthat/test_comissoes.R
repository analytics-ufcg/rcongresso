context("Comissões")

setup <- function() {
  comissao_capadr <<- fetch_comissoes_camara("CAPADR")
  
  return(TRUE)
}

check_api <- function(){
  tryCatch(setup(), error = function(e){return(FALSE)})
}

test <- function(){
  test_that("GET comissão inexistente", {expect_true(nrow(fetch_comissoes_camara(-1)) == 0)})
  
  test_that("Is dataframe", {
    expect_true(is.data.frame(comissao_capadr))
  })
  
  test_that("Not Empty", {
    expect_true(nrow(comissao_capadr) != 0)
  })
  
  test_that("fetch_comissoes_camara()", {
    expect_true(all(sapply(comissao_capadr, class) %in% .COLNAMES_COMISSOES))
  })
}

if(check_api()){
  test()
} else skip()
