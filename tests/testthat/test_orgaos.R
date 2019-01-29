context("Orgãos")

setup <- function(){
  orgaos_camara <<- fetch_orgaos_camara()
  comissao_capadr <<- fetch_orgao_camara("CAPADR")

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
  test_that("Is dataframe", {
    expect_true(is.data.frame(orgaos_camara))
  })

  test_that("Have many rows", {
    expect_true(nrow(orgaos_camara) >= 5000)
  })

  test_that("Is dataframe", {
    expect_true(is.data.frame(comissao_capadr))
  })

  test_that("Not Empty", {
    expect_true(nrow(comissao_capadr) != 0)
  })
}

if(check_api()){
  test()
} else skip()
