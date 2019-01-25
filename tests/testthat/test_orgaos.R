context("Orgãos")

setup <- function(){
  orgaos_camara <<- fetch_orgaos_camara()

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
  # Testes
  test_that("Is dataframe", {
    expect_true(is.data.frame(orgaos_camara))
  })

  test_that("Have many rows", {
    expect_true(nrow(orgaos_camara) >= 5000)
  })
}

if(check_api()){
  test()
} else skip()
