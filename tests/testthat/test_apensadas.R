context("Apensadas")

setup <- function() {
  apensadas <<- fetch_apensadas_camara(345311)

  return(TRUE)
}

check_api <- function(){
  tryCatch(setup(), error = function(e){
    message(e)
    return(FALSE)
  })
}

test <- function(){
  test_that("Is a vector", {
    expect_true(is.vector(apensadas))
  })

  test_that("Not Empty", {
    expect_true(length(apensadas) > 0)
  })
}

if(check_api()){
  test()
} else skip()
