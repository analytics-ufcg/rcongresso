context("Emendas")

setup <- function() {
  emendas <<- fetch_emendas_camara('pl', 6726, 2016)

  return(TRUE)
}

check_api <- function(){
  tryCatch(setup(), error = function(e){
    message(e)
    return(FALSE)
  })
}

test <- function(){
  test_that("Is dataframe", {
    expect_true(is.data.frame(emendas))
  })

  test_that("Not Empty", {
    expect_true(nrow(emendas) != 0)
  })

  test_that("fetch_autor_camara()", {
    expect_true(all(sapply(emendas, class) %in% .COLNAMES_EMENDAS_CAMARA))
  })
}

if(check_api()){
  test()
} else skip()
