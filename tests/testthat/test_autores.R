context("Autores")

setup <- function() {
  autor_info <<- fetch_autor_camara(345311)

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
    expect_true(is.data.frame(autor_info))
  })

  test_that("Not Empty", {
    expect_true(nrow(autor_info) != 0)
  })

  test_that("fetch_autor_camara()", {
    expect_true(all(sapply(autor_info, class) %in% .COLNAMES_AUTORES))
  })
}

if(check_api()){
  test()
} else skip()
