context("Utils")

setup <- function(){
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
  test_that("Underscore - .to_underscore",{
    expect_equal(.to_underscore(
      c("A.a", "A_.a", "AAA", ".aA", "a.a", "A_a", "1Aa", "a1A")), 
      c("a_a", "a__a", "aaa", "_a_a", "a_a", "a_a", "1aa", "a1a"))
  })
}

if(check_api()){
  test()
} else skip()

