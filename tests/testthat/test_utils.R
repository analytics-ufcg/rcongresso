context("Utils")

test_that("Underscore - .to_underscore",{
  expect_equal(.to_underscore(
    c("A.a", "A_.a", "AAA", ".aA", "a.a", "A_a", "1Aa", "a1A")),
    c("a_a", "a__a", "aaa", "_a_a", "a_a", "a_a", "1aa", "a1a"))
})

test_that(".coerce_types", {
  df <- tibble::tribble(~ a, ~ b,
                        "a", "b")
  .COLNAMES_TESTE_1 <- c("a"="character", "b"="character", "c"="character")
  .COLNAMES_TESTE_2 <- c("a"="character")
  .COLNAMES_TESTE_3 <- c("a"="integer", "b"="integer")
  expect_true(
    is.data.frame(df %>% 
                    .assert_dataframe_completo(.COLNAMES_TESTE_1) %>%
                              .coerce_types(.COLNAMES_TESTE_1)))
  expect_true(
    is.data.frame(df %>% 
                    .assert_dataframe_completo(.COLNAMES_TESTE_2) %>%
                    .coerce_types(.COLNAMES_TESTE_2)))
  expect_true(
    is.data.frame(df %>% 
                    .assert_dataframe_completo(.COLNAMES_TESTE_3) %>%
                    .coerce_types(.COLNAMES_TESTE_3)))
})
