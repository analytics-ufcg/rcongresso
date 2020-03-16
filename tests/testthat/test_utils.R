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

test_that(".get_with_exponential_backoff_cached returns NULL with warning when base_URL is null", {
  suppressWarnings(expect_null(.get_with_exponential_backoff_cached(base_url=NULL)))
  expect_warning(.get_with_exponential_backoff_cached(base_url=NULL))
})

test_that(".get_with_exponential_backoff_cached returns NULL with warning when base_URL is empty", {
  suppressWarnings(expect_null(.get_with_exponential_backoff_cached(base_url='')))
  expect_warning(.get_with_exponential_backoff_cached(base_url=''))
})

test_that(".get_with_exponential_backoff_cached returns error when cannot obtain requested data", {
  expect_error(expect_warning(.get_with_exponential_backoff_cached(base_url="https://dadosabertos.camara.leg.br",
                                                    path='/api/v2/proposicoes/-1',
                                                    base_sleep_time=1,
                                                    max_attempts=1)))
})

test_that(".get_with_exponential_backoff_cached returns json when accept_json is set TRUE", {
  prop_data_resp <- .get_with_exponential_backoff_cached(base_url="https://dadosabertos.camara.leg.br",
                                       path='/api/v2/proposicoes/256171',
                                       base_sleep_time=1,
                                       max_attempts=1,
                                       accept_json=TRUE)
  expect_true(httr::http_type(prop_data_resp) == "application/json")
})

test_that(".unnest_df_column returns correct result", {
  bill_id <- 102721
  json_emendas <- .senado_api(path = paste0(.SENADO_PATH, .EMENDAS_SENADO_PATH, bill_id), asList = TRUE)
  
  base_emendas_cols <- c("CodigoEmenda", "NumeroEmenda", "DataApresentacao", 
                         "ColegiadoApresentacao", "DescricaoTurno", "DescricaoTipoEmenda")
  
  emendas_raw_df <- 
    json_emendas %>%
    magrittr::extract2("EmendaMateria") %>%
    magrittr::extract2("Materia") %>%
    magrittr::extract2("Emendas") %>%
    magrittr::extract2("Emenda")
  
  textos_emendas <- .unnest_df_column(emendas_raw_df,base_emendas_cols,'TextosEmenda.TextoEmenda')
  autoria_emendas <- .unnest_df_column(emendas_raw_df,base_emendas_cols,'AutoriaEmenda.Autor')
  decisao_emendas <-.unnest_df_column(emendas_raw_df,base_emendas_cols,'Decisoes.Decisao')
  
  expect_true(nrow(textos_emendas) == nrow(emendas_raw_df))
  expect_true(nrow(autoria_emendas) == nrow(emendas_raw_df))
  expect_true(nrow(decisao_emendas) == nrow(emendas_raw_df))
  
  expect_equal(stringr::str_count(decisao_emendas$Descricao, ';'), as.integer(c(3,3)))
})

test_that(".get_with_exponential_backoff_cached does not apply exponential backoff when response code is 404", {
  start_time <- Sys.time()
  tryCatch({
    .get_with_exponential_backoff_cached(base_url="https://dadosabertos.camara.leg.br",
                                         path='/api/v2/proposicoes/-1',
                                         base_sleep_time=1,
                                         max_attempts=1)
  }, warning = function(w) {
  }, error = function(e) {
  }, finally = {
    end_time <- Sys.time()  
  })
  
  elapsed_time <- end_time - start_time
  
  expect_true(elapsed_time < 3)
})
