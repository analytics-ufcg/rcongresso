context("Comissoes")

SIGLA_ID <<- c("ccjc", "cft", "cdc", "cdeics", "cctci", "cme", "capadr", "cpd")

APELIDO <<- c("CONSTITUIÇÃO E JUSTIÇA E DE CIDADANIA", "FINANÇAS E TRIBUTAÇÃO",
              "DEFESA DO CONSUMIDOR", "DESENVOLV. ECONÔMICO, INDÚSTRIA, COMÉRCIO E SERV.",
              "CIÊNCIA E TECNOLOGIA, COMUNICAÇÃO E INFORMÁTICA",
              "MINAS E ENERGIA", "AGRICULTURA, PECUÁRIA, ABASTECIMENTO DESENV. RURAL",
              "DEFESA DOS DIREITOS DAS PESSOAS COM DEFICIÊNCIA")

test_that('fetch_composicao_comissoes_camara() returns valid dataframe', {
  df_sigla_apelido <- tibble::tribble(~ sigla, ~ apelido, 
                                      SIGLA_ID, APELIDO) %>% 
                      tidyr::unnest()
  
  get_is_empty <- function(sigla, apelido) {
    nrow(fetch_composicao_comissoes_camara(sigla, apelido)) != 0
  }
  expect_true(
    all(purrr::map2(df_sigla_apelido$sigla, df_sigla_apelido$apelido,
                       ~ get_is_empty(.x, .y))))
})
