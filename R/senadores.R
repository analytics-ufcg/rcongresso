fetch_legislatura <- function(legis_initial, legis_final) {
  senator_data <- .senado_api(paste0(.SENADORES_PATH, legis_initial, "/", legis_final), asList = TRUE)
  senator_data <- senator_data$ListaParlamentarLegislatura$Parlamentares

  senator_ids <-
    senator_data %>%
    magrittr::extract2("Parlamentar")

  mandatos <-
    senator_ids %>%
    dplyr::select("Mandatos.Mandato") %>%
    dplyr::transmute(mandato = Mandatos.Mandato)

  #a <- mandatos$mandato[[2]]$UfParlamentar

  #mandatos <-
  #  dplyr::mutate(uf = purrr::map(mandatos$mandato[[.auxiliary_cont(0)]]$UfParlamentar, ~.auxiliary_ufParlamentar(.x)))

  #a <-
  #  mandatos %>%
  #  dplyr::mutate(uf = purrr::map(numero_da_coluna, ~))

  legislatura <-
    senator_ids %>%
    dplyr::mutate(casa = 'senado') %>%
    dplyr::select(id_parlamentar = IdentificacaoParlamentar.CodigoParlamentar,
                    nome_eleitoral = IdentificacaoParlamentar.NomeParlamentar,
                    nome_completo = IdentificacaoParlamentar.NomeCompletoParlamentar,
                    genero = IdentificacaoParlamentar.SexoParlamentar,
                    partido = IdentificacaoParlamentar.SiglaPartidoParlamentar,
                    uf = IdentificacaoParlamentar.UfParlamentar)

  return(legislatura)
}

#.auxiliary_ufParlamentar <- function(ufParlamentar) {
#  return(ufParlamentar)
#}

#.auxiliary_cont <- function(cont) {
#  cont = cont + 1
#  return(cont)
#}
