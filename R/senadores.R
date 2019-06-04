fetch_legislatura <- function(legis_initial, legis_final) {
  senator_data <- .senado_api(paste0(.SENADORES_PATH, legis_initial, "/", legis_final), asList = TRUE)
  senator_data <- senator_data$ListaParlamentarLegislatura$Parlamentares

  senator_ids <-
    senator_data %>%
    magrittr::extract2("Parlamentar") %>%
    tibble::tibble()

  mandatos <-
    senator_data %>%
    magrittr::extract2("Parlamentar") %>%
    magrittr::extract2("Mandatos")

  legislatura <-
    senator_ids %>%
    dplyr::select(id_parlamentar = IdentificacaoParlamentar.CodigoParlamentar,
                    nome_eleitoral = IdentificacaoParlamentar.NomeParlamentar,
                    nome_completo = IdentificacaoParlamentar.NomeCompletoParlamentar,
                    genero = IdentificacaoParlamentar.SexoParlamentar,
                    casa = 'senado',
                    partido = IdentificacaoParlamentar.SiglaPartidoParlamentar,
                    uf = IdentificacaoParlamentar.UfParlamentar)


  return(legislatura)
}
