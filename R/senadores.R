#' @title Fetches info of the senators
#' @description Returns its id, name, uf, gender.
#' @param legis_initial Initial legisture
#' @param legis_final Final legisture
#' @return Dataframe containing the senators info.
#' @examples
#' senators <- fetch_senadores(40, 56)
#' @rdname fetch_senadores
#' @export
fetch_senadores <- function(legis_initial, legis_final) {
  senator_data <- .senado_api(paste0(.SENADORES_PATH, legis_initial, "/", legis_final), asList = TRUE)
  senator_data <- senator_data$ListaParlamentarLegislatura$Parlamentares

  senator_ids <-
    senator_data %>%
    magrittr::extract2("Parlamentar")

  legislatura <-
    senator_ids %>%
    dplyr::mutate(casa = 'senado')

  legislatura <- legislatura %>%
    dplyr::select(id_parlamentar = IdentificacaoParlamentar.CodigoParlamentar,
                  nome_eleitoral = IdentificacaoParlamentar.NomeParlamentar,
                  nome_completo = IdentificacaoParlamentar.NomeCompletoParlamentar,
                  genero = IdentificacaoParlamentar.SexoParlamentar,
                  partido = IdentificacaoParlamentar.SiglaPartidoParlamentar,
                  uf = IdentificacaoParlamentar.UfParlamentar,
                  casa = casa) %>%
    .assert_dataframe_completo(.COLNAMES_LEGISLATURA_SENADORES) %>%
    .coerce_types(.COLNAMES_LEGISLATURA_SENADORES)
}
