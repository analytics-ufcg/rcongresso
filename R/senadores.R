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

  senators <-
    senator_data %>%
    magrittr::extract2("Parlamentar")

  legislatura <-
    senators %>%
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

  legislatura
}

#' @title Fetches senators id
#' @description Returns its id.
#' @param legis_initial Initial legisture
#' @param legis_final Final legisture
#' @return Dataframe containing the senators id.
#' @examples
#' senators <- fetch_ids_senadores(40, 56)
#' @rdname fetch_ids_senadores
#' @export
fetch_ids_senadores <- function(legis_initial, legis_final) {
  senator_data <- .senado_api(paste0(.SENADORES_PATH, legis_initial, "/", legis_final), asList = TRUE)
  senator_data <- senator_data$ListaParlamentarLegislatura$Parlamentares

  senators <-
    senator_data %>%
    magrittr::extract2("Parlamentar") %>%
    dplyr::distinct(id = IdentificacaoParlamentar.CodigoParlamentar)

  senators
}

#' @title Fetches senator's info
#' @description Returns its info, as name, gender, uf, situation, etc.
#' @param id Senator's id
#' @return Dataframe containing the senator's info.
#' @examples
#' senator <- fetch_senador(5573)
#' @rdname fetch_senador
#' @export
fetch_senador <- function(id = NULL) {

  if (is.null(id)) {
    warning("O parâmetro id deve ser passado.")
  } else {
    senator_data <- .senado_api(paste0(.SENADOR_PATH, id), asList = TRUE)
    senator_data <- senator_data$DetalheParlamentar$Parlamentar

    senator <- .create_senator_dataframe(senator_data) %>%
    .assert_dataframe_completo(.COLNAMES_SENADORES_INFO) %>%
    .coerce_types(.COLNAMES_SENADORES_INFO)

    senator
  }

}

#' @title Fetches details abaout all senators
#' @description Fetches details about senators from a given list
#' @param ids_dep Dataframe containing all senators IDs
#' @return Dataframe containing details about the senators
#' @rdname fetch_all_senadores
#' @export
fetch_all_senadores <- function(ids_sen) {
  senadores <- tibble::tibble()

  if (is.null(dim(ids_sen)) | !is.data.frame(ids_sen) ) {
    warning("Objeto deve ser um dataframe nao-nulo")
  } else if (nrow(ids_sen) == 0) {
    warning("Dataframe vazio")
  } else {
    senadores <- purrr::map_df(ids_sen$id, ~ fetch_senador(.x))
  }

  senadores
}
