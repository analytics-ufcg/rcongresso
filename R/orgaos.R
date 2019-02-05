#' @title Fetches data about all Câmara's órgãos
#' @description Fetches a dataframe containing information about all Câmara's órgãos
#' @return A dataframe
#' @export
fetch_orgaos_camara <- function() {
  .camara_api(.ORGAOS_FILE_CAMARA_PATH) %>%
    .assert_dataframe_completo(.COLNAMES_ORGAOS) %>%
    .coerce_types(.COLNAMES_ORGAOS)
}

#' @title Fetches Câmara órgão
#' @description Fetches a dataframe containing information about the órgão requested
#' @param sigla Órgão's id
#' @return Returns a dataframe containing information about the órgão requested
#' @rdname fetch_orgao_camara
#' @export
fetch_orgao_camara <- function(sigla = NULL) {
    parametros <- as.list(environment(), all = TRUE)
    .camara_api(.ORGAOS_CAMARA_PATH, parametros) %>%
        .assert_dataframe_completo(.COLNAMES_ORGAO) %>%
        .coerce_types(.COLNAMES_ORGAO)
}
