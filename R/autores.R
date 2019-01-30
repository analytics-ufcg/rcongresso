#' @title Fetches proposition's author
#' @description Fetches a dataframe containing information about the author of the proposition
#' @param proposicao_id Proposition's ID
#' @return A dataframe containing details about the author of the proposition
#' @examples
#' fetch_autor_camara(2121442)
#' @export
fetch_autor_camara <- function (proposicao_id = NULL) {
  autor_uri <- paste0(.PROPOSICOES_PATH, '/', proposicao_id, "/autores")
  .camara_api(autor_uri) %>%
    .assert_dataframe_completo(.COLNAMES_AUTORES) %>%
    .coerce_types(.COLNAMES_AUTORES)
}
