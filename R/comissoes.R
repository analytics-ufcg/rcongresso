#' @title Fetches camara comissao
#' @description Fetches a dataframe containing information about the comissao requested
#' @param sigla Comissao's id
#' @return Returns a dataframe containing information about the comissao requested
#' @rdname fetch_comissao_camara
#' @export
fetch_comissao_camara <- function(sigla = NULL) {
  parametros <- as.list(environment(), all = TRUE)
  .camara_api(.ORGAOS_CAMARA_PATH, parametros) %>%
    .assert_dataframe_completo(.COLNAMES_COMISSOES) %>%
    .coerce_types(.COLNAMES_COMISSOES)
} 