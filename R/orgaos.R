#' @title Baixa os órgãos na Câmara
#' @description Retorna um dataframe contendo os órgãos da câmara
#' @return Dataframe contendo os órgãos da Câmara
fetch_orgaos_camara <- function() {
  .camara_api(.ORGAOS_FILE_CAMARA_PATH) %>%
    tibble::as.tibble() %>%
    dplyr::rename(tipo_orgao_id=idTipoOrgao, descricao=nome)
}
