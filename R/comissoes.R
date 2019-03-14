#' @title Fetches comissions on Champber
#' @description Fetches a dataframe containing information about the comission requested
#' @param sigla Comissao's id
#' @return Returns a dataframe containing information about the comission requested
#' fetch_composicao_comissoes_camara('cmads')
fetch_composicao_comissoes_camara <- function(sigla_comissao) {
  orgaos_camara <-
    fetch_orgaos_camara() %>%
    dplyr::mutate_all(as.character) %>%
    dplyr::filter(trimws(sigla) == toupper(sigla_comissao)) %>%
    dplyr::select(uri) %>% head(1)
  
  column_names <- names(.COLNAMES_COMPOSICAO_CAMARA)
  df <- data.frame(matrix(ncol = length(column_names), nrow = 0))
  
  if (nrow(orgaos_camara) == 0) {
    warning("Comissão não encontrada")
  } else {
    id <- stringr::str_extract(orgaos_camara[[1]], '\\d+(?!.*\\d)')
    eventos_list <- .get_from_url(.CAMARA_WEBSITE_LINK, .COMPOSICAO_CAMARA_PATH, paste0('IDOrgao=', id))$content %>% 
      rawToChar() %>%
      XML::xmlParse() %>%
      XML::xmlToList()
    
    df <-
      eventos_list %>%
      jsonlite::toJSON() %>%
      jsonlite::fromJSON() %>%
      magrittr::extract2('membros') %>%
      tibble::as.tibble() %>%
      t() %>%
      as.data.frame() %>%
      tibble::rownames_to_column("VALUE")
  }
 
  names(df) <- names(.COLNAMES_COMPOSICAO_CAMARA)
  df %>%
    rowwise() %>%
    dplyr::mutate(partido = ifelse(length(partido) == 0, "", partido)) %>%
    dplyr::mutate(uf = ifelse(length(uf) == 0, "", uf)) %>%
    dplyr::mutate(id = ifelse(length(id) == 0, "", id)) %>%
    tidyr::unnest() %>%
    dplyr::arrange(nome) %>% 
    .assert_dataframe_completo(.COLNAMES_COMPOSICAO_CAMARA) %>%
    .coerce_types(.COLNAMES_COMPOSICAO_CAMARA)
}