#' @title Retorna as emendas de uma proposição na Camara
#' @description Retorna dataframe com os dados das emendas de uma proposição na Camara
#' @param id ID de uma proposição da Camara
#' @param sigla Sigla da proposição
#' @param numero Numero da proposição
#' @param ano Ano da proposição
#' @return Dataframe com as informações sobre as emendas de uma proposição na Camara
#' @examples
#' fetch_emendas_camara('pl', 490, 2007)
fetch_emendas_camara <- function(sigla=NULL, numero=NULL, ano=NULL) {
  emendas_substitutivos_redacaofinal_list <-
    .get_from_url(base_url = .CAMARA_WEBSITE_LINK,
                  path = .EMENDAS_SUBSTITUTIVOS_REDACAOFINAL_CAMARA_PATH,
                  query = paste0('tipo=', sigla, '&numero=', numero, '&ano=', ano))$content %>%
    rawToChar() %>%
    XML::xmlParse() %>%
    XML::xmlToList()

  emendas_df <-
    emendas_substitutivos_redacaofinal_list %>%
    magrittr::extract2('Emendas') %>%
    purrr::map_dfr(as.list)

  emendas_df %>%
    .assert_dataframe_completo(.COLNAMES_EMENDAS_CAMARA) %>%
    .coerce_types(.COLNAMES_EMENDAS_CAMARA)
}
