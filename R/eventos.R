#' @title Fetch events about a proposition in the Câmara
#' @description Returns a dataframe with timestamp, origen and description about each event
#' @param prop_id Proposition ID
#' @return Dataframe
#' @examples
#' fetch_eventos_camara(2121442)
#' @export
fetch_eventos_camara <- function(prop_id) {
  .get_with_exponential_backoff_cached(base_url=.CAMARA_WEBSITE_LINK,
                  path=.EVENTOS_PROPOSICAO_CAMARA_PATH,
                  query=paste0('idProposicao=', prop_id))$content %>%
      xml2::read_html() %>%
      rvest::html_nodes(xpath = '//*[@id="content"]/table') %>%
      rvest::html_table() %>%
      .[[1]] %>%
      dplyr::select(-Links) %>%
      dplyr::rename(
                 timestamp="Data e hora",
                 origem="Origem",
                 # O R check só aceita ASCII no código... Então a palavra
                 # "Descrição" foi codificada abaixo:
                 descricao="Descri\u00e7\u00e3o") %>%
      dplyr::mutate(timestamp=lubridate::dmy_hm(timestamp))
}

#' @title Get the last events about a proposition in the Câmara
#' @description Returns a dataframe with timestamp, origen and description about each event
#' @param prop_id Proposition ID
#' @return Dataframe
#' @examples
#' get_latest_eventos_camara(2121442)
#' @export
get_latest_eventos_camara <- function(prop_id) {
    rcongresso::fetch_eventos_camara(prop_id) %>%
        dplyr::filter(timestamp <= lubridate::now())
}

#' @title Get the next events about a proposition in the Câmara
#' @description Returns a dataframe with timestamp, origen and description about each event
#' @param prop_id Proposition ID
#' @return Dataframe
#' @examples
#' get_next_eventos_camara(2121442)
#' @export
get_next_eventos_camara <- function(prop_id) {
    rcongresso::fetch_eventos_camara(prop_id) %>%
        dplyr::filter(timestamp > lubridate::now())
}
