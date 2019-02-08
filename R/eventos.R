#' @title Recupera os eventos (sessões/reuniões) de uma proposição na Câmara
#' @description Retorna um dataframe contendo o timestamp, o local e a descrição do evento
#' @param prop_id ID da proposição
#' @return Dataframe contendo o timestamp, o local e a descrição do evento.
#' @examples
#' fetch_eventos_camara(2121442)
#' @export
fetch_eventos_camara <- function(prop_id) {
    .get_from_url(base_url=.CAMARA_WEBSITE_LINK,
                  path=.EVENTOS_PROPOSICAO_CAMARA_PATH,
                  query=paste0('idProposicao=', 2121442))$content %>%
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

#' @title Recupera os últimos eventos (sessões/reuniões) de uma proposição na Câmara
#' @description Retorna um dataframe contendo o timestamp, o local e a descrição do evento
#' @param prop_id ID da proposição
#' @return Dataframe contendo o timestamp, o local e a descrição do evento.
#' @examples
#' get_latest_eventos_camara(2121442)
#' @export
get_latest_eventos_camara <- function(prop_id) {
    rcongresso::fetch_eventos_camara(prop_id) %>%
        dplyr::filter(timestamp <= lubridate::now())
}

#' @title Recupera os próximos eventos (sessões/reuniões) de uma proposição na Câmara
#' @description Retorna um dataframe contendo o timestamp, o local e a descrição do evento
#' @param prop_id ID da proposição
#' @return Dataframe contendo o timestamp, o local e a descrição do evento.
#' @examples
#' get_next_eventos_camara(2121442)
#' @export
get_next_eventos_camara <- function(prop_id) {
    rcongresso::fetch_eventos_camara(prop_id) %>%
        dplyr::filter(timestamp > lubridate::now())
}
