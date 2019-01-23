
#' @title Fetches all the sessions when a proposition was/is going to be discussed in a Congress House
#' @description Returns all the sessions when a proposition was/is going to be discussed in a Congress House.
#' @param id_prop Proposition's ID
#' @return Dataframe containing all the sessions.
#' @examples
#' sessoes_pec_241_2016 <- fetch_sessoes(2088351,'camara')
#' sessoes_lei_qualidade_fiscal <- fetch_sessoes(91341,'senado')
#' @rdname fetch_sessoes
#' @export
fetch_sessoes <- function(id_prop, casa) {
  sessoes <- tibble::tibble()
  
  casa <- tolower(casa)
  if (casa == 'camara') {
    sessoes <- fetch_sessoes_camara(id_prop)
  } else if (casa == 'senado') {
    sessoes <- fetch_sessoes_senado(id_prop)
  } else {
    print('Parâmetro "casa" não identificado.')
    return()
  }
  
  return(sessoes)
}



#' @title Fetches all the sessions when a proposition was/is going to be discussed in the Senate
#' @description Returns all the sessions when a proposition was/is going to be discussed in the Senate.
#' @param id_prop Proposition's ID
#' @return Dataframe containing all the sessions.
#' @examples
#' sessoes_lei_qualidade_fiscal <- fetch_sessoes_senado(91341)
#' @rdname fetch_sessoes_senado
#' @export
fetch_sessoes_senado <- function(id_prop) {
  url_base_sessions <-
    "http://legis.senado.leg.br/dadosabertos/materia/ordia/"
  url <- paste0(url_base_sessions, id_prop)
  
  json_sessions <- jsonlite::fromJSON(url, flatten = T)
  
  sessions_data <- json_sessions %>%
    magrittr::extract2("OrdiaMateria") %>%
    magrittr::extract2("Materia")
  
  ordem_do_dia_df <- sessions_data %>%
    magrittr::extract2("OrdensDoDia") %>%
    magrittr::extract2("OrdemDoDia") %>%
    magrittr::extract2("SessaoPlenaria") %>%
    purrr::map_df( ~ .) %>%
    tidyr::unnest() %>%
    rename_table_to_underscore()
  
  ordem_do_dia_df
}

#' @title Fetches all the sessions when a proposition was/is going to be discussed in the Chamber of Deputies
#' @description Returns all the sessions when a proposition was/is going to be discussed in the Chamber of Deputies.
#' @param id_prop Proposition's ID
#' @return Dataframe containing all the sessions.
#' @examples
#' sessoes_pec_241_2016 <- fetch_sessoes_camara(2088351)
#' @rdname fetch_sessoes_camara
#' @export
fetch_sessoes_camara <- function(id_prop) {
  events_base_url <-
    'http://www.camara.gov.br/proposicoesWeb/sessoes_e_reunioes?idProposicao='
  bill_events_url <- paste0(events_base_url, id_prop)
  events <- bill_events_url %>%
    xml2::read_html() %>%
    rvest::html_nodes(xpath = '//*[@id="content"]/table') %>%
    rvest::html_table()
  events_df <- events[[1]]
  names(events_df) <- c('timestamp', 'origem', 'descricao', 'links')
  events_df %>%
    dplyr::select(-links) %>%
    dplyr::mutate(timestamp = lubridate::dmy_hm(timestamp))
}
