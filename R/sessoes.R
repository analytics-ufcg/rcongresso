#' @title Fetches all the sessions when a proposition was/is going to be discussed in a Congress House
#' @description Returns all the sessions when a proposition was/is going to be discussed in a Congress House.
#' @param id_prop Proposition's ID
#' @param casa Proposition's casa
#' @return Dataframe containing all the sessions.
#' @examples
#' sessoes_pec_241_2016 <- fetch_sessoes(2088351,'camara')
#' sessoes_lei_qualidade_fiscal <- fetch_sessoes(91341,'senado')
#' @rdname fetch_sessoes
#' @export
fetch_sessoes <- function(id_prop, casa) {
  sessoes <- tibble::tibble()
  if (tolower(casa) == 'camara') {
    sessoes <- fetch_sessoes_camara(id_prop)
  } else if (tolower(casa) == 'senado') {
    sessoes <- fetch_sessoes_senado(id_prop)
  } else {
    print('casa param is missing.')
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
  
  sessoes <- 
    .senado_api(paste0(.SENADO_SESSOES_PATH, id_prop), asList = T) %>%
    magrittr::extract2("OrdiaMateria") %>%
    magrittr::extract2("Materia") %>%
    magrittr::extract2("OrdensDoDia") %>%
    magrittr::extract2("OrdemDoDia") %>%
    magrittr::extract2("SessaoPlenaria") %>%
    purrr::map_df( ~ .) %>%
    tidyr::unnest() %>%
    rename_table_to_underscore()
  
  sessoes %>%
    .assert_dataframe_completo(.COLNAMES_SESSOES_SENADO) %>%
    .coerce_types(.COLNAMES_SESSOES_SENADO)
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
  
  sessoes <- .get_from_url(base_url = .CAMARA_WEBSITE_LINK, 
                               path = .CAMARA_SESSOES_PATH, 
                               query = paste0("idProposicao=", id_prop)) %>%
    xml2::read_html() %>%
    rvest::html_nodes(xpath = '//*[@id="content"]/table') %>%
    rvest::html_table()
  
  if(length(sessoes) != 0) {
    sessoes_df <- 
      sessoes[[1]] %>%
      dplyr::select(-"Links") %>%
      magrittr::set_colnames(names(.COLNAMES_SESSOES_CAMARA)) %>%
      dplyr::mutate(timestamp = lubridate::dmy_hm(timestamp)) %>%
      .assert_dataframe_completo(.COLNAMES_SESSOES_CAMARA) %>%
      .coerce_types(.COLNAMES_SESSOES_CAMARA) 
  }
  else {
    sessoes_df <- data.frame()
  }
  
  return(sessoes_df)
}
