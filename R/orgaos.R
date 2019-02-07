#' @title Fetches data about all Câmara's órgãos
#' @description Fetches a dataframe containing information about all Câmara's órgãos
#' @return A dataframe
#' @export
fetch_orgaos_camara <- function() {
  a <- .camara_api(.ORGAOS_FILE_CAMARA_PATH) %>%
    dplyr::mutate(idOrgao = str_extract(uri, "([0-9]+)$")) %>%
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

#' @title Baixa dados da agenda de um orgão da Camara
#' @description Retorna um dataframe contendo dados sobre a agenda de um orgão da camara
#' @param orgao_id ID do orgão
#' @param initial_date data inicial no formato dd/mm/yyyy
#' @param end_date data final no formato dd/mm/yyyy
#' @return Dataframe
#' @importFrom RCurl getURL
.fetch_agendas_comissoes_camara_auxiliar <- function(orgao_id, initial_date, end_date){

  url <-
    RCurl::getURL(paste0(
      .CAMARA_API_LINK_V1,
      .ORGAOS_SCHEDULE_CAMARA,
      orgao_id, "&datIni=", initial_date, "&datFim=", end_date))

  eventos_list <-
    XML::xmlParse(url) %>%
    XML::xmlToList()

  df <-
    eventos_list %>%
    jsonlite::toJSON() %>%
    jsonlite::fromJSON()

  if(purrr::is_list(df)){
    df <-
      df %>%
      purrr::list_modify(".attrs" = NULL) %>%
      tibble::as.tibble() %>%
      t() %>%
      as.data.frame()

    names(df) <- c("comissao","cod_reuniao", "num_reuniao", "data", "hora", "local",
                   "estado", "tipo", "titulo_reuniao", "objeto", "proposicoes")

    proposicoes <- df$proposicoes
    df <-
      df %>%
      dplyr::select(-c(num_reuniao, objeto, proposicoes)) %>%
      lapply(unlist) %>%
      as.data.frame() %>%
      tibble::add_column(proposicoes)

    df <-
      df %>%
      as.data.frame() %>%
      dplyr::filter(trimws(estado) != 'Cancelada') %>%
      tidyr::unnest()

    if(nrow(df) != 0) {
      df <-
        df %>%
        dplyr::mutate(sigla = purrr::map(proposicoes, ~ .x[['sigla']]),
                      id_proposicao = purrr::map(proposicoes, ~ .x[['idProposicao']]))
    }

  }else{

    df <- tibble::frame_data(~ comissao, ~ cod_reuniao, ~ num_reuniao, ~ data, ~ hora, ~ local,
                             ~ estado, ~ tipo, ~ titulo_reuniao, ~ objeto, ~ proposicoes)
  }

  return(df)
}

#' @title Fetches all Chamber of deputies organs schedule
#' @description Return a dataframe containing information about the Chamber of deputies organs schedule
#' @param initial_date initial date dd/mm/yyyy
#' @param end_date end_date dd/mm/yyyy
#' @return Returns a dataframe
#' @rdname fetch_agenda_orgaos_camara
#' @export
#' @examples
#' fetch_agenda_orgaos_camara('12/05/2018', '26/05/2018')
fetch_agenda_orgaos_camara <- function(initial_date, end_date) {
  orgaos <-
    fetch_orgaos_camara()

  agenda <- purrr::map_df(orgaos$orgao_id, .fetch_agendas_comissoes_camara_auxiliar, initial_date, end_date)
  if (nrow(agenda) == 0) {
    tibble::frame_data(~ data, ~ sigla, ~ id_proposicao, ~ local)
  }else {
    agenda %>%
      dplyr::select(data, sigla, id_proposicao, local = comissao) %>%
      dplyr::mutate(data = as.Date(data, "%d/%m/%Y")) %>%
      dplyr::arrange(data)
  }
}
