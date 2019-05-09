#' @title Fetch related requerimentos in Camara
#' @description Returns a dataframe with data from requerimentos related to a given proposition in Camara
#' @param prop_id ID of a Proposicao
#' @param mark_deferimento whether to retrieve status of requerimento
#' @return Dataframe
#' @export
fetch_related_requerimentos_camara <- function(prop_id, mark_deferimento = FALSE) {
  reqs <- fetch_relacionadas(prop_id) %>%
    dplyr::filter(siglaTipo == "REQ") %>%
    dplyr::distinct()

  if(nrow(reqs) == 0)
    return(tibble::tibble())

  reqs_data <- purrr::map_df(reqs$id, ~ fetch_proposicao_camara(.x)) %>%
    dplyr::mutate(id_req = id,
                  id_prop = prop_id,
                  casa = .CAMARA) %>% #Adding proposition number to final dataframe
    dplyr::select(-id) %>%
    dplyr::select(id_prop, casa, id_req, dplyr::everything())

  if (!mark_deferimento) {
    return(reqs_data)
  } else {
    regexes <-
    tibble::tribble(
      ~ deferimento,
      ~ regex,
      'req_indeferido',
      '^Indefiro',
      'req_deferido',
      '^(Defiro)|(Aprovado)',
      'req_arquivado',
      '^Arquivado'
    )

    reqs_trams <- fetch_tramitacao(reqs_data$id_req, .CAMARA)

    related_reqs <-
      reqs_trams %>%
      # mark reqs_trams rows based on regexes
      fuzzyjoin::regex_left_join(regexes, by = c(despacho = 'regex')) %>%
      dplyr::group_by(id_prop) %>%
      dplyr::arrange(data_hora) %>%
      dplyr::summarise(deferimento_final =
                         dplyr::if_else(any(deferimento %in% c('req_deferido')),'deferido',
                                        dplyr::if_else(any(deferimento %in% c('req_indeferido')),'indeferido',
                                                       dplyr::if_else(any(deferimento %in% c('req_arquivado')),'arquivado',
                                                                      'tramitando')))) %>%
      dplyr::ungroup() %>%
      dplyr::select(id_req = id_prop, deferimento = deferimento_final) %>%
      dplyr::left_join(reqs_data, by = "id_req") %>%
      dplyr::select(id_prop, casa, id_req, dplyr::everything()) %>%
      rename_table_to_underscore() %>%
      .assert_dataframe_completo(.COLNAMES_REQUERIMENTOS_CAMARA) %>%
      .coerce_types(.COLNAMES_REQUERIMENTOS_CAMARA, order_cols = FALSE)

  }
}

#' @title Fetch events of a requerimento
#' @description Returns a dataframe with events of a given requerimento (presentation, deferral, etc.)
#' @param req_id ID of a requerimento
#' @return Dataframe
#' @export
fetch_events_requerimento_camara <- function(req_id) {
  regexes <-
    tibble::tribble(
      ~ evento,
      ~ regex,
      'req_apresentacao',
      '^Apresentacao',
      'req_indeferido',
      '^Indefiro',
      'req_deferido',
      '^(Defiro)|(Aprovado)',
      'req_arquivado',
      '^Arquivado')

  req_tram <-
    fetch_tramitacao(req_id, .CAMARA) %>%
    dplyr::mutate(despacho = iconv(despacho, from="UTF-8", to="ASCII//TRANSLIT"))

  eventos_req <-
    req_tram %>%
    # mark reqs_trams rows based on rege  xes
    fuzzyjoin::regex_left_join(regexes, by = c(despacho = 'regex')) %>%
    dplyr::filter(!is.na(evento)) %>%
    dplyr::mutate(id_req = id_prop) %>%
    dplyr::select(-regex, -id_prop) %>%
    dplyr::select(id_req, data_hora, evento, dplyr::everything()) %>%
    .assert_dataframe_completo(.COLNAMES_EVENTOS_REQUERIMENTOS_CAMARA)  %>%
    .coerce_types(.COLNAMES_EVENTOS_REQUERIMENTOS_CAMARA, order_cols = F)
}

#' @title Fetch events of a requerimento
#' @description Returns a dataframe with events of a given requerimento (presentation, deferral, etc.)
#' @param req_id ID of a requerimento
#' @return Dataframe
#' @example event_senado <- fetch_eventos_requerimento_senado(129081)
#' @export
fetch_eventos_requerimento_senado <- function(req_id) {
  regexes <-
    tibble::tribble(
      ~ evento,
      ~ regex,
      'req_apresentacao',
      '^Apresentacao',
      'req_indeferido',
      '^Indefiro',
      'req_deferido',
      '^(Defiro)|(Aprovado)',
      'req_arquivado',
      '^Arquivado')

  req_tram <-
    fetch_tramitacao(req_id, .SENADO) %>%
    dplyr::mutate(despacho = iconv(despacho, from="UTF-8", to="ASCII//TRANSLIT"))

  eventos_req <-
    req_tram %>%
    fuzzyjoin::regex_left_join(regexes, by = c(despacho = 'regex')) %>%
    dplyr::filter(!is.na(evento)) %>%
    dplyr::mutate(id_req = id_prop) %>%
    dplyr::select(-regex, -id_prop) %>%
    dplyr::select(id_req, data_hora, evento, dplyr::everything()) %>%
    .assert_dataframe_completo(.COLNAMES_EVENTOS_REQUERIMENTOS_SENADO)  %>%
    .coerce_types(.COLNAMES_EVENTOS_REQUERIMENTOS_SENADO, order_cols = F)
}

#' @title Fetch requerimentos
#' @description Returns a dataframe with data of a given requerimento (author, year, etc.)
#' @param ID of a requerimento
#' @return Dataframe
#' @export
fetch_requerimento_senado <- function(req_id) {
  proposicoes <- fetch_proposicao_senado(req_id)

  requerimento <- proposicoes %>%
    dplyr::filter(sigla_subtipo_materia %in%
                    c("RQS", "RCS", "RMA", "RRE",
                      "RQN", "RDR", "RTG", "RQJ", "RQI", "ROS", "REQ"))

  requerimento
}

#' @title Fetch info of a requerimento
#' @description Returns a dataframe with data of a given requerimento (ids, year, description, etc.)
#' @return Dataframe
#' @export
fetch_requerimentos_senado <- function() {
  url <- paste0(.SENADO_LEGISLATURAATUAL_PATH)
  json_requerimento <- .senado_api(url, asList = T)

  requerimento_data <-
    json_requerimento %>%
    magrittr::extract2("ListaMateriasLegislaturaAtual") %>%
    magrittr::extract2("Materias") %>%
    magrittr::extract2("Materia") %>%
    dplyr::filter(IdentificacaoMateria.SiglaSubtipoMateria %in%
                    c("RQS", "RCS", "RMA", "RRE", "RQN",
                      "RDR", "RTG", "RQJ", "RQI", "ROS", "REQ"))

  requerimento_data
}

#' @title Requirements's deferments
#' @description Verify deferments to a id's list of requirements
#' @param proposicao_id ID of requirements
#' @return Dataframe
#' @examples
#' fetch_deferimento(c("102343", "109173", "115853"))
#' @export
fetch_deferimento <- function(proposicao_id) {
  regexes <-
    tibble::frame_data(
      ~ deferimento,
      ~ regex,
      "indeferido",
      .REGEX_DEFERIMENTO_INDEFERIDO,
      "deferido",
      .REGEX_DEFERIMENTO_DEFERIDO
    )

  fetch_one_deferimento <- function(proposicao_id) {
    json <-
      .senado_api(paste0(.DEFERIMENTO_SENADO_PATH,
                         proposicao_id), asList = T)

    resultados <-
      json$MovimentacaoMateria$Materia$OrdensDoDia$OrdemDoDia$DescricaoResultado
    # handle NULL
    if (is.null(resultados))
      resultados <- c('')

    resultados %>%
      tibble::as.tibble() %>%
      dplyr::mutate(proposicao_id = proposicao_id) %>%
      fuzzyjoin::regex_left_join(regexes, by = c(value = "regex")) %>%
      tidyr::fill(deferimento) %>%
      utils::tail(., n = 1) %>%
      dplyr::select(proposicao_id, deferimento)
  }

  proposicao_id %>%
    unlist %>%
    unique %>%
    lapply(fetch_one_deferimento) %>%
    plyr::rbind.fill() %>%
    .assert_dataframe_completo(.COLNAMES_DEFRIMENTO) %>%
    .coerce_types(.COLNAMES_DEFRIMENTO)
}
