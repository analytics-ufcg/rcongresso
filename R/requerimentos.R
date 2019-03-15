#' @title Fetch related request 
#' @description Returns a dataframe with data from requests related to a given proposition
#' @param prop_id Proposition's id
#' @param mark_deferimento whether to retrieve request status
#' @return Dataframe
#' @export
fetch_related_requerimentos_camara <- function(prop_id, mark_deferimento = FALSE) {
  reqs <- fetch_relacionadas(prop_id) %>% 
    dplyr::filter(siglaTipo == "REQ") %>%
    dplyr::distinct()
  reqs_data <- purrr::map_df(reqs$id, ~ fetch_proposicao_camara(.x))
  
  if (!mark_deferimento)
    reqs_data <- reqs_data %>% 
      dplyr::mutate(id_req = id,
                    id_prop = prop_id,
                    casa = .CAMARA) %>% #Adding proposition number to final dataframe
      dplyr::select(id_prop, casa, id_req, dplyr::everything())
    return(reqs_data)
  else
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
  
    reqs_trams <- fetch_tramitacao(reqs_data$id, 'camara')
    
    related_reqs <-
      reqs_trams %>%
      # mark reqs_trams rows based on regexes
      fuzzyjoin::regex_left_join(regexes, by = c(despacho = 'regex')) %>%
      dplyr::filter(!is.na(deferimento)) %>%
      dplyr::mutate(id_req = id_prop) %>% 
      dplyr::mutate(id_prop = prop_id,
                    casa = .CAMARA) %>% #Adding proposition number to final dataframe
      dplyr::select(id_prop, casa, id_req, deferimento) %>%
      dplyr::left_join(reqs_data, by = c("id_req" = "id")) %>%
      rename_table_to_underscore() %>%
      .assert_dataframe_completo(.COLNAMES_REQUERIMENTOS_CAMARA) %>%
      .coerce_types(.COLNAMES_REQUERIMENTOS_CAMARA, order_cols = FALSE) 
}

fetch_eventos_requerimento_camara <- function(req_id) {
  req_data <- fetch_proposicao_camara(req_id)
  
  regexes <-
    tibble::tribble(
      ~ evento,
      ~ regex,
      'req_apresentacao',
      '^Apresentação',
      'req_indeferido',
      '^Indefiro',
      'req_deferido',
      '^(Defiro)|(Aprovado)',
      'req_arquivado',
      '^Arquivado')
  
  req_tram <- fetch_tramitacao(req_data$id, 'camara')
  
  eventos_req <-
    req_tram %>%
    # mark reqs_trams rows based on regexes
    fuzzyjoin::regex_left_join(regexes, by = c(despacho = 'regex')) %>%
    dplyr::filter(!is.na(evento)) %>%
    dplyr::mutate(id_req = id_prop) %>% 
    dplyr::mutate(id_prop = prop_id,
                  casa = .CAMARA) %>% #Adding proposition number to final dataframe
    dplyr::select(-regex) %>%
    dplyr::select(id_prop, casa, id_req, data_hora, evento, dplyr::everything()) %>%
    .assert_dataframe_completo(.COLNAMES_EVENTOS_REQUERIMENTOS_CAMARA)  %>%
    .coerce_types(.COLNAMES_EVENTOS_REQUERIMENTOS_CAMARA, order_cols = F)
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