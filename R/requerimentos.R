#' @title Fetch related request 
#' @description Returns a dataframe with data from requests related to a given proposition
#' @param prop_id Proposition's id
#' @param mark_deferimento whether to retrieve request status
#' @return Dataframe
#' @export
fetch_related_requerimentos <- function(prop_id, mark_deferimento = TRUE) {
  regexes <-
    tibble::tribble(
      ~ deferimento,
      ~ regex,
      'indeferido',
      '^Indefiro',
      'deferido',
      '^(Defiro)|(Aprovado)',
      'arquivado',
      '^Arquivado'
    )
  
  reqs <- fetch_relacionadas(prop_id) %>% 
    dplyr::filter(siglaTipo == "REQ") %>%
    dplyr::distinct()
  reqs_data <- purrr::map_df(reqs$id, ~ fetch_proposicao_camara(.x))
  
  if (!mark_deferimento)
    return(reqs_data)
  
  reqs_trams <- fetch_tramitacao(reqs_data$id, 'camara')
  
  related_reqs <-
    reqs_trams %>%
    # mark reqs_trams rows based on regexes
    fuzzyjoin::regex_left_join(regexes, by = c(despacho = 'regex')) %>%
    dplyr::group_by(id_prop) %>%
    dplyr::arrange(data_hora) %>%
    dplyr::summarise(deferimento_final = 
                       dplyr::if_else(any(deferimento %in% c('deferido')),'deferido',
                        dplyr::if_else(any(deferimento %in% c('indeferido')),'indeferido',
                          dplyr::if_else(any(deferimento %in% c('arquivado')),'arquivado',
                            'tramitando')))) %>%
    dplyr::select(id_prop, deferimento = deferimento_final) %>%
    # and mark proposicoes based on last tramitacao mark
    dplyr::left_join(reqs_data, by = c('id_prop' = 'id')) %>%
    .assert_dataframe_completo(.COLNAMES_REQUERIMENTOS) %>%
    .coerce_types(.COLNAMES_REQUERIMENTOS) 
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