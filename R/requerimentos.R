#' @title Fetch related requerimentos in Camara
#' @description Returns a dataframe with data from requerimentos related to a given proposition in Camara
#' @param prop_id ID of a Proposicao
#' @param mark_deferimento whether to retrieve status of requerimento
#' @return Dataframe
#' @export
fetch_related_requerimentos_camara <- function(prop_id, mark_deferimento = FALSE) {
  reqs <- fetch_relacionadas("camara",prop_id) %>%
    dplyr::filter(siglaTipo == "REQ") %>%
    dplyr::distinct()

  if(nrow(reqs) == 0)
    return(tibble::tibble())

  reqs_data <- purrr::map_df(reqs$id, ~ fetch_proposicao(.x, .CAMARA)) %>%
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

    reqs_trams <- purrr::map_df(reqs_data$id_req, ~ fetch_tramitacao(.x, .CAMARA))

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

#' @title Fetch related requerimentos in Senado
#' @description Returns a dataframe with data from requerimentos related to a given proposition in Senado
#' @param prop_id ID of a Proposicao
#' @param mark_deferimento whether to retrieve status of requerimento
#' @return Dataframe
#' @export
fetch_related_requerimentos_senado <- function(prop_id, mark_deferimento = FALSE) {
  reqs <- .filter_requerimento_senado(prop_id)

  if(nrow(reqs) == 0)
    return(tibble::as_tibble())

  reqs_data <- purrr::map_df(reqs$codigo_materia, ~ fetch_proposicao_senado(.x)) %>%
    dplyr::mutate(id_req = codigo_materia,
                  id_prop = prop_id,
                  casa = .SENADO) %>%
    dplyr::select(-codigo_materia) %>%
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

    reqs_trams <- fetch_tramitacao(reqs_data$id_req, .SENADO)

    related_reqs <-
      reqs_trams %>%
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
      .assert_dataframe_completo(.COLNAMES_REQUERIMENTOS_SENADO) %>%
      .coerce_types(.COLNAMES_REQUERIMENTOS_SENADO, order_cols = FALSE)
  }
}

#' @title Fetch events of a requerimento
#' @description Returns a dataframe with events of a given requerimento (presentation, deferral, etc.)
#' @param req_id ID of a requerimento
#' @return Dataframe
#' @export
fetch_events_requerimento_senado <- function(req_id) {
  regexes <-
    tibble::tribble(
      ~ evento,
      ~ regex,
      'req_remetida_camara',
      '^REMETIDA',
      'req_aprov_substituto',
      '^APROVADO O SUBSTITUTO',
      'req_incluida_ordem_dia',
      '^INCLUIDA EM ORDEM',
      'req_pronto_deliberacao',
      '^PRONTO PARA DELIBERACAO',
      'req_aguardando_recebimento',
      '^AGUARDANDO RECEBIMENTO',
      'req_aguardando_leitura_parecer',
      '^AGUARDANDO LEITURA PARECER',
      'req_aprov_parecer_comissao',
      '^APROVADO PARECER DA COMISSAO',
      'req_incluida_pauta_reuniao',
      '^INCLUIDA NA PAUTA DA REUNIAO',
      'req_pronta_pauta_reuniao',
      '^PRONTA PARA A PAUTA DA REUNIAO',
      'req_pedido',
      '^PEDIDO',
      'req_pronta_pauta_comissao',
      '^PRONTA PARA A PAUTA DA COMISSAO',
      'req_audiencia',
      '^AUDIENCIA',
      'req_materia',
      '^MATERIA',
      'req_aguardando_designacao',
      '^AGUARDANDO DESIGNACAO',
      'req_aguardando_inclusao',
      '^AGUARDANDO INCLUSAO',
      'req_incluido',
      '^INCLUIDO',
      'req_agendado',
      '^AGENDADO',
      'req_aguardando_leitura',
      '^AGUARDANDO LEITURA',
      'req_aguardando_decisao_mesa',
      '^AGUARDANDO DECISAO')

  req_tram <- fetch_tramitacao(req_id, .SENADO) %>%
    dplyr::mutate(situacao_descricao_situacao = iconv(situacao_descricao_situacao,
                                                      from="UTF-8",
                                                      to="ASCII//TRANSLIT"))

  eventos_req <-
    req_tram %>%
    fuzzyjoin::regex_left_join(regexes, by = c(situacao_descricao_situacao = 'regex')) %>%
    dplyr::filter(!is.na(evento)) %>%
    dplyr::select(-regex) %>%
    dplyr::select(codigo_materia, data_hora, dplyr::everything()) %>%
    .assert_dataframe_completo(.COLNAMES_EVENTOS_REQUERIMENTOS_SENADO)  %>%
    .coerce_types(.COLNAMES_EVENTOS_REQUERIMENTOS_SENADO, order_cols = F)
}

#' @title Filter requerimentos
#' @description Returns a dataframe with data of a given requerimento (author, year, etc.)
#' @param ID of a requerimento
#' @return Dataframe
.filter_requerimento_senado <- function(req_id) {
  prop_relacionadas <- fetch_relacionadas_senado(req_id)

  requerimentos <-  prop_relacionadas %>%
    dplyr::filter(sigla_subtipo_materia %in%
                    c("RQS", "RCS", "RMA", "RRE",
                      "RQN", "RDR", "RTG", "RQJ", "RQI", "ROS", "REQ"))

  requerimentos
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
      tibble::as_tibble() %>%
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
