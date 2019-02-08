#' @title Get the Senate's schedule
#' @description Return a list with 3 dataframes: schedule, bills and speakers. All the dfs contains a column named
#' codigo_sessao
#' @param initial_date inital date yyyy-mm-dd
#' @return list
#' @examples
#' fetch_agenda_senado('2018-07-03')
#' @rdname fetch_agenda_senado
fetch_agenda_senado <- function(initial_date) {
  url <- paste0(.AGENDA_SENADO_PATH, gsub('-','', initial_date))
  json_proposicao <- .senado_api(url, asList = T)
  if (is.null(json_proposicao$AgendaPlenario)) {
    return(list(agenda = tibble::as_tibble(), materias = tibble::as_tibble(), oradores = tibble::as_tibble()))
  }

  agenda <-
    json_proposicao$AgendaPlenario$Sessoes$Sessao %>%
    rename_table_to_underscore() %>%
    tibble::as_tibble()

  descricoes_inuteis <- c('SESSAO SOLENE', 'SESSAO NAO DELIBERATIVA', 'NAO HAVERA SESSAO', 'SESSAO ESPECIAL')

  agenda <-
    agenda %>%
    dplyr::filter(!(iconv(c(tipo_sessao), from="UTF-8", to="ASCII//TRANSLIT") %in% descricoes_inuteis))

  materia <- tibble::tibble()
  if('materias_materia' %in% names(agenda)) {
    materia <- purrr::map_df(agenda$materias_materia, dplyr::bind_rows, .id = "codigo_sessao")

    materia_not_null <-
      agenda %>%
      dplyr::filter(materias_materia != "NULL")

    num_de_materias <-
      materia %>%
      dplyr::group_by(codigo_sessao) %>%
      dplyr::summarise(id = 0)

    num_de_materias$id <- materia_not_null$codigo_sessao

    materia <-
      merge(materia, num_de_materias) %>%
      dplyr::select(-codigo_sessao) %>%
      dplyr::rename("codigo_sessao" = id) %>%
      rename_table_to_underscore()
  }

  oradores <- tibble::tibble()
  if('oradores_tipo_orador_orador_sessao_orador' %in% names(agenda)) {
    oradores <- purrr::map_df(agenda$oradores_tipo_orador_orador_sessao_orador, dplyr::bind_rows, .id = "codigo_sessao")

    oradores_not_null <-
      agenda %>%
      dplyr::filter(oradores_tipo_orador_orador_sessao_orador != "NULL")

    num_de_oradores <-
      oradores %>%
      dplyr::group_by(codigo_sessao) %>%
      dplyr::summarise(id = 0)

    num_de_oradores$id <- oradores_not_null$codigo_sessao

    oradores <-
      merge(oradores, num_de_oradores) %>%
      dplyr::select(-codigo_sessao) %>%
      dplyr::rename("codigo_sessao" = id) %>%
      rename_table_to_underscore()
  }

  agenda <- list(agenda = agenda, materias = materia, oradores = oradores)
}

#' @title Get the schedule of Deputies' Chamber
#' @description Return a dataframe with the meetings and sessions schedule of Deputies' Chamber
#' @param initial_date initial date yyyy-mm-dd
#' @param end_date end date yyyy-mm-dd
#' @return Dataframe
#' @examples
#' fetch_agenda_camara('2018-07-03', '2018-07-10')
#' @rdname fetch_agenda_camara
#' @export
fetch_agenda_camara <- function(initial_date, end_date) {
  url <- paste0(.AGENDA_CAMARA_PATH, initial_date, "&dataFim=", end_date, "&ordem=ASC&ordenarPor=dataHoraInicio")
  json_proposicao <- .camara_api(url)

  descricoes_inuteis <- c('Seminario', 'Diligencia', 'Sessao Nao Deliberativa de Debates', 'Reuniao de Instalacao e Eleicao',
                          'Outro Evento', 'Mesa Redonda', 'Sessao Nao Deliberativa Solene')
  agenda <-
    json_proposicao %>%
    dplyr::filter(situacao != 'Cancelada' &
                    !(iconv(c(descricaoTipo), from="UTF-8", to="ASCII//TRANSLIT") %in% descricoes_inuteis)) %>%
    tidyr::unnest()

  agenda %>%
    dplyr::rowwise() %>%
    dplyr::do(fetch_pauta_camara(
      .$id, .$dataHoraInicio, .$dataHoraFim, .$sigla, .$nome) %>%
        tibble::as.tibble()) %>%
    unique() %>%
    .assert_dataframe_completo(.COLNAMES_AGENDA_CAMARA) %>%
    .coerce_types(.COLNAMES_AGENDA_CAMARA)
}

#' @title Get the agenda of a meeting
#' @description Return a dataframe with data about the agenda
#' @param id event id
#' @param hora_inicio inital time
#' @param hora_fim end time
#' @param sigla_orgao Acronym of the organ
#' @param nome_orgao  Name of the organ
#' @return Dataframe
#' @examples
#' fetch_pauta_camara('53184', '2018-07-03T10:00', '2018-07-03T12:37', 'CVT', 'Comissão de Viação e Transportes VIAÇÃO E TRANSPORTES')
#' @rdname fetch_pauta_camara
fetch_pauta_camara <- function(id, hora_inicio, hora_fim, sigla_orgao, nome_orgao) {
  url <- paste0(.PAUTAS_CAMARA, id, "/pauta")
  json_proposicao <- .camara_api(url)

  json_proposicao %>%
    tibble::as.tibble() %>%
    dplyr::mutate(hora_inicio = hora_inicio,
                  hora_fim = hora_fim,
                  sigla_orgao = sigla_orgao,
                  nome_orgao = nome_orgao) %>%
    .assert_dataframe_completo(.COLNAMES_PAUTA_CAMARA) %>%
    .coerce_types(.COLNAMES_PAUTA_CAMARA)
}
