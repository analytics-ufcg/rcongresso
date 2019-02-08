#' @title Get the schedule of Deputies' Chamber
#' @description Return a dataframe with the meetings and sessions schedule of Deputies' Chamber
#' @param initial_date initial date yyyy-mm-dd
#' @param end_date end date yyyy-mm-dd
#' @return Dataframe
#' @examples
#' fetch_agenda_camara('2018-07-03', '2018-07-10')
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

  x <- agenda %>%
    dplyr::rowwise() %>%
    dplyr::do(fetch_pauta_camara(
      .$id, .$dataHoraInicio, .$dataHoraFim, .$sigla, .$nome) %>%
        tibble::as.tibble()) %>%
    unique() %>%
    .assert_dataframe_completo(.COLNAMES_AGENDA) %>%
    .coerce_types(.COLNAMES_AGENDA)
}

#' @title Get the agenda of a meeting
#' @description Return a dataframe with data about the agenda
#' @param id event id
#' @param hora_inicio inital time
#' @param hora_fim end time
#' @return Dataframe
#' @examples
#' fetch_pauta_camara('53184', '2018-07-03T10:00', '2018-07-03T12:37', 'CVT', 'Comissão de Viação e Transportes VIAÇÃO E TRANSPORTES')
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
