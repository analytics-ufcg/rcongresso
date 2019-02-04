#' @title Get the schedule of Deputies' Chamber
#' @description Return a dataframe with the meetings and sessions schedule of Deputies' Chamber
#' @param initial_date initial date yyyy-mm-dd
#' @param end_date dend date formato yyyy-mm-dd
#' @return Dataframe
#' @examples
#' fetch_agenda_camara('2018-07-03', '2018-07-10')
#' @export
fetch_agenda_camara <- function(initial_date, end_date) {
  url <- paste0(.AGENDA_CAMARA_PATH, initial_date, "&dataFim=", end_date, "&ordem=ASC&ordenarPor=dataHoraInicio")
  json_proposicao <- .camara_api(url)

  descricoes_inuteis <- c('Seminário', 'Diligência', 'Sessão Não Deliberativa de Debates', 'Reunião de Instalação e Eleição', 'Outro Evento', 'Mesa Redonda', 'Sessão Não Deliberativa Solene')
  agenda <-
    json_proposicao %>%
    dplyr::filter(situacao != 'Cancelada' &
                    !(descricaoTipo %in% descricoes_inuteis)) %>%
    tidyr::unnest()

  agenda %>%
    dplyr::rowwise() %>%
    dplyr::do(fetch_pauta_camara(
      .$id, .$dataHoraInicio, .$dataHoraFim, .$sigla, .$nome) %>%
        tibble::as.tibble()) %>%
    unique() %>%
    .assert_dataframe_completo(.COLNAMES_AGENDA) %>%
    .coerce_types(.COLNAMES_AGENDA)
}
