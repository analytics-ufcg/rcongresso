#' @title Get the Senate's schedule
#' @description Return a list with 3 dataframes: schedule, bills and speakers. All the dfs contains a column named
#' codigo_sessao
#' @param initial_date inital date yyyy-mm-dd
#' @return list
#' @examples
#' fetch_agenda_senado('2018-07-03')
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
