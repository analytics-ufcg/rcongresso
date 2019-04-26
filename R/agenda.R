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
  if(nrow(agenda) != 0 && 'oradores_tipo_orador_orador_sessao_orador' %in% names(agenda)) {
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

#' @title Dataframe with the Senate schedule
#' @description Return a dataframe with the Senate schedule
#' @param initial_date initial date yyyy-mm-dd
#' @param end_date end date yyyy-mm-dd
#' @return Dataframe
#' @examples
#' .get_data_frame_agenda_senado('2016-05-15', '2016-05-25')
.get_data_frame_agenda_senado <- function(initial_date, end_date) {
  url <-
    paste0(.AGENDA_SENADO_COMISSOES, gsub('-','', initial_date), "/", gsub('-','', end_date), "/detalhe")
  json_proposicao <- .senado_api(url, asList = T)

  json_proposicao$Reunioes$Reuniao %>%
    tibble::as_tibble() %>%
    rename_table_to_underscore() %>%
    dplyr::filter(situacao != 'Cancelada')
}

#' @title Comissions schedule Senate
#' @description Return a dataframe with the Senate's Comissions schedule
#' @param initial_date initial date yyyy-mm-dd
#' @param end_date end date yyyy-mm-dd
#' @return Dataframe
#' @examples
#' fetch_agenda_senado_comissoes('2016-05-15', '2016-05-25')
fetch_agenda_senado_comissoes <- function(initial_date, end_date) {
  tipos_inuteis <- c('Outros eventos', 'Reuniao', 'Reuniao de Subcomissao')

  agenda <-
    .get_data_frame_agenda_senado(initial_date, end_date) %>%
    dplyr::filter(!(iconv(c(tipo), from="UTF-8", to="ASCII//TRANSLIT") %in% tipos_inuteis)) %>%
    unique()

  if (nrow(agenda) != 0) {
    if ("partes_parte" %in% names(agenda)) {
      agenda <-
        agenda %>%
        dplyr::mutate(id_proposicao = purrr::map(partes_parte, ~ .get_id_proposicao_agenda_senado_comissoes(.))) %>%
        dplyr::mutate(nome = purrr::map(partes_parte, ~ .get_nome_proposicao_agenda_senado_comissoes(.))) %>%
        dplyr::filter(id_proposicao != "")

      if (nrow(agenda) != 0) {
        agenda <-
          agenda %>%
          dplyr::rowwise() %>%
          dplyr::mutate(local = strsplit(titulo_da_reuniao, ",")[[1]][[1]]) %>%
          dplyr::select(c(data, nome, id_proposicao, local)) %>%
          dplyr::mutate(id_proposicao = strsplit(as.character(id_proposicao), ",")) %>%
          dplyr::mutate(nome = strsplit(as.character(nome), ",")) %>%
          tidyr::unnest() %>%
          dplyr::mutate(data = lubridate::ymd_hms(paste(data, hora))) %>% 
          dplyr::select(c(data, nome, id_proposicao, local))
      }else {
        return(tibble::tibble(data = double(), sigla = character(), id_proposicao = character(), local = character()))
      }

    }else {
      agenda <-
        agenda %>%
        dplyr::filter(partes_parte_tipo == "Deliberativa") 

      if (nrow(agenda) != 0) {
        agenda <-
          agenda %>%
          dplyr::mutate(id_proposicao = purrr::map(partes_parte_itens_item, ~ .$Codigo)) %>%
          dplyr::mutate(nome = purrr::map(partes_parte_itens_item, ~ .$Nome)) %>% 
          dplyr::select(data, hora, id_proposicao, nome, titulo_da_reuniao) %>%
          tidyr::unnest() %>%
          dplyr::rowwise() %>%
          dplyr::mutate(local = strsplit(titulo_da_reuniao, ",")[[1]][[1]]) %>%
          dplyr::mutate(data =  lubridate::ymd_hms(paste(data, hora))) %>% 
          dplyr::select(c(data, nome, id_proposicao, local))
      }else {
        return(tibble::tibble(data = double(), sigla = character(), id_proposicao = character(), local = character()))
      }
    }

    new_names <- c("data", "sigla", "id_proposicao", "local")
    names(agenda) <- new_names

    agenda %>%
      dplyr::arrange(data)

  }else {
    tibble::tibble(data = double(), sigla = character(), id_proposicao = character(), local = character())
  }

}

#' @title Extract proposition name
#' @description Receive as param a list from the Senate schedule and return the propositions name that are in 'pauta'
#' @param lista_com_nome list that has the name
#' @return char
.get_nome_proposicao_agenda_senado_comissoes <- function(lista_com_nome){
  nome <- ""
  if(length(lista_com_nome$Tipo) == 1) {
    if (lista_com_nome$Tipo == "Deliberativa") {
      nome <- paste(lista_com_nome$Itens$Item$Nome, collapse = ",")
    }
  }else {
    if ("Deliberativa" %in% lista_com_nome$Tipo) {
      if(!is.null(lista_com_nome$Itens.Item)) {
        nome <- paste(lista_com_nome$Nome, collapse = ",")
      }
    }
  }
  nome
}

#' @title Extract the proposition id
#' @description Receive as param a list from the Senate schedule and return the propositions ids that are in 'pauta'
#' @param lista_com_id list that has the id
#' @return char
.get_id_proposicao_agenda_senado_comissoes <- function(lista_com_id){
  id <- ""
  if(length(lista_com_id$Tipo) == 1 ) {
    if (lista_com_id$Tipo == "Deliberativa") {
      id <- paste(lista_com_id$Itens$Item$Codigo, collapse = ",")
    }
  }else {
    if ("Deliberativa" %in% lista_com_id$Tipo) {
      if(!is.null(lista_com_id$Itens.Item)) {
        paste(lista_com_id$Itens.Item$Codigo, collapse = ",")
      }
    }
  }
  id
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
  json_proposicao <- .camara_api(.AGENDA_CAMARA_PATH, query = list(
    dataInicio = initial_date,
    dataFim = end_date,
    ordem = "ASC",
    ordenarPor = "dataHoraInicio")
  )

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
