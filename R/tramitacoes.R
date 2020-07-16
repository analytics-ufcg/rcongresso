#' @title Fetches the tramitation of a proposition
#' @description Returns the tramitation of a proposition by its id.
#' @param id_prop Proposition's ID
#' @param casa Proposition's casa
#' @return Dataframe containing all the tramitation.
#' @examples
#' tramitacao_pec241 <- fetch_sessoes(2088351,'camara')
#' tramitacao_pls229 <- fetch_sessoes(91341,'senado')
#' @rdname fetch_tramitacao
#' @export
fetch_tramitacao <- function(id_prop, casa) {
  if (tolower(casa) == "camara") {
    fetch_tramitacao_camara(id_prop)
  } else if (tolower(casa) == "senado") {
    fetch_tramitacao_senado(id_prop)
  } else {
    print("casa param is missing.")
    return()
  }
}

#' @title Fetches the tramitation of a proposition in the Chamber of Deputies
#' @description Returns the tramitation of a proposition by its id.
#' @param id_prop Proposition's ID
#' @param data_inicio initial date format AAAA-MM-DD
#' @param data_fim end date format AAAA-MM-DD
#' @return Dataframe containing all the tramitation.
#' @examples
#' tramitacao_pec241 <- fetch_tramitacao_camara(2088351)
#' @seealso
#'   \code{\link[rcongresso]{fetch_id_proposicao_camara}}
#' @rdname fetch_tramitacao_camara
#' @export
fetch_tramitacao_camara <- function(id_prop, data_inicio = NA, data_fim = NA){
  query <- NULL
  if (!is.na(data_inicio) & !is.na(data_fim)) {
    query <- list(dataInicio = data_inicio, dataFim = data_fim)
  }

  unique(id_prop) %>%
    as.integer %>%
    tibble::tibble(id_prop = .) %>%
    dplyr::mutate(path = paste0(.CAMARA_PROPOSICOES_PATH, "/", id_prop, "/tramitacoes")) %>%
    dplyr::group_by(id_prop, path) %>%
      dplyr::do(.camara_api(.$path, query = query)) %>%
    dplyr::ungroup() %>%
    dplyr::select(-path) %>%
    .assert_dataframe_completo(.COLNAMES_TRAMITACOES_CAMARA) %>%
    .coerce_types(.COLNAMES_TRAMITACOES_CAMARA) %>%
    .rename_df_columns()
}

#' @title Fetches the tramitation of a proposition in the Senate
#' @description Returns the tramitation of a proposition by its id.
#' @param id_prop Proposition's ID
#' @param data_ref Ref's date format YYYYMMDD
#' @return Dataframe containing all the tramitation.
#' @examples
#' tramitacao_pls229 <- fetch_tramitacao_senado(91341)
#' @rdname fetch_tramitacao_senado
#' @export
fetch_tramitacao_senado <- function(id_prop, data_ref = NA) {
    query <- NULL
  if (!is.na(data_ref)) {
    query <- list(dataref = data_ref)
  }

  url <-
    paste0(.SENADO_TRAMITACAO_PROPOSICAO_PATH,
           id_prop)

  json_tramitacao <- .senado_api(url, asList = T, query = query)

  tramitacao_data <-
    json_tramitacao %>%
    magrittr::extract2("MovimentacaoMateria") %>% 
    magrittr::extract2("Materia") 
  
  tramitacao_ids <-
    tramitacao_data %>%
    magrittr::extract2("IdentificacaoMateria") %>%
    tibble::as_tibble()
  
  proposicao_tramitacoes_df <-
    (tramitacao_data %>%
    magrittr::extract2("Autuacoes") %>%
    magrittr::extract2("Autuacao"))$InformesLegislativos.InformeLegislativo[[1]] %>% 
    tibble::as_tibble() %>% 
    tibble::add_column(!!!tramitacao_ids)

  proposicao_tramitacoes_df <-
    proposicao_tramitacoes_df[, !sapply(proposicao_tramitacoes_df, is.list)]

  proposicao_tramitacoes_df <-
    .rename_tramitacao_df(proposicao_tramitacoes_df) %>%
    dplyr::arrange(data) %>% 
    tibble::rowid_to_column("sequencia") %>% 
    dplyr::rename(data_hora = data, 
                  origem_tramitacao_local_codigo_local = local_codigo_local,
                  origem_tramitacao_local_sigla_local = local_sigla_local,
                  origem_tramitacao_local_nome_local = local_nome_local,
                  origem_tramitacao_local_sigla_casa_local = local_sigla_casa_local,
                  origem_tramitacao_local_nome_casa_local = local_nome_casa_local,
                  texto_tramitacao = descricao)%>%
    dplyr::mutate(sequencia = as.integer(sequencia))
  
  
  situacoes <-
    (tramitacao_data %>%
       magrittr::extract2("Autuacoes") %>%
       magrittr::extract2("Autuacao"))$HistoricoSituacoes.Situacao[[1]] %>% 
    tibble::as_tibble() %>% 
    .rename_tramitacao_df()
  
  names(situacoes) <-
    purrr::map_chr(names(situacoes), function(x) {
      return(paste0("situacao_", x))
    })
  
  if (length(situacoes) > 0) {
    proposicao_tramitacoes_df <- 
      proposicao_tramitacoes_df %>% 
      dplyr::left_join(situacoes, by = c("data_hora"="situacao_data_situacao"))
  }
  
  return(proposicao_tramitacoes_df)
}
