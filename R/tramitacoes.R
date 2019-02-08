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
#' @return Dataframe containing all the tramitation.
#' @examples
#' tramitacao_pec241 <- fetch_tramitacao_camara(2088351)
#' @seealso
#'   \code{\link[rcongresso]{fetch_id_proposicao}}
#' @rdname fetch_tramitacao_camara
#' @export
fetch_tramitacao_camara <- function(id_prop){
  path <- NULL
  unique(id_prop) %>%
    as.integer %>%
    tibble::tibble(id_prop = .) %>%
      dplyr::mutate(path = paste0(.CAMARA_PROPOSICOES_PATH, "/", id_prop, "/tramitacoes")) %>%
        dplyr::group_by(id_prop, path) %>%
        dplyr::do(
                 .camara_api(.$path)
               ) %>%
        dplyr::ungroup() %>%
        dplyr::select(-path) %>%
        .assert_dataframe_completo(.COLNAMES_TRAMITACOES_CAMARA) %>%
        .coerce_types(.COLNAMES_TRAMITACOES_CAMARA)
}

#' @title Fetches the tramitation of a proposition in the Senate
#' @description Returns the tramitation of a proposition by its id.
#' @param id_prop Proposition's ID
#' @return Dataframe containing all the tramitation.
#' @examples
#' tramitacao_pls229 <- fetch_tramitacao_senado(91341)
#' @seealso
#'   \code{\link[rcongresso]{fetch_id_proposicao}}
#' @rdname fetch_tramitacao_senado
#' @export
fetch_tramitacao_senado <- function(id_prop){
  url <-
    paste0(.SENADO_PROPOSICOES_PATH,
           id_prop)

  json_tramitacao <- .senado_api(url, asList = T)

  tramitacao_data <-
    json_tramitacao %>%
    magrittr::extract2("MovimentacaoMateria") %>%
    magrittr::extract2("Materia")
  tramitacao_ids <-
    tramitacao_data %>%
    magrittr::extract2("IdentificacaoMateria") %>%
    tibble::as.tibble()
  tramitacao_actual_situation <-
    tramitacao_data %>%
    magrittr::extract2("SituacaoAtual") %>%
    magrittr::extract2("Autuacoes") %>%
    magrittr::extract2("Autuacao") %>%
    magrittr::extract2("Situacao") %>%
    tibble::as.tibble()
  proposicao_tramitacoes_df <-
    tramitacao_data %>%
    magrittr::extract2("Tramitacoes") %>%
    magrittr::extract2("Tramitacao") %>%
    tibble::as.tibble() %>%
    tibble::add_column(!!!tramitacao_ids)

  proposicao_tramitacoes_df <-
    proposicao_tramitacoes_df[, !sapply(proposicao_tramitacoes_df, is.list)]

  proposicao_tramitacoes_df <-
    .rename_tramitacao_df(proposicao_tramitacoes_df) %>%
    dplyr::rename(data_hora = data_tramitacao, sequencia = numero_ordem_tramitacao)

  proposicao_tramitacoes_df
}
