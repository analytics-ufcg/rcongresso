#' Fetches details from a proposition.
#'
#' @param id_prop Proposition's ID
#'
#' @return Dataframe containing information about the proposition.
#'
#' @examples
#' pec241 <- fetch_proposicao(2088351)
#'
#' @export
fetch_proposicao <- function(id_prop){
  id <- NULL
  tibble::tibble(id = id_prop) %>%
    dplyr::mutate(path = paste0(.PROPOSICOES_PATH, "/", id)) %>%
    dplyr::rowwise() %>%
    dplyr::do(
      .congresso_api(.$path)$dados %>%
        .remove_lists_and_nulls()
      ) %>%
    dplyr::ungroup() %>%
    return()
}

#' Fetches all the votings which a proposition went through.
#'
#' @param id_prop Proposition's ID
#'
#' @return Dataframe containing all the votings.
#'
#' @examples
#' votacoes_pec241 <- fetch_votacoes(2088351)
#'
#' @export
fetch_votacoes <- function(id_prop){
  id <- NULL
  tibble::tibble(id = id_prop) %>%
    dplyr::mutate(path = paste0(.PROPOSICOES_PATH, "/", id, "/votacoes")) %>%
    dplyr::rowwise() %>%
    dplyr::do(.congresso_api(.$path)[[1]]) %>%
    dplyr::ungroup() %>%
    return()
}

#' Retrieves the proposition ID from its type, number and year.
#'
#' @param tipo Proposition type (i.e., PEC, PL, PDC)
#' @param numero Proposition number
#' @param ano Proposition year
#'
#' @return Proposition's ID.
#'
#' @examples
#' pec241_id <- fetch_id_proposicao("PEC", 241, 2016)
#'
#' @export
fetch_id_proposicao <- function(tipo, numero, ano){
  tibble::tibble(tipo, numero, ano) %>%
    dplyr::rowwise() %>%
    dplyr::do(
      .congresso_api(.PROPOSICOES_PATH,
                     list(siglaTipo = .$tipo, numero = .$numero, ano = .$ano,
                          ordem = "ASC", ordenarPor = "id"))$dados$id %>%
        .to_tibble()
    ) %>%
    unlist() %>%
    as.vector() %>%
    return()
}

#' Fetches all the proposition types.
#'
#' @return Proposition types
#'
#' @examples
#' tipos_proposicao <- fetch_tipos_proposicao()
#'
#' @export
.fetch_tipos_proposicao <- function(){

  prop_types <- .congresso_api(.TIPOS_PROPOSICOES_PATH)

  return(prop_types$dados)
}

#' Fetches the type of the proposition from its id.
#'
#' @param id_tipo_prop Proposition's type ID
#'
#' @return Dataframe containing the proposition's type info.
#'
#' @examples
#' tipo_prop129 <- fetch_tipo_proposicao(129)
#'
#' @export
fetch_tipo_proposicao <- function(id_tipo_prop){
  prop_types <- .fetch_tipos_proposicao() %>%
    dplyr::mutate(id = as.numeric(.$id))

  tibble::tibble(id = id_tipo_prop) %>%
    dplyr::left_join(prop_types, by = "id") %>%
    return()

}

#' Recovers the proposition status including: sequence, organ
#' and date info about its processing in the parlament.
#'
#' @param id_prop Proposition's ID
#'
#' @return Dataframe containing the proposition's status
#'
#' @examples
#' pec241_status <- fetch_status_proposicao(2088351)
#'
#' @export
fetch_status_proposicao <- function(id_prop){
  id <- NULL
  tibble::tibble(id = id_prop) %>%
    dplyr::mutate(path = paste0(.PROPOSICOES_PATH, "/", id)) %>%
    dplyr::group_by(id) %>%
    dplyr::do(
      .congresso_api(.$path)$dados$statusProposicao %>%
        .remove_lists_and_nulls()
    ) %>%
    dplyr::ungroup() %>%
    return()
}
