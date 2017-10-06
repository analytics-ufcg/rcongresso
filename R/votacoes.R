#' Fetches details about a voting.
#'
#' @param id_votacao Voting's ID
#'
#' @return Dataframe containing details about a voting, including tittle,
#'  start voting time, finish voting time, result and approval.
#'
#' @examples
#' votacao_segundoturno_pec241 <- fetch_votacao(7252)
#'
#' @export
fetch_votacao <- function(id_votacao){
  id <- NULL
  tibble::tibble(id = id_votacao) %>%
    dplyr::mutate(path = paste0(.VOTACOES_PATH, "/", id)) %>%
    dplyr::rowwise() %>%
    dplyr::do(
      .congresso_api(.$path)$dados %>%
        .remove_lists_and_nulls()
    ) %>%
    dplyr::ungroup() %>%
    return()
}

#' Fetch how the groups and parties in the chamber of deputies
#' instructed their members to vote on a given voting.
#'
#' @param id_votacao Voting's ID.
#'
#' @return Dataframe containing the parties and their votes
#'
#' @examples
#' orientacoesbancada_segundoturno_pec241 <- fetch_orientacoes(7252)
#'
#' @export
fetch_orientacoes <- function(id_votacao){
  id <- NULL
  tibble::tibble(id = id_votacao) %>%
    dplyr::mutate(path = paste0(.VOTACOES_PATH, "/", id)) %>%
    dplyr::group_by(id) %>%
    dplyr::do(
      .congresso_api(.$path)$dados$orientacoes
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(id_votacao = id) %>%
    dplyr::select(-id) %>%
    return()
}

#' Fetches the votes from a voting.
#' @param id_votacao ID da votação
#'
#' @return Dataframe containing all the deputy's vote on this voting.
#'
#' @examples
#' votos_segundoturno_pec241 <- fetch_votos(7252)
#'
#' @export
fetch_votos <- function(id_votacao){
  path <- query <- NULL
  queries <- tibble::tibble(id_votacao = id_votacao) %>%
    dplyr::mutate(path = paste0(.VOTACOES_PATH, "/", id_votacao, "/votos")) %>%
    dplyr::rowwise() %>%
    dplyr::do(tibble::tibble(id_votacao = .$id_votacao,
              path = .$path,
              query = paste0("pagina=", 1:5, "&itens=100"))) %>%
    dplyr::ungroup()

  votantes_dataframe <- queries %>%
    dplyr::group_by(id_votacao, path, query) %>%
    dplyr::do(
      .congresso_api(.$path, .$query)$dados %>%
        .empty_list_to_dataframe()
      ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-path, -query)

  return(votantes_dataframe)
}

#' Selects the last voting among all the votings given.
#'
#' @param votacoes Dataframe containing all the votings related to a proposition.
#'
#' @return Dataframe containing only the last voting related to a proposition.
#'
#'
#' @examples
#' votacoes_pec241 <- fetch_votacoes(2088351)
#' ultima_votacao <- ultima_votacao(votacoes_pec241)
#'
#' @export
ultima_votacao <- function(votacoes) {
  uriProposicaoPrincipal <- id <- NULL
  ultimas_votacoes <- votacoes %>%
    dplyr::group_by(uriProposicaoPrincipal) %>%
    dplyr::filter(id == max(id)) %>%
    unique() %>%
    dplyr::ungroup() %>%
    dplyr::select(id, uriProposicaoPrincipal)

  return(ultimas_votacoes)

}

#' Fetch how only the parties in the chamber of deputies
#' instructed their members to vote on a given voting.
#'
#' @param votacao Dataframe contendo informações sobre a votação
#'
#' @return Dataframe contendo o posicionamento de cada partido naquela votação
#'
#' @examples
#' pos_partidos <- get_votos_partidos(7252)
#'
#' @export
# regex quebra para casos de GOV. e PCdoB.
get_votos_partidos <- function(votacao) {
  nomeBancada <- voto <- bancada_associada <- id_votacao <- partido <- NULL
  pos_bancadas <- fetch_orientacoes(votacao) %>%
    dplyr::mutate(bancada_associada = nomeBancada) %>%
    dplyr::select(partido = nomeBancada, orientacao_partido = voto,
                  bancada_associada, id_votacao) %>%
    tidyr::separate_rows(partido, sep = .REGEX_PATTERN) %>%
    dplyr::mutate(partido = toupper(.$partido))

  return(pos_bancadas)
}

#' Finds the id of the proposition to which a given voting refers.
#'
#' @param id_votacao Voting's ID.
#'
#' @return A proposition in dataframe format
#'
#' @examples
#' pec241 <- fetch_proposicao_from_votacao(7252)
#'
#' @export
fetch_proposicao_from_votacao <- function(id_votacao) {
  id <- NULL
  tibble::tibble(id_votacao) %>%
    dplyr::mutate(path = paste0(.VOTACOES_PATH, "/", id_votacao)) %>%
    dplyr::group_by(id_votacao) %>%
    dplyr::do(
      .congresso_api(.$path)$dados$proposicao %>%
        .remove_lists_and_nulls()
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(id_proposicao = id) %>%
    dplyr::select(-id) %>%
    return()
}
