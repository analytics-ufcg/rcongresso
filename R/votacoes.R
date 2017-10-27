#' @title Fetches details about a voting
#' @description Fetches details about a voting.
#' @param id_votacao Voting's ID
#' @return Dataframe containing details about a voting, including tittle,
#'  start voting time, finish voting time, result and approval
#' @examples
#' segundoturno_pec241 <- fetch_votacao(7252)
#' @seealso
#'  \code{\link[rcongresso]{fetch_votos}}
#' @rdname fetch_votacao
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
    dplyr::ungroup()
}

#' @title Fetches the positions of a group on a voting
#' @description Fetch how the groups in the chamber of deputies
#' instructed their members to vote on a given voting.
#' @param id_votacao  Voting's ID
#' @return Dataframe containing the groups and their votes
#' @examples
#' orientacoes_pec241 <- fetch_orientacoes(7252)
#' @seealso
#'  \code{\link[rcongresso]{get_votos_partidos}}
#' @rdname fetch_orientacoes
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
    dplyr::select(-id)
}

#' @title Fetches individual votes from a voting
#' @description Some other informations about the deputy are returned, such as: name, party, id.
#' @param id_votacao Voting's ID
#' @return Dataframe containing votes for each deputy on this voting.
#' @details DETAILS
#' @examples
#' votos_pec241 <- fetch_votos(7252)
#' @seealso
#'  \code{\link[rcongresso]{fetch_votacao}}
#' @rdname fetch_votos
#' @export
fetch_votos <- function(id_votacao){
  path <- query <- NULL
  queries <- tibble::tibble(id_votacao = id_votacao) %>%
    dplyr::mutate(path = paste0(.VOTACOES_PATH, "/", id_votacao, "/votos")) %>%
    dplyr::rowwise() %>%
    dplyr::do(
      tibble::tibble(id_votacao = .$id_votacao,
              path = .$path,
              query = paste0("pagina=", 1:5, "&itens=100"))
    ) %>%
    dplyr::ungroup()

  queries %>%
    dplyr::group_by(id_votacao, path, query) %>%
    dplyr::do(
      .congresso_api(.$path, .$query)$dados %>%
        .empty_list_to_dataframe()
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-path, -query)
}

#' @title Gets the last voting on a given voting
#' @description Filters only the last voting from each proposition in the dataframe.
#' @param votacoes Dataframe containing all the votings related to a proposition
#' @return Dataframe containing only the last voting related to a proposition
#' @examples
#' votacoes_pec241 <- fetch_votacoes(2088351)
#' ultima_votacao <- ultima_votacao(votacoes_pec241)
#' @rdname ultima_votacao
#' @export
ultima_votacao <- function(votacoes) {
  uriProposicaoPrincipal <- id <- NULL

  votacoes %>%
    dplyr::group_by(uriProposicaoPrincipal) %>%
    dplyr::filter(id == max(id)) %>%
    unique() %>%
    dplyr::ungroup() %>%
    dplyr::select(id, uriProposicaoPrincipal)
}

#' @title Fetches the positions by party on a voting
#' @description Fetch how parties in the chamber of deputies
#' instructed their members to vote on a given voting.
#' @param votacao Voting's id
#' @return Dataframe with each party's orientation for its deputies.
#' @examples
#' pos_partidos <- get_votos_partidos(7252)
#' @seealso
#'  \code{\link[rcongresso]{fetch_orientacoes}}
#' @rdname get_votos_partidos
#' @export
get_votos_partidos <- function(votacao) {
  nomeBancada <- voto <- bancada_associada <- id_votacao <- partido <- NULL

  fetch_orientacoes(votacao) %>%
    dplyr::mutate(bancada_associada = nomeBancada) %>%
    dplyr::select(partido = nomeBancada, orientacao_partido = voto,
                  bancada_associada, id_votacao) %>%
    tidyr::separate_rows(partido, sep = .REGEX_PATTERN) %>%
    dplyr::mutate(partido = toupper(.$partido))
}

#' @title Finds the id of the proposition to which a given voting refers
#' @description Finds the id of the proposition to which a given voting refers.
#' @param id_votacao Voting's ID
#' @return Dataframe containing proposition details
#' @examples
#' pec241 <- fetch_proposicao_from_votacao(7252)
#' @rdname fetch_proposicao_from_votacao
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
    dplyr::select(-id)
}
