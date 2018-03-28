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
fetch_votacao <- function(id_votacao = NULL){
  tibble::tibble(id_votacao) %>%
    dplyr::mutate(path = paste0(.VOTACOES_PATH, "/", id_votacao)) %>%
    dplyr::rowwise() %>%
    dplyr::do(
      .congresso_api(.$path)
    ) %>%
    dplyr::select(-which(grepl("orientacoes", names(.)))) %>%
    .assert_dataframe_completo(.COLNAMES_VOTACAO) %>%
    .coerce_types(.COLNAMES_VOTACAO)
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
fetch_orientacoes <- function(id_votacao = NULL){
  tibble::tibble(id_votacao) %>%
    dplyr::mutate(path = paste0(.VOTACOES_PATH, "/", id_votacao)) %>%
    dplyr::group_by(id_votacao) %>%
    dplyr::do(
      .congresso_api(.$path, asList = TRUE)$orientacoes
    ) %>%
    dplyr::ungroup() %>%
    .assert_dataframe_completo(.COLNAMES_ORIENTACOES) %>%
    .coerce_types(.COLNAMES_ORIENTACOES)
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
fetch_votos <- function(id_votacao = NULL){
  path <- query <- NULL
  queries <- tibble::tibble(id_votacao) %>%
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
      .congresso_api(.$path, .$query)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-path, -query) %>%
    .assert_dataframe_completo(.COLNAMES_VOTOS) %>%
    .coerce_types(.COLNAMES_VOTOS)
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
ultima_votacao <- function(votacoes = NULL) {
  uriProposicaoPrincipal <- id <- NULL

  votacoes %>%
    dplyr::group_by(uriProposicaoPrincipal) %>%
    dplyr::filter(id == max(id)) %>%
    unique() %>%
    dplyr::ungroup() %>%
    dplyr::select(id, uriProposicaoPrincipal) %>%
    .assert_dataframe_completo(.COLNAMES_ULTIMAVOTACAO) %>%
    .coerce_types(.COLNAMES_ULTIMAVOTACAO)
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
get_votos_partidos <- function(votacao = NULL) {
  nomeBancada <- voto <- bancada_associada <- id_votacao <- partido <- NULL

  fetch_orientacoes(votacao) %>%
    dplyr::mutate(bancada_associada = nomeBancada) %>%
    dplyr::select(partido = nomeBancada, orientacao_partido = voto,
                  bancada_associada, id_votacao) %>%
    tidyr::separate_rows(partido, sep = .REGEX_PATTERN) %>%
    dplyr::mutate(partido = toupper(.$partido)) %>%
    .assert_dataframe_completo(.COLNAMES_VOTOSPARTIDOS) %>%
    .coerce_types(.COLNAMES_VOTOSPARTIDOS)
}

#' @title Finds the id of the proposition to which a given voting refers
#' @description Finds the id of the proposition to which a given voting refers.
#' @param id_votacao Voting's ID
#' @return Dataframe containing proposition details
#' @examples
#' pec241 <- fetch_proposicao_from_votacao(7252)
#' @rdname fetch_proposicao_from_votacao
#' @export
fetch_proposicao_from_votacao <- function(id_votacao = NULL) {
  id <- NULL
  tibble::tibble(id_votacao) %>%
    dplyr::mutate(path = paste0(.VOTACOES_PATH, "/", id_votacao)) %>%
    dplyr::group_by(id_votacao) %>%
    dplyr::do(
      .congresso_api(.$path, asList = TRUE)$proposicao %>%
        .get_dataframe()
    ) %>%
    dplyr::ungroup() %>%
    dplyr::rename(id_proposicao = id) %>%
    .assert_dataframe_completo(.COLNAMES_PROP_VOTACAO) %>%
    .coerce_types(.COLNAMES_PROP_VOTACAO)
}
