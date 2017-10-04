#' Recupera da API detalhes sobre uma votação específica.
#'
#' @param id_votacao ID da votação
#'
#' @return Dataframe contendo os detalhes de uma votação, incluindo título, a hora de início da votação,
#' hora de término da votação, placar e sua aprovação.
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
    return()
}

#' Recupera da API as orientações das bancadas em uma determinada votação.
#'
#' @param id_votacao ID da votação
#'
#' @return Dataframe contendo as orientações das bancadas na votação especificada
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
    dplyr::mutate(id_votacao=id) %>%
    dplyr::select(-id) %>%
    return()
}

#' Recupera os votos referentes àquela votação específica.
#' @param id_votacao ID da votação
#'
#' @return Dataframe contendo o posicionamento de cada deputado, além de informações sobre estes.
#'
#' @examples
#' votos_segundoturno_pec241 <- fetch_votos(7252)
#'
#' @export
fetch_votos <- function(id_votacao){
  path <- query <- NULL
  queries = tibble::tibble(id_votacao = id_votacao) %>%
    dplyr::mutate(path = paste0(.VOTACOES_PATH, "/", id_votacao, "/votos")) %>%
    dplyr::rowwise() %>%
    dplyr::do(tibble::tibble(id_votacao = .$id_votacao,
              path = .$path,
              query = paste0("pagina=", 1:5,"&itens=100"))) %>%
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

#' Recebe um dataframe de votações e seleciona a última votação de cada proposição.
#'
#' @param votacoes Dataframe contendo as votações a serem filtradas
#'
#' @return Dataframe contendo apenas as últimas votações relacionadas às proposições
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

#' Dada uma votação, retorna o posicionamento de cada partido naquela votação.
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
    dplyr::mutate(bancada_associada=nomeBancada) %>%
    dplyr::select(partido=nomeBancada, orientacao_partido=voto, bancada_associada, id_votacao) %>%
    tidyr::separate_rows(partido, sep='(?=[A-Z][^A-Z])') %>%
    dplyr::mutate(partido = toupper(.$partido))

  return(pos_bancadas)
}

#' Dado o id da votação, retorna a proposição associada àquela votação.
#'
#' @param id_votacao ID da votação
#'
#' @return Dataframe contendo informações sobre a proposição relacionada à votação
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
    dplyr::mutate(id_proposicao=id) %>%
    dplyr::select(-id) %>%
    return()
}

