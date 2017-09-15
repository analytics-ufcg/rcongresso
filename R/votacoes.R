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
  path <- paste0(.VOTACOES_PATH, "/", id_votacao)
  votacao_json <- .congresso_api(path)

  votacao_json$dados$orientacoes %>%
    dplyr::mutate(id_votacao) %>%
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
  path <- paste0(.VOTACOES_PATH, "/", id_votacao, "/votos")
  queries <- tibble::tibble(query = paste0("pagina=", 1:5,"&itens=513"))

  votantes_dataframe <- queries %>%
    dplyr::rowwise() %>%
    dplyr::do(.congresso_api(path, .$query)$dados)

  votantes_dataframe <- votantes_dataframe %>%
    dplyr::mutate(id_votacao)

  return(votantes_dataframe)
}

# Recebe um dataframe de votações e seleciona a última de acordo com o maior ID
ultima_votacao <- function(votacoes) {

  ultimas_votacoes <- votacoes %>%
    dplyr::group_by(uriProposicaoPrincipal) %>%
    dplyr::filter(id == max(id)) %>%
    unique() %>%
    dplyr::ungroup() %>%
    dplyr::select(id, uriProposicaoPrincipal)

  return(ultimas_votacoes)

}

# regex quebra para casos de GOV. e PCdoB.
get_votos_partidos <- function(votacao) {

  pos_bancadas <- fetch_orientacoes(votacao) %>%
    dplyr::mutate(bancada_associada=nomeBancada) %>%
    dplyr::select(partido=nomeBancada, orientacao_partido=voto, bancada_associada, id_votacao) %>%
    tidyr::separate_rows(partido, sep='(?=[A-Z][^A-Z])') %>%
    dplyr::mutate(partido = toupper(partido))

  return(pos_bancadas)
}

