#' Recupera da API detalhes sobre uma votação específica.
#'
#' @param id_votacao ID da votação
#'
#' @return Lista contendo os detalhes de uma votação, incluindo o posicionamento de cada bancada
#'
#' @examples
#' votacao_segundoturno_pec241 <- fetch_votacao(7252)
#'
#' @export
fetch_votacao <- function(id_votacao){

  path <- paste0(.VOTACOES_PATH, "/", id_votacao)

  votacao_json <- .congresso_api(path)

  return(votacao_json$dados)
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
  querys <- tibble::tibble(query = paste0("pagina=", 1:5,"&itens=513"))

  votantes_dataframe <- querys %>%
    dplyr::rowwise() %>%
    dplyr::do(.congresso_api(path, .$query)$dados)

  return(votantes_dataframe)
}


