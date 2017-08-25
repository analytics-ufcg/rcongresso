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

  full_link <- paste0(.API_LINK, "votacoes/", id_votacao)

  votacao_json <- .get_json(full_link)

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

  votantes_dataframe <- tibble::tibble(link = paste0(.API_LINK, "votacoes/", id_votacao, "/votos?pagina=", 1:5,"&itens=513")) %>%
    dplyr::rowwise() %>%
    dplyr::do(.get_json(.$link)[[1]] %>% as.data.frame())

  return(votantes_dataframe)
}


