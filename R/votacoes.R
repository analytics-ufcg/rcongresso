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

  votantes_dataframe <- data.frame()

  for(i in 1:5){

    full_link <- paste0(.API_LINK, "votacoes/", id_votacao, "/votos?pagina=", i,"&itens=513")

    print(full_link)

    v <- .get_json(full_link)
    v_df <- v$dados

    votantes_dataframe <- rbind(votantes_dataframe, v_df)

  }

  return(votantes_dataframe)
}


