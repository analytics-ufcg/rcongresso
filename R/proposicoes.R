#' Recupera da API uma proposição contendo detalhes adicionais.
#'
#' @param id_prop ID da proposição
#'
#' @return Lista contendo informações sobre a proposição
#'
#' @examples
#' pec241 <- fetch_proposicao(2088351)
#'
#' @export
fetch_proposicao <- function(id_prop){

  path <- paste0("proposicoes/", id_prop)

  prop_json <- .get_json(path)

  return(prop_json$dados)

}

#' Recupera da API todas as votações pelas quais uma proposição já passou.
#'
#' @param id_prop ID da proposição
#'
#' @return Dataframe contendo as várias votações pelas quais uma proposição passou.
#'
#' @examples
#' votacoes_pec241 <- fetch_votacoes(2088351)
#'
#' @export
fetch_votacoes <- function(id_prop){

  path <- paste0("proposicoes/", id_prop, "/votacoes")

  voting_json <- .get_json(path)

  return(voting_json$dados)

}

#' Recupera da API o ID da preposição de acordo com o tipo, número da proposição e ano que
#' esta foi feita.
#'
#' @param tipo Tipo da proposição (i.e., PEC, PL, PDC)
#' @param numero Número da proposição
#' @param ano Ano da proposição
#'
#' @return ID da proposição
#'
#' @examples
#' pec241_id <- fetch_id_proposicao("PEC", 241, 2016)
#'
#' @export
fetch_id_proposicao <- function(tipo, numero, ano){

  path <- paste0("proposicoes?siglaTipo=", tipo, "&numero=", numero, "&ano=", ano,"&ordem=ASC&ordenarPor=id")

  prop_json <- .get_json(path)

  return(prop_json$dados$id)
}

#' Recupera da API os tipos de proposição disponíveis.
#'
#' @return Tipos de proposição
#'
#' @examples
#' tipos_proposicao <- fetch_tipos_proposicao()
#'
#' @export
fetch_tipos_proposicao <- function(){
  path <- paste0("referencias/tiposProposicao")

  prop_types <- .get_json(path)

  return(prop_types$dados)
}
