#' Recupera da API todas as proposições disponíveis na base de dados da Câmara dos Deputados.
#'
#' @return Dataframe contendo todas as informações das proposições
#'
#' @examples
#' proposicoes <- fetch_proposicoes()
#'
#' @export
fetch_proposicoes <- function(){

  props_link <- paste0(.API_LINK, "proposicoes?ordem=ASC&ordenarPor=id&itens=100")
  props_json <- .get_json(props_link)
  props_dataframe <- props_json$dados

  next_page <- props_json$links$href[2]
  self_page <- props_json$links$href[3]
  first_page <- props_json$links$href[3]
  last_page <- props_json$links$href[4]

  # Loop para pegar todas as proposições disponíveis no site. Uma requisição pega apenas 100 itens por vez,
  # então percorremos as próximas páginas até a última e pegamos todas as proposições.
  while(TRUE){
    p_json <- .get_json(next_page)
    p_dataframe <- p_json$dados

    props_dataframe <- rbind(props_dataframe, p_dataframe)

    self_page <- next_page
    next_page <- p_json$links$href[2]

    if(next_page == first_page) {
      break;
    }

  }

  return(props_dataframe)

}

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

  full_link <- paste0(.API_LINK, "proposicoes/", id_prop)

  prop_json <- .get_json(full_link)

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

  full_link <- paste0(.API_LINK, "proposicoes/", id_prop, "/votacoes")

  voting_json <- .get_json(full_link)

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

  full_link <- paste0(.API_LINK, "proposicoes?siglaTipo=", tipo, "&numero=", numero, "&ano=", ano,"&ordem=ASC&ordenarPor=id")

  prop_json <- .get_json(full_link)

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
  prop_types_link <- paste0(.API_LINK, "referencias/tiposProposicao")

  prop_types <- .get_json(prop_types_link)

  return(prop_types$dados)
}
