# Função que retorna todas as proposições disponíveis na base de dados da Câmara dos Deputados
# Return: Dataframe contendo todas as informações das proposições.
fetch_proposicoes <- function(){

  props_link <- "https://dadosabertos.camara.leg.br/api/v2/proposicoes?ordem=ASC&ordenarPor=id&itens=100"
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

    print(self_page)

    if(next_page == first_page) {
      break;
    }

  }

  return(props_dataframe)

}

# Recupera uma proposição contendo detalhes adicionais.
# id_prop: ID da proposição.
# Return: Lista contendo informações sobre a proposição.
fetch_proposicao <- function(id_prop){

  full_link <- paste("https://dadosabertos.camara.leg.br/api/v2/proposicoes/", id_prop, sep="")

  prop_json <- .get_json(full_link)

  return(prop_json$dados)

}

# Recupera todas as votações por quais uma proposição já passou.
# id_prop: ID da proposição.
# Return: Dataframe contendo as várias votações pelas quais uma proposição passou.
fetch_votacoes <- function(proposicao){

  full_link <- paste("https://dadosabertos.camara.leg.br/api/v2/proposicoes/", proposicao, "/votacoes", sep="")

  voting_json <- .get_json(full_link)

  return(voting_json$dados)

}

fetch_id_proposicao <- function(tipo, numero, ano){

  full_link <- paste("https://dadosabertos.camara.leg.br/api/v2/proposicoes?siglaTipo=", tipo, "&numero=", numero, "&ano=", ano,"&ordem=ASC&ordenarPor=id", sep="")

  print(full_link)
  prop_json <- .get_json(full_link)

  return(prop_json$dados$id)
}

fetch_tipo_proposicao <- function(){
  prop_types_link <- "https://dadosabertos.camara.leg.br/api/v2/referencias/tiposProposicao"

  prop_types <- .get_json(prop_types_link)

  return(prop_types$dados)
}
