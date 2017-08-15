# This function returns all the propositions available on Camara's site
# Return: Dataframe containing all the propositions info.
get_all_propositions <- function(){

  propositions <- httr::GET("https://dadosabertos.camara.leg.br/api/v2/proposicoes?ordem=ASC&ordenarPor=id&itens=100")
  raise <- httr::content(propositions, as="text")
  propositions_json <- jsonlite::fromJSON(raise)
  propositions_dataframe <- propositions_json$dados

  next_page <- propositions_json$links$href[2]
  self_page <- propositions_json$links$href[3]
  first_page <- propositions_json$links$href[3]
  last_page <- propositions_json$links$href[4]

  # Loop to get all the available propositions. It will take longer than 15 minutes to get all of
  # them. Nowdays, the site has more than 6K pages to go through (14/08/2017).
  while(TRUE){
    p <- httr::GET(next_page)
    r <- httr::content(p, as="text")
    p_json <- jsonlite::fromJSON(r)
    p_dataframe <- p_json$dados

    propositions_dataframe <- rbind(propositions_dataframe, p_dataframe)

    self_page <- next_page
    next_page <- p_json$links$href[2]

    print(self_page)

    if(next_page == first_page) {
      break;
    }

  }

  return(propositions_dataframe)

}

# Get details about a proposition
# id_prop: Proposition ID
# Return: List containing info about the proposition
get_proposition <- function(id_prop){

  full_link <- paste("https://dadosabertos.camara.leg.br/api/v2/proposicoes/", id_prop, sep="")

  prop <- httr::GET(full_link)
  r <- httr::content(prop, as="text")
  prop_json <- jsonlite::fromJSON(r)
  prop_list <- prop_json$dados

  return(prop_list)

}

# Get all the voting whose a proposition went through
# id_prop: Proposition ID
# Return: Dataframe containing info about all the voting
get_proposition_voting <- function(id_prop){

  full_link <- paste("https://dadosabertos.camara.leg.br/api/v2/proposicoes/", id_prop, "/votacoes", sep="")

  prop <- httr::GET(full_link)
  r <- httr::content(prop, as="text")
  prop_json <- jsonlite::fromJSON(r)
  prop_dataframe <- prop_json$dados

  return(prop_dataframe)

}
