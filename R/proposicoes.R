# This function returns all the propositions available on Camara's site
get_all_propositions <- function(){

  propositions <- GET("https://dadosabertos.camara.leg.br/api/v2/proposicoes?ordem=ASC&ordenarPor=id&itens=100")
  raise <- content(propositions, as="text")
  propositions_json <- jsonlite::fromJSON(raise)
  propositions_dataframe <- propositions_json$dados

  next_page <- propositions_json$links$href[2]
  self_page <- propositions_json$links$href[3]
  first_page <- propositions_json$links$href[3]
  last_page <- propositions_json$links$href[4]

  # Loop to get all the available propositions. It will take longer than 15 minutes to get all of
  # them. Nowdays, the site has more than 6K pages to go through (14/08/2017).
  while(TRUE){
    p <- GET(next_page)
    r <- content(p, as="text")
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
