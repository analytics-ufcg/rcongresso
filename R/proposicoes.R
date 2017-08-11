get_all_proposicoes <- function(){

  # Here's gonna have a loop or something to fetch all the propositions available in every page.
  # The GET command recovers only 100 items for request.
  propositions <- GET("https://dadosabertos.camara.leg.br/api/v2/proposicoes?ordem=ASC&ordenarPor=id&itens=100")
  raise <- content(propositions, as="text")
  propositions_json <- jsonlite::fromJSON(raise)
  propositions_dataframe <- propositions_json$dados
  propositions_references <- propositions_json$links

}
