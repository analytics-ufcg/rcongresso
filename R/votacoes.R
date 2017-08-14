# Gets details about a voting
# id_vot: Voting's ID
# Return: List containing the details about the voting, including the results by party.
get_voting <- function(id_vot){

  full_link <- paste("https://dadosabertos.camara.leg.br/api/v2/votacoes/", id_vot, sep="")

  voting <- GET(full_link)
  r <- content(voting, as="text")
  voting_json <- jsonlite::fromJSON(r)
  voting_list <- voting_json$dados

  return(voting_list)
}
