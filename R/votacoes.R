# Gets details about a voting
# id_vot: Voting's ID
# Return: List containing the details about the voting, including the results by party.
get_voting <- function(id_vot){

  full_link <- paste("https://dadosabertos.camara.leg.br/api/v2/votacoes/", id_vot, sep="")

  voting <- httr::GET(full_link)
  r <- httr::content(voting, as="text")
  voting_json <- jsonlite::fromJSON(r)
  voting_list <- voting_json$dados

  return(voting_list)
}

# Gets votes of every deputy about a voting
# id_vot: Voting's ID
# Return: Dataframe containing the data/vote of every deputy.
get_voters <- function(id_vot){

  full_link <- paste("https://dadosabertos.camara.leg.br/api/v2/votacoes/", id_vot, "/votos?itens=513", sep="")

  voters <- httr::GET(full_link)
  r <- httr::content(voters, as="text")
  voters_json <- jsonlite::fromJSON(r)
  voters_dataframe <- voters_json$dados

  return(voters_dataframe)
}


