# Gets details about a voting
# id_vot: Voting's ID
# Return: List containing the details about the voting, including the results by party.
get_voting <- function(id_vot){

  full_link <- paste("https://dadosabertos.camara.leg.br/api/v2/votacoes/", id_vot, sep="")

  voting_json <- .get_json(full_link)

  return(voting_json$dados)
}

# Gets votes of every deputy about a voting
# id_vot: Voting's ID
# Return: Dataframe containing the data/vote of every deputy.
get_voters <- function(id_vot){

  full_link <- paste("https://dadosabertos.camara.leg.br/api/v2/votacoes/", id_vot, "/votos?itens=513", sep="")

  voters <- .get_json(full_link)

  return(voters$dados)
}


