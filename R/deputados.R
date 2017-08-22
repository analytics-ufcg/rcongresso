#' Recupera da API os detalhes de um deputado.
#'
#' @param dep_id ID do Deputado
#'
#' @return Lista contendo detalhes sobre o deputado.
#'
#' @examples
#' abel_mesquita_info <- fetch_deputado(178957)
#'
#' @export
fetch_deputado <- function(dep_id){

  full_link <- paste(.API_LINK, "deputados/", dep_id, sep="")

  dep_json <- .get_json(full_link)

  return(dep_json$dados)

}

#' Recupera todos os gastos da cota parlamentar, dos últimos seis meses, um deputado específico.
#'
#' @param dep_id ID do deputado
#'
#' @return Dataframe contendo detalhes sobre os gastos do deputado
#'
#' @examples
#' gastos_abel_mesquita <- fetch_despesas_deputado(178957)
#'
#' @export
fetch_despesas_deputado <- function(dep_id) {

  full_link <- paste(.API_LINK, "deputados/", dep_id, "/despesas?ordem=ASC&ordenarPor=numAno", sep="")

  dep_json <- .get_json(full_link)

  return(dep_json$dados)

}
