#' Função que recupera um json a partir de um link que use o protocolo REST, desde que esse permita a conversão.
#'
#' @param full_link Link para a solicitação GET
#'
#' @return Json contendo as informações requeridas
#'
#' @examples
#' pec241_json <- .get_json("https://dadosabertos.camara.leg.br/api/v2/proposicoes/2088351")
#'
#' @export
.get_json <- function(full_link){

  prop <- httr::GET(full_link)
  r <- httr::content(prop, as="text")
  prop_json <- jsonlite::fromJSON(r, flatten = T)

  return(prop_json)
}
