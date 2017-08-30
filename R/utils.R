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
.get_json <- function(response){

  r <- httr::content(response, as="text")
  r_json <- jsonlite::fromJSON(r, flatten = T, simplifyVector = FALSE)

  return(r_json)
}

# Esse método deve receber a query, os parâmetros e o path separadamente, pelo que entendi. É uma boa prática fazer essa
# separação e utilizar o 'modify_url' lidar com essa parte ao invés de usar um paste0. Ao invés de chamar o .get_json()
# nos métodos principais, estes devem chamar essa função e essa deve chamar o .get_json() ou podemos unificar as duas
# funções.
.congresso_api <- function(query){

  api_url <- httr::modify_url(.API_LINK, query = query)

  resp <- httr::GET(api_url, httr::accept_json())

  if (httr::http_type(resp) != "application/json") {
    stop(.ERRO_RETORNO_JSON, call. = FALSE)
  }

  resp_json <- .get_json(resp)

  return(resp_json)

}
