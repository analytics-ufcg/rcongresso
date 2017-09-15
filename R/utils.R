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
  r_json <- jsonlite::fromJSON(r, flatten = T)

  return(r_json)
}

#' Função principal do wrapper da API que é responśavel pelos GETs e separação dos paths e queries de consulta.
#'
#' @param path Caminho de acesso do objeto
#' @param query Lista de parâmetros que se quer utilizar para realizar a consulta
#'
#' @export
.congresso_api <- function(path=NULL, query=NULL){

  ua <- httr::user_agent(.RCONGRESSO_LINK)
  api_url <- httr::modify_url(.API_LINK, path = path, query = query)

  resp <- httr::GET(api_url, ua, httr::accept_json())

  httr::stop_for_status(resp)

  if (httr::http_type(resp) != "application/json") {
    stop(.ERRO_RETORNO_JSON, call. = FALSE)
  }

  resp_json <- .get_json(resp)

  return(resp_json)
}

.remove_lists_and_nulls <- function(x){
  x <- x[-which(sapply(x, is.list))]
  x <- x[-which(sapply(x, is.null))]
  tibble::as.tibble(x)
}
