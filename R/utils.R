if (getRversion() >= "2.15.1")  utils::globalVariables(".")

#' Recovers a json from a URL using HTTP.
#' @param full_link URL admitting GET HTTP requests.
#' @return the json.
#' @examples
#' pec241_json <- .get_json("https://dadosabertos.camara.leg.br/api/v2/proposicoes/2088351")
#' @export
.get_json <- function(response){
  httr::content(response, as = "text") %>%
    jsonlite::fromJSON(flatten = TRUE)
}

.use_backoff_exponencial <- function(path = NULL, query=NULL, timeout = 0, tentativa = 0){
  final_timeout <- timeout*2.05
  Sys.sleep(final_timeout)
  .get_from_api(path, query, final_timeout, tentativa)
}

.get_from_api <- function(path=NULL, query=NULL, timeout = 1, tentativa = 0){
  ua <- httr::user_agent(.RCONGRESSO_LINK)
  api_url <- httr::modify_url(.API_LINK, path = path, query = query)

  #print(api_url)

  resp <- httr::GET(api_url, ua, httr::accept_json())

  if(httr::status_code(resp) >= .COD_ERRO_CLIENTE &&
     httr::status_code(resp) < .COD_ERRO_SERV){
    .MENSAGEM_ERRO_REQ(httr::status_code(resp), api_url)
  } else if(httr::status_code(resp) >= .COD_ERRO_SERV) {
    if(tentativa < .MAX_TENTATIVAS_REQ){
      .use_backoff_exponencial(path, query, timeout, tentativa+1)
    } else .MENSAGEM_ERRO_REQ(httr::status_code(resp), api_url)
  }

  if (httr::http_type(resp) != "application/json") {
    stop(.ERRO_RETORNO_JSON, call. = FALSE)
  }

  resp
}

.get_hrefs <- function(path=NULL, query=NULL) {
  resp <- .get_from_api(path, query)
  .get_json(resp)$links
}

#' Wraps an access to the congress API given a relative path and query arguments.
#' @param path URL relative to the API base URL
#' @param query Query parameters
#' @export
.congresso_api <- function(path=NULL, query=NULL, asList = FALSE){

  resp <- .get_from_api(path, query)
  obtained_data <- .get_json(resp)$dados

  if(!is.data.frame(obtained_data) && !asList){
    #print("conversao")
    obtained_data %>%
      .get_dataframe()
  } else obtained_data

}

#' In case of receiving a list, this function converts the list into a dataframe.
#' @param x List
.get_dataframe <- function(x){
  x %>%
    #lapply(.replace_null) %>%
    unlist() %>%
    #.coerce_numeric() %>%
    as.list() %>%
    as.data.frame(stringsAsFactors = FALSE)
}

#' Garantees that the dataframe x has all the columns passed by y.
#' @param x dataframe
#' @param y vector of characters containing the names of columns.
.assert_dataframe_completo <- function(x, y){
  if(nrow(x) != 0){
    colnames_x <- colnames(x)
    colnames_y <- names(y)
    types_y <- unname(y)
    indexes <- !(colnames_y %in% colnames_x)

    x[colnames_y[indexes]] <- ifelse(types_y[indexes] == "character", NA_character_, NA_real_)

    x
  } else tibble::tibble()
}

#' Garantees that the dataframe obj has all the correct types passed by types.
#' @param obj dataframe
#' @param types named vector of the columns names and types
.coerce_types <- function(obj, types){
  if(nrow(obj) != 0){
    obj <- obj[,order(colnames(obj))]
    types <- unname(types[sort(names(types))])
    out <- lapply(1:length(obj),FUN = function(i){
      FUN1 <- .switch_types(types[i]); suppressWarnings(obj[,i] %>% unlist() %>% FUN1)})
    names(out) <- colnames(obj)
    as.data.frame(out,stringsAsFactors = FALSE)
  } else tibble::tibble()
}

#' Returns a conversion function given a type name.
#' @param x type name
.switch_types <- function(x){
  switch(x,
         character = as.character,
         numeric = as.numeric,
         integer = as.integer,
         is.na = NA,
         list = as.list,
         logical = as.logical)
}

#' Converts a vector of integer into a tibble. Also useful when the user is working with
#' dplyr functions.
#' @param num A integer vector
#' @return A tibble
#' @examples
#' df <- .to_tibble(c(1,2))
#' @export
.to_tibble <- function(num) {
  if (is.null(num)) tibble::tibble()
  else tibble::tibble(num)
}

#' Verifies from the input if all the parameters are available and handles correctly
#' about the transformation into a valid URL query.
#' @param parametros A list of parameters from the input
#' @return A list of parameters without NULL
#' @examples
#' parametros <- .verifica_parametros_entrada(list(NULL, itens=100, pagina=1))
#' @export
.verifica_parametros_entrada <- function(parametros) {
  is_missing <- parametros %>%
    purrr::map_lgl(is.null) %>%
    which()
  parametros[-is_missing]
}

#' Fetches a proposition using a list of queries
#'
#' @param parametros queries used on the search
#'
#' @return Dataframe containing information about the proposition.
#'
#' @examples
#' pec241 <- .fetch_using_queries(siglaTipo = "PEC", numero = 241, ano = 2016)
#'
#' @export
.fetch_using_queries <- function(parametros, API_path, asList = FALSE){
  if (!is.null(parametros$itens) && (parametros$itens == -1)){
    .fetch_all_items(.verifica_parametros_entrada(parametros), API_path)
  }
  else if (!is.null(parametros$itens)){
    .fetch_itens(.verifica_parametros_entrada(parametros), API_path)
  }
  else{
    .verifica_parametros_entrada(parametros) %>%
      tibble::as.tibble() %>%
      dplyr::rowwise() %>%
      dplyr::do(
        .congresso_api(API_path, ., asList)
      )
  }
}

#' Fetches details from a proposition.
#'
#' @param id Proposition's ID
#'
#' @return Dataframe containing information about the proposition.
#'
#' @examples
#' pec241 <- .fetch_using_id(2088351)
#'
#' @export
.fetch_using_id <- function(id, API_path, asList = FALSE){
  tibble::tibble(id) %>%
    dplyr::mutate(path = paste0(API_path, "/", id)) %>%
    dplyr::rowwise() %>%
    dplyr::do(
      .congresso_api(.$path, asList = asList)
    ) %>%
    dplyr::ungroup()
}

#' Abstracts the pagination logic. There are three situations on a request from the API:
#' 1. The items number is less than the max by request, that is 100.
#' 2. The items number is divisible by 100.
#' 3. The items number is not divisible by 100.
#'
#' Case 1 and 2 are solved using the same logic: Fetches from the API the exact quantity
#' to fill n pages. 100 items fill into 1 page, 200 items fill into 2 pages and so on...
#'
#' Case 3 is solved using the previous thinking adding a little detail: Fetches all the items
#' until it fills completly the pages with 100 items each one, then insert the remaining
#' items. 530 items can also be read as 500 items + 30 items, then 5 pages with 100 items and
#' 1 page with 30 items.
#'
.fetch_itens <- function(query, API_path){

  query$pagina <- seq(1, query$itens/.MAX_ITENS)

  if((query$itens < .MAX_ITENS) || (query$itens %% .MAX_ITENS == 0)){
    query %>%
      tibble::as.tibble() %>%
      dplyr::rowwise() %>%
      dplyr::do(
        .congresso_api(API_path, .)
      )
  } else {
    req_ultima_pagina <- query
    req_ultima_pagina$itens <- query$itens %% .MAX_ITENS
    req_ultima_pagina$pagina <- max(seq(1, query$itens/.MAX_ITENS)) +1
    query$itens <- .MAX_ITENS

    query %>%
      tibble::as.tibble() %>%
      dplyr::rowwise() %>%
      dplyr::do(
        .congresso_api(API_path, .)
      ) %>%
      dplyr::bind_rows(req_ultima_pagina %>%
                         tibble::as.tibble() %>%
                         dplyr::rowwise() %>%
                         dplyr::do(
                           .congresso_api(API_path, .)
                         ))
  }
}

#' Check if a proposition/party exists and returns a warning message when it does not.
#' @param x returned object from proposition or party function
#' @param message error message expected
.verifica_id <- function(x, message) {
  if(is.null(x)){
    warning(message)
    x
  } else x
}

.fetch_all_items <- function(query, API_path){

  href <- rel <- NULL

  query$itens <- .MAX_ITENS

  # Pegar pelo "last" e não buscar pelo índice diretamente, já que o índice pode variar.
  list_param <- .get_hrefs(path = API_path, query = query) %>%
    dplyr::filter(rel == "last") %>%
    dplyr::select(href) %>%
    purrr::pluck(1) %>%
    strsplit("/") %>%
    purrr::pluck(1, length(.[[1]])) %>%
    strsplit("&") %>%
    purrr::pluck(1)

  # Procurar pelo parâmetro página. Mesma lógica do babado aqui em cima.
  index_ult_pag <- list_param %>%
    stringr::str_detect("pagina")

  ult_pag <- list_param[index_ult_pag] %>%
    strsplit("=")

  query$itens <- as.integer(ult_pag[[1]][2]) * .MAX_ITENS

  .fetch_itens(query, API_path)
}
