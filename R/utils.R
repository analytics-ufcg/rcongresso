if (getRversion() >= "2.15.1")  utils::globalVariables(".")

#' Recovers a json from a URL using HTTP.
#' @param full_link URL admitting GET HTTP requests.
#' @return the json.
#' @examples
#' pec241_json <- .get_json("https://dadosabertos.camara.leg.br/api/v2/proposicoes/2088351")
#' @export
.get_json <- function(response){
  httr::content(response, as = "text") %>%
    jsonlite::fromJSON(flatten = T)
}

#' Wraps an access to the congress API given a reletive path and query arguments.
#' @param path URL relative to the API base URL
#' @param query Query parameters
#' @export
.congresso_api <- function(path=NULL, query=NULL){

  ua <- httr::user_agent(.RCONGRESSO_LINK)
  api_url <- httr::modify_url(.API_LINK, path = path, query = query)

  resp <- httr::GET(api_url, ua, httr::accept_json())

  httr::stop_for_status(resp)

  if (httr::http_type(resp) != "application/json") {
    stop(.ERRO_RETORNO_JSON, call. = FALSE)
  }

  .get_json(resp)
}

#' Removes all the nested lists and null fields from a list.
#' @param x A List
#' @return A clean list (without NULL and nested lists)
#' @examples
#' clean_list <- .remove_lists_and_nulls(list(NULL, list(), 1))
#' @export
.remove_lists_and_nulls <- function(x){
  arr_null <- x %>%
    purrr::map_lgl(is.null) %>%
    which()
  if (length(arr_null)){
    x <- x[-arr_null]
  }

  arr_lists <- x %>%
    purrr::map_lgl(is.list) %>%
    which()
  if (length(arr_lists)){
    x <- x[-arr_lists]
  }

  tibble::as.tibble(x)
}

#' Returns an empty dataframe instead of an empty list. This is useful to operate inside
#' dplyr functions that work only with dataframes.
#' @param lista An empty list
#' @return An empty dataframe
#' @examples
#' empty_df <- .empty_list_to_dataframe(list())
#' @export
.empty_list_to_dataframe <- function(lista) {
  if (is.list(lista) && !length(lista)){
    as.data.frame(lista)
  } else return(lista)
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
.fetch_using_queries <- function(parametros, API_path){
  if (!is.null(parametros$itens)){
    .fetch_all_itens(.verifica_parametros_entrada(parametros), API_path)
  }
  else{
    .verifica_parametros_entrada(parametros) %>%
      tibble::as.tibble() %>%
      dplyr::rowwise() %>%
      dplyr::do(
        .congresso_api(API_path, .)$dados %>%
          .remove_lists_and_nulls()
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
.fetch_using_id <- function(id, API_path){
  tibble::tibble(id) %>%
    dplyr::mutate(path = paste0(API_path, "/", id)) %>%
    dplyr::rowwise() %>%
    dplyr::do(
      .congresso_api(.$path)$dados %>%
        .remove_lists_and_nulls()
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
.fetch_all_itens <- function(query, API_path){

  query$pagina <- seq(1, query$itens/.MAX_ITENS)

  if((query$itens < .MAX_ITENS) || (query$itens %% .MAX_ITENS == 0)){
    query %>%
      tibble::as.tibble() %>%
      dplyr::rowwise() %>%
      dplyr::do(
        .congresso_api(API_path, .)$dados %>%
          .remove_lists_and_nulls()
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
        .congresso_api(API_path, .)$dados %>%
          .remove_lists_and_nulls()
      ) %>%
      dplyr::bind_rows(req_ultima_pagina %>%
                         tibble::as.tibble() %>%
                         dplyr::rowwise() %>%
                         dplyr::do(
                           .congresso_api(API_path, .)$dados %>%
                             .remove_lists_and_nulls()
                         ))
  }
}
