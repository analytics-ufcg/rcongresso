if (getRversion() >= "2.15.1")  utils::globalVariables(".")

#' Extracts the JSON data from an HTTP response
#' @param response The HTTP response
#' @return The json
.get_json <- function(response){
  httr::content(response, as = "text") %>%
    jsonlite::fromJSON(flatten = TRUE)
}

.req_succeeded <- function(status_code) {
  return(status_code >= .COD_REQ_SUCCESS_MIN && status_code < .COD_REQ_SUCCESS_MAX)
}

.is_client_error <- function(status_code) {
  return(status_code >= .COD_ERRO_CLIENTE && status_code < .COD_ERRO_SERV)
}

.is_server_error <- function(status_code) {
  return(status_code >= .COD_ERRO_SERV)
}

.throw_req_error <- function(error_code, api_url){
  stop(sprintf(.MENSAGEM_ERRO_REQ, error_code, api_url), call. = FALSE)
}

.use_backoff_exponencial <- function(api_base=NULL, path = NULL, query=NULL, timeout = 0, tentativa = 0){
  final_timeout <- timeout*2.05
  Sys.sleep(final_timeout)
  .get_from_api(api_base, path, query, final_timeout, tentativa)
}

.get_from_url <- function(base_url=NULL, path=NULL, query=NULL, timeout = 1){
  url <- httr::modify_url(base_url, path = path, query = query)

  resp <- .get_from_url_with_exponential_backoff(url, timeout)

  resp
}

.get_from_url_with_exponential_backoff <- function(url=NULL, timeout = 1, ...) {
  tries <- 0
  final_timeout <- timeout
  status_code = -1

  while(!.req_succeeded(status_code) && (tries < .MAX_TENTATIVAS_REQ)) {
    #print(paste("URL:",url))
    resp <- httr::GET(url, ...)
    status_code = httr::status_code(resp)
    Sys.sleep(.DEF_POST_REQ_SLEEP_TIME)

    if(.is_client_error(status_code)){
      .throw_req_error(status_code, url)
    } else if(.is_server_error(status_code)) {
      Sys.sleep(final_timeout)
      final_timeout <- final_timeout*2.05
    }

    tries <- tries + 1
  }

  if (tries >= .MAX_TENTATIVAS_REQ) {
    .throw_req_error(status_code, url)
  }

  resp
}

.get_from_api <- function(api_base=NULL, path=NULL, query=NULL, timeout = 1, tentativa = 0){
  ua <- httr::user_agent(.RCONGRESSO_LINK)
  api_url <- httr::modify_url(api_base, path = path, query = query)

  resp <- .get_from_cache(api_url)

  if (is.null(resp)) {
      resp_in_cache <- FALSE
      #print(paste("URL:",api_url))
      resp <- httr::GET(api_url, ua, httr::accept_json())
      Sys.sleep(.DEF_POST_REQ_SLEEP_TIME)
  } else {
      resp_in_cache <- TRUE
  }

  # Handle errors and retries
  if(httr::status_code(resp) >= .COD_ERRO_CLIENTE &&
     httr::status_code(resp) < .COD_ERRO_SERV){
    if (!resp_in_cache) .put_in_cache(api_url, resp)
    .throw_req_error(httr::status_code(resp), api_url)
  } else if(httr::status_code(resp) >= .COD_ERRO_SERV) {
    if(tentativa < .MAX_TENTATIVAS_REQ){
      .use_backoff_exponencial(api_base, path, query, timeout, tentativa+1)
    } else {
      if (!resp_in_cache) .put_in_cache(api_url, resp)
      .throw_req_error(httr::status_code(resp), api_url)
    }
  }

  if (!resp_in_cache) .put_in_cache(api_url, resp)

  if (httr::http_type(resp) != "application/json") {
    stop(.ERRO_RETORNO_JSON, call. = FALSE)
  }

  return(resp)
}

.get_hrefs <- function(path=NULL, query=NULL) {
  resp <- .get_from_api(.CAMARA_API_LINK, path, query)
  .get_json(resp)$links
}

#' Wraps an access to the camara API given a relative path and query arguments.
#' @param path URL relative to the API base URL
#' @param query Query parameters
#' @export
.camara_api <- function(path=NULL, query=NULL, asList = FALSE){

  resp <- .get_from_api(.CAMARA_API_LINK, path, query)
  obtained_data <- .get_json(resp)$dados

  if(!is.data.frame(obtained_data) && !asList){
    obtained_data %>%
      .get_dataframe()
  } else obtained_data

}

#' Wraps an access to the senate API given a relative path and query arguments.
#' @param path URL relative to the API base URL
#' @param query Query parameters
#' @param asList If return should be a list or a dataframe
#' @export
.senado_api <- function(path=NULL, query=NULL, asList = FALSE){

  resp <- .get_from_api(.SENADO_API_LINK, path, query)
  obtained_data <- .get_json(resp)

  if(!is.data.frame(obtained_data) && !asList){
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

#' Prints a warning and a list.
#' @param msg warning message
#' @param l list
.print_warning_and_list <- function(msg, l) {
  cat(crayon::red("\n", msg, "\n  ", paste(l, collapse="\n   "),"\n"))
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

    if (any(indexes)) {
      .print_warning_and_list("Not found columns:", colnames_y[indexes])
    }
    nao_esperadas = colnames_x[!(colnames_x %in% colnames_y)]
    if (length(nao_esperadas)) {
      .print_warning_and_list("Unexpected columns:", nao_esperadas)
    }

    x[colnames_y[indexes]] <- ifelse(types_y[indexes] == "character", NA_character_, NA_real_)

    x
  } else tibble::tibble()
}

#' Garantees that the dataframe obj has all the correct types passed by types.
#' @param obj dataframe
#' @param types named vector of the columns names and types
.coerce_types <- function(obj, types, order_cols=TRUE){
  if(nrow(obj) != 0){
    if (order_cols) {
      obj <- obj[,order(colnames(obj))]
      types <- unname(types[sort(names(types))])
    } else {
      types <- unname(types[names(types)])
    }

    out <- lapply(1:length(obj),FUN = function(i){
      dynamic_cast <<-.switch_types(types[i])
      obj[,i] %>% unlist() %>% dynamic_cast
    })
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
#' df <- rcongresso:::.to_tibble(c(1,2))
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
#' parametros <- rcongresso:::.verifica_parametros_entrada(list(NULL, itens=100, pagina=1))
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
#' @param API_path API path
#' @param asList If return should be a list or a dataframe
#'
#' @return Dataframe containing information about the proposition.
#'
#' @examples
#' pec241 <- rcongresso:::.fetch_using_queries(
#'    parametros = list(id = "2088351"),
#'    API_path = "/api/v2/proposicoes"
#' )
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
        .camara_api(API_path, ., asList)
      )
  }
}

#' Fetches details from a proposition.
#'
#' @param id Proposition's ID
#' @param API_path API path
#' @param asList If return should be a list or a dataframe
#'
#' @return Dataframe containing information about the proposition.
#'
#' @examples
#' pec241 <- rcongresso:::.fetch_using_id(2088351, API_path = "/api/v2/proposicoes")
#'
#' @export
.fetch_using_id <- function(id, API_path, asList = FALSE){
  tibble::tibble(id) %>%
    dplyr::mutate(path = paste0(API_path, "/", id)) %>%
    dplyr::rowwise() %>%
    dplyr::do(
      .camara_api(.$path, asList = asList)
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
#' @param query query parameters
#' @param API_path API path
.fetch_itens <- function(query, API_path){

  query$pagina <- seq(1, query$itens/.MAX_ITENS)

  if((query$itens < .MAX_ITENS) || (query$itens %% .MAX_ITENS == 0)){
    query %>%
      tibble::as.tibble() %>%
      dplyr::rowwise() %>%
      dplyr::do(
        .camara_api(API_path, .)
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
        .camara_api(API_path, .)
      ) %>%
      dplyr::bind_rows(req_ultima_pagina %>%
                         tibble::as.tibble() %>%
                         dplyr::rowwise() %>%
                         dplyr::do(
                           .camara_api(API_path, .)
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

#' @title Renames dataframe's columns
#' @description Renames dataframe's columns using underscore and lowercase pattern.
#' @param df Dataframe
#' @return Dataframe with renamed columns.
.rename_df_columns <- function(df) {
  names(df) <- names(df) %>%
    .to_underscore
  df
}

#' @title Renames a vector with the pattern of underscores and lowercases
#' @description Renames each item from vector with the pattern: split by underscore and lowercase
#' @param x Strings vector
#' @return Vector containing the renamed strings.
#' @export
.to_underscore <- function(x) {
  gsub('([A-Za-z])([A-Z])([a-z])', '\\1_\\2\\3', x) %>%
    gsub('.', '_', ., fixed = TRUE) %>%
    gsub('([a-z])([A-Z])', '\\1_\\2', .) %>%
    tolower()
}

#' @title Changes the column names of the input dataframe to underscore
#' @description Changes the column names of the input dataframe to underscore
#' @param df input dataframe
#' @return dataframe with underscore column names
#' @export
rename_table_to_underscore <- function(df) {
  new_names = names(df) %>%
    .to_underscore()

  names(df) <- new_names

  df
}

#' @title Renames the cols of the bill's passage on Senate
#' @description Renames each item from vector with the pattern: split by underscore and lowercase
#' @param tramitacao_df Dataframe
#' @return Dataframe containing the renamed strings.
#' @export
.rename_tramitacao_df <- function(tramitacao_df) {
  new_names = names(tramitacao_df) %>%
    .to_underscore() %>%
    stringr::str_replace(
      "identificacao_tramitacao_|
      identificacao_tramitacao_origem_tramitacao_local_|
      identificacao_tramitacao_destino_tramitacao_local_|
      identificacao_tramitacao_situacao_",
      ""
    )

  names(tramitacao_df) <- new_names

  tramitacao_df
}

#' @title Renames the cols of the bill's voting on Senate
#' @description Renames each item from vector with the pattern: split by underscore and lowercase
#' @param df Dataframe
#' @return Dataframe containing the renamed strings.
#' @export
.rename_votacoes_df <- function(df) {
  new_names = names(df) %>%
    .to_underscore() %>%
    stringr::str_replace(
      "sessao_plenaria_|tramitacao_identificacao_tramitacao_|identificacao_parlamentar_",
      ""
    )

  names(df) <- new_names

  df
}

#' @title Get the author on Chamber
#' @description Return a dataframe with the link, name, code, type and house
#' @param prop_id Proposition ID
#' @return Dataframe contendo o link, o nome, o código do tipo, o tipo e a casa de origem do autor.
#' @examples
#' .extract_autor_in_camara(2121442)
#' @export
.extract_autor_in_camara <- function(prop_id) {
  camara_exp <- "camara dos deputados"
  senado_exp <- "senado federal"

  url <- paste0(.CAMARA_PROPOSICOES_PATH, "/", prop_id, "/autores")
  json_voting <- .camara_api(url, asList = T)

  authors <- json_voting %>%
    dplyr::rename(
      autor.uri = uri,
      autor.nome = nome,
      autor.tipo = tipo,
      autor.cod_tipo = codTipo) %>%
    dplyr::mutate(casa_origem =
                    dplyr::case_when(
                      stringr::str_detect(iconv(c(tolower(autor.nome)), from="UTF-8", to="ASCII//TRANSLIT"), camara_exp) | autor.tipo == "Deputado" ~ "Camara dos Deputados",
                      stringr::str_detect(tolower(autor.nome), senado_exp) | autor.tipo == "Senador" ~ "Senado Federal",
                      autor.cod_tipo == 40000 ~ "Senado Federal",
                      autor.cod_tipo == 2 ~ "Camara dos Deputados"))

  partido_estado <- rcongresso::extract_partido_estado_autor(authors$autor.uri %>% tail(1))

  authors %>%
    dplyr::mutate(autor.nome = paste0(autor.nome, " ", partido_estado))
}
