#' @title Retrieves details about a deputy
#' @description ID, name, birth date, birth city among other informations are returned.
#' @param id Deputy's ID
#' @param nome Deputy's name (partially or complete)
#' @param idLegislatura Id of one or more mandates of the deputy
#' @param siglaUf State's abbreviation of the deputy
#' @param siglaPartido Party's abbreviation of the deputy
#' @param siglaSexo Gender's abbreviation
#' @param itens Items quantity. '-1' returns all current deputies at the request moment
#' @param ordenarPor Element that will be used to sort the returned list
#' @return Dataframe containing details about the deputy.
#' @examples
#' abel_mesquita_info <- fetch_deputado(id = 178957)
#' @rdname fetch_deputado
#' @export
fetch_deputado <- function(id = NULL, nome = NULL, idLegislatura = NULL, siglaUf = NULL, siglaPartido = NULL,
                           siglaSexo = NULL, itens = NULL, ordenarPor = NULL){

  parametros <- as.list(environment(), all = TRUE)

  if ( !length(.verifica_parametros_entrada(parametros))) {
    .camara_api(.DEPUTADOS_PATH) %>%
      .assert_dataframe_completo(.COLNAMES_DEP_INFO) %>%
      .coerce_types(.COLNAMES_DEP_INFO)
  } else if ( is.null(id)) {
    .fetch_using_queries(parametros, .DEPUTADOS_PATH) %>%
      .assert_dataframe_completo(.COLNAMES_DEP_INFO) %>%
      .coerce_types(.COLNAMES_DEP_INFO)
  } else {
    .fetch_using_id(id, .DEPUTADOS_PATH) %>%
      .assert_dataframe_completo(.COLNAMES_DEP_INFO_ID) %>%
      .coerce_types(.COLNAMES_DEP_INFO_ID)
  }

}

#' @title Fetches expenditures from deputy
#' @description Fetches expenditures from deputy with his/her parlamentary quota. If no time
#' parameters are passed, the fucntion returns the expenditures in the last six months.
#' @param id Deputy's ID. Mandatory field
#' @param idLegislatura Deputy's ID
#' @param ano Expenditure's year
#' @param mes Expenditure's month
#' @param cnpjCpfFornecedor Provider's CPF or CNPJ
#' @param itens Items quantity. '-1' returns all the expenditures
#' @return Dataframe containing details about the deputy's expenditures
#' @examples
#' gastos_abel_mesquita <- fetch_despesas_deputado(id = 178957)
#' gastos_abel_junho2017 <- fetch_despesas_deputado(id = 178957, ano = 2017, mes = 06, itens = -1)
#' @rdname fetch_despesas_deputado
#' @export
fetch_despesas_deputado <- function(id = NULL, idLegislatura = NULL, ano = NULL, mes = NULL,
                                    cnpjCpfFornecedor = NULL, itens = NULL) {

  parametros <- as.list(environment(), all = TRUE)
  path <- paste0(.DEPUTADOS_PATH, "/", id, "/despesas")

  .camara_api(path) %>%
    .assert_dataframe_completo(.COLNAMES_DEP_GASTOS) %>%
    .coerce_types(.COLNAMES_DEP_GASTOS)
}

#' @title Get the state and party of an author
#' @description Return state and party
#' @param uri uri that contains data about the author
#' @return State and party
#' @export
extract_partido_estado_autor <- function(uri) {
  if (!is.na(uri)) {
    resp <- .get_from_api(uri, NULL, NULL)
    autor <- .get_json(resp)$dados

    autor_uf <-
      autor %>%
      magrittr::extract2('ufNascimento')

    autor_partido <-
      autor %>%
      magrittr::extract2('ultimoStatus') %>%
      magrittr::extract2('siglaPartido')

    paste0(autor_partido, '/', autor_uf)
  } else {
    ''
  }
}

#' @title Fetches details abaout all deputys
#' @description Fetches details about deputys from the 40º legislature to the current
#' @param ids_dep Dataframe containing all deputys IDs
#' @return Dataframe containing details about the deputy's
#' @rdname fetch_all_deputados
#' @export
fetch_all_deputados <- function(ids_dep) {

  deputados <- tibble::tibble()

  if (is.null(dim(ids_dep)) | !is.data.frame(ids_dep) ) {
    warning("Objeto deve ser um dataframe nao-nulo")
  } else if (nrow(ids_dep) == 0) {
    warning("Dataframe vazio")
  } else {
    deputados <- purrr::map_df(ids_dep$id, ~(fetch_deputado(.x) %>%
                                               dplyr::mutate_all(~ as.character(.))))

    deputados <- deputados %>%
      .assert_dataframe_completo(.COLNAMES_DEP_INFO_ID) %>%
      .coerce_types(.COLNAMES_DEP_INFO_ID) %>%
      .rename_df_columns()
  }

  deputados
}

#' @title Fetches all deputys IDs
#' @description Fetches all deputys IDs from the given legislature to the current
#' @param legislatura_base Legislatura inicial para retornar os deputados
#' @return Dataframe containing all deputys IDs
#' @rdname fetch_all_deputados
#' @export
fetch_ids_deputados <- function(legislatura_base = .LEGISLATURA_INICIAL) {

  url <- paste0(.CAMARA_API_LINK, .URL_TABELA_DEP)
  tabela_deputados <- readr::read_delim(
    url,
    delim = ";",
    col_types = list(
      .default = readr::col_character(),
      idLegislaturaInicial = readr::col_double(),
      idLegislaturaFinal = readr::col_double(),
      dataNascimento = readr::col_date(format = ""),
      dataFalecimento = readr::col_date(format = "")
    )
  ) %>%
    dplyr::filter(idLegislaturaInicial >= legislatura_base)

  ids_dep <-
    tabela_deputados %>%
    dplyr::rowwise() %>%
    dplyr::mutate(id = as.character(.get_id(as.character(uri)))) %>%
    dplyr::select(id)
  ids_dep
}


#' @title Fetches all fronts from deputy
#' @description Fetches all fronts from a deputy from the given legislature
#' @param id_dep Deputy's ID
#' @param legislatura_base Fronts legislature
#' @return Dataframe containing all fronts from a deputy
#' @rdname fetch_frentes_deputado
#' @export
fetch_frentes_deputado <- function(id_dep, legislatura_base = .LEGISLATURA_ATUAL) {
  frentes <- .camara_api(paste0(.DEPUTADOS_PATH, "/", id_dep, "/frentes"))
  frentes <- frentes %>%
    dplyr::mutate(id_deputado = id_dep) %>%
    dplyr::mutate(id_deputado = as.integer(id_deputado)) %>%
    dplyr::rename(id_frente = id) %>%
    .rename_df_columns() %>%
    .assert_dataframe_completo(.COLNAMES_DEPUTADO_FRENTES) %>%
    .coerce_types(.COLNAMES_DEPUTADO_FRENTES) %>%
    dplyr::filter(id_legislatura == legislatura_base)

  frentes
}

.get_id <- function(uri) {
  return(tail(unlist(strsplit(uri, "/")), 1))
}
