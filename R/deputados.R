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

  if(!length(.verifica_parametros_entrada(parametros))){
    .congresso_api(.DEPUTADOS_PATH) %>%
      .assert_dataframe_completo(.COLNAMES_DEP_INFO) %>%
      .coerce_types(.COLNAMES_DEP_INFO)
  }
  else if(is.null(id)){
    .fetch_using_queries(parametros, .DEPUTADOS_PATH) %>%
      .assert_dataframe_completo(.COLNAMES_DEP_INFO) %>%
      .coerce_types(.COLNAMES_DEP_INFO)
  }
  else{
    data <- NULL
    .fetch_using_id(id, .DEPUTADOS_PATH) %>%
      .assert_dataframe_completo(.COLNAMES_DEP_INFO_ID) %>%
      tidyr::nest(which(grepl("redeSocial", names(.)))) %>%
      dplyr::rename(redeSocial = data) %>%
      .coerce_types(.COLNAMES_DEP_INFO_ID) %>%
      tidyr::nest(which(grepl("redeSocial", names(.)))) %>%
      dplyr::rename(redeSocial = data)

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

  .fetch_using_queries(parametros, path) %>%
    .assert_dataframe_completo(.COLNAMES_DEP_GASTOS) %>%
    .coerce_types(.COLNAMES_DEP_GASTOS)
}
