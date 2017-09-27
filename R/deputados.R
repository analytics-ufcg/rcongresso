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

  path <- paste0(.DEPUTADOS_PATH, "/", dep_id)

  .congresso_api(path)$dados %>%
    .remove_lists_and_nulls() %>%
    return()
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

  path <- paste0(.DEPUTADOS_PATH, "/", dep_id, "/despesas")
  query <- list(ordem="ASC", ordenarPor="numAno")

  dep_json <- .congresso_api(path, query)

  return(dep_json$dados)

}
