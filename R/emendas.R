#' @title Retorna as emendas de uma proposição na Camara
#' @description Retorna dataframe com os dados das emendas de uma proposição na Camara
#' @param id ID de uma proposição da Camara
#' @param sigla Sigla da proposição
#' @param numero Numero da proposição
#' @param ano Ano da proposição
#' @return Dataframe com as informações sobre as emendas de uma proposição na Camara
#' @examples
#' fetch_emendas_camara(408406)
fetch_emendas_camara <- function(id=NULL, sigla=NULL, numero=NULL, ano=NULL) {
  if (!is.null(id)) {
    proposicao <- fetch_proposicao_camara(id)
    .fetch_emendas_by_queries_camara(proposicao$tipo_materia, proposicao$numero, proposicao$ano)
  } else {
    .fetch_emendas_by_queries_camara(sigla, numero, ano)
  }
}

# Retorna um dataframe detalhado de emendas de uma proposição
.fetch_emendas_by_queries_camara <- function(sigla=NULL, numero=NULL, ano=NULL) {
  emendas_ids_list <- .fetch_emendas_ids_camara(sigla, numero, ano)

  emendas_df <-
    purrr::map_df(emendas_ids_list, fetch_proposicao_camara) %>%
    dplyr::mutate(data_apresentacao = as.character(as.Date(data_apresentacao))) %>%
    dplyr::select(prop_id,
                  data_apresentacao,
                  numero,
                  status_proposicao_sigla_orgao,
                  autor_nome,
                  casa,
                  tipo_materia,
                  ementa) %>%
    dplyr::rename(prop_id = codigo_emenda,
                  status_proposicao_sigla_orgao = local,
                  autor_nome = autor,
                  tipo_materia = tipo_documento,
                  ementa = inteiro_teor)
                  
  emendas_df
}

# Retorna uma lista de ids de emendas de uma proposição
.fetch_emendas_ids_camara <- function(sigla=NULL, numero=NULL, ano=NULL) {
  emendas_substitutivos_redacaofinal_list <-
    .camara_site(
      .EMENDAS_SUBSTITUTIVOS_REDACAOFINAL_PATH,
      paste0('tipo=', sigla, '&numero=', numero, '&ano=', ano)
    )

  emendas_ids_list <-
    emendas_substitutivos_redacaofinal_list %>%
    magrittr::extract2('Emendas') %>%
    purrr::map_dfr(as.list) %>%
    dplyr::pull(CodProposicao)

  emendas_ids_list
}
