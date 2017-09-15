#' Função que constroi o dataframe modelo utilizado para as análises realizadas pela plataforma "Quem me representa?"
#'
#' @param proposicao Uma proposição especifica recuperada pelo método fetch_proposicao()
#' @param votacao Uma votação específica recuperada pelo método fetch_votacao()
#' @param votos Os votos referentes a esta votação em questão
#'
#' @return Dataframe contendo 10 colunas com as informações: Nome do parlamentar, ID do parlamentar,
#'    sigla do partido, sigla da uf, voto, número da proposição, ano, ementa, horário da votação e
#'    orientação do governo
#'
#' @examples
#' pec241 <- fetch_proposicao(2088351)
#' votacao_segundoturno_pec241 <- fetch_votacao(7252)
#' dataframe_pec241 <- constroi_dataframe(pec241, votacao_segundoturno_pec241)
#'
#' @export
constroi_dataframe <- function(proposicao, votacao) {
  colnames(proposicao)[1] <- "id_proposicao"
  colnames(proposicao)[2] <- "uri_proposicao"

  colnames(votacao)[1] <- "id_votacao"
  colnames(votacao)[2] <- "uri_votacao"

  votos <- votacao %>%
    dplyr::rowwise() %>%
    dplyr::do(fetch_votos(.$id_votacao))

  pos_bancadas <- votacao %>%
    dplyr::rowwise() %>%
    dplyr::do(get_votos_partidos(.$id_votacao))

  votos %>%
    dplyr::left_join(votacao, by="id_votacao") %>%
    dplyr::left_join(proposicao, by=c("uriProposicaoPrincipal" = "uri_proposicao")) %>%
    dplyr::left_join(pos_bancadas, by=c("parlamentar.siglaPartido" = "partido", "id_votacao")) %>%
    return()
}
