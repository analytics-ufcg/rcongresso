#' Função que constroi o dataframe modelo utilizado para as análises realizadas pela plataforma "Quem me representa?"
#'
#' @param proposicao Um dataframe contendo proposições recuperado pelo método fetch_proposicao()
#' @param votacao Uma dataframe contendo votações recuperado pelo método fetch_votacao()
#'
#' @return Dataframe contendo o resultado do join desses dataframes com os votos ocorridos nas votações
#' passadas como parâmetro.
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

  proposicao <- proposicao %>%
    dplyr::rowwise() %>%
    dplyr::mutate(tipo_prop = as.character(fetch_tipo_proposicao(idTipo)))

  votos <- fetch_votos(votacao$id_votacao)

  orientacao_governo <- votacao %>%
    dplyr::rowwise() %>%
    dplyr::do(
      fetch_orientacoes(.$id_votacao) %>%
        dplyr::filter(nomeBancada=="GOV.") %>%
        dplyr::select(orientacao_governo = voto, id_votacao)
      )

  pos_bancadas <- votacao %>%
    dplyr::rowwise() %>%
    dplyr::do(get_votos_partidos(.$id_votacao))

  votos %>%
    dplyr::left_join(votacao, by="id_votacao") %>%
    dplyr::left_join(proposicao, by=c("uriProposicaoPrincipal" = "uri_proposicao")) %>%
    dplyr::left_join(pos_bancadas, by=c("parlamentar.siglaPartido" = "partido", "id_votacao")) %>%
    dplyr::left_join(orientacao_governo, by="id_votacao") %>%
    return()
}
