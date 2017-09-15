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

#' Função que constroi o dataframe modelo utilizado para as análises realizadas pela plataforma "Quem me representa?"
#' com várias proposições.
#'
#' @param proposicoes Uma lista de IDs das proposições previamente escolhidas pelo usuário
#'
#' @return Dataframe contendo 12 colunas com as informações: Nome do parlamentar, ID do parlamentar,
#'    sigla do partido, sigla da uf, voto, sigla da proposição, número da proposição, ano, ementa, horário do início da votação, horário do fim
#'    da votação e orientação do governo. A votação de cada proposição é a mais recente (a última ocorrida na câmara).
#'
#' @export
get_all_votacoes <- function(ids_proposicoes) {

  dataframe_votos <- data.frame()

  for(id_prop in ids_proposicoes){

    print(id_prop)

    prop <- fetch_proposicao(id_prop)
    id_votacao <- fetch_votacoes(id_prop) %>% dplyr::select(id) %>% max() # Pega a última votação com base no ID (ids maiores são mais recentes)

    votacao <- fetch_votacao(id_votacao)
    votos <- fetch_votos(id_votacao)

    dataframe_votos <- dataframe_votos %>% rbind(constroi_dataframe(prop, votacao, votos))

  }

  return(dataframe_votos)

}

# Recebe um dataframe de votações e seleciona a última de acordo com o maior ID
ultima_votacao <- function(votacoes) {

  ultimas_votacoes <- votacoes %>%
    dplyr::group_by(uriProposicaoPrincipal) %>%
    dplyr::filter(id == max(id)) %>%
    unique() %>%
    dplyr::ungroup() %>%
    dplyr::select(id, uriProposicaoPrincipal)

  return(ultimas_votacoes)

}
