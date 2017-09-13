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
#' votos_segundoturno_pec241 <- fetch_votos(7252)
#' dataframe_pec241 <- constroi_dataframe(pec241, votacao_segundoturno_pec241, votos_segundoturno_pec241)
#'
#' @export
constroi_dataframe <- function(proposicao, votacao, votos) {

  prop_types <- fetch_tipos_proposicao()
  p <- prop_types %>% dplyr::filter(prop_types$id==proposicao$idTipo)

  dataframe_final <- data.frame()

  # Quero gerar um for para pegar as colunas a partir de uma lista ao invés de fazer dessa forma.
  # A variável de controle do for seria o parametro votantes$...
  dataframe_final <- rbind(dataframe_final, data.frame(votos$parlamentar.nome))
  dataframe_final <- cbind(dataframe_final, data.frame(votos$parlamentar.id))
  dataframe_final <- cbind(dataframe_final, data.frame(votos$parlamentar.siglaPartido))
  dataframe_final <- cbind(dataframe_final, data.frame(votos$parlamentar.siglaUf))
  dataframe_final <- cbind(dataframe_final, data.frame(votos$voto))

  dataframe_final <- cbind(dataframe_final, data.frame(p$sigla))
  dataframe_final <- cbind(dataframe_final, data.frame(proposicao$numero))
  dataframe_final <- cbind(dataframe_final, data.frame(proposicao$ano))
  dataframe_final <- cbind(dataframe_final, data.frame(proposicao$ementa))

  dataframe_final <- cbind(dataframe_final, data.frame(votacao$dataHoraInicio))
  dataframe_final <- cbind(dataframe_final, data.frame(votacao$dataHoraFim))

  # Select da orientacao_governo da votação
  orientacoes <- votacao$orientacoes

  orientacao_governo <- orientacoes %>%
    dplyr::filter(nomeBancada=="GOV.") %>%
    dplyr::select(voto)

  dataframe_final <- cbind(dataframe_final, orientacao_governo)

  # Deixando os nomes dos partidos maiúsculo facilita o join futuramente
  dataframe_final$votos.parlamentar.siglaPartido <- toupper(dataframe_final$votos.parlamentar.siglaPartido)

  orientacao_partidos <- .get_votos_partidos(votacao)

  dataframe_final <- dplyr::left_join(dataframe_final, orientacao_partidos, by=c("votos.parlamentar.siglaPartido" = "partido"))

  colnames(dataframe_final) <- c("nome_parl","id_parl","siglaPartido", "siglaUF", "voto_parl",
                                 "sigla_prop","num_prop","ano_prop","ementa_prop","horaInicio_votacao","horaFim_votacao",
                                 "orientacao_governo","bancada_associada","orientacao_partido"
                                 )

return(dataframe_final)

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

  ultimas_votacoes <- votacoes %>% dplyr::group_by(uriProposicaoPrincipal) %>%
    dplyr::select(id) %>% dplyr::group_by(max(id)) %>%
    dplyr::select(`max(id)`) %>% unique()

  return(ultimas_votacoes)

}
