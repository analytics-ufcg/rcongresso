# Dúvida: Quais deverão ser os parâmetros dessa função?
# 1: Três parâmetros: Uma proposição, um 'objeto' votação e um 'objeto' votantes (Dessa maneira, a responsabilidade de fazer
# um get desses três 'objetos' é responsabilidade de quem vai chamar a função)
# 2: Quatro parâmetros: O tipo da proposição, o número da proposição e o ano da proposição e o ID
# da votação que eu quero (Dessa maneira, o método faria uma busca automática pela proposição e
# o ID da votação seria responsabilidade de quem está chamando o método, o que não faz muito sentido
# porque se ele tem o ID da votação ele deverá ter o ID da proposição também e não facilita muito.)
# 3: Quatro parâmetros: O tipo da proposição, o número da proposição e o ano da proposição e
# a data da votação (Acho que essa seria a maneira ideal e mais amigável para o usuário. Ele passa
# os dados referentes à proposição a à votação e aí tem tudo junto daquela votação especificamente.
# Temos que lidar com o caso que há mais de uma votação no mesmo dia e o parse da data.)

# Para facilitar vou escolhar a opção 1 e testar a construção do dataframe. Depois eu posso mudar.

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

  # Faz um select da linha onde o nome da bancada seja PT aí é só pegar o voto
  # do partido a partir dessa linha selecionada.
  # orientacoes[orientacoes$nomeBancada=="PT",]

  # Antes de retornar o dataframe seria bom renomear as colunas. Eu vou fazer isso após conseguir
  # pegar todos os dados que são necessários. Ainda quero confirmar se peguei tudo mesmo.
return(dataframe_final)

}
