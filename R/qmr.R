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
constroi_dataframe <- function(proposicao, votacao, votantes) {

  prop_types <- rcongresso::fetch_tipos_proposicao()
  p <- prop_types %>% filter(prop_types$id==proposicao$idTipo)

  dataframe_final <- data.frame()

  # Quero gerar um for para pegar as colunas a partir de uma lista ao invés de fazer dessa forma.
  # A variável de controle do for seria o parametro votantes$...
  dataframe_final <- rbind(dataframe_final, data.frame(votantes$parlamentar.nome))
  dataframe_final <- cbind(dataframe_final, data.frame(votantes$parlamentar.id))
  dataframe_final <- cbind(dataframe_final, data.frame(votantes$parlamentar.siglaPartido))
  dataframe_final <- cbind(dataframe_final, data.frame(votantes$parlamentar.siglaUf))
  dataframe_final <- cbind(dataframe_final, data.frame(votantes$voto))

  dataframe_final <- cbind(dataframe_final, data.frame(proposicao$numero))
  dataframe_final <- cbind(dataframe_final, data.frame(proposicao$ano))
  dataframe_final <- cbind(dataframe_final, data.frame(proposicao$ementa))

  dataframe_final <- cbind(dataframe_final, data.frame(votacao$dataHoraFim))

  # Select da orientacao_governo da votação
  orientacoes <- votacao$orientacoes
  orientacao_governo <- orientacoes[orientacoes$nomeBancada=="GOV.",]$voto

  dataframe_final <- cbind(dataframe_final, data.frame(orientacao_governo))

  # Faz um select da linha onde o nome da bancada seja PT aí é só pegar o voto
  # do partido a partir dessa linha selecionada.
  # orientacoes[orientacoes$nomeBancada=="PT",]

  # Antes de retornar o dataframe seria bom renomear as colunas. Eu vou fazer isso após conseguir
  # pegar todos os dados que são necessários. Ainda quero confirmar se peguei tudo mesmo.
return(dataframe_final)

}
