# regex quebra para casos de GOV. e PCdoB.
.get_votos_partidos <- function(votacao) {

  pos_bancadas <- votacao$orientacoes %>%
    dplyr::mutate(bancada_associada=nomeBancada) %>%
    dplyr::select(partido=nomeBancada, orientacao_partido=voto, bancada_associada) %>%
    tidyr::separate_rows(partido, sep='(?=[A-Z][^A-Z])') %>%
    dplyr::mutate(partido = toupper(partido))

  return(pos_bancadas)
}
