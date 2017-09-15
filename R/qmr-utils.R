# regex quebra para casos de GOV. e PCdoB.
get_votos_partidos <- function(votacao) {

  pos_bancadas <- fetch_orientacoes(votacao) %>%
    dplyr::mutate(bancada_associada=nomeBancada) %>%
    dplyr::select(partido=nomeBancada, orientacao_partido=voto, bancada_associada, id_votacao) %>%
    tidyr::separate_rows(partido, sep='(?=[A-Z][^A-Z])') %>%
    dplyr::mutate(partido = toupper(partido))

  return(pos_bancadas)
}
