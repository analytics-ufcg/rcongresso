# regex tests
# PT_SPLIT <- gsub('([[:upper:]]+[[:lower:]])', ' \\1', x)
# PT_SPLIT <- gsub('([[:upper:]]+[[:lower:]])^(?!.*PtdoB).*$', ' \\1', x)
# b <- unlist(strsplit(y, split=" "))

# PT_SPLIT <- gsub('([[(?=.*PtdoB).*$]]|([[:upper:]]+[[:lower:]]))', ' \\1', x_modified)
# PT_SPLIT <- gsub('([[^(?=.*PtdoB).*$]]|([[:upper:]]+[[:lower:]]))', ' \\1', x_modified)
# PT_SPLIT <- gsub('([[:upper:]]+[[:lower:]]|*.PtdoB))', ' \\1', x_modified)

.get_votos_partidos <- function(votacao) {
  pos_bancadas <- votacao$orientacoes %>% dplyr::select(nomeBancada, voto)

  # Eu estava pensando em um jeito de separar os nomes dos partidos, mas sempre ao fazer isso perdemos a referência
  # para os votos porque "quebramos" o voto da bancada em muitos partidos. A solução temporária que encontrei foi essa:
  # Ao passar por cada bancada, antes de splitar, eu associo a bancada à todos os partidos filhos daquela bancada.
  # Dessa forma, posso recuperar o voto para cada partido fazendo um join pelo nome da bancada.
  partidos_df <- data.frame()
  for(bancada in pos_bancadas$nomeBancada){
    split_partidos <- gsub('([[(?=.*PtdoB).*$]]|([[:upper:]]+[[:lower:]]))', ' \\1', bancada)
    split_partidos <- gsub('Repr[.]*', ' \\1', split_partidos)
    vetor_partidos <- unlist(strsplit(split_partidos, split=" "))
    tmp <- data.frame(partido=vetor_partidos)
    tmp$bancada_associada <- bancada
    partidos_df <- rbind(partidos_df, tmp)
  }

  partidos_df <- dplyr::left_join(pos_bancadas, partidos_df, by=c("nomeBancada" = "bancada_associada"))
  partidos_df$partido <- toupper(partidos_df$partido)
  colnames(partidos_df)[2] <- "orientacao_partido"

  return(partidos_df)

}
