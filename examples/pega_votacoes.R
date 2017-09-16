#!/usr/bin/env Rscript
library(dplyr)
library(readr)
library(rcongresso)

args = commandArgs(trailingOnly = TRUE)
if (length(args) != 1) {
  stop(
    "Uso: pega_votacoes.R <arquivo_de_proposicoes>"
  )
}

arquivo_proposicoes = args[1]
#arquivo_proposicoes = "arquivo_proposicoes.txt"

ids_proposicoes <- arquivo_proposicoes  %>%
  read_csv(col_types = cols(
    tipo = col_character(),
    numero = col_integer(),
    ano = col_integer()
  )) %>%
  rowwise() %>%
  do(tibble(id = fetch_id_proposicao(.$tipo, .$numero, .$ano)))


proposicoes <- fetch_proposicao(ids_proposicoes$id)

ultimas_votacoes <- fetch_votacoes(ids_proposicoes$id) %>%
  ultima_votacao()

votacoes_relevantes <- fetch_votacao(ultimas_votacoes$id)

constroi_dataframe(proposicoes, votacoes_relevantes) %>%
  format_csv() %>%
  writeLines(stdout())
