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

ids_proposicoes <- read_csv(arquivo_proposicoes) %>%
  rowwise() %>%
  do(tibble(id = fetch_id_proposicao(.$tipo, .$numero, .$ano)))

votacoes_relevantes <- ids_proposicoes %>%
  fetch_votacoes() %>%
  group_by(proposicao) %>%
  ultima_votacao()

votacoes_relevantes %>%
  format_csv() %>%
  writeLines(stdout())
