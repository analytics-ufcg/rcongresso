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

ids_proposicoes <- arquivo_proposicoes  %>%
  rowwise() %>%
  do(tibble(id = fetch_id_proposicao(.$tipo, .$numero, .$ano)))

# Essa parte me deixou bastante confuso. Eu não tenho certeza se a organização dos dados está correta.
# A função fetch_proposicao() retorna uma lista e não um dataframe. Sabendo disso, como eu devo armazenar essas
# várias listas? No momento elas estão separadas por linha, mas não sei se é o mais adequado.
proposicoes <- ids_proposicoes %>% rowwise() %>% do(prop=fetch_proposicao(.$id))

ids_ultimas_votacoes <- fetch_votacoes(ids_proposicoes) %>% ultima_votacao()

# Mesmo problema acima: Retorno de listas e não sei a melhor forma de agrupá-las.
votacoes_relevantes <- ids_ultimas_votacoes %>%
  rowwise() %>% do(votings=fetch_votacao(.$`max(id)`))

# Idem
votos <- ids_ultimas_votacoes %>%
  rowwise() %>% do(votes=fetch_votos(.$`max(id)`))


# Eu não sei como utilizar essa parte, exatamente. Na construção do dataframe não dependemos somente das votações,
# mas também das proposições e votos. O ideal é "juntar" as colunas do dataframe e passar tudo para cá?
votacoes_relevantes %>%
  format_csv() %>%
  writeLines(stdout())

#
