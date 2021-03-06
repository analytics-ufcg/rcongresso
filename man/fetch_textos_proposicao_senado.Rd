% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/proposicoes.R
\name{fetch_textos_proposicao_senado}
\alias{fetch_textos_proposicao_senado}
\title{Fetches the documents of a proposition}
\usage{
fetch_textos_proposicao_senado(id)
}
\arguments{
\item{id}{Proposition's ID}
}
\value{
Dataframe containing all the info about the proposition's documents
}
\description{
Returns the proposition's documents info.
}
\examples{
textos_pls229 <- fetch_textos_proposicao_senado(91341)
}
