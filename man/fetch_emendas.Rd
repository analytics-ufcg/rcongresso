% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/emendas.R
\name{fetch_emendas}
\alias{fetch_emendas}
\title{Returns emendas of a proposição from Chamber of deputies or Senate}
\usage{
fetch_emendas(id, casa)
}
\arguments{
\item{id}{Proposição's ID from congress}

\item{casa}{senado or camara}
}
\value{
Dataframe with informations about emendas of a proposição from Chamber of deputies or Senate
}
\description{
Fetchs a dataframe with emendas's data of a proposição from Chamber of deputies or Senate
}
\examples{
\dontrun{
fetch_emendas(91341,'senado')
}
}
