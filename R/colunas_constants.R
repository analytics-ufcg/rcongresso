# Deputados
.COLNAMES_DEP_INFO <- c("id"="integer","uri"="character","nome"="character","siglaPartido"="character",
                       "uriPartido"="character","siglaUf"="character","idLegislatura"="integer","urlFoto"="character")

.COLNAMES_DEP_INFO_ID <- c("id"="integer","uri"="character","nomeCivil"="character","ultimoStatus.id"="integer",
                           "ultimoStatus.uri"="character","ultimoStatus.nome"="character","ultimoStatus.siglaPartido"="character",
                           "ultimoStatus.uriPartido"="character","ultimoStatus.siglaUf"="character",
                           "ultimoStatus.idLegislatura"="integer","ultimoStatus.urlFoto"="character",
                           "ultimoStatus.data"="character","ultimoStatus.nomeEleitoral"="character",
                           "ultimoStatus.gabinete.nome"="numeric","ultimoStatus.gabinete.predio"="character",
                           "ultimoStatus.descricaoStatus"="character","redeSocial"="list",
                           "ultimoStatus.gabinete.sala"="numeric","ultimoStatus.gabinete.andar"="numeric",
                           "ultimoStatus.gabinete.telefone"="character","ultimoStatus.gabinete.email"="character",
                           "ultimoStatus.situacao"="numeric","ultimoStatus.condicaoEleitoral"="character","cpf"="character",
                           "sexo"="character","urlWebsite"="character","dataNascimento"="character","dataFalecimento"="character",
                           "ufNascimento"="character","municipioNascimento"="character","escolaridade"="character")

.COLNAMES_DEP_GASTOS <- c("ano"="numeric","mes"="numeric","tipoDespesa"="character","idDocumento"="integer","tipoDocumento"="numeric",
                         "idTipoDocumento"="integer","dataDocumento"="character","numDocumento"="numeric","valorDocumento"="numeric",
                         "urlDocumento"="character","nomeFornecedor"="character","cnpjCpfFornecedor"="character","valorLiquido"="numeric",
                         "valorGlosa"="numeric","numRessarcimento"="numeric","idLote"="integer","parcela"="numeric", "idDep"="integer")

# Proposições
.COLNAMES_PROPOSICAO <- c("id"="integer","uri"="character","siglaTipo"="character","idTipo"="integer",
                     "numero"="integer","ano"="integer","ementa"="character")

.COLNAMES_PROPOSICAO_POR_ID <- c("id"="integer","uri"="character","siglaTipo"="character","idTipo"="integer",
                            "numero"="numeric","ano"="numeric","ementa"="character","dataApresentacao"="character",
                            "tipoAutor"="character","idTipoAutor"="numeric","descricaoTipo"="character",
                            "keywords"="character", "urlInteiroTeor"="character","uriOrgaoNumerador"="character",
                            "uriUltimoRelator"="character","statusProposicao.dataHora"="character",
                            "statusProposicao.sequencia"="numeric","statusProposicao.siglaOrgao"="character",
                            "statusProposicao.uriOrgao"="character","statusProposicao.regime"="character",
                            "statusProposicao.descricaoTramitacao"="character","statusProposicao.idTipoTramitacao"="integer",
                            "statusProposicao.descricaoSituacao"="character","statusProposicao.idSituacao"="integer",
                            "statusProposicao.despacho"="character","statusProposicao.url"="character","uriAutores"="character",
                            "ementaDetalhada"="character","uriPropPrincipal"="character","uriPropAnterior"="character","uriPropPosterior"="character",
                            "urnFinal"="character","texto"="character","justificativa"="character")

.COLNAMES_VOTACOES <- c("id"="integer","uri"="character","titulo"="character","uriEvento"="character","uriProposicaoPrincipal"="character",
                        "tipoVotacao"="character","aprovada"="logical","placarSim"="integer","placarNao"="integer","placarAbstencao"="integer",
                        "proposicao.id"="integer","proposicao.uri"="character","proposicao.siglaTipo"="character","proposicao.idTipo"="integer",
                        "proposicao.numero"="integer","proposicao.ano"="integer","proposicao.ementa"="character")

.COLNAMES_TIPO_PROPOSICAO <- c("id"="integer","sigla"="character","nome"="character","descricao"="character")

.COLNAMES_RELACIONADAS <- c("id"="integer","uri"="character","siglaTipo"="character","idTipo"="integer","numero"="integer","ano"="integer","ementa"="character")


# Votações

# Endpoint: {/votacoes/{id}/votos}
.COLNAMES_VOTOS <- c("id_votacao"="integer","voto"="character","parlamentar.id"="integer",
                           "parlamentar.uri"="character","parlamentar.nome"="character",
                           "parlamentar.siglaPartido"="character","parlamentar.uriPartido"="character",
                           "parlamentar.siglaUf"="character","parlamentar.idLegislatura"="integer",
                           "parlamentar.urlFoto"="character")

# Endpoint: {/votacoes/{id}}
.COLNAMES_VOTACAO <- c("aprovada"="character", "dataHoraFim"="character", "dataHoraInicio"="character", "despacho"="character",
                       "ementaParecer"="character", "id"="integer", "numPresentes"="integer", "numVotantes"="integer",
                       "placarAbstencao"="integer", "placarNao"="integer", "placarSim"="integer", "proposicao.ano"="integer",
                       "proposicao.ementa"="character", "proposicao.id"="integer", "proposicao.idTipo"="integer", "proposicao.numero"="integer",
                       "proposicao.siglaTipo"="character", "proposicao.uri"="character","relator.id"="integer",
                       "relator.idLegislatura"="integer", "relator.nome"="character", "relator.siglaPartido"="character",
                       "relator.siglaUf"="character", "relator.uri"="character", "relator.uriPartido"="character", "relator.urlFoto"="character",
                       "tipoVotacao"="character", "titulo"="character", "uri"="character", "uriEvento"="character", "uriProposicaoPrincipal"="character")

# Endpoint: {/votacoes/{id}}
.COLNAMES_ORIENTACOES <- c("uriBancada"="character","nomeBancada"="character",
                                 "voto"="character","id_votacao"="integer")

.COLNAMES_VOTOSPARTIDOS <- c("partido"="character","orientacao_partido"="character","bancada_associada"="character",
                                   "id_votacao"="integer")

.COLNAMES_ULTIMAVOTACAO <- c("id"="integer","uriProposicaoPrincipal"="character")

.COLNAMES_PROP_VOTACAO <- c("id_votacao"="integer","id_proposicao"="integer","uri"="character","siglaTipo"="character",
                                     "idTipo"="integer","numero"="numeric","ano"="numeric","ementa"="character")

# Partidos
.COLNAMES_PARTIDOS <- c("id"="integer","sigla"="character","nome"="character","uri"="character")
.COLNAMES_PARTIDOS_ID <- c("id"="integer","nome"="character","numeroEleitoral"="integer",
                           "sigla"="character","status.data"="character","status.idLegislatura"="integer",
                           "status.lider.idLegislatura"="integer",
                           "status.lider.nome"="character","status.lider.siglaPartido"="character",
                           "status.lider.uriPartido"="character","status.lider.uf"="character",
                           "status.lider.urlFoto"="character","status.lider.uriPartido"="character",
                           "status.lider.uri"="character","urlLogo"="character","status.situacao"="character",
                           "status.totalMembros"="character","status.totalPosse"="character","status.uriMembros"="character",
                           "uri"="character","urlFacebook"="character","urlWebsite"="character")

