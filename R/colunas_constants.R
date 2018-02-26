# Deputados
.COLNAMES_DEP_INFO <- c("id"="integer","uri"="character","nome"="character","siglaPartido"="character",
                       "uriPartido"="character","siglaUf"="character","idLegislatura"="integer","urlFoto"="character")

.COLNAMES_DEP_INFO_ID <- c("id"="integer","uri"="character","nomeCivil"="character","ultimoStatus.id"="integer",
                           "ultimoStatus.uri"="character","ultimoStatus.nome"="character","ultimoStatus.siglaPartido"="character",
                           "ultimoStatus.uriPartido"="character","ultimoStatus.siglaUf"="character",
                           "ultimoStatus.idLegislatura"="integer","ultimoStatus.urlFoto"="character",
                           "ultimoStatus.data"="character","ultimoStatus.nomeEleitoral"="character",
                           "ultimoStatus.gabinete.nome"="numeric","ultimoStatus.gabinete.predio"="character",
                           "ultimoStatus.descricaoStatus"="character","redeSocial"="character",
                           "ultimoStatus.gabinete.sala"="numeric","ultimoStatus.gabinete.andar"="numeric",
                           "ultimoStatus.gabinete.telefone"="character","ultimoStatus.gabinete.email"="character",
                           "ultimoStatus.situacao"="numeric","ultimoStatus.condicaoEleitoral"="character","cpf"="character",
                           "sexo"="character","urlWebsite"="character","dataNascimento"="character","dataFalecimento"="character",
                           "ufNascimento"="character","municipioNascimento"="character","escolaridade"="character")

.COLNAMES_DEP_GASTOS <- c("ano"="numeric","mes"="numeric","tipoDespesa"="character","idDocumento"="integer","tipoDocumento"="numeric",
                         "idTipoDocumento"="integer","dataDocumento"="character","numDocumento"="numeric","valorDocumento"="numeric",
                         "urlDocumento","nomeFornecedor","cnpjCpfFornecedor"="character","valorLiquido"="numeric","valorGlosa"="numeric",
                         "numRessarcimento"="numeric","idLote"="integer","parcela"="numeric", "idDep"="integer")

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

# Votações
.COLNAMES_VOTOS <- c("id_votacao"="integer","voto"="character","parlamentar.id"="integer",
                           "parlamentar.uri"="character","parlamentar.nome"="character",
                           "parlamentar.siglaPartido"="character","parlamentar.uriPartido"="character",
                           "parlamentar.siglaUf"="character","parlamentar.idLegislatura"="integer",
                           "parlamentar.urlFoto"="character")

.COLNAMES_VOTACAO <- c("id"="integer","uri"="character","titulo"="character","uriEvento"="character",
                             "proposicao.id"="integer","proposicao.uri"="character","proposicao.ementa"="character",
                             "uriProposicaoPrincipal"="character","tipoVotacao"="character","aprovada"="character",
                             "placarSim"="numeric","placarNao"="numeric","placarAbstencao"="numeric","relator"="character",
                             "ementaParecer"="character","dataHoraInicio"="character","dataHoraFim"="character",
                             "numVotantes"="numeric","numPresentes"="numeric","despacho"="character")

.COLNAMES_ORIENTACOES <- c("uriBancada"="character","nomeBancada"="character",
                                 "voto"="character","id_votacao"="integer")

.COLNAMES_VOTOSPARTIDOS <- c("partido"="character","orientacao_partido"="character","bancada_associada"="character",
                                   "id_votacao"="integer")

.COLNAMES_ULTIMAVOTACAO <- c("id"="integer","uriProposicaoPrincipal"="character")

.COLNAMES_PROP_VOTACAO <- c("id_votacao"="integer","id_proposicao"="integer","uri"="character","siglaTipo"="character",
                                     "idTipo"="integer","numero"="numeric","ano"="numeric","ementa"="character")
