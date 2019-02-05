# Deputados
.COLNAMES_DEP_INFO <- c("id"="integer","uri"="character","nome"="character","siglaPartido"="character",
                       "uriPartido"="character","siglaUf"="character","idLegislatura"="integer","urlFoto"="character")

.COLNAMES_DEP_INFO_ID <- c(
    "id"="integer","uri"="character","nomeCivil"="character","ultimoStatus.id"="integer",
    "ultimoStatus.uri"="character","ultimoStatus.nome"="character","ultimoStatus.siglaPartido"="character",
    "ultimoStatus.uriPartido"="character","ultimoStatus.siglaUf"="character",
    "ultimoStatus.idLegislatura"="integer","ultimoStatus.urlFoto"="character",
    "ultimoStatus.data"="character","ultimoStatus.nomeEleitoral"="character",
    "ultimoStatus.gabinete.nome"="numeric","ultimoStatus.gabinete.predio"="character",
    "ultimoStatus.gabinete.sala"="numeric","ultimoStatus.gabinete.andar"="numeric",
    "ultimoStatus.gabinete.telefone"="character","ultimoStatus.gabinete.email"="character",
    "ultimoStatus.situacao"="numeric","ultimoStatus.condicaoEleitoral"="character","cpf"="character",
    "sexo"="character","dataNascimento"="character",
    "ufNascimento"="character","municipioNascimento"="character","escolaridade"="character"
    ## "dataFalecimento"="character", "ultimoStatus.descricaoStatus"="character","redeSocial"="list","urlWebsite"="character"
)

.COLNAMES_DEP_GASTOS <- c("ano"="numeric","mes"="numeric","tipoDespesa"="character","codDocumento"="integer","tipoDocumento"="numeric",
                         "codTipoDocumento"="integer","dataDocumento"="character","numDocumento"="numeric","valorDocumento"="numeric",
                         "urlDocumento"="character","nomeFornecedor"="character","cnpjCpfFornecedor"="character","valorLiquido"="numeric",
                         "valorGlosa"="numeric","numRessarcimento"="numeric","codLote"="integer","parcela"="numeric"
                         ## "idDep"="integer"
                         )

# Proposições
.COLNAMES_PROPOSICAO <- c("id"="integer","uri"="character","siglaTipo"="character","codTipo"="integer",
                     "numero"="integer","ano"="integer","ementa"="character")

.COLNAMES_PROPOSICAO_POR_ID <- c("id"="integer","uri"="character","siglaTipo"="character","codTipo"="integer",
                            "numero"="numeric","ano"="numeric","ementa"="character","dataApresentacao"="character",
                            "descricaoTipo"="character",
                            "keywords"="character", "urlInteiroTeor"="character",
                            "statusProposicao.dataHora"="character",
                            "statusProposicao.sequencia"="numeric","statusProposicao.siglaOrgao"="character",
                            "statusProposicao.uriOrgao"="character","statusProposicao.regime"="character",
                            "statusProposicao.descricaoTramitacao"="character","statusProposicao.codTipoTramitacao"="integer",
                            "statusProposicao.descricaoSituacao"="character","statusProposicao.codSituacao"="integer",
                            "statusProposicao.despacho"="character","statusProposicao.url"="character","uriAutores"="character",
                            "ementaDetalhada"="character","uriPropPrincipal"="character", "uriPropPosterior"="character"
                            ## "uriOrgaoNumerador"="character","tipoAutor"="character","idTipoAutor"="numeric","uriUltimoRelator"="character","uriPropPrincipal"="character","uriPropAnterior"="character", "urnFinal"="character","texto"="character","justificativa"="character",
                            )

.COLNAMES_VOTACOES <- c("id"="integer","uri"="character","titulo"="character","uriEvento"="character","uriProposicaoPrincipal"="character",
                        "tipoVotacao"="character","aprovada"="logical","placarSim"="integer","placarNao"="integer","placarAbstencao"="integer",
                        "proposicao.id"="integer","proposicao.uri"="character","proposicao.siglaTipo"="character","proposicao.codTipo"="integer",
                        "proposicao.numero"="integer","proposicao.ano"="integer","proposicao.ementa"="character")

.COLNAMES_TIPO_PROPOSICAO <- c("cod"="integer","sigla"="character","nome"="character","descricao"="character")

.COLNAMES_RELACIONADAS <- c("id_prop"="integer","id"="integer","uri"="character","siglaTipo"="character","codTipo"="integer","numero"="integer",
                            "ano"="integer","ementa"="character")

.COLNAMES_TRAMITACOES <- c("id_prop"="integer","dataHora"="character","descricaoSituacao"="character","descricaoTramitacao"="character",
                           "despacho"="character","codSituacao"="integer","codTipoTramitacao"="character","regime"="character",
                           "sequencia"="integer","siglaOrgao"="character","uriOrgao"="character","url"="character")

# Votações

# Endpoint: {/votacoes/{id}/votos}
.COLNAMES_VOTOS <- c("id_votacao"="integer","voto"="character","parlamentar.id"="integer",
                           "parlamentar.uri"="character","parlamentar.nome"="character",
                           "parlamentar.siglaPartido"="character","parlamentar.uriPartido"="character",
                           "parlamentar.siglaUf"="character","parlamentar.idLegislatura"="integer",
                           "parlamentar.urlFoto"="character")

# Endpoint: {/votacoes/{id}}
.COLNAMES_VOTACAO <- c("dataHoraFim"="character", "dataHoraInicio"="character",
                       "id"="integer", "placarAbstencao"="integer",
                       "placarNao"="integer", "placarSim"="integer",
                       "proposicao.ementa"="character",
                       "proposicao.id"="integer",
                       "proposicao.uri"="character", "tipoVotacao"="character",
                       "titulo"="character", "uri"="character",
                       "uriEvento"="character",
                       "uriProposicaoPrincipal"="character"
                       ## "aprovada"="character", "despacho"="character", "ementaParecer"="character", "numPresentes"="integer",  "numVotantes"="integer", "proposicao.ano"="integer", "proposicao.codTipo"="integer", "proposicao.numero"="integer","proposicao.siglaTipo"="character", "relator.id"="integer", "relator.idLegislatura"="integer", "relator.nome"="character", "relator.siglaPartido"="character", "relator.siglaUf"="character", "relator.uri"="character", "relator.uriPartido"="character", "relator.urlFoto"="character",
                       )

# Endpoint: {/votacoes/{id}}
.COLNAMES_ORIENTACOES <- c("uriBancada"="character","nomeBancada"="character",
                                 "voto"="character","id_votacao"="integer")

.COLNAMES_VOTOSPARTIDOS <- c("partido"="character","orientacao_partido"="character","bancada_associada"="character",
                                   "id_votacao"="integer")

.COLNAMES_ULTIMAVOTACAO <- c("id"="integer","uriProposicaoPrincipal"="character")

.COLNAMES_PROP_VOTACAO <- c("id_votacao"="integer","id_proposicao"="integer","uri"="character",
                            "ementa"="character"
                            ## "siglaTipo"="character","codTipo"="integer","numero"="numeric","ano"="numeric",
                            )

# Partidos
.COLNAMES_PARTIDOS <- c("id"="integer","sigla"="character","nome"="character","uri"="character")
.COLNAMES_PARTIDOS_ID <- c(
    "id"="integer", "nome"="character", "sigla"="character",
    "status.data"="character", "status.idLegislatura"="integer",
    "status.lider.idLegislatura"="integer", "status.lider.nome"="character",
    "status.lider.siglaPartido"="character",
    "status.lider.uriPartido"="character","status.lider.uf"="character",
    "status.lider.urlFoto"="character","status.lider.uriPartido"="character",
    "status.lider.uri"="character", "urlLogo"="character",
    "status.situacao"="character", "status.totalMembros"="character",
    "status.totalPosse"="character", "status.uriMembros"="character",
    "uri"="character"
    ## "numeroEleitoral"="integer",,"urlFacebook"="character","urlWebsite"="character"
)

# Relatorias
.COLNAMES_RELATORIAS <- c("codigo_tipo_relator"="character", "descricao_tipo_relator"="character", "data_designacao"="character",
                          "data_destituicao"="character", "descricao_motivo_destituicao"="character", "codigo_parlamentar"="character",
                          "nome_parlamentar"="character", "nome_completo_parlamentar"="character","sexo_parlamentar"="character",
                          "forma_tratamento"="character", "url_foto_parlamentar"="character", "url_pagina_parlamentar"="character",
                          "email_parlamentar"="character", "sigla_partido_parlamentar"="character", "uf_parlamentar"="character",
                          "codigo_comissao"="character", "sigla_comissao"="character", "nome_comissao"="character",
                          "sigla_casa_comissao"="character", "nome_casa_comissao"="character")

# Comissoes
.COLNAMES_COMISSOES <- c("id"="integer", "uri"="character", "sigla"="character", "nome"="character", "apelido"="character", "codTipoOrgao"="integer",
                         "tipoOrgao"="character")
.COLNAMES_COMISSOES_SENADO <- c("CARGO"="character","num.x"="integer","PARTIDO"="character","UF"="character","TIPO_VAGA"="character",
                                "PARLAMENTAR.x"="character")

# Autores
.COLNAMES_AUTORES <- c("uri"="character", "nome"="character", "codTipo"="integer", "tipo"="character")

# Sessões
.COLNAMES_SESSOES_CAMARA <- c("timestamp"="character", "origem"="character", "descricao"="character")
.COLNAMES_SESSOES_SENADO <- c("codigo_sessao"="character", "sigla_casa_sessao"="character", "nome_casa_sessao"="character", "codigo_sessao_legislativa"="character",
                              "sigla_tipo_sessao" ="character", "numero_sessao"="character", "data_sessao"="character", "hora_inicio_sessao"="character")

# Agenda
.COLNAMES_AGENDA <- c("hora_inicio"="character","hora_fim"="character","sigla_orgao"="character","nome_orgao"="character","regime"="character",
                      "codRegime"="integer","ordem"="integer","uriProposicaoRelacionada"="character","situacaoItem"="character","uriVotacao"="character",
                      "proposicao_.id"="integer","proposicao_.uri"="character","proposicao_.siglaTipo"="character","proposicao_.codTipo"="integer","proposicao_.numero"="integer",
                      "proposicao_.ano"="integer","proposicao_.ementa"="character")

