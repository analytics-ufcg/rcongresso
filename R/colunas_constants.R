# Deputados
.COLNAMES_DEP_INFO <- c("id"="integer","uri"="character","nome"="character","siglaPartido"="character",
                       "uriPartido"="character","siglaUf"="character","idLegislatura"="integer","urlFoto"="character")

.COLNAMES_DEP_INFO_ID <- c(
    "id"="integer","uri"="character","nomeCivil"="character","ultimoStatus.id"="integer",
    "ultimoStatus.uri"="character","ultimoStatus.nome"="character","ultimoStatus.siglaPartido"="character",
    "ultimoStatus.uriPartido"="character","ultimoStatus.siglaUf"="character",
    "ultimoStatus.idLegislatura"="integer","ultimoStatus.urlFoto"="character",
    "ultimoStatus.data"="character","ultimoStatus.nomeEleitoral"="character",
    "ultimoStatus.gabinete.email"="character",
    "ultimoStatus.gabinete.nome"="character",
    "ultimoStatus.gabinete.predio"="character",
    "ultimoStatus.gabinete.sala"="character",
    "ultimoStatus.gabinete.andar"="character",
    "ultimoStatus.gabinete.telefone"="character",
    "ultimoStatus.situacao"="numeric","ultimoStatus.condicaoEleitoral"="character","cpf"="character",
    "sexo"="character","dataNascimento"="character",
    "redeSocial1"="character",
    "redeSocial2"="character",
    "redeSocial3"="character",
    "redeSocial4"="character",
    "redeSocial5"="character",
    "ufNascimento"="character","municipioNascimento"="character","escolaridade"="character"
    ## "dataFalecimento"="character", "ultimoStatus.descricaoStatus"="character","redeSocial"="list","urlWebsite"="character"
)

.COLNAMES_TAB_DEP <- c("id"="character", "nome"="character", "nome_civil"="character", "id_legislatura_inicial"="character",
                       "id_legislatura_final"="character", "cpf"="character", "sigla_sexo"="character",
                       "url_rede_social"="character", "url_website"="character", "data_nascimento"="character",
                       "data_falecimento"="character", "uf_nascimento"="character", "municipio_nascimento"="character",
                       "uri"="character")


.COLNAMES_DEP_GASTOS <- c("ano"="numeric","mes"="numeric","tipoDespesa"="character","codDocumento"="integer","tipoDocumento"="numeric",
                         "codTipoDocumento"="integer","dataDocumento"="character","numDocumento"="numeric","valorDocumento"="numeric",
                         "urlDocumento"="character","nomeFornecedor"="character","cnpjCpfFornecedor"="character","valorLiquido"="numeric",
                         "valorGlosa"="numeric","numRessarcimento"="numeric","codLote"="integer","parcela"="numeric"
                         ## "idDep"="integer"
                         )

# Proposições
.COLNAMES_PROPOSICAO_CAMARA <- c("id"="integer","uri"="character","siglaTipo"="character","codTipo"="integer",
                     "numero"="integer","ano"="integer","ementa"="character")

.COLNAMES_PROPOSICAO_POR_ID_CAMARA <- c("id"="integer","uri"="character","siglaTipo"="character","codTipo"="integer",
                            "numero"="numeric","ano"="numeric","ementa"="character","dataApresentacao"="character",
                            "descricaoTipo"="character", "ementaDetalhada"="character", "keywords"="character",
                             "urlInteiroTeor"="character", "statusProposicao.codSituacao"="character",
                            "statusProposicao.dataHora"="character", "statusProposicao.descricaoSituacao"="character",
                            "statusProposicao.sequencia"="numeric","statusProposicao.siglaOrgao"="character",
                            "statusProposicao.uriOrgao"="character","statusProposicao.regime"="character",
                            "statusProposicao.descricaoTramitacao"="character","statusProposicao.codTipoTramitacao"="integer",
                            "statusProposicao.despacho"="character","statusProposicao.url"="character","uriAutores"="character",
                            "uriPropPosterior"="character", "uriPropPrincipal"="character","uriUltimoRelator"="character",
                            "statusProposicao.uriUltimoRelator" = "character"
                            )

.COLNAMES_PROPOSICAO_SENADO <- c("ementa_materia"="character", "explicacao_ementa_materia"="character", "apelido_materia"="character", "indicador_complementar"="character",
                                 "data_apresentacao"="character", "data_leitura"="character", "sigla_casa_leitura"="character", "nome_casa_leitura"="character",
                                 "codigo_materia"="character", "sigla_casa_identificacao_materia"="character", "nome_casa_identificacao_materia"="character",
                                 "sigla_subtipo_materia"="character", "descricao_subtipo_materia"="character","numero_materia"="character", "ano_materia"="character",
                                 "descricao_objetivo_processo"="character", "descricao_identificacao_materia"="character", "indicador_tramitando"="character",
                                 "codigo_assunto_especifico"="character", "assunto_especifico"="character", "codigo_assunto_geral"="character", "assunto_geral"="character",
                                 "nome_poder_origem"="character", "sigla_casa_origem"="character", "nome_casa_origem"="character", "proposicoes_relacionadas"="character",
                                 "proposicoes_apensadas"="character", "codigo_natureza"="integer", "nome_natureza"="character", "descricao_natureza"="character",
                                 "autor_nome"="character")

.COLNAMES_VOTACOES_CAMARA <- c("id"="integer","uri"="character","titulo"="character","uriEvento"="character","uriProposicaoPrincipal"="character",
                        "tipoVotacao"="character","aprovada"="logical","placarSim"="integer","placarNao"="integer","placarAbstencao"="integer",
                        "proposicao.id"="integer","proposicao.uri"="character","proposicao.siglaTipo"="character","proposicao.codTipo"="integer",
                        "proposicao.numero"="integer","proposicao.ano"="integer","proposicao.ementa"="character")

.COLNAMES_TIPO_PROPOSICAO <- c("cod"="integer","sigla"="character","nome"="character","descricao"="character")

.COLNAMES_RELACIONADAS <- c("id_prop"="integer","id"="integer","uri"="character","siglaTipo"="character","codTipo"="integer","numero"="integer",
                            "ano"="integer","ementa"="character")

.COLNAMES_RELACIONADAS_SENADO <- c("codigo_texto"="character", "descricao_tipo_texto"="character","tipo_documento"="character",
                                   "formato_texto"="character", "data_texto"="character", "url_texto"="character", "descricao_texto"="character",
                                   "identificacao_comissao_codigo_comissao"="character", "identificacao_comissao_sigla_comissao"="character",
                                   "identificacao_comissao_nome_comissao"="character", "identificacao_comissao_sigla_casa_comissao"="character",
                                   "identificacao_comissao_nome_casa_comissao"="character", "codigo_materia"="character", "sigla_casa_identificacao_materia"="character",
                                   "nome_casa_identificacao_materia"="character", "sigla_subtipo_materia"="character", "descricao_subtipo_materia"="character",
                                   "numero_materia"="character", "ano_materia"="character", "descricao_objetivo_processo"="character",
                                   "descricao_identificacao_materia"="character", "indicador_tramitando"="character",
                                   "apelido_materia"="character", "assunto_especifico"="character", "assunto_geral"="character", "autor_nome"="character",
                                   "codigo_assunto_geral"="character", "codigo_natureza"="character", "data_apresentacao"="character",
                                   "data_leitura"="character", "descricao_identificacao_materia.x"="character", "descricao_natureza"="character",
                                   "descricao_objetivo_processo.x"="character", "descricao_subtipo_materia.x"="character", "ementa_materia"="character",
                                   "explicacao_ementa_materia"="character", "indicador_complementar"="character", "indicador_tramitando.x"="character",
                                   "nome_casa_identificacao_materia.x"="character", "nome_casa_leitura"="character", "nome_casa_origem"="character",
                                   "nome_natureza"="character", "nome_poder_origem"="character", "numero_materia.x"="character", "proposicoes_apensadas"="character",
                                   "proposicoes_relacionadas"="character", "sigla_casa_identificacao_materia.x"="character", "sigla_casa_leitura"="character",
                                   "sigla_casa_origem"="character", "sigla_subtipo_materia.x"="character", "numero_emenda"="character",
                                   "sigla_casa_identificacao_materia.y"="character", "nome_casa_identificacao_materia.y"="character",
                                   "sigla_subtipo_materia.y"="character", "descricao_subtipo_materia.y"="character", "numero_materia.y"="character",
                                   "ano_materia.y"="character", "descricao_objetivo_processo.y"="character", "descricao_identificacao_materia.y"="character",
                                   "indicador_tramitando.y"="character", "ano_materia.x"="character", "codigo_assunto_especifico"="character")


.COLNAMES_TRAMITACOES_CAMARA <- c("id_prop"="integer","dataHora"="character","descricaoSituacao"="character","descricaoTramitacao"="character",
                           "despacho"="character","codSituacao"="integer","codTipoTramitacao"="character","regime"="character",
                           "sequencia"="integer","siglaOrgao"="character","uriOrgao"="character","url"="character", "uriUltimoRelator" = "character")

.COLNAMES_TRAMITACOES_SENADO <- c("codigo_tramitacao"="character","numero_autuacao"="character","data_hora"="character","sequencia"="character",
                           "data_recebimento"="character","origem_tramitacao_local_codigo_local"="character","origem_tramitacao_local_tipo_local"="character",
                           "origem_tramitacao_local_sigla_casa_local"="character","origem_tramitacao_local_nome_casa_local"="character",
                           "origem_tramitacao_local_sigla_local"="character","origem_tramitacao_local_nome_local"="character",
                           "destino_tramitacao_local_codigo_local"="character","destino_tramitacao_local_tipo_local"="character",
                           "destino_tramitacao_local_sigla_casa_local"="character", "destino_tramitacao_local_nome_casa_local"="character",
                           "destino_tramitacao_local_sigla_local"="character","destino_tramitacao_local_nome_local"="character",
                           "situacao_codigo_situacao"="character","situacao_sigla_situacao"="character","situacao_descricao_situacao"="character",
                           "codigo_materia"="character","sigla_casa_identificacao_materia"="character","nome_casa_identificacao_materia"="character",
                           "sigla_subtipo_materia"="character","descricao_subtipo_materia"="character","numero_materia"="character",
                           "descricao_identificacao_materia"="character","indicador_tramitando"="character")

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
.COLNAMES_VOT_SEN <-
  c(
    "descricao_identificacao_materia"="character",
    "indicador_votacao_secreta"="character",
    "descricao_votacao"="character",
    "descricao_resultado"="character",
    "codigo_sessao"="character",
    "sigla_casa_sessao"="character",
    "nome_casa_sessao"="character",
    "codigo_sessao_legislativa"="character",
    "sigla_tipo_sessao"="character",
    "numero_sessao"="character",
    "data_sessao"="character",
    "hora_inicio_sessao"="character",
    "codigo_tramitacao"="character",
    "numero_autuacao"="character",
    "data_tramitacao"="character",
    "numero_ordem_tramitacao"="character",
    "texto_tramitacao"="character",
    "indicador_recebimento"="character",
    "data_recebimento"="character",
    "origem_tramitacao_local_codigo_local"="character",
    "origem_tramitacao_local_tipo_local"="character",
    "origem_tramitacao_local_sigla_casa_local"="character",
    "origem_tramitacao_local_nome_casa_local"="character",
    "origem_tramitacao_local_sigla_local"="character",
    "origem_tramitacao_local_nome_local"="character",
    "destino_tramitacao_local_codigo_local"="character",
    "destino_tramitacao_local_tipo_local"="character",
    "destino_tramitacao_local_sigla_casa_local"="character",
    "destino_tramitacao_local_nome_casa_local"="character",
    "destino_tramitacao_local_sigla_local"="character",
    "destino_tramitacao_local_nome_local"="character",
    "situacao_codigo_situacao"="character",
    "situacao_sigla_situacao"="character",
    "situacao_descricao_situacao"="character",
    "descricao_voto"="character",
    "codigo_parlamentar"="character",
    "nome_parlamentar"="character",
    "nome_completo_parlamentar"="character",
    "sexo_parlamentar"="character",
    "forma_tratamento"="character",
    "url_foto_parlamentar"="character",
    "url_pagina_parlamentar"="character",
    "email_parlamentar"="character",
    "sigla_partido_parlamentar"="character",
    "uf_parlamentar"="character",
    "codigo_materia"="character",
    "sigla_casa_identificacao_materia"="character",
    "nome_casa_identificacao_materia"="character",
    "sigla_subtipo_materia"="character",
    "descricao_subtipo_materia"="character",
    "descricao_identificacao_materia"="character",
    "numero_materia"="character",
    "ano_materia"="character",
    "indicador_tramitando"="character",
    "descricao_objetivo_processo"="character"
  )

# Partidos
.COLNAMES_PARTIDOS <- c("id"="integer","sigla"="character","nome"="character","uri"="character")
.COLNAMES_PARTIDOS_ID <- c(
    "id"="integer", "nome"="character", "sigla"="character",
    "status.data"="character", "status.lider.nome"="character",
    "status.idLegislatura"="integer", "status.lider.siglaPartido"="character",
    "status.lider.uf"="character",
    "status.lider.idLegislatura"="integer",
    "status.lider.uriPartido"="character",
    "status.lider.urlFoto"="character","status.lider.uriPartido"="character",
    "status.lider.uri"="character", "urlLogo"="character",
    "status.situacao"="character", "status.totalMembros"="character",
    "status.totalPosse"="character", "status.uriMembros"="character",
    "uri"="character"
)

# Relatorias
.COLNAMES_RELATORIAS <- c("codigo_tipo_relator"="character", "descricao_tipo_relator"="character", "data_designacao"="character",
                          "data_destituicao"="character", "descricao_motivo_destituicao"="character", "codigo_parlamentar"="character",
                          "nome_parlamentar"="character", "nome_completo_parlamentar"="character","sexo_parlamentar"="character",
                          "forma_tratamento"="character", "url_foto_parlamentar"="character", "url_pagina_parlamentar"="character",
                          "email_parlamentar"="character", "sigla_partido_parlamentar"="character", "uf_parlamentar"="character",
                          "codigo_comissao"="character", "sigla_comissao"="character", "nome_comissao"="character",
                          "sigla_casa_comissao"="character", "nome_casa_comissao"="character")
# Emendas
.COLNAMES_EMENDAS_SENADO <- c("codigo_emenda"="character", "numero"="integer", "data_apresentacao"="character", "local"="character",
                       "descricao_turno"="character", "descricao_tipo_emenda"="character", "autor"="character", "id_autor"="character",
                       "partido"="character", "casa"="character", "tipo_documento"="character", "inteiro_teor"="character")
.COLNAMES_EMENDAS_CAMARA <- c("codigo_emenda"="integer","data_apresentacao"="character","numero"="numeric","local"="chraracter",
                              "autor"="chraracter","casa"="chraracter","tipo_documento"="chraracter","inteiro_teor"="chraracter")
.COLNAMES_EMENDAS_GERAL <-c("prop_id"="numeric","codigo_emenda"="integer","data_apresentacao"="character","numero"="numeric",
                            "local"="character","autor"="character","casa"="character","tipo_documento"="character","inteiro_teor"="character")

# Autores
.COLNAMES_AUTORES <- c("uri"="character", "nome"="character", "codTipo"="integer", "tipo"="character")

# Sessões
.COLNAMES_SESSOES_CAMARA <- c("timestamp"="character", "origem"="character", "descricao"="character")
.COLNAMES_SESSOES_SENADO <- c("codigo_sessao"="character", "sigla_casa_sessao"="character", "nome_casa_sessao"="character", "codigo_sessao_legislativa"="character",
                              "sigla_tipo_sessao" ="character", "numero_sessao"="character", "data_sessao"="character", "hora_inicio_sessao"="character")
# Agenda
.COLNAMES_AGENDA_SENADO <- c("dia_util"="character","data"="character","dia_semana"="character",
                             "mes"="character","horario"="character","hora"="character",
                             "numero_sessao"="character","tipo_sessao"="character","local_sessao"="character",
                             "codigo_sessao"="character","casa"="character","sessao_legislativa"="character",
                             "legislatura"="character","codigo_situacao_sessao"="character","situacao_sessao"="character",
                             "realizada_status"="character","evento_descricao_tipo_evento"="character","evento_data"="character",
                             "evento_dia_semana"="character","evento_horario"="character","evento_tipo_sessao"="character",
                             "evento_descricao_evento"="character","evento_fim_inscricao"="character","evento_indicador_publica_orador"="character",
                             "oradores_tipo_orador_descricao_tipo_orador"="character","oradores_tipo_orador_orador_sessao_orador"="list","materias_materia"="list")
.COLNAMES_AGENDA_ORADORES_SENADO <- c("ordem"="character","parlamentar"="character","codigo_tipo_orador"="character","codigo_sessao"="character")
.COLNAMES_AGENDA_MATERIA_SENADO <- c("codigo_materia"="character","identificacao"="character","sigla_materia"="character","numero_materia"="character","ano_materia"="character",
                                     "descricao_objetivo_processo"="character","descricao_identificacao_materia"="character","sigla_casa_iniciadora"="character",
                                     "ementa"="character","parecer"="character","apreciacao"="character","apreciacao_papeleta"="character","ementa_papeleta"="character","cabecalho"="character",
                                     "nome_autor"="character","tipo_pauta"="character","descricao_tipo_pauta"="character","sequencia_ordem"="character","origem"="character","codigo_sessao"="character")
.COLNAMES_AGENDA_COMISSOES_SENADO <- c("data"="Date","sigla"="character","id_proposicao"="character","local"="character")

.COLNAMES_AGENDA_CAMARA <- c("hora_inicio"="character","hora_fim"="character","sigla_orgao"="character","nome_orgao"="character","regime"="character",
                      "codRegime"="integer","ordem"="integer","uriProposicaoRelacionada"="character","situacaoItem"="character","uriVotacao"="character",
                      "proposicao_.id"="integer","proposicao_.uri"="character","proposicao_.siglaTipo"="character","proposicao_.codTipo"="integer","proposicao_.numero"="integer",
                      "proposicao_.ano"="integer","proposicao_.ementa"="character")

.COLNAMES_PAUTA_CAMARA <- c("regime"="character","codRegime"="integer","ordem"="integer","uriProposicaoRelacionada"="character","situacaoItem"="character","uriVotacao"="character",
                            "proposicao_.id"="integer","proposicao_.uri"="character","proposicao_.siglaTipo"="character","proposicao_.codTipo"="integer","proposicao_.numero"="integer",
                            "proposicao_.ano"="integer","proposicao_.ementa"="character","hora_inicio"="character","hora_fim"="character","sigla_orgao"="character","nome_orgao"="character")

# Órgãos
# Endpoint: {/api/v2/orgaos/{id}}
.COLNAMES_ORGAO <- c(
    "id"="integer", "uri"="character", "sigla"="character", "nome"="character",
    "apelido"="character", "codTipoOrgao"="integer", "tipoOrgao"="character"
)
# Endpoint: {/arquivos/orgaos/json/orgaos.json}
.COLNAMES_ORGAOS <- c(
    "uri"="character", "sigla"="character", "apelido"="character",
    "nome"="character", "codTipoOrgao"="integer", "tipoOrgao"="character",
    "dataInicio"="character", "dataInstalacao"="character",
    "dataFim"="character", "descricaoSituacao"="character", "casa"="character",
    "sala"="character", "urlWebsite"="character", "codSituacao"="integer"
)

# Comissões
.COLNAMES_COMISSOES_SENADO <- c("CARGO"="character","num.x"="integer","PARTIDO"="character","UF"="character","TIPO_VAGA"="character",
                                "PARLAMENTAR.x"="character")

# Deferimento
.COLNAMES_DEFRIMENTO <- c("proposicao_id"="character","deferimento"="character")

# Requerimentos
.COLNAMES_REQUERIMENTOS_CAMARA <- c("id_prop"="numeric","casa"="character","id_req"="integer","deferimento"="character", "ano"="numeric", "cod_tipo"="integer",
                                    "data_apresentacao"="character",  "descricao_tipo"="character", "ementa"="character", "ementa_detalhada"="character",
                                    "keywords"="character", "numero"="numeric", "sigla_tipo"="character", "status_proposicao_cod_situacao"="character",
                                    "status_proposicao_cod_tipo_tramitacao"="integer", "status_proposicao_data_hora"="character", "status_proposicao_descricao_situacao"="character",
                                    "status_proposicao_descricao_tramitacao"="character", "status_proposicao_despacho"="character", "status_proposicao_regime"="character",
                                    "status_proposicao_sequencia"="numeric", "status_proposicao_sigla_orgao"="character",
                                    "status_proposicao_uri_orgao"="character", "status_proposicao_url"="character", "uri"="character", "uri_autores"="character",
                                    "uri_prop_posterior"="character", "uri_prop_principal"="character", "url_inteiro_teor"="character", "uri_ultimo_relator" = "character",
                                    "status_proposicao_uri_ultimo_relator" = "character")

.COLNAMES_REQUERIMENTOS_SENADO <- c("id_prop"="numeric","casa"="character","id_req"="integer","deferimento"="character", "ano"="numeric", "cod_tipo"="integer",
                                    "data_apresentacao"="character",  "descricao_tipo"="character", "ementa"="character", "ementa_detalhada"="character",
                                    "keywords"="character", "numero"="numeric", "sigla_tipo"="character", "status_proposicao_cod_situacao"="character",
                                    "status_proposicao_cod_tipo_tramitacao"="integer", "status_proposicao_data_hora"="character", "status_proposicao_descricao_situacao"="character",
                                    "status_proposicao_descricao_tramitacao"="character", "status_proposicao_despacho"="character", "status_proposicao_regime"="character",
                                    "status_proposicao_sequencia"="numeric", "status_proposicao_sigla_orgao"="character",
                                    "status_proposicao_uri_orgao"="character", "status_proposicao_url"="character")

.COLNAMES_EVENTOS_REQUERIMENTOS_CAMARA <- c("id_req"="integer","data_hora"="character", "evento"="character","cod_situacao"="integer",
                                            "cod_tipo_tramitacao"="character","descricao_situacao"="character","descricao_tramitacao"="character","despacho"="character",
                                            "regime"="character","sequencia"="integer","sigla_orgao"="character","uri_orgao"="character","url"="character",
                                            "uri_ultimo_relator" = "character")

.COLNAMES_EVENTOS_REQUERIMENTOS_SENADO <- c("codigo_materia"="character","codigo_tramitacao"="character", "numero_autuacao"="character", "texto_tramitacao"="character",
                                            "indicador_recebimento"="character", "data_recebimento"="character", "origem_tramitacao_local_codigo_local"="character",
                                            "origem_tramitacao_local_tipo_local"="character", "origem_tramitacao_local_sigla_casa_local"="character",
                                            "origem_tramitacao_local_nome_casa_local"="character", "origem_tramitacao_local_sigla_local"="character",
                                            "origem_tramitacao_local_nome_local"="character", "destino_tramitacao_local_codigo_local"="character",
                                            "destino_tramitacao_local_tipo_local"="character", "destino_tramitacao_local_sigla_casa_local"="character",
                                            "destino_tramitacao_local_nome_casa_local"="character", "destino_tramitacao_local_sigla_local"="character",
                                            "destino_tramitacao_local_nome_local"="character", "situacao_codigo_situacao"="character",
                                            "situacao_sigla_situacao"="character", "situacao_descricao_situacao"="character", "sigla_casa_identificacao_materia"="character",
                                            "nome_casa_identificacao_materia"="character", "sigla_subtipo_materia"="character", "descricao_subtipo_materia"="character",
                                            "numero_materia"="character", "ano_materia"="character", "descricao_objetivo_processo"="character",
                                            "descricao_identificacao_materia"="character", "indicador_tramitando"="character", "data_hora"="character", "sequencia"="character",
                                            "evento"="character")
