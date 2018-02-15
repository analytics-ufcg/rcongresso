# Link default da API
.API_LINK <- "https://dadosabertos.camara.leg.br"

# Mensagens de erro
.ERRO_RETORNO_JSON <- "API did not return json"

# Paths
.PROPOSICOES_PATH <- "/api/v2/proposicoes"
.VOTACOES_PATH <- "/api/v2/votacoes"
.DEPUTADOS_PATH <- "/api/v2/deputados"
.TIPOS_PROPOSICOES_PATH <- "/api/v2/referencias/tiposProposicao"

# Link do repositório do rcongresso
.RCONGRESSO_LINK <- "https://github.com/analytics-ufcg/rcongresso"

# Regex Pattern
.REGEX_PATTERN <- "(?=[A-Z][^A-Z])"

# Requests
.MAX_ITENS <- 100

# Tests
.COLNAMES_DEP_INFO_ID <- c("id"="numeric","uri"="character","nomeCivil"="character","ultimoStatus.id"="numeric",
                              "ultimoStatus.uri"="character","ultimoStatus.nome"="character","ultimoStatus.siglaPartido"="character",
                              "ultimoStatus.uriPartido"="character","ultimoStatus.siglaUf"="character",
                              "ultimoStatus.idLegislatura"="numeric","ultimoStatus.urlFoto"="character",
                              "ultimoStatus.data"="character","ultimoStatus.nomeEleitoral"="character",
                              "ultimoStatus.gabinete.nome"="numeric","ultimoStatus.gabinete.predio"="character",
                              "ultimoStatus.descricaoStatus"="character","redeSocial"="character",
                              "ultimoStatus.gabinete.sala"="numeric","ultimoStatus.gabinete.andar"="numeric",
                              "ultimoStatus.gabinete.telefone"="character","ultimoStatus.gabinete.email"="character",
                              "ultimoStatus.situacao"="numeric","ultimoStatus.condicaoEleitoral"="character","cpf"="character",
                              "sexo"="character","urlWebsite"="character","dataNascimento"="character","dataFalecimento"="character",
                              "ufNascimento"="character","municipioNascimento"="character","escolaridade"="character")
