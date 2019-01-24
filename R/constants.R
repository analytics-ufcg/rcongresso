# Link default da API
.CAMARA_API_LINK <- "https://dadosabertos.camara.leg.br"
.SENADO_API_LINK <- "http://legis.senado.leg.br"

# Links Alternativos
.CAMARA_WEBSITE_LINK <- "http://www.camara.gov.br"

# Mensagens de erro
.ERRO_RETORNO_JSON <- "API did not return json"
.WARNING_PROPOSICAO_ID <- "Pode haver campos incorretos na proposicao. Verifique o tipo, numero e ano."
.WARNING_SIGLA_PARTIDO <- "Algumas siglas podem estar incorretas."

# Paths Câmara
.PROPOSICOES_PATH <- "/api/v2/proposicoes"
.VOTACOES_PATH <- "/api/v2/votacoes"
.DEPUTADOS_PATH <- "/api/v2/deputados"
.PARTIDOS_PATH <- "/api/v2/partidos"
.TIPOS_PROPOSICOES_PATH <- "/api/v2/referencias/tiposProposicao"
.CAMARA_SESSOES_PATH <- "/proposicoesWeb/sessoes_e_reunioes"

# Paths Senado
.SENADO_SESSOES_PATH <- "/dadosabertos/materia/ordia/"
  
# Link do repositório do rcongresso
.RCONGRESSO_LINK <- "https://github.com/analytics-ufcg/rcongresso"

# Regex Pattern
.REGEX_PATTERN <- "(?=[A-Z][^A-Z])"

# Requests
.MAX_ITENS <- 100
.LAST_PAGE_INDEX <- 4
.COD_ERRO_CLIENTE <- 400
.COD_ERRO_SERV <- 500
.MAX_TENTATIVAS_REQ <- 5
.MENSAGEM_ERRO_REQ <- function(error_code, api_url){stop(sprintf("Falha na requisicao a API dos Dados Abertos. Erro %s ao tentar acessar: %s",
                                                  error_code, api_url), call. = FALSE)}
