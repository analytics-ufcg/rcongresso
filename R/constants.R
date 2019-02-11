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
.CAMARA_PROPOSICOES_PATH <- "/api/v2/proposicoes"
.VOTACOES_PATH <- "/api/v2/votacoes"
.DEPUTADOS_PATH <- "/api/v2/deputados"
.SENADO_PATH <- "/dadosabertos/materia"
.RELATORIA_SENADO_PATH <- "/relatorias/"
.EMENDAS_SENADO_PATH <- "/emendas/"
.PARTIDOS_PATH <- "/api/v2/partidos"
.CAMARA_SESSOES_PATH <- "/proposicoesWeb/sessoes_e_reunioes"
.TIPOS_PROPOSICOES_PATH <- "/api/v2/referencias/proposicoes/siglaTipo"
.ORGAOS_CAMARA_PATH <- "/api/v2/orgaos"
.AGENDA_CAMARA_PATH <- "/api/v2/eventos"
.ORGAOS_FILE_CAMARA_PATH <- "/arquivos/orgaos/json/orgaos.json"
.PAUTAS_CAMARA <- "/api/v2/eventos/"

# Path site Câmara
.EMENDAS_SUBSTITUTIVOS_REDACAOFINAL_CAMARA_PATH <- "/SitCamaraWS/Orgaos.asmx/ObterEmendasSubstitutivoRedacaoFinal"
.EVENTOS_PROPOSICAO_CAMARA_PATH <- "/proposicoesWeb/sessoes_e_reunioes"

# Paths Senado
.SENADO_SESSOES_PATH <- "/dadosabertos/materia/ordia/"
.SENADO_PROPOSICOES_PATH <- "/dadosabertos/materia/movimentacoes/"
.ORGAOS_SENADO_PATH <- "/dadosabertos/comissao/"
.DEFERIMENTO_SENADO_PATH <- "/dadosabertos/materia/movimentacoes/"
.AGENDA_SENADO_PATH <- "/dadosabertos/plenario/agenda/mes/"

# Link do repositório do rcongresso
.RCONGRESSO_LINK <- "https://github.com/analytics-ufcg/rcongresso"

# Regex Pattern
.REGEX_PATTERN <- "(?=[A-Z][^A-Z])"
.REGEX_DEFERIMENTO_INDEFERIDO <- "^Indefiro"
.REGEX_DEFERIMENTO_DEFERIDO <- "^(Defiro)|(Aprovado)"

# Requests
.MAX_ITENS <- 100
.LAST_PAGE_INDEX <- 4
.COD_REQ_SUCCESS_MIN <- 200
.COD_REQ_SUCCESS_MAX <- 300
.COD_ERRO_CLIENTE <- 400
.COD_ERRO_SERV <- 500
.MAX_TENTATIVAS_REQ <- 5
.MENSAGEM_ERRO_REQ <- "Falha na requisicao a API dos Dados Abertos. Erro %s ao tentar acessar: %s"
