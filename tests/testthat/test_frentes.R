context("Frentes")

setup <- function() {
  frente_amazonia <- fetch_frentes(54013, "camara")
  frente_corrupcao <- fetch_frentes(53888, "camara")
  frente_senado <- fetch_frentes(53888, "senado")

  membros_amazonia <- fetch_membros_frentes(54013, "camara")
  membros_corrupcao <- fetch_membros_frentes(53888, "camara")
  return(TRUE)
}

test_that("Is dataframe", {
  expect_true(is.data.frame(frente_amazonia))
  expect_true(is.data.frame(frente_corrupcao))

  expect_true(is.data.frame(membros_corrupcao))
  expect_true(is.data.frame(membros_amazonia))

})

test_that("Not Empty", {
  expect_true(nrow(frente_amazonia) != 0)
  expect_true(nrow(frente_corrupcao) != 0)

  expect_true(nrow(membros_amazonia) != 0)
  expect_true(nrow(membros_corrupcao) != 0)
})

test_that("Columns", {
  expect_true(ncol(frente_amazonia) == 12)
  expect_true(ncol(frente_corrupcao) == 12)

  expect_true(ncol(membros_amazonia) == 14)
  expect_true(ncol(membros_corrupcao) == 14)

  colnames_frente <- c("id_coordenador", "id_frente", "id_legislatura", "id_legislatura_coordenador",
                       "nome_coordenador", "partido_coordenador", "situacao", "telefone", "titulo_frente",
                       "uf_coordenador", "url_coordenador", "url_documento")
  #expect_true(names(frente_amazonia) == colnames_frente)
})

#test_that("fetch_frentes()", {
#  frente <- tibble::tibble(
#    id_coordenador = as.integer(204569),
#    id_frente = as.integer(54013),
#    id_legislatura = as.integer(56),
#    id_legislatura_coordenador = as.integer(56),
#    nome_coordenador = "DELEGADO PABLO",
#    partido_coordenador = "PSL",
#    situacao = "03/04/2019 - Recebidos o REQ 1078/2019, a Ata, o Estatuto e as adesões.\r\n03/04/2019 - Encaminhado ao SEPRO para conferência de assinaturas.\r\n10/04/2019 - Confirmadas 211 assinaturas de Deputados. Não há Senadores. \r\n10/04/2019 - Vai à publicação/05/2019 - Recebido o OF 27/2019, solicitando inclusão de nova adesão. \r\n28/02/2019 - Encaminhado ao SEPRO para conferência de assinaturas. \r\n11/03/2019 - Confirmadas 2 adesões de Deputados. Não há Senadores.\r\n11/03/2019 - Vai à publicação.\r\n14/05/2019 - Recebido o OF 27/2019, solicitando inclusão de nova adesão. Ao SEPRO, para conferência.\r\n14/05/2019 - Confirmada 1 adesão de Deputado. Não há Senadores. Publique-se.\r\n21/08/2019 - Recebido o OF 102/2019, solicitando inclusão de novas adesões. Ao SEPRO, para conferência.\r\n22/08/2019 - Confirmadas 3 adesões de Deputados. Não há Senadores. Publique-se.",
#    telefone = "3215 5373",
#    titulo_frente = "Frente Parlamentar em Defesa da Amazônia",
#    uf_coordenador = "AM",
#    url_coordenador = "https://dadosabertos.camara.leg.br/api/v2/deputados/204569",
#    url_documento = "https://www.camara.leg.br/internet/deputado/Frente_Parlamentar/63845-integra.pdf")

#  expect_equal(frente_amazonia, frente)

#})

test_that("Rows", {
  expect_true(nrow(membros_amazonia) == 215)
  expect_true(nrow(membros_corrupcao) == 232)
})

