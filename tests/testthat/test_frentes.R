context("Frentes")

setup <- function() {
  frente_ambientalista <- fetch_frentes(54012, "camara")
  frente_amazonia <- fetch_frentes(54013, "camara")
  frente_corrupcao <- fetch_frentes(53888, "camara")
  frente_senado <- fetch_frentes(53888, "senado")

  membros_ambientalista <- fetch_membros_frentes(54012, "camara")
  membros_corrupcao <- fetch_membros_frentes(53888, "camara")
  return(TRUE)
}

test_that("Is dataframe", {
  expect_true(is.data.frame(frente_ambientalista))
  expect_true(is.data.frame(frente_amazonia))
  expect_true(is.data.frame(frente_corrupcao))

  expect_true(is.data.frame(membros_ambientalista))
  expect_true(is.data.frame(membros_corrupcao))

})

test_that("Not Empty", {
  expect_true(nrow(frente_ambientalista) != 0)
  expect_true(nrow(frente_amazonia) != 0)
  expect_true(nrow(frente_corrupcao) != 0)

  expect_true(nrow(membros_ambientalista) != 0)
  expect_true(nrow(membros_corrupcao) != 0)
})

test_that("Columns", {
  expect_true(ncol(frente_ambientalista) == 12)
  expect_true(ncol(frente_amazonia) == 12)
  expect_true(ncol(frente_corrupcao) == 12)

  expect_true(ncol(membros_ambientalista) == 14)
  expect_true(ncol(membros_corrupcao) == 14)
})

test_that("fetch_frentes()", {
  frente <- tibble::tibble(
    id_coordenador = as.integer(160511),
    id_frente = as.integer(54012),
    id_legislatura = as.integer(56),
    id_legislatura_coordenador = as.integer(56),
    nome_coordenador = "ALESSANDRO MOLON",
    partido_coordenador = "PSB",
    situacao = "04/04/2019 - Recebidos o REQ 1069/2019, a Ata, o Estatuto e as adesões.\r\n04/04/2019 - Encaminhado ao SEPRO para conferência de assinaturas.\r\n10/04/2019 - Confirmadas 216 assinaturas de Deputados. Há Senadores. \r\n10/04/2019 - Of. 137/2019/SGM encaminhado ao Senado para conferência de assinaturas dos Senadores.\r\n16/04/2019 - Confirmadas 5 assinaturas de Senadores. \r\n16/04/2019 - Vai à publicação.\r\n08/05/2019 - Recebido o OF 11/19, solicitando inclusão do Senador Fabiano Contarato.\r\n09/05/2019 - Of. 169/2019/SGM encaminhado ao Senado para conferência da assinatura.\r\n16/05/2019 - Confirmada a assinatura do Senador. Publique-se.",
    telefone = "3215 5304",
    titulo_frente = "Frente Parlamentar Ambientalista",
    uf_coordenador = "RJ",
    url_coordenador = "https://dadosabertos.camara.leg.br/api/v2/deputados/160511",
    url_documento = "https://www.camara.leg.br/internet/deputado/Frente_Parlamentar/63843-integra.pdf")

  expect_equal(frente_ambientalista, frente)
  expect_warning(frente_senado)
})

test_that("Rows", {
  expect_true(nrow(membros_ambientalista) == 219)
  expect_true(nrow(membros_corrupcao) == 232)
})
