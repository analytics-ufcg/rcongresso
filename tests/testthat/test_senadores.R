context("Senadores")

#Setup
legislatura_inicial <- 40
legislatura_final <- 56

senadores <- fetch_senadores(legislatura_inicial, legislatura_final)

test_that("fetch_senadores(legis_inicial, legis_final)", {
  expect_true(is.data.frame(senadores))
})

test_that("fetch_senadores(legis_inicial, legis_final)", {
  expect_error(fetch_senadores(-1, -1))
})
