context("Orgãos")

setup <- function(){
  orgaos_camara <<- fetch_orgaos_camara()
  comissao_capadr <<- fetch_orgao_camara("CAPADR")
  agenda_completa_comissoes_camara <<- fetch_agenda_orgaos_camara('12/05/2018', '26/05/2018')
  agenda_CCJC <<- fetch_agendas_comissoes_camara_auxiliar('2003', '12/05/2018', '26/05/2018')

  return(TRUE)
}

# Setup
check_api <- function(){
  tryCatch(setup(), error = function(e){
    message(e)
    return(FALSE)
    })
}

test <- function(){
  test_that("Is dataframe", {
    expect_true(is.data.frame(orgaos_camara))
    expect_true(is.data.frame(agenda_completa_comissoes_camara))
    expect_true(is.data.frame(agenda_CCJC))
  })

  test_that("Have many rows", {
    expect_true(nrow(orgaos_camara) >= 5000)
  })

  test_that("Is dataframe", {
    expect_true(is.data.frame(comissao_capadr))
  })

  test_that("Not Empty", {
    expect_true(nrow(comissao_capadr) != 0)
  })

  test_that("fetch_agenda_orgaos_camara()", {
    expect_true(all(sapply(agenda_completa_comissoes_camara, class) %in% .COLNAMES_AGENDA_COMISSOES_CAMARA))
  })

  test_that("fetch_agendas_comissoes_camara_auxiliar()", {
    expect_true(all(sapply(agenda_CCJC, class) %in% .COLNAMES_AGENDA_COMISSAO_CAMARA))
  })
}

if(check_api()){
  test()
} else skip()
