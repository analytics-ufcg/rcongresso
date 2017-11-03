# rcongresso

[![Build Status](https://travis-ci.org/analytics-ufcg/rcongresso.svg?branch=master)](https://travis-ci.org/analytics-ufcg/rcongresso)
[![codecov](https://codecov.io/gh/analytics-ufcg/rcongresso/branch/master/graph/badge.svg)](https://codecov.io/gh/analytics-ufcg/rcongresso)
[![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/rcongresso)](https://cran.r-project.org/package=rcongresso)
![](https://cranlogs.r-pkg.org/badges/grand-total/rcongresso)

Pacote R para acessar dados do congresso nacional baseado na API RESTful criada em 2017 (https://dadosabertos.camara.leg.br/), gerando data frames e interagindo bem com o tidyverse.

Site: https://analytics-ufcg.edu.br/rcongresso 

Para instalar:

```R
# Versão estável: 
install.packages("rcongresso")

# A mais atual: 
# install.packages('devtools')
devtools::install_github('analytics-ufcg/rcongresso')
```

Exemplos de uso:

```R
vignette("introducao-rcongresso", package="rcongresso")
vignette("purrr-e-rcongresso", package="rcongresso")

# se você instalou do github, antes faça: devtools::build_vignettes()
```
