# rcongresso

[![Build Status](https://travis-ci.org/analytics-ufcg/rcongresso.svg?branch=master)](https://travis-ci.org/analytics-ufcg/rcongresso)
[![codecov](https://codecov.io/gh/analytics-ufcg/rcongresso/branch/master/graph/badge.svg)](https://codecov.io/gh/analytics-ufcg/rcongresso)

Pacote R para acessar dados do congresso nacional baseado na API RESTful criada em 2017 (https://dadosabertos.camara.leg.br/) e como uma tidy tool que interaja bem com o tidyverse.

Para instalar:

```R
# Caso você não possua o devtools
# install.packages('devtools')

# Instala o pacote a partir do github
devtools::install_github('analytics-ufcg/rcongresso')
```

Uma vignette de exemplo do uso:

```R
library(rcongresso)
devtools::build_vignettes()
vignette("qmr-uma-votacao", package="rcongresso")
```
