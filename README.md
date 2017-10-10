# rcongresso

[![Build Status](https://travis-ci.org/analytics-ufcg/rcongresso.svg?branch=master)](https://travis-ci.org/analytics-ufcg/rcongresso)
[![codecov](https://codecov.io/gh/analytics-ufcg/rcongresso/branch/master/graph/badge.svg)](https://codecov.io/gh/analytics-ufcg/rcongresso)
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/rcongresso)](http://cran.r-project.org/package=rcongresso)

Pacote R para acessar dados do congresso nacional baseado na API RESTful criada em 2017 (https://dadosabertos.camara.leg.br/) e como uma tidy tool que interaja bem com o tidyverse.

Para instalar:

```R
# Instale a partir do repositório CRAN:
install.packages("rcongresso")

# Ou, alternativamente, você pode instalar a versão de desenvolvedor do GitHub:
# install.packages('devtools')
devtools::install_github('analytics-ufcg/rcongresso')
```

Uma vignette de exemplo do uso:

```R
library(rcongresso)
devtools::build_vignettes()
vignette("introducao-rcongresso", package="rcongresso")
```
