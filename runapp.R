setwd("U://Pastas pessoais/Pedro/Códigos e experimentos/R/acessos/")

source("aplicativo/pacotes.R")

load(file = "aplicativo/base01")
load(file = "aplicativo/base02")
load(file = "aplicativo/base03")

runApp(appDir = "aplicativo/", launch.browser=TRUE)
