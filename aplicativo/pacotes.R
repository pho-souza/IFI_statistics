pacotes<-c("shiny","dplyr","ggplot2","tidyverse","ggthemes","rsconnect","lubridate",
           "shinydashboard","readxl","rvest","data.table","lubridate","rsconnect","scales",
           "shinyWidgets","plotly")

#source("acessos.R")

for(i in 1:NROW(pacotes)){
  if(lapply(pacotes,require,character.only = T)[i]==F){
    install.packages(pacotes[i])
  }
}

lapply(pacotes,library,character.only=T)
