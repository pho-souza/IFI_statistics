#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

pacotes<-c("shiny","dplyr","ggplot2","tidyverse","ggthemes","rsconnect","lubridate","data.table","Hmisc",
           "shinydashboard","readxl","rvest","data.table","lubridate","rsconnect","scales","shinyWidgets",
           "plotly")

#source("acessos.R")

for(i in 1:NROW(pacotes)){
  if(lapply(pacotes,require,character.only = T)[i]==F){
    install.packages(pacotes[i])
  }
}

lapply(pacotes,library,character.only=T)


load("base03")
load("base02")
load("base01")

library(readxl)
library(tidyverse)
library(rvest)
library(data.table)
library(lubridate)

# Importação dos dados da Lista do Excel com todos os documentos da IFI
itens <- read_excel(path = "Lista.xlsx")



# Definição da função para extrair dados do site da IFI
count.views.now <- function(pub){
  my.url <- paste(c("https://www2.senado.leg.br/bdsf/handle/id/",as.character(pub),"/statistics"),collapse="")
  file <- read_html(my.url)
  sections <- html_text(html_nodes(x=file,css="h2"))
  n.sections <- length(sections)
  tables <- html_nodes(file,"table")
  tables <- html_table(tables)
  tables <- tables[1:length(sections)]
  names(tables) <- sections
  value <- tables
}

count.ifi <- function(){
  attach(what="U:/Pastas pessoais/Alessandro/atualiza/acessos/RAF",name="RAF")
  store <- vector(mode="list",length=nrow(RAFs))
  for (i in 1:nrow(RAFs)){    store[[i]] <- count.views.now(RAFs[i,"Código"])    }
  attr(x=store,which="time") <- date()
  load(file="U:/Pastas pessoais/Alessandro/atualiza/acessos/number")
  name.update <- paste(c("U:/Pastas pessoais/Alessandro/atualiza/acessos/","RAFs_",number),collapse="")
  number <- number + 1
  save(number,file="U:/Pastas pessoais/Alessandro/atualiza/acessos/number")
  save(store,file=name.update)
  detach(name="RAF")
}

# Abrir a base01, que contém os dados da IFI brutos. 
#load(file = "base01")
# Abrir base inicialmente

atualizador <- function(){
  itens <- read_excel(path = "Lista.xlsx")
  
  load(file = "base01") 
  for(i in 1:NROW(itens)){
    teste01 <- count.views.now(itens$ID[i])
    #teste01.total$Numero <- ifelse(teste01.total$ID==itens$ID[i],itens$Numero[i],teste01.total$Numero)
    #
    print(i)
    
    if(#i == 1 & 
      exists(x = "teste01.total") != T){
      teste01.total <- teste01$`Arquivos visitados`
      teste01.total$ID <- itens$ID[i]
      teste01.total$Nome <- itens$Nome[i]
      teste01.total$Categoria <- itens$Categoria[i]
      teste01.total$Numero <- ifelse(teste01.total$ID == itens$ID[i],itens$Numero[i],teste01.total$Numero)
      teste01.total$Data <- as.POSIXct(as.character("2019-01-15 14:34:30"),format="%Y-%m-%d %H")
      teste01.total$Data_Original <- itens$Data[i]
      names(teste01.total) <- c("Nome_Arquivo","visualizações","ID","Nome","Categoria","Numero","Data", "Data_Original")
      
      
    }
    else
    {
      tmp.total <- teste01$`Arquivos visitados`
      tmp.total$ID <- itens$ID[i]
      tmp.total$Nome <- itens$Nome[i]
      tmp.total$Categoria <- itens$Categoria[i]
      tmp.total$Numero <- ifelse(tmp.total$ID == itens$ID[i], itens$Numero[i],tmp.total$Numero)
      tmp.total$Data <- as.POSIXct(Sys.time())
      tmp.total$Data_Original <- itens$Data[i]
      names(tmp.total) <- c("Nome_Arquivo","visualizações","ID","Nome","Categoria","Numero","Data", "Data_Original")
      teste01.total <- rbind.data.frame(teste01.total,tmp.total)
      #rm(tmp.total)
    }
  }
  
  for(i in 1:NROW(itens)){
    teste01.total$Numero = ifelse(teste01.total$ID == itens$ID[i],itens$Numero[i],teste01.total$Numero)
  }
  
  # Salva a base após atualizar
  save(teste01.total,file = "base01")
  save(teste01.total,file = "base001-bkp")

  troca <- as.character(c("%20","%C3%A9"))
  trocado <- as.character(c(" ", "é"))
  subs <- cbind.data.frame(troca,trocado)
  subs <- subs %>% mutate(troca=as.character(troca),trocado=as.character(trocado))
  rm(troca, trocado)
  
  
  # Aqui substitui os elementos do texto referentes à acentos e outros elementos
  for(i in 1:NROW(subs)){
    itens$`Nome arquivo` <- gsub(pattern = subs$troca[i],replacement = subs$trocado[i], x = itens$`Nome arquivo`)
  }
  itens$Nome_Arquivo_Legacy <- paste(itens$`Nome arquivo`,"(legacy)",sep="")
  
  print("01")
  #filtro dos arquivos principais 
  teste02.total <- teste01.total %>%
    #mutate(Nome_Arquivo = gsub("%20"," ",Nome_Arquivo)) %>%
    filter(Nome_Arquivo %in% itens$`Nome arquivo`) %>%
    filter(Data < as.Date.character("2019-07-19")) %>%
    mutate(ID= factor(ID), Categoria = factor(Categoria), Nome = factor(Nome),Nome_Arquivo = factor(Nome_Arquivo),Numero = as.integer(Numero))
  
  teste02.temp <- teste01.total %>%
    #mutate(Nome_Arquivo = gsub("%20"," ",Nome_Arquivo)) %>%
    filter(Nome_Arquivo %in% itens$Nome_Arquivo_Legacy | Nome_Arquivo %in% itens$`Nome arquivo`) %>%
    filter(Data >= as.Date.character("2019-07-19")) %>%
    mutate(ID= factor(ID), Categoria = factor(Categoria), Nome = factor(Nome),Nome_Arquivo = factor(Nome_Arquivo),Numero = as.integer(Numero))
  
  teste02.total <- rbind.data.frame(teste02.total,teste02.temp)
  
  teste02.total <- teste02.total %>% 
    group_by(Data,Categoria,ID,Numero,Nome,Data_Original) %>%
    summarise(`visualizações`=sum(`visualizações`))
  
  teste02.total <- teste02.total %>%
    group_by(Nome) %>%
    arrange(desc(Nome),Data) %>%
    mutate(visualiza_lag = visualizações-lag(x=`visualizações`,order_by = Data,default = NA)) %>%
    mutate(visualiza_lag = as.integer(ifelse(is.na(visualiza_lag),0,visualiza_lag)))
  
  
  print("02")
  teste02.total <- teste02.total %>%
    group_by(Data) %>%
    mutate(Hora = sub(pattern = "[0-9]{4}-[0-9]{2}-[0-9]{2} ",replacement = "", x = Data)) %>%
    mutate(Hora = as.POSIXct(paste("2019-01-01 ", Hora))) %>%
    ungroup(Data) %>%
    mutate(Data = as.Date(Data))
  
  # Seleciona o último valor de cada data
  teste03.total <- as.data.frame(teste02.total) %>%
    group_by(Data,Nome) %>%
    filter(Hora==max(Hora))
  
  teste03.total <- teste03.total %>%
    group_by(Nome) %>%
    arrange(desc(Nome),Data) %>%
    mutate(visualiza_lag = visualizações-lag(x=`visualizações`,order_by = Data,default = NA)) %>%
    mutate(visualiza_lag = ifelse(is.na(visualiza_lag),0,visualiza_lag))
  
  teste03.total$visualiza_lag <- as.integer(teste03.total$visualiza_lag)
  
  teste03.total <- as.data.frame(teste03.total)
  
  print("03")
  
  teste02.total$Data[which.max(teste02.total$Data)]
  
  teste02.total <- teste02.total %>%
    group_by(Categoria) %>%
    arrange(Data_Original)
  
  teste03.total <- teste03.total %>%
    group_by(Categoria) %>%
    arrange(Data_Original)
  
  save(teste02.total, file="base02")
  save(teste03.total, file="base03")
  write_excel_csv(teste01.total, "relatorio01.xlsx")

  rm(teste01.total)
  rm(teste02.total)
  rm(teste03.total)

  load("base01")
  load("base02")
  load("base03")
  
  
}



shinyServer(function(input, output, session) {

  #### visualização da data
  output$datas <- renderUI({
    
    if(input$modo_operacao == "Hora"){
      dateInput(inputId="datas",
                label = "Escolha o dia",
                min = min(teste02.total$Data),
                max = max(teste02.total$Data),
                language = "pt-BR",
                format = "dd/mm/yyyy",
                value = max(teste02.total$Data))
    }
    else{
      dateRangeInput(inputId="datas",
                     label = "Período de datas",
                     min = min(teste03.total$Data),
                     start = min(teste03.total$Data),
                     end = max(teste03.total$Data),
                     max = max(teste03.total$Data),
                     language = "pt-BR",
                     format = "dd/mm/yyyy",
                     separator = "a")
    }
  })
  
  ### Output de atualização
  observeEvent(
    input$Atualizador,label = "Testando",
    {
      output$texto_saida <- renderText({
        "Atualizando"
      })
      # output$texto_saida <- renderText({
      #    "Começar o update"
      # })
      #atualizador()
      atualizador()
      if(exists("teste01.total")==T){
        rm(teste01.total)
      }
      if(exists("teste02.total")==T){
        rm(teste02.total)
      }
      if(exists("teste03.total")==T){
        rm(teste03.total)
      }
        load("base03")
        load("base02")
        load("base01")
      
      showModal(modalDialog(title = "ATUALIZADO"
      ))
      output$texto_saida <- renderText({
        "ATUALIZADO"
      })
  })
  
  observeEvent(
    input$Fechador,label = "Fechando",
    {
      stopApp()
      q(save = "no")
    })
  # other code ...
    
  #### Output de seleção
  output$selecao <- renderUI({
    if(exists("teste01.total")==T|exists("teste02.total")==T|exists("teste03.total")==T){
      cat("Selecao \n teste 01")
      load("base03")
      load("base02")
      load("base01")
    }
    
    
    if(input$tudo == FALSE){
     if(input$categoria=="Todos"){
        pickerInput(inputId = "selecao", label = "Selecione os documentos",
                    multiple = T,
                    choices = as.character(unique(teste03.total$Nome)))
      }
      else{
        selecao_nome <- teste03.total %>%
          filter(Categoria == input$categoria) %>%
          ungroup() %>%
          select(Nome)
  
        selecao_nome$Nome
        pickerInput("selecao", "Selecione os documentos",
                    multiple = T,
                    choices = as.character(unique(selecao_nome$Nome)))
      }
    }
      else{
        if(input$categoria=="Todos"){
          pickerInput(inputId = "selecao", label = "Selecione os documentos",
                      multiple = T,
                      choices = as.character(unique(teste03.total$Nome)),
                      selected = as.character(unique(teste03.total$Nome)))
        }
        else{
          selecao_nome <- teste03.total %>%
            filter(Categoria == input$categoria) %>%
            ungroup() %>%
            select(Nome)
          
          pickerInput(inputId = "selecao",label =  "Selecione os documentos",
                         multiple = T,
                         choices = as.character(unique(selecao_nome$Nome)),
                         selected = as.character(unique(selecao_nome$Nome)))
      }
    }
  })
  
  
  output$exporta <- downloadHandler(
    contentType = "text/csv",
    filename = paste("dados",input$modo_operacao,".csv",sep=""),
    # Exportação de acordo com o dataframe
    content = function(file) {
      if(input$modo_operacao == "Hora"){
        
      write_excel_csv2(x=teste02.total %>%
                         filter(Data == input$datas) %>%
                         filter(Nome %in% input$selecao),
                       path = file)
      }
      else{
        write_excel_csv2(x=teste03.total %>%
                           filter(Data >= input$datas[1]) %>%
                           filter(Data <= input$datas[2]) %>%
                           filter(Nome %in% input$selecao),
                         path = file)
      }
    }
  )


  output$GraficoLinha <-renderPlotly({
    if(input$modo_operacao == "Dia"){
      if(exists("teste01.total")==T|exists("teste02.total")==T|exists("teste03.total")==T){
        load("base03")
        load("base02")
        load("base01")
      }
      dados <- teste03.total %>%
        filter(Nome %in% input$selecao) #%>%
    
    xrange <- dados %>%
      filter(Data >= input$datas[1]) %>%
      filter(Data <= input$datas[2])
    
    xrange <- as.data.frame(xrange)
    
    #plot(x = xrange$Data,y = xrange$`visualizações`,type = "l")
    if(input$modo_ver == "Defasagem"){
    p <- plot_ly(x = xrange$Data,
                 y=xrange$visualiza_lag,
                 type = "scatter",
                 mode = "lines+markers",
                 color = paste(xrange$Categoria,xrange$Numero,sep = " "),
                 symbol = "circle",
                 colors = "Set2",
                 name = paste(xrange$Categoria,xrange$Numero,sep = " "))
    }
    if(input$modo_ver == "Absoluto"){
      p <- plot_ly(x = xrange$Data,
                   y=xrange$`visualizações`,
                   type = "scatter",
                   mode = "lines+markers",
                   color = paste(xrange$Categoria,xrange$Numero,sep = " "),
                   colors = "Set2",
                   symbol = "circle",
                   name = paste(xrange$Categoria,xrange$Numero,sep = " "))
    }
    p
    # ggplot(data=xrange, aes(colour = paste(Categoria,Numero),
    #                         group_by(Categoria,Numero),
    #                         x=Data,
    #                         y=`visualizações`),ymin=0) +
    #   geom_line(linejoin = "round",linemitre = 2,position = proximo)+
    #   #geom_point()+
    #   geom_label(aes(label=`visualizações`),position = proximo)+
    #   xlab("Período") +
    #   ylab("Nº de acessos") +
    #   ggthemes::theme_tufte() +
    #   theme(line = element_line(lineend = "butt", linetype = "dotted"),
    #         legend.text = element_text(size=8, family = "sans"),
    #         legend.title = element_blank(),
    #         axis.title = element_text(size=8, family = "sans"),
    #         legend.background = element_blank()#,
    #         #legend.key =
    #   )+
    #   scale_x_date(labels = date_format("%d/%m"),date_breaks = "1 day")+
    #   expansor
    

    }
    else{
      if(exists("teste01.total")==T){
        load("base03")
        load("base02")
        load("base01")
      }
      
      dados <- teste02.total %>%
        filter(Nome %in% input$selecao)
      
      xrange <- dados %>%
        filter(Data == input$datas)
      
      date(xrange$Hora) <- xrange$Data
      # xrange <- as.data.frame(xrange)
      # 
      # valor <- nearPoints(df = xrange, coordinfo =  input$clicar, xvar = xrange$Hora, yvar = xrange$`visualizações`,
      #            threshold = 5, maxpoints = 1,panelvar1 = "Nome")
      # 
      # cat("O valor é\n")
      # print(valor)
      if(input$modo_ver == "Defasagem"){
      p <- plot_ly(x = xrange$Hora,
                   y=xrange$visualiza_lag,
                   type = "scatter",
                   mode = "lines+markers",
                   name = paste(xrange$Categoria,xrange$Numero,sep = " "),
                   color = paste(xrange$Categoria,xrange$Numero,sep = " "),
                   colors = "Set2",
                   symbol = "circle")
      }
      if(input$modo_ver == "Absoluto"){
        p <- plot_ly(x = xrange$Hora,
                     y=xrange$`visualizações`,
                     type = "scatter",
                     mode = "lines+markers",
                     name = paste(xrange$Categoria,xrange$Numero,sep = " "),
                     color = paste(xrange$Categoria,xrange$Numero,sep = " "),
                     colors = "Set2",
                     symbol = "circle")
      }
      p
      
      # ggplot(data=xrange, aes(colour = paste(Categoria,Numero),
      #                         group_by(Categoria,Numero),
      #                         x=Hora,
      #                         y=`visualizações`)) +
      #   geom_line(linejoin = "round",linemitre = 2,position = proximo)+
      #   #geom_point()+
      #   geom_label(aes(label=`visualizações`),position = proximo)+
      #   xlab(as.character(paste("Dia: ", xrange$Data))) +
      #   ylab("Nº de acessos") +
      #   ggthemes::theme_tufte() +
      #   theme(line = element_line(lineend = "butt", linetype = "dotted"),
      #         legend.text = element_text(size=8, family = "sans"),
      #         legend.title = element_blank(),
      #         axis.title = element_text(size=8, family = "sans"),
      #         legend.background = element_blank(),
      #         legend.key = 
      #   )+
      #   expansor
    
    }     
  })
  
# Aqui dados da tabela abaixo   
  output$Ta_bela <- renderDataTable({
    if(input$modo_operacao == "Dia"){
      if(exists("teste01.total")){
        load("base03")
        load("base02")
        load("base01")
      }
    teste03.total %>%
      filter(Nome %in% input$selecao) %>%
      filter(Data >= input$datas[1]) %>%
      filter(Data <= input$datas[2])
      
    }
    else{
      if(exists("teste01.total")|exists("teste02.total")|exists("teste03.total")){
        load("base03")
        load("base02")
        load("base01")
      }
      teste02.total %>%
        filter(Nome %in% input$selecao) %>%
        filter(Data == input$datas)
    }

  })
  
})

