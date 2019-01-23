#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(tidyverse)
library(shinydashboard)
library(readxl)
#library(xlsx)

shinyServer(function(input, output) {
  

  load("U:/Pastas pessoais/Pedro/Códigos e experimentos/R/acessos/base03")
  load("U:/Pastas pessoais/Pedro/Códigos e experimentos/R/acessos/base02")
  load("U:/Pastas pessoais/Pedro/Códigos e experimentos/R/acessos/base01")
  
  #### visualização da data
  output$datas <- renderUI({
    if(input$modo_operacao == "Hora"){
      dateInput(inputId="datas",
                label = "Escolha o dia",
                min = min(teste02.total$Data),
                max = max(teste02.total$Data),
                language = "pt-BR",
                format = "dd/mm/yyyy",
                value = min(teste02.total$Data))
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
  
  
  #### Output de seleção
  output$selecao <- renderUI({
   if(input$categoria=="Todos"){
      selectizeInput("selecao", "Selecione os documentos",
                  multiple = T,
                  choices = unique(teste03.total$Nome))
   }
    else{
      selecao_nome <- teste03.total %>%
        filter(Categoria == input$categoria) %>%
        ungroup() %>%
        select(Nome)

      selectizeInput("selecao", "Selecione os documentos",
                  multiple = T,
                  choices = unique(selecao_nome)
                  )
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


  output$GraficoLinha <-renderPlot({
    proximo <- ifelse(input$modo_ver=="Escala próxima",
                      "dodge",
                      "stack")
    if(input$modo_operacao == "Dia"){
      dados <- teste03.total %>%
      filter(Nome %in% input$selecao) #%>%
    
    xrange <- dados %>%
      filter(Data >= input$datas[1]) %>%
      filter(Data <= input$datas[2])

    ggplot(data=xrange, aes(colour = paste(Categoria,Numero),
                            group = Numero,
                            x=Data,
                            y=visualizações)) +
      geom_line(linejoin = "round",linemitre = 2,position = proximo)+
      #geom_point()+
      geom_label(aes(label=visualizações),position = proximo)+
      xlab("Período") +
      ylab("Nº de acessos") +
      ggthemes::theme_tufte() +
      theme(line = element_line(lineend = "butt", linetype = "dotted"),
            legend.text = element_text(size=8, family = "sans"),
            legend.title = element_blank(),
            axis.title = element_text(size=8, family = "sans"),
            legend.background = element_blank(),
            legend.key = 
      )
    }
    else{
      dados <- teste02.total %>%
        filter(Nome %in% input$selecao)
      
      xrange <- dados %>%
        filter(Data == input$datas)
      
      ggplot(data=xrange, aes(color = paste(Categoria,Numero),
                              group = Numero,
                              x=Hora,
                              y=visualizações)) +
        geom_line(linejoin = "round",linemitre = 2,position = proximo)+
        #geom_point()+
        geom_label(aes(label=visualizações),position = proximo)+
        xlab(as.character(paste("Dia: ", xrange$Data))) +
        ylab("Nº de acessos") +
        ggthemes::theme_tufte() +
        theme(line = element_line(lineend = "butt", linetype = "dotted"),
              legend.text = element_text(size=8, family = "sans"),
              legend.title = element_blank(),
              axis.title = element_text(size=8, family = "sans"),
              legend.background = element_blank(),
              legend.key = 
        )
    }
            
  })
  
# Aqui dados da tabela abaixo   
  output$Ta_bela <- renderDataTable({
    if(input$modo_operacao == "Dia"){
    teste03.total %>%
      filter(Nome %in% input$selecao) %>%
      filter(Data >= input$datas[1]) %>%
      filter(Data <= input$datas[2])
      
    }
    else{
      teste02.total %>%
        filter(Nome %in% input$selecao) %>%
        filter(Data == input$datas)
    }

  })
  
})

