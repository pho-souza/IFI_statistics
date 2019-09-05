#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyFiles)
library(shinythemes)
library(shinydashboard)
library(plotly)
library(utf8)
library(knitr)

navbarPage("Pagina de Acessos",
           theme = shinytheme("yeti"),
           tabPanel("Gráfico",
                  
                  #windowTitle = "Dados da IFI")
                  sidebarPanel(width = 3,
    
    actionButton(inputId = "Atualizador",label = "Atualizar"),
    actionButton(inputId = "Fechador",label = "Fechar"),
    box(width = "100%",background = "black",
        
        # Escolha do modo de uso
        radioButtons(inputId = "modo_operacao",
                     label="Qual período você deseja?",
                     choices=c("Dia","Hora"),
                     inline = T),
      
        # Escolha do modo de visualização do gráfico
        radioButtons(inputId = "modo_ver",
                     label="Como prefere visualizar gráfico?",
                     choices=c("Absoluto","Defasagem"),
                     #choiceValues = c("proxima","completa"),
                     inline = F),
      # Data
      uiOutput("datas"),
      # Aqui seleciona a categoria do documento 
    radioButtons("categoria","Selecione a categoria",
            choices = c("Todos",unique(as.character(teste02.total$Categoria)))),
    checkboxInput("tudo","Deseja selecionar todos?",value = FALSE),
    
    #selectInput("selecao","Selecione um valor", choices = unique(seletor_nome),multiple = T)
    uiOutput("selecao"),
    # Mostra o resultado de categoria
    #uiOutput("nome_file"),
    # Botão de Download
    downloadButton(label = "Exportar dados em planilha:",title = "Exportar dados",outputId = 'exporta'),
    textOutput(outputId = "texto_saida")
    )
  ),  
#     # Show a plot of the generated distribution
  mainPanel(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
      h1("Aqui se mostra o principal"),
    
      plotlyOutput("GraficoLinha"),
      #plotOutput("GraficoLinha",click = "clicar",hover = "hover_mouse"),
      dataTableOutput("Ta_bela"),
      textOutput("Final")
#        
    )
),
tabPanel("Relatório"
         
  )
)







