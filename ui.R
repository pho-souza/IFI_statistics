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
library(shinydashboard)

dashboardPage(
#   
#  theme = "bootstrap.css",
#   
# 
#   # Application title
  dashboardHeader(title=tags$a(href="https://www12.senado.leg.br/ifi", target="_blank", 
                               tags$img(height = "20px", alt="IFI Logo", src="logotipo_ifi.png")),
                  tags$li(class = "dropdown",
                          tags$a(href="https://www12.senado.leg.br/ifi", target="_blank", 
                                 tags$img(height = "20px", alt="IFI Logo", src="logotipo_ifi.png")
                          )
                  )
                ),
                  
                  #windowTitle = "Dados da IFI")
  dashboardSidebar(
    box(width = "100%",background = "black",
        
        # Escolha do modo de uso
        radioButtons(inputId = "modo_operacao",
                     label="Qual período você deseja?",
                     choices=c("Dia","Hora"),
                     inline = T),
      
        # Escolha do modo de visualização do gráfico
        radioButtons(inputId = "modo_ver",
                     label="Como prefere visualizar gráfico?",
                     choices=c("Escala próxima","Escala completa"),
                     #choiceValues = c("proxima","completa"),
                     inline = F),
      # Data
      uiOutput("datas"),
      
      
      # Aqui seleciona a categoria do documento 
    radioButtons("categoria","Selecione a categoria",
            choices = c("Todos",unique(as.character(teste02.total$Categoria)))),
    #selectInput("selecao","Selecione um valor", choices = unique(seletor_nome),multiple = T)
    uiOutput("selecao"),
    # Mostra o resultado de categoria
    #uiOutput("nome_file"),
    # Botão de Download
    downloadButton(label = "Exportar dados em planilha:",title = "Exportar dados",outputId = 'exporta')
    )
  ),  
#     # Show a plot of the generated distribution
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
      h1("Aqui se mostra o principal"),

      plotOutput("GraficoLinha"),
      dataTableOutput("Ta_bela"),
      textOutput("Final")
#        
    )
)







