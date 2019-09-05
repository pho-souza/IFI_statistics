#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define a interface do usuário para o app que gera um histograma.
ui <- fluidPage(
  
  # Título do app.
  titlePanel("Meu primeiro shiny app!"),
  
  # Barra lateral com as definições do input e do output.
  sidebarLayout(
    # Barra lateral para os inputs.
    sidebarPanel("Painel 01",
      
      # Input: número de classes do histograma.
      selectInput("select_categoria",
                  "Escolha qual iten você quer",
                  c("Todos",unique(teste01.pais$Categoria)))
      
      
    ),
    
    # Painel principal para mostrar os outputs.
    mainPanel("Painel principal",
      
      # Output: Histograma
      dataTableOutput(outputId = "tabela01")
      
    )
  )
)


# Define o código necessário para a construção de um histograma.
server <- function(input, output) {
  #dataInput <- reactive(input$select_agreg,)
  # if(input$select_agreg == "Total geral"){
  #   tabela <- teste01.total
  # }
  # if(input$select_agreg == "Países"){
  #   tabela <- teste01.pais
  # }
  output$tabela01 <- renderDataTable(teste01.total)
  
}

shinyApp(ui = ui, server = server)
