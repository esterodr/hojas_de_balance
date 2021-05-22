source("data.R")
source("graficos.R")
source("funciones.R")

library(shiny)

shinyServer(function(input, output) {
  
  grafico <- reactiveValues(data = NULL)
  
  observeEvent(input$Simular, {
    
    if (input$politica1=="Adelantos Transitorios"){ 
      input2 = input$politica2
    } else if (input$politica1=="Gasto e Impuestos") {
      input2 = input$politica2l
    } else if (input$politica1=="Leliq") {
      input2 = input$politica2b
    } else if (input$politica1=="Pr\u00E9stamos") {
      input2 = input$politica2c
    } else if (input$politica1=="Reservas") {
      input2 = input$politica2d
    } else if (input$politica1=="Emitir Tit.Pub.") {
      input2 = input$politica2e
    } else if (input$politica1=="Balanza Comercial") {
      input2 = input$politica2f
    } else if (input$politica1=="Dep\u00F3sitos") {
      input2 = input$politica2g
    } else if (input$politica1=="LEBAC (*)") {
      input2 = input$politica2h
    } else if (input$politica1=="Cancelar Tit.Pub.") {
      input2 = input$politica2i
    } else if (input$politica1=="Operar Tit.Pub.Pesos") {
      input2 = input$politica2j
    } else if (input$politica1=="Operar Tit.Pub.USD") {
      input2 = input$politica2k
    }
    
    grafico$data <- balancesheet(data,input$politica1,input2,input$Monto)
    
  })
  
  observeEvent(input$Reiniciar, {
    data$data$ValorFinal <- data$data$ValorInicial
    data$data$Variacion <- 0
    data$data$VarAcum <- 0
    data$data$Color <- "white"
    data$data$Color2 <- "white"
    grafico$data <- balancesheet(data,"NA","NA","NA")
    data$text <- ""
    data <<- data 
  })  
  
  output$balancesheet <- renderPlot({
    if (is.null(grafico$data)) return(balancesheet(data,"NA","NA","NA"))
    grafico$data
  })
  
})
