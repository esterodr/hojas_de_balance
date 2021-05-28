library(shiny)

shinyUI(fluidPage(
    
    fluidRow(class = "text-center",
             tags$h3("Simulación de las Hojas de Balance de la Econom\u00EDa Argentina")),
    
    fluidRow(class = "text-center",
             plotOutput("balancesheet")
             ),
    
    fluidRow(class = "text-center",
             tags$h6("Valores en millones de pesos. (*) Las LEBAC ya no existen, se las incluye a fines expositivos.
               Los valores son una aproximación a la realidad excepto: (**) Valores completamente arbitrarios.")),
    
    fluidRow(p()),
    
    fluidRow(class = "text-center",
             column(3,
                    selectInput(
                        inputId="politica1",
                        label="Indique el instrumento a utilizar",
                        choices=c("Adelantos Transitorios","Leliq","Reservas",
                                  "Gasto e Impuestos","Emitir Tit.Pub.",
                                  "Cancelar Tit.Pub.", "Operar Tit.Pub.Pesos",
                                  "Operar Tit.Pub.USD","Dep\u00F3sitos","Pr\u00E9stamos",
                                  "Cta.Cte./Circulante","Efectivo Minimo","Balanza Comercial","LEBAC (*)"),
                        selected = NULL,
                        multiple = FALSE,
                        selectize = TRUE,
                        width = NULL,
                        size = NULL)
             ),
             column(3,
                    conditionalPanel(
                        condition = "input.politica1 == 'Adelantos Transitorios'",
                        selectInput("politica2", "Elija la acción a realizar",
                                    list("Otorgar", "Cancelar"))
                    ),
                    
                    conditionalPanel(
                        condition = "input.politica1 == 'Leliq'",
                        selectInput("politica2b", "Elija la acción a realizar",
                                    list("Suscribir", "Rescatar"))
                    ),
                    
                    conditionalPanel(
                        condition = "input.politica1 == 'Pr\u00E9stamos'",
                        selectInput("politica2c", "Elija la acción a realizar",
                                    list("Otorgar", "Cancelar"))
                    ),
                    
                    conditionalPanel(
                        condition = "input.politica1 == 'Reservas'",
                        selectInput("politica2d", "Elija la acción a realizar",
                                    list("Comprar al SPnF","Comprar al SF", "Comprar al G",
                                         "Vender al SPnF","Vender al SF", "Vender al G"))
                    ),
                    
                    conditionalPanel(
                        condition = "input.politica1 == 'Emitir Tit.Pub.'",
                        selectInput("politica2e", "Elija una opci\u00F3n",
                                    list("En pesos con SPnF", "En pesos con SF",
                                         "En pesos con RM", "En USD con SPnF",
                                         "En USD con RM"))
                    ),
                    
                    conditionalPanel(
                        condition = "input.politica1 == 'Balanza Comercial'",
                        selectInput("politica2f", "Elija el saldo",
                                    list("Super\u00E1vit", "D\u00E9ficit"))
                    ),
                    
                    conditionalPanel(
                        condition = "input.politica1 == 'Dep\u00F3sitos'",
                        selectInput("politica2g", "Elija la acci\u00F3n a realizar",
                                    list("Depositar Pesos", "Depositar USD",
                                         "Extraer Pesos", "Extraer USD",
                                         "Constituir Plazo Fijo", "Cancelar Plazo Fijo"))
                    ),
                    
                    conditionalPanel(
                        condition = "input.politica1 == 'LEBAC (*)'",
                        selectInput("politica2h", "Elija la acción a realizar",
                                    list("Suscribir con SPnF", "Suscribir con RM",
                                         "Cancelar con SPnF", "Cancelar con RM"))
                    ),
                    
                    conditionalPanel(
                        condition = "input.politica1 == 'Cancelar Tit.Pub.'",
                        selectInput("politica2i", "Elija una opci\u00F3n",
                                    list("En pesos con SPnF", "En pesos con SF",
                                         "En pesos con RM", "En USD con SPnF",
                                         "En USD con RM"))
                    ),
                    
                    conditionalPanel(
                        condition = "input.politica1 == 'Operar Tit.Pub.Pesos'",
                        selectInput("politica2j", "Elija una opci\u00F3n",
                                    list("BC compra al SF",
                                         "BC compra al SPnF", "BC compra al RM",
                                         "SF compra al BC", "SF compra al SPnF",
                                         "SF compra al RM", 
                                         "SPnf compra al BC", "SPnf compra al SF",
                                         "SPnf compra al RM", "RM compra al BC",
                                         "RM compra al SF", "RM compra al SPnF"))
                    ),
                    
                    conditionalPanel(
                        condition = "input.politica1 == 'Operar Tit.Pub.USD'",
                        selectInput("politica2k", "Elija una opci\u00F3n",
                                    list("BC compra al SPnF", "BC compra al RM",
                                         "SPnF compra al BC","SPnF compra al RM",
                                         "RM compra al BC","RM compra al SPnF"))
                    ),
                    
                    conditionalPanel(
                        condition = "input.politica1 == 'Gasto e Impuestos'",
                        selectInput("politica2l", "Elija una opci\u00F3n",
                                    list("Gasto P\u00FAblico", "Cobrar Impuestos a SF",
                                         "Cobrar Impuestos a SPnF"))
                    ),
                    
                    conditionalPanel(
                        condition = "input.politica1 == 'Cta.Cte./Circulante'",
                        selectInput("politica2m", "Elija una opci\u00F3n",
                                    list("Circulante a Cta.Cte.", "Cta.Cte. a Circulante",
                                         "USD a Cta.Cte.USD", "Cta.Cte.USD a USD"))
                    ),
                    
                    conditionalPanel(
                        condition = "input.politica1 == 'Efectivo Minimo'",
                        selectInput("politica2n", "Elija una opci\u00F3n",
                                    list("Sustituir LELIQ por Bono"))
                    )
                    ),
             column(3,
                    numericInput("Monto","Monto",value=0,min=0)),
             
             column(3,style = "margin-top: 25px;",
                    actionButton("Simular","Simular política"),
                    actionButton("Reiniciar","Reiniciar"))
    ),
    
    fluidRow(class = "text-center",
        tags$hr(),
        tags$h6(
            "Para leer la documentaci\u00F3n de este proyecto, click ",
            tags$a(href="https://github.com/esterodr/hojas_de_balance/blob/main/README.md", 
                   "aquí.")
        )
    )
))
