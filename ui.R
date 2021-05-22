library(shiny)

shinyUI(fluidPage(
    
    fluidRow(class = "text-center",
             titlePanel("Simulación de las Hojas de Balance de la Econom\u00EDa Argentina")),
    
    fluidRow(class = "text-center",
             plotOutput("balancesheet")
             ),
    
    fluidRow(class = "text-left",
             p("Valores en millones de pesos. Ante variaciones en el stock de dep\u00F3sitos a la vista 
             se ajusta autom\u00E1ticamente la integraci\u00F3n de efectivo m\u00EDnimo. (*) Las LEBAC ya no existen, se las incluye a fines expositivos.
               El resto de los datos son reales salvo: (**) Valores completamente arbitrarios; (***)
               Se supone que el Tesoro tiene disponible inicialmente para gastar el 10% del total de dep\u00F3sitos a la vista del Sector Público")),
    
    fluidRow(class = "text-center",
             p()),
    
    fluidRow(class = "text-center",
             column(4,
                    selectInput(
                        inputId="politica1",
                        label="Indique el instrumento a utilizar",
                        choices=c("Adelantos Transitorios","Leliq","Reservas",
                                  "Gasto e Impuestos","Emitir Tit.Pub.",
                                  "Cancelar Tit.Pub.", "Operar Tit.Pub.Pesos",
                                  "Operar Tit.Pub.USD","Dep\u00F3sitos","Pr\u00E9stamos",
                                  "Balanza Comercial","LEBAC (*)"),
                        selected = NULL,
                        multiple = FALSE,
                        selectize = TRUE,
                        width = NULL,
                        size = NULL)
             ),
             column(4,
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
                    )
                    ),
             column(4,
                    numericInput("Monto","Monto",value=0,min=0))
    ),
    
    fluidRow(class = "text-center",
             column(9),
             column(3,
                    actionButton("Simular","Simular política"),
                    actionButton("Reiniciar","Reiniciar"))
    )
    
    
))
