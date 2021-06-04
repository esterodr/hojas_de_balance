
ajustar_encaje <- function(data,moneda="p") {
  if(moneda=="d") {
    
    m1 <- round(data$encaje$VistaUSD*data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.USD"])
                
    data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="EM_Cta.Cte.USD"] <- data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="EM_Cta.Cte.USD"] + m1
    data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Cta.Cte.USD"] <- data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Cta.Cte.USD"] - m1
    
    if((data$data$ValorFinal[data$data$Agente=="SF"&data$data$Nombre=="Cta.Cte.USD"] + 
       data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Cta.Cte.USD"])<0) {
      
      m2 <- data$data$ValorFinal[data$data$Agente=="SF"&data$data$Nombre=="Cta.Cte.USD"] + 
        data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Cta.Cte.USD"]
      data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="USD"] <- data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="USD"] + m2
      data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Cta.Cte.USD"] <- data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Cta.Cte.USD"] - m2
      data$data$Variacion[data$data$Agente=="BC"&data$data$Nombre=="Cta.Cte.USD"] <- data$data$Variacion[data$data$Agente=="BC"&data$data$Nombre=="Cta.Cte.USD"] - m2
      data$data$Variacion[data$data$Agente=="BC"&data$data$Nombre=="Reservas"] <- data$data$Variacion[data$data$Agente=="BC"&data$data$Nombre=="Reservas"] - m2
      
    }
    } else {
    bote <- round(data$encaje$bote*(data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.Vista"] +
                                      data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Plazo.Fijo"]))
    leliq <- round(data$encaje$leliq*(data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Plazo.Fijo"]))
    cc <- round(data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.Vista"]*data$encaje$VistaPesos)
    #extra <- round(data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.Vista"]*(1-data$encaje$VistaPesos))
    data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="EM_Cta.Cte."] <- data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="EM_Cta.Cte."] + cc
    data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Cta.Cte."] <- data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Cta.Cte."] - cc
    data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="EM_LELIQ"] <- data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="EM_LELIQ"] + leliq
    data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="EM_Tit.Pub.Pesos"] <- data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="EM_Tit.Pub.Pesos"] + bote
    data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="LELIQ"] <- data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="LELIQ"] - leliq
    data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Tit.Pub.Pesos"] <- data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Tit.Pub.Pesos"] - bote
    if((data$data$ValorFinal[data$data$Agente=="SF"&data$data$Nombre=="Cta.Cte."] + 
        data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Cta.Cte."])<0) {
      
      m2 <- data$data$ValorFinal[data$data$Agente=="SF"&data$data$Nombre=="Cta.Cte."] + 
        data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Cta.Cte."]
      data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Circulante"] <- data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Circulante"] + m2
      data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Cta.Cte."] <- data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Cta.Cte."] - m2
      data$data$Variacion[data$data$Agente=="BC"&data$data$Nombre=="Cta.Cte."] <- data$data$Variacion[data$data$Agente=="BC"&data$data$Nombre=="Cta.Cte."] - m2
      data$data$Variacion[data$data$Agente=="BC"&data$data$Nombre=="Circulante"] <- data$data$Variacion[data$data$Agente=="BC"&data$data$Nombre=="Circulante"] + m2
      
    }
    
  }
  
  data
}

ajustar_usd <- function(data) {
  data$data$Variacion[data$data$Agente=="BC"&data$data$Nombre=="Reservas_USD"] <- 
    round((data$data$ValorFinal[data$data$Agente=="BC"&data$data$Nombre=="Reservas"] +
             data$data$Variacion[data$data$Agente=="BC"&data$data$Nombre=="Reservas"])/data$tc -
            data$data$ValorFinal[data$data$Agente=="BC"&data$data$Nombre=="Reservas_USD"])
  data$data$Variacion[data$data$Agente=="RM"&data$data$Nombre=="LEBAC_USD"] <- 
    round((data$data$ValorFinal[data$data$Agente=="RM"&data$data$Nombre=="LEBAC"] +
             data$data$Variacion[data$data$Agente=="RM"&data$data$Nombre=="LEBAC"])/data$tc -
            data$data$ValorFinal[data$data$Agente=="RM"&data$data$Nombre=="LEBAC_USD"])
  data$data$Variacion[data$data$Agente=="RM"&data$data$Nombre=="Tit.Pub.Pesos_USD"] <- 
    round((data$data$ValorFinal[data$data$Agente=="RM"&data$data$Nombre=="Tit.Pub.Pesos"] +
             data$data$Variacion[data$data$Agente=="RM"&data$data$Nombre=="Tit.Pub.Pesos"])/data$tc -
            data$data$ValorFinal[data$data$Agente=="RM"&data$data$Nombre=="Tit.Pub.Pesos_USD"])
  data$data$Variacion[data$data$Agente=="RM"&data$data$Nombre=="Tit.Pub.USD_USD"] <- 
    round((data$data$ValorFinal[data$data$Agente=="RM"&data$data$Nombre=="Tit.Pub.USD"] +
             data$data$Variacion[data$data$Agente=="RM"&data$data$Nombre=="Tit.Pub.USD"])/data$tc -
            data$data$ValorFinal[data$data$Agente=="RM"&data$data$Nombre=="Tit.Pub.USD_USD"])
  data$data$Variacion[data$data$Agente=="RM"&data$data$Nombre=="Divisas_USD"] <- 
    round((data$data$ValorFinal[data$data$Agente=="RM"&data$data$Nombre=="Divisas"] +
             data$data$Variacion[data$data$Agente=="RM"&data$data$Nombre=="Divisas"])/data$tc -
            data$data$ValorFinal[data$data$Agente=="RM"&data$data$Nombre=="Divisas_USD"])
  data$data$Variacion[data$data$Agente=="RM"&data$data$Nombre=="Resultado_USD"] <- 
    round((data$data$ValorFinal[data$data$Agente=="RM"&data$data$Nombre=="Resultado"] +
             data$data$Variacion[data$data$Agente=="RM"&data$data$Nombre=="Resultado"])/data$tc -
            data$data$ValorFinal[data$data$Agente=="RM"&data$data$Nombre=="Resultado_USD"])
  data
}

ajustar_resultado <- function(data) {
  data$data$Variacion[data$data$Agente=="BC"&data$data$Nombre=="Resultado"] <- 
    sum(data$data$Variacion[data$data$Agente=="BC"&data$data$Tipo=="A"]) -
          sum(data$data$Variacion[data$data$Agente=="BC"&data$data$Tipo=="P"])
  data$data$Variacion[data$data$Agente=="T"&data$data$Nombre=="Resultado"] <- 
    sum(data$data$Variacion[data$data$Agente=="T"&data$data$Tipo=="A"]) -
          sum(data$data$Variacion[data$data$Agente=="T"&data$data$Tipo=="P"])
  data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Resultado"] <- 
    sum(data$data$Variacion[data$data$Agente=="SF"&data$data$Tipo=="A"]) -
          sum(data$data$Variacion[data$data$Agente=="SF"&data$data$Tipo=="P"])
  data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Resultado"] <- 
    sum(data$data$Variacion[data$data$Agente=="H"&data$data$Tipo=="A"]) -
          sum(data$data$Variacion[data$data$Agente=="H"&data$data$Tipo=="P"])
  data$data$Variacion[data$data$Agente=="RM"&data$data$Nombre=="Resultado"] <- 
    sum(data$data$Variacion[data$data$Agente=="RM"&data$data$Tipo=="A"]) -
          sum(data$data$Variacion[data$data$Agente=="RM"&data$data$Tipo=="P"])
  data
}

emision_at <- function(monto) {
  
  if(monto<=0) {
    
    data$text <- "Ingrese un monto positivo, por favor"
  
  } else {
    
    data$data$Variacion <- 0
    
    data$data$Variacion[data$data$Agente=="BC"&data$data$Nombre=="ATs"] <- monto
    data$data$Variacion[data$data$Agente=="BC"&data$data$Nombre=="Dep.Tesoro"] <- monto
    data$data$Variacion[data$data$Agente=="T"&data$data$Nombre=="Dep.Tesoro.BCRA"] <- monto
    data$data$Variacion[data$data$Agente=="T"&data$data$Nombre=="ATs"] <- monto
    
    data$data$VarAcum <- data$data$VarAcum + data$data$Variacion
    data$data$ValorFinal <- data$data$ValorInicial + data$data$VarAcum
    
    data$data$Color <- ifelse(data$data$Variacion==0,"white",
                              ifelse(data$data$Variacion>0,"green","red"))
    data$data$Color2 <- ifelse(data$data$VarAcum==0,"white",
                              ifelse(data$data$VarAcum>0,"green","red"))
    
    data$text <- ""
    
    }
  
  data 
  
} 

cancelar_at <- function(monto) {
  
  if(monto<=0) {
    
    data$text <- "Ingrese un monto positivo, por favor"
    
  } else if(monto>sum(data$data$ValorFinal[data$data$Agente=="T"&
    data$data$Nombre %in% c("Dep.Tesoro.BCRA","Dep.Tesoro.Vista")])) {
    
      data$text <- "El Tesoro no cuenta con fondos suficientes para esta operaci\u00F3n."
    
  } else {
    
    data$data$Variacion <- 0
    
    m1 <- min(monto,data$data$ValorFinal[data$data$Agente=="T"&data$data$Nombre=="Dep.Tesoro.BCRA"])
    m2 <- monto - m1
    
    data$data$Variacion[data$data$Agente=="BC"&data$data$Nombre=="ATs"] <- -1*monto
    data$data$Variacion[data$data$Agente=="BC"&data$data$Nombre=="Dep.Tesoro"] <- -1*m1
    data$data$Variacion[data$data$Agente=="T"&data$data$Nombre=="Dep.Tesoro.BCRA"] <- -1*m1
    data$data$Variacion[data$data$Agente=="T"&data$data$Nombre=="ATs"] <- -1*monto
    data$data$Variacion[data$data$Agente=="T"&data$data$Nombre=="Dep.Tesoro.Vista"] <- -1*m2
    data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.Vista"] <- -1*m2
    data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Cta.Cte."] <- -1*m2
    data$data$Variacion[data$data$Agente=="BC"&data$data$Nombre=="Cta.Cte."] <- -1*m2
    data <- ajustar_encaje(data)
    
    data$data$Variacion[data$data$Nombre=="Base Monetaria"] <- sum(data$data$Variacion[data$data$Agente=="BC"&
                                                                                                          data$data$Nombre %in% c("Circulante","Cta.Cte.")])
    data$data$Variacion[data$data$Nombre=="M2"] <- data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data$data$Variacion[data$data$Nombre=="M3"] <- data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Plazo.Fijo"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data$data$Variacion[data$data$Nombre=="M3.Privado"] <- data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Plazo.Fijo"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data <- ajustar_resultado(data)
    data <- ajustar_usd(data)
    
    data$data$VarAcum <- data$data$VarAcum + data$data$Variacion
    data$data$ValorFinal <- data$data$ValorInicial + data$data$VarAcum
    
    data$data$Color <- ifelse(data$data$Variacion==0,"white",
                              ifelse(data$data$Variacion>0,"green","red"))
    data$data$Color2 <- ifelse(data$data$VarAcum==0,"white",
                               ifelse(data$data$VarAcum>0,"green","red"))
    
    data$text <- ""
    
  }
  
  data 
  
} 

cancelar_le <- function(monto) {
  
  if(monto<=0) {
    
    data$text <- "Ingrese un monto positivo, por favor"
    
  } else if(monto>data$data$ValorFinal[data$data$Agente=="BC"&data$data$Nombre=="LELIQ"]) {
    
    data$text <- "El stock de LELIQ no es tan grande"
    
  } else {
    
    data$data$Variacion <- 0
    
    data$data$Variacion[data$data$Agente=="BC"&data$data$Nombre=="LELIQ"] <- -1*monto
    data$data$Variacion[data$data$Agente=="BC"&data$data$Nombre=="Cta.Cte."] <- monto
    m1 <- min(monto,data$data$ValorFinal[data$data$Agente=="SF"&data$data$Nombre=="LELIQ"])
    m2 <- monto-m1
    data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="LELIQ"] <- -1*m1
    data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Cta.Cte."] <- m1
    data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="EM_LELIQ"] <- -1*m2
    data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="EM_Cta.Cte."] <- m2
    
    data$data$Variacion[data$data$Nombre=="Base Monetaria"] <- sum(data$data$Variacion[data$data$Agente=="BC"&
                                                                                         data$data$Nombre %in% c("Circulante","Cta.Cte.")])
    data$data$Variacion[data$data$Nombre=="M2"] <- data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]

    data$data$Variacion[data$data$Nombre=="M3"] <- data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Plazo.Fijo"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data$data$Variacion[data$data$Nombre=="M3.Privado"] <- data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Plazo.Fijo"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data <- ajustar_resultado(data)
    data <- ajustar_usd(data)
    
    data$data$VarAcum <- data$data$VarAcum + data$data$Variacion
    data$data$ValorFinal <- data$data$ValorInicial + data$data$VarAcum
    
    data$data$Color <- ifelse(data$data$Variacion==0,"white",
                              ifelse(data$data$Variacion>0,"green","red"))
    data$data$Color2 <- ifelse(data$data$VarAcum==0,"white",
                               ifelse(data$data$VarAcum>0,"green","red"))
    
    data$text <- ""
      
    }
  data
} 

emision_le <- function(monto) {
  
  if(monto<=0) {
    
    data$text <- "Ingrese un monto positivo, por favor"
    
  } else if(monto> (data$data$ValorFinal[data$data$Agente=="SF"&data$data$Nombre=="Cta.Cte."] +
                    data$data$ValorFinal[data$data$Agente=="SF"&data$data$Nombre=="Circulante"])) {
    
    data$text <- "Las entidades financieras no cuentan con suficiente liquidez para esta operaci\u00F3n."
    
  } else {
    
    data$data$Variacion <- 0
    
    m1 <- min(monto-data$data$ValorFinal[data$data$Agente=="SF"&data$data$Nombre=="Cta.Cte."])
    m2 <- monto-m1
    data$data$Variacion[data$data$Agente=="BC"&data$data$Nombre=="LELIQ"] <- monto
    data$data$Variacion[data$data$Agente=="BC"&data$data$Nombre=="Cta.Cte."] <- -1*m2
    data$data$Variacion[data$data$Agente=="BC"&data$data$Nombre=="Circulante"] <- -1*m1
    data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="LELIQ"] <- monto
    data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Circulante"] <- -1*m1
    data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Cta.Cte."] <- -1*m2
    
    data$data$Variacion[data$data$Nombre=="Base Monetaria"] <- sum(data$data$Variacion[data$data$Agente=="BC"&
                                                                                         data$data$Nombre %in% c("Circulante","Cta.Cte.")])
    data$data$Variacion[data$data$Nombre=="M2"] <- data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data$data$Variacion[data$data$Nombre=="M3"] <- data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Plazo.Fijo"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data$data$Variacion[data$data$Nombre=="M3.Privado"] <- data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Plazo.Fijo"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data <- ajustar_resultado(data)
    data <- ajustar_usd(data)
    
    data$data$VarAcum <- data$data$VarAcum + data$data$Variacion
    data$data$ValorFinal <- data$data$ValorInicial + data$data$VarAcum
    
    data$data$Color <- ifelse(data$data$Variacion==0,"white",
                              ifelse(data$data$Variacion>0,"green","red"))
    data$data$Color2 <- ifelse(data$data$VarAcum==0,"white",
                               ifelse(data$data$VarAcum>0,"green","red"))
    
    data$text <- ""
    
  }
  data
}

gasto <- function(monto) {
  
  if(monto<=0) {
    
    data$text <- "Ingrese un monto positivo, por favor"
    
  } else if(monto > (data$data$ValorFinal[data$data$Agente=="T"&data$data$Nombre=="Dep.Tesoro.BCRA"] +
                     data$data$ValorFinal[data$data$Agente=="T"&data$data$Nombre=="Dep.Tesoro.Vista"])) {
    
    data$text <- "El Tesoro no cuenta con fondos suficientes para esta operaci\u00F3n."
    
  } else {
    
    data$data$Variacion <- 0
    
    m1 <- min(monto,data$data$ValorFinal[data$data$Agente=="T"&data$data$Nombre=="Dep.Tesoro.Vista"])
    m2 <- monto-m1
    
    data$data$Variacion[data$data$Agente=="T"&data$data$Nombre=="Dep.Tesoro.BCRA"] <- -1*m2
    data$data$Variacion[data$data$Agente=="T"&data$data$Nombre=="Dep.Tesoro.Vista"] <- -1*m1
    data$data$Variacion[data$data$Agente=="BC"&data$data$Nombre=="Dep.Tesoro"] <- -1*m2
    data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Dep.Vista"] <- monto
    data$data$Variacion[data$data$Agente=="BC"&data$data$Nombre=="Cta.Cte."] <- m2
    data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Cta.Cte."] <- m2
    data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.Vista"] <- m2
    data <- ajustar_encaje(data)
    
    data$data$Variacion[data$data$Nombre=="Base Monetaria"] <- sum(data$data$Variacion[data$data$Agente=="BC"&
                                                                                         data$data$Nombre %in% c("Circulante","Cta.Cte.")])
    data$data$Variacion[data$data$Nombre=="M2"] <- data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data$data$Variacion[data$data$Nombre=="M3"] <- data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Plazo.Fijo"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data$data$Variacion[data$data$Nombre=="M3.Privado"] <- data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Plazo.Fijo"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data <- ajustar_resultado(data)
    data <- ajustar_usd(data)
    
    data$data$VarAcum <- data$data$VarAcum + data$data$Variacion
    data$data$ValorFinal <- data$data$ValorInicial + data$data$VarAcum
    
    data$data$Color <- ifelse(data$data$Variacion==0,"white",
                              ifelse(data$data$Variacion>0,"green","red"))
    data$data$Color2 <- ifelse(data$data$VarAcum==0,"white",
                               ifelse(data$data$VarAcum>0,"green","red"))
    
    data$text <- ""
    
  }
  data
} 

impuestos_sf <- function(monto) {
  
  if(monto<=0) {
    
    data$text <- "Ingrese un monto positivo, por favor"
    
  } else if(monto > (data$data$ValorFinal[data$data$Agente=="SF"&data$data$Nombre=="Circulante"] +
                     data$data$ValorFinal[data$data$Agente=="SF"&data$data$Nombre=="Cta.Cte."])/data$encaje$VistaPesos) {
    
    data$text <- "El Sector Financiero no cuenta con liquidez suficientes para esta operaci\u00F3n."
    
  } else {
    
    data$data$Variacion <- 0
    
    data$data$Variacion[data$data$Agente=="T"&data$data$Nombre=="Dep.Tesoro.Vista"] <- monto
    data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.Vista"] <- monto
    data <- ajustar_encaje(data)

    data$data$Variacion[data$data$Nombre=="Base Monetaria"] <- sum(data$data$Variacion[data$data$Agente=="BC"&
                                                                                         data$data$Nombre %in% c("Circulante","Cta.Cte.")])
    data$data$Variacion[data$data$Nombre=="M2"] <- data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data$data$Variacion[data$data$Nombre=="M3"] <- data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Plazo.Fijo"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data$data$Variacion[data$data$Nombre=="M3.Privado"] <- data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Plazo.Fijo"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data <- ajustar_resultado(data)
    data <- ajustar_usd(data)
    
    data$data$VarAcum <- data$data$VarAcum + data$data$Variacion
    data$data$ValorFinal <- data$data$ValorInicial + data$data$VarAcum
    
    data$data$Color <- ifelse(data$data$Variacion==0,"white",
                              ifelse(data$data$Variacion>0,"green","red"))
    data$data$Color2 <- ifelse(data$data$VarAcum==0,"white",
                               ifelse(data$data$VarAcum>0,"green","red"))
    
    data$text <- ""
    
  }
  data
}

impuestos_sp <- function(monto) {
  
  if(monto<=0) {
    
    data$text <- "Ingrese un monto positivo, por favor"
    
  } else if(monto > (data$data$ValorFinal[data$data$Agente=="H"&data$data$Nombre=="Dep.Vista"])) {
    
    data$text <- "El SPnF no cuenta con suficientes dep\u00F3sitos a la vista."
    
  } else {
    
    data$data$Variacion <- 0
    
    data$data$Variacion[data$data$Agente=="T"&data$data$Nombre=="Dep.Tesoro.Vista"] <- monto
    data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Dep.Vista"] <- -1*monto
    
    data$data$Variacion[data$data$Nombre=="Base Monetaria"] <- sum(data$data$Variacion[data$data$Agente=="BC"&
                                                                                         data$data$Nombre %in% c("Circulante","Cta.Cte.")])
    data$data$Variacion[data$data$Nombre=="M2"] <- data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data$data$Variacion[data$data$Nombre=="M3"] <- data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Plazo.Fijo"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data$data$Variacion[data$data$Nombre=="M3.Privado"] <- data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Plazo.Fijo"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data <- ajustar_resultado(data)
    data <- ajustar_usd(data)
    
    data$data$VarAcum <- data$data$VarAcum + data$data$Variacion
    data$data$ValorFinal <- data$data$ValorInicial + data$data$VarAcum
    
    data$data$Color <- ifelse(data$data$Variacion==0,"white",
                              ifelse(data$data$Variacion>0,"green","red"))
    data$data$Color2 <- ifelse(data$data$VarAcum==0,"white",
                               ifelse(data$data$VarAcum>0,"green","red"))
    
    data$text <- ""
    
  }
  data
}

prestamos_dar <- function(monto) {
  
  if(monto<=0) {
    
    data$text <- "Ingrese un monto positivo, por favor"
    
  } else if (monto>(data$data$ValorFinal[data$data$Agente=="SF"&data$data$Nombre=="Circulante"] +
                    data$data$ValorFinal[data$data$Agente=="SF"&data$data$Nombre=="Cta.Cte."])/
             data$encaje$VistaPesos){
    
    data$text <- "Las entidades financieras no disponen de suficiente liquidez para un pr\u00E9stamo tan grande"
    
  } else {
    
    data$data$Variacion <- 0
    
    data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Prestamos"] <- monto
    data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Prestamos"] <- monto
    data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Dep.Vista"] <- monto
    data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.Vista"] <- monto
    data <- ajustar_encaje(data)
    
    data$data$Variacion[data$data$Nombre=="Base Monetaria"] <- sum(data$data$Variacion[data$data$Agente=="BC"&
                                                                                         data$data$Nombre %in% c("Circulante","Cta.Cte.")])
    data$data$Variacion[data$data$Nombre=="M2"] <- data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data$data$Variacion[data$data$Nombre=="M3"] <- data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Plazo.Fijo"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data$data$Variacion[data$data$Nombre=="M3.Privado"] <- data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Plazo.Fijo"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data <- ajustar_resultado(data)
    data <- ajustar_usd(data)
    
    data$data$VarAcum <- data$data$VarAcum + data$data$Variacion
    data$data$ValorFinal <- data$data$ValorInicial + data$data$VarAcum
    
    data$data$Color <- ifelse(data$data$Variacion==0,"white",
                              ifelse(data$data$Variacion>0,"green","red"))
    data$data$Color2 <- ifelse(data$data$VarAcum==0,"white",
                               ifelse(data$data$VarAcum>0,"green","red"))
    
    data$text <- ""
    
  }
  data
}

prestamos_can <- function(monto) {
  
  if(monto<=0) {
    
    data$text <- "Ingrese un monto positivo, por favor"
    
  } else if(monto > (data$data$ValorFinal[data$data$Agente=="H"&data$data$Nombre=="Dep.Vista"] +
                     data$data$ValorFinal[data$data$Agente=="H"&data$data$Nombre=="Circulante"])) {
    
    data$text <- "El Sector Privado no cuenta con fondos suficientes para esta operaci\u00F3n"
    
    
  } else if(monto > (data$data$ValorFinal[data$data$Agente=="H"&data$data$Nombre=="Prestamos"])) {
    
    data$text <- "La cantidad de Pr\u00E9stamos no es tan grande."
    
  } else {
    
    data$data$Variacion <- 0
    
    m1 <- min(monto,data$data$ValorFinal[data$data$Agente=="H"&data$data$Nombre=="Dep.Vista"])
    m2 <- monto - m1
    
    data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Prestamos"] <- -1*monto
    data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Prestamos"] <- -1*monto
    data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Dep.Vista"] <- -1*m1
    data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.Vista"] <- -1*m1
    data <- ajustar_encaje(data)
    
    data$data$Variacion[data$data$Nombre=="M3.Privado"] <- data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Plazo.Fijo"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data <- ajustar_resultado(data)
    data <- ajustar_usd(data)
    
    data$data$VarAcum <- data$data$VarAcum + data$data$Variacion
    data$data$ValorFinal <- data$data$ValorInicial + data$data$VarAcum
    
    data$data$Color <- ifelse(data$data$Variacion==0,"white",
                              ifelse(data$data$Variacion>0,"green","red"))
    data$data$Color2 <- ifelse(data$data$VarAcum==0,"white",
                               ifelse(data$data$VarAcum>0,"green","red"))
    
    data$text <- ""

  }
  data
}

trade_sup <- function(monto) {
  
  if(monto<=0) {
    
    data$text <- "Ingrese un monto positivo, por favor"
    
  } else {
    
    data$data$Variacion <- 0
    
    data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Dep.USD"] <- monto
    data$data$Variacion[data$data$Agente=="RM"&data$data$Nombre=="Divisas"] <- monto
    data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.USD"] <- monto
    data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Cta.Cte.USD"] <- monto
    data$data$Variacion[data$data$Agente=="BC"&data$data$Nombre=="Cta.Cte.USD"] <- monto
    data$data$Variacion[data$data$Agente=="BC"&data$data$Nombre=="Reservas"] <- monto
    data <- ajustar_encaje(data,moneda="d")
    
    data$data$Variacion[data$data$Nombre=="Base Monetaria"] <- sum(data$data$Variacion[data$data$Agente=="BC"&
                                                                                         data$data$Nombre %in% c("Circulante","Cta.Cte.")])
    data$data$Variacion[data$data$Nombre=="M2"] <- data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data$data$Variacion[data$data$Nombre=="M3"] <- data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Plazo.Fijo"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data$data$Variacion[data$data$Nombre=="M3.Privado"] <- data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Plazo.Fijo"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data <- ajustar_resultado(data)
    data <- ajustar_usd(data)
    
    data$data$VarAcum <- data$data$VarAcum + data$data$Variacion
    data$data$ValorFinal <- data$data$ValorInicial + data$data$VarAcum
    
    data$data$Color <- ifelse(data$data$Variacion==0,"white",
                              ifelse(data$data$Variacion>0,"green","red"))
    data$data$Color2 <- ifelse(data$data$VarAcum==0,"white",
                               ifelse(data$data$VarAcum>0,"green","red"))
    
    data$text <- ""
    
  }
  data
}

trade_def <- function(monto) {
  
  if(monto<=0) {
    
    data$text <- "Ingrese un monto positivo, por favor"
    
  } else if(monto>data$data$ValorFinal[data$data$Agente=="H"&data$data$Nombre=="Dep.USD"]) {
    
     data$text <- "El sector privado no tiene suficientes d\u00F3lares para esta operaci\u00F3n"
    
  } else {
    
    data$data$Variacion <- 0
    
    data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Dep.USD"] <- -1*monto
    data$data$Variacion[data$data$Agente=="RM"&data$data$Nombre=="Divisas"] <- -1*monto
    data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.USD"] <- -1*monto
    data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Cta.Cte.USD"] <- -1*monto
    data$data$Variacion[data$data$Agente=="BC"&data$data$Nombre=="Cta.Cte.USD"] <- -1*monto
    data$data$Variacion[data$data$Agente=="BC"&data$data$Nombre=="Reservas"] <- -1*monto
    data <- ajustar_encaje(data,moneda="d")
    
    data$data$Variacion[data$data$Nombre=="Base Monetaria"] <- sum(data$data$Variacion[data$data$Agente=="BC"&
                                                                                         data$data$Nombre %in% c("Circulante","Cta.Cte.")])
    data$data$Variacion[data$data$Nombre=="M2"] <- data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data$data$Variacion[data$data$Nombre=="M3"] <- data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Plazo.Fijo"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data$data$Variacion[data$data$Nombre=="M3.Privado"] <- data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Plazo.Fijo"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data <- ajustar_resultado(data)
    data <- ajustar_usd(data)
    
    data$data$VarAcum <- data$data$VarAcum + data$data$Variacion
    data$data$ValorFinal <- data$data$ValorInicial + data$data$VarAcum
    
    data$data$Color <- ifelse(data$data$Variacion==0,"white",
                              ifelse(data$data$Variacion>0,"green","red"))
    data$data$Color2 <- ifelse(data$data$VarAcum==0,"white",
                               ifelse(data$data$VarAcum>0,"green","red"))
    
    data$text <- ""
    
  }
  data
}

res_c_sp <- function(monto) {
  
  if(monto<=0) {
    
    data$text <- "Ingrese un monto positivo, por favor"
    
  } else if(monto>data$data$ValorFinal[data$data$Agente=="H"&data$data$Nombre=="Dep.USD"]) {
    
    data$text <- "El SPNF no tiene suficientes d\u00F3lares para esta operaci\u00F3n"
    
  } else {
    
    data$data$Variacion <- 0
    
    data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Dep.USD"] <- -1*monto
    data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.USD"] <- -1*monto
    data$data$Variacion[data$data$Agente=="BC"&data$data$Nombre=="Cta.Cte.USD"] <- -1*monto
    data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Cta.Cte.USD"] <- -1*monto
    data <- ajustar_encaje(data,moneda = "d")
    data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Dep.Vista"] <- monto
    data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.Vista"] <- monto
    data$data$Variacion[data$data$Agente=="BC"&data$data$Nombre=="Cta.Cte."] <- monto
    data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Cta.Cte."] <- monto
    data <- ajustar_encaje(data)
    
    data$data$Variacion[data$data$Nombre=="Base Monetaria"] <- sum(data$data$Variacion[data$data$Agente=="BC"&
                                                                                         data$data$Nombre %in% c("Circulante","Cta.Cte.")])
    data$data$Variacion[data$data$Nombre=="M2"] <- data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data$data$Variacion[data$data$Nombre=="M3"] <- data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Plazo.Fijo"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data$data$Variacion[data$data$Nombre=="M3.Privado"] <- data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Plazo.Fijo"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data <- ajustar_resultado(data)
    data <- ajustar_usd(data)
    
    data$data$VarAcum <- data$data$VarAcum + data$data$Variacion
    data$data$ValorFinal <- data$data$ValorInicial + data$data$VarAcum
    
    data$data$Color <- ifelse(data$data$Variacion==0,"white",
                              ifelse(data$data$Variacion>0,"green","red"))
    data$data$Color2 <- ifelse(data$data$VarAcum==0,"white",
                               ifelse(data$data$VarAcum>0,"green","red"))
    
    data$text <- ""
    
  }
  data
}

res_c_sf <- function(monto) {
  
  if(monto<=0) {
    
    data$text <- "Ingrese un monto positivo, por favor"
    
  } else if(monto>(data$data$ValorFinal[data$data$Agente=="SF"&data$data$Nombre=="USD"] +
                   data$data$ValorFinal[data$data$Agente=="SF"&data$data$Nombre=="Cta.Cte.USD"])) {
    
    data$text <- "El Sector Financiero no tiene suficientes d\u00F3lares para esta operaci\u00F3n"
    
  } else {
    
    data$data$Variacion <- 0
    
    m1 <- min(monto,data$data$ValorFinal[data$data$Agente=="SF"&data$data$Nombre=="Cta.Cte.USD"])
    m2 <- monto - m1
    
    data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Cta.Cte.USD"] <- -1*m1
    data$data$Variacion[data$data$Agente=="BC"&data$data$Nombre=="Cta.Cte.USD"] <- -1*m1
    data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="USD"] <- -1*m2
    data$data$Variacion[data$data$Agente=="BC"&data$data$Nombre=="Reservas"] <- m2
    data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Cta.Cte."] <- monto
    data$data$Variacion[data$data$Agente=="BC"&data$data$Nombre=="Cta.Cte."] <- monto
    
    data$data$Variacion[data$data$Nombre=="Base Monetaria"] <- sum(data$data$Variacion[data$data$Agente=="BC"&
                                                                                         data$data$Nombre %in% c("Circulante","Cta.Cte.")])
    data$data$Variacion[data$data$Nombre=="M2"] <- data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data$data$Variacion[data$data$Nombre=="M3"] <- data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Plazo.Fijo"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data$data$Variacion[data$data$Nombre=="M3.Privado"] <- data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Plazo.Fijo"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data <- ajustar_resultado(data)
    data <- ajustar_usd(data)
    
    data$data$VarAcum <- data$data$VarAcum + data$data$Variacion
    data$data$ValorFinal <- data$data$ValorInicial + data$data$VarAcum
    
    data$data$Color <- ifelse(data$data$Variacion==0,"white",
                              ifelse(data$data$Variacion>0,"green","red"))
    data$data$Color2 <- ifelse(data$data$VarAcum==0,"white",
                               ifelse(data$data$VarAcum>0,"green","red"))
    
    data$text <- ""
    
  }
  data
}

res_c_g <- function(monto) {
  
  if(monto<=0) {
    
    data$text <- "Ingrese un monto positivo, por favor"
    
  } else if(monto>data$data$ValorFinal[data$data$Agente=="T"&data$data$Nombre=="Dep.USD"]) {
    
    data$text <- "El Tesoro no tiene suficientes d\u00F3lares para esta operaci\u00F3n"
    
  } else {
    
    data$data$Variacion <- 0
    
    data$data$Variacion[data$data$Agente=="T"&data$data$Nombre=="Dep.USD"] <- -1*monto
    data$data$Variacion[data$data$Agente=="BC"&data$data$Nombre=="Cta.Cte.USD"] <- -1*monto
    data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.USD"] <- -1*monto
    data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Cta.Cte.USD"] <- -1*monto
    data <- ajustar_encaje(data,moneda = "d")
    data$data$Variacion[data$data$Agente=="T"&data$data$Nombre=="Dep.Tesoro.BCRA"] <- monto
    data$data$Variacion[data$data$Agente=="BC"&data$data$Nombre=="Dep.Tesoro"] <- monto
    
    data$data$Variacion[data$data$Nombre=="Base Monetaria"] <- sum(data$data$Variacion[data$data$Agente=="BC"&
                                                                                         data$data$Nombre %in% c("Circulante","Cta.Cte.")])
    data$data$Variacion[data$data$Nombre=="M2"] <- data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data$data$Variacion[data$data$Nombre=="M3"] <- data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Plazo.Fijo"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data$data$Variacion[data$data$Nombre=="M3.Privado"] <- data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Plazo.Fijo"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data <- ajustar_resultado(data)
    data <- ajustar_usd(data)
    
    data$data$VarAcum <- data$data$VarAcum + data$data$Variacion
    data$data$ValorFinal <- data$data$ValorInicial + data$data$VarAcum
    
    data$data$Color <- ifelse(data$data$Variacion==0,"white",
                              ifelse(data$data$Variacion>0,"green","red"))
    data$data$Color2 <- ifelse(data$data$VarAcum==0,"white",
                               ifelse(data$data$VarAcum>0,"green","red"))
    
    data$text <- ""
    
  }
  data
}

res_v_sp <- function(monto) {
  
  if(monto<=0) {
    
    data$text <- "Ingrese un monto positivo, por favor"
    
  } else if(monto>(data$data$ValorFinal[data$data$Agente=="H"&data$data$Nombre=="Dep.Vista"])) {
    
    data$text <- "El sector privado no tiene suficientes fondos para esta operaci\u00F3n"
    
  } else if(monto>data$data$ValorFinal[data$data$Agente=="BC"&data$data$Nombre=="Reservas"]) {
    
    data$text <- "El Banco Central no tiene suficientes Reservas para esta operaci\u00F3n"
    
  } else {
    
    data$data$Variacion <- 0
    
    data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Dep.USD"] <- monto
    data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.USD"] <- monto
    data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Cta.Cte.USD"] <- monto
    data$data$Variacion[data$data$Agente=="BC"&data$data$Nombre=="Cta.Cte.USD"] <- monto
    data <- ajustar_encaje(data,moneda = "d")
    data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Dep.Vista"] <- -1*monto
    data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.Vista"] <- -1*monto
    data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Cta.Cte."] <- -1*monto
    data$data$Variacion[data$data$Agente=="BC"&data$data$Nombre=="Cta.Cte."] <- -1*monto
    data <- ajustar_encaje(data)
    
    data$data$Variacion[data$data$Nombre=="Base Monetaria"] <- sum(data$data$Variacion[data$data$Agente=="BC"&
                                                                                         data$data$Nombre %in% c("Circulante","Cta.Cte.")])
    data$data$Variacion[data$data$Nombre=="M2"] <- data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data$data$Variacion[data$data$Nombre=="M3"] <- data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Plazo.Fijo"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data$data$Variacion[data$data$Nombre=="M3.Privado"] <- data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Plazo.Fijo"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data <- ajustar_resultado(data)
    data <- ajustar_usd(data)
    
    data$data$VarAcum <- data$data$VarAcum + data$data$Variacion
    data$data$ValorFinal <- data$data$ValorInicial + data$data$VarAcum
    
    data$data$Color <- ifelse(data$data$Variacion==0,"white",
                              ifelse(data$data$Variacion>0,"green","red"))
    data$data$Color2 <- ifelse(data$data$VarAcum==0,"white",
                               ifelse(data$data$VarAcum>0,"green","red"))
    
    data$text <- ""
    
  }
  data
}

res_v_sf <- function(monto) {
  
  if(monto<=0) {
    
    data$text <- "Ingrese un monto positivo, por favor"
    
  } else if(monto>(data$data$ValorFinal[data$data$Agente=="SF"&data$data$Nombre=="Circulante"])) {
    
    data$text <- "El Sector Financiero no tiene suficientes fondos para esta operaci\u00F3n"
    
  } else if(monto>data$data$ValorFinal[data$data$Agente=="BC"&data$data$Nombre=="Reservas"]) {
    
    data$text <- "El Banco Central no tiene suficientes Reservas para esta operaci\u00F3n"
    
  } else {
    
    data$data$Variacion <- 0
    
    data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Cta.Cte.USD"] <- monto
    data$data$Variacion[data$data$Agente=="BC"&data$data$Nombre=="Cta.Cte.USD"] <- monto
    data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Cta.Cte."] <- -1*monto
    data$data$Variacion[data$data$Agente=="BC"&data$data$Nombre=="Cta.Cte."] <- -1*monto
    data <- ajustar_encaje(data)
    
    data$data$Variacion[data$data$Nombre=="Base Monetaria"] <- sum(data$data$Variacion[data$data$Agente=="BC"&
                                                                                         data$data$Nombre %in% c("Circulante","Cta.Cte.")])
    data$data$Variacion[data$data$Nombre=="M2"] <- data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data$data$Variacion[data$data$Nombre=="M3"] <- data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Plazo.Fijo"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data$data$Variacion[data$data$Nombre=="M3.Privado"] <- data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Plazo.Fijo"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data <- ajustar_resultado(data)
    data <- ajustar_usd(data)
    
    data$data$VarAcum <- data$data$VarAcum + data$data$Variacion
    data$data$ValorFinal <- data$data$ValorInicial + data$data$VarAcum
    
    data$data$Color <- ifelse(data$data$Variacion==0,"white",
                              ifelse(data$data$Variacion>0,"green","red"))
    data$data$Color2 <- ifelse(data$data$VarAcum==0,"white",
                               ifelse(data$data$VarAcum>0,"green","red"))
    
    data$text <- ""
    
  }
  data
}

res_v_g <- function(monto) {
  
  if(monto<=0) {
    
    data$text <- "Ingrese un monto positivo, por favor"
    
  } else if(monto>(data$data$ValorFinal[data$data$Agente=="T"&data$data$Nombre=="Dep.Tesoro.BCRA"] +
                   data$data$ValorFinal[data$data$Agente=="T"&data$data$Nombre=="Dep.Tesoro.Vista"])) {
    
    data$text <- "El Tesoro no tiene suficientes fondos para esta operaci\u00F3n"
    
  } else if(monto>data$data$ValorFinal[data$data$Agente=="BC"&data$data$Nombre=="Reservas"]) {
    
    data$text <- "El Banco Central no tiene suficientes Reservas para esta operaci\u00F3n"
    
  } else {
    
    data$data$Variacion <- 0
    
    m1 <- min(monto,data$data$ValorFinal[data$data$Agente=="T"&data$data$Nombre=="Dep.Tesoro.BCRA"])
    m2 <- monto-m1
    
    data$data$Variacion[data$data$Agente=="T"&data$data$Nombre=="Dep.USD"] <- monto
    data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.USD"] <- monto
    data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Cta.Cte.USD"] <- monto
    data$data$Variacion[data$data$Agente=="BC"&data$data$Nombre=="Cta.Cte.USD"] <- monto
    data <- ajustar_encaje(data,moneda = "d")
    data$data$Variacion[data$data$Agente=="T"&data$data$Nombre=="Dep.Tesoro.BCRA"] <- -1*m1
    data$data$Variacion[data$data$Agente=="BC"&data$data$Nombre=="Dep.Tesoro"] <- -1*m1
    data$data$Variacion[data$data$Agente=="T"&data$data$Nombre=="Dep.Tesoro.Vista"] <- -1*m2
    data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.Vista"] <- -1*m2
    data$data$Variacion[data$data$Agente=="BC"&data$data$Nombre=="Cta.Cte."] <- -1*m2
    data <- ajustar_encaje(data)
    
    data$data$Variacion[data$data$Nombre=="Base Monetaria"] <- sum(data$data$Variacion[data$data$Agente=="BC"&
                                                                                         data$data$Nombre %in% c("Circulante","Cta.Cte.")])
    data$data$Variacion[data$data$Nombre=="M2"] <- data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data$data$Variacion[data$data$Nombre=="M3"] <- data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Plazo.Fijo"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data$data$Variacion[data$data$Nombre=="M3.Privado"] <- data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Plazo.Fijo"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data <- ajustar_resultado(data)
    data <- ajustar_usd(data)
    
    data$data$VarAcum <- data$data$VarAcum + data$data$Variacion
    data$data$ValorFinal <- data$data$ValorInicial + data$data$VarAcum
    
    data$data$Color <- ifelse(data$data$Variacion==0,"white",
                              ifelse(data$data$Variacion>0,"green","red"))
    data$data$Color2 <- ifelse(data$data$VarAcum==0,"white",
                               ifelse(data$data$VarAcum>0,"green","red"))
    
    data$text <- ""
    
  }
  data
}

dep_p <- function(monto) {
  
  if(monto<=0) {
    
    data$text <- "Ingrese un monto positivo, por favor"
    
  } else if(monto>(data$data$ValorFinal[data$data$Agente=="H"&data$data$Nombre=="Circulante"])) {
    
    data$text <- "El SPnF no tiene suficientes fondos para esta operaci\u00F3n"
    
  } else {
    
    data$data$Variacion <- 0
    
    data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"] <- -1*monto
    data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Dep.Vista"] <- monto
    data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Circulante"] <- monto
    data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.Vista"] <- monto
    data <- ajustar_encaje(data)
    
    data$data$Variacion[data$data$Nombre=="Base Monetaria"] <- sum(data$data$Variacion[data$data$Agente=="BC"&
                                                                                         data$data$Nombre %in% c("Circulante","Cta.Cte.")])
    data$data$Variacion[data$data$Nombre=="M2"] <- data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data$data$Variacion[data$data$Nombre=="M3"] <- data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Plazo.Fijo"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data$data$Variacion[data$data$Nombre=="M3.Privado"] <- data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Plazo.Fijo"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data <- ajustar_resultado(data)
    data <- ajustar_usd(data)
    
    data$data$VarAcum <- data$data$VarAcum + data$data$Variacion
    data$data$ValorFinal <- data$data$ValorInicial + data$data$VarAcum
    
    data$data$Color <- ifelse(data$data$Variacion==0,"white",
                              ifelse(data$data$Variacion>0,"green","red"))
    data$data$Color2 <- ifelse(data$data$VarAcum==0,"white",
                               ifelse(data$data$VarAcum>0,"green","red"))
    
    data$text <- ""
    
  }
  data
}

dep_USD <- function(monto) {
  
  if(monto<=0) {
    
    data$text <- "Ingrese un monto positivo, por favor"
    
  } else if(monto>(data$data$ValorFinal[data$data$Agente=="H"&data$data$Nombre=="USD"])) {
    
    data$text <- "El SPnF no tiene suficientes fondos para esta operaci\u00F3n"
    
  } else {
    
    data$data$Variacion <- 0
    
    data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="USD"] <- -1*monto
    data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Dep.USD"] <- monto
    data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="USD"] <- monto
    data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.USD"] <- monto
    data <- ajustar_encaje(data,moneda = "d")
    
    data$data$Variacion[data$data$Nombre=="Base Monetaria"] <- sum(data$data$Variacion[data$data$Agente=="BC"&
                                                                                         data$data$Nombre %in% c("Circulante","Cta.Cte.")])
    data$data$Variacion[data$data$Nombre=="M2"] <- data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data$data$Variacion[data$data$Nombre=="M3"] <- data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Plazo.Fijo"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data$data$Variacion[data$data$Nombre=="M3.Privado"] <- data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Plazo.Fijo"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data <- ajustar_resultado(data)
    data <- ajustar_usd(data)
    
    data$data$VarAcum <- data$data$VarAcum + data$data$Variacion
    data$data$ValorFinal <- data$data$ValorInicial + data$data$VarAcum
    
    data$data$Color <- ifelse(data$data$Variacion==0,"white",
                              ifelse(data$data$Variacion>0,"green","red"))
    data$data$Color2 <- ifelse(data$data$VarAcum==0,"white",
                               ifelse(data$data$VarAcum>0,"green","red"))
    
    data$text <- ""
    
  }
  data
}

extraer_p <- function(monto) {
  
  if(monto<=0) {
    
    data$text <- "Ingrese un monto positivo, por favor"
    
  } else if(monto>(data$data$ValorFinal[data$data$Agente=="H"&data$data$Nombre=="Dep.Vista"])) {
    
    data$text <- "El SPnF no tiene suficientes fondos para esta operaci\u00F3n"
    
  } else if(monto>(data$data$ValorFinal[data$data$Agente=="SF"&data$data$Nombre=="Circulante"] +
                   data$data$ValorFinal[data$data$Agente=="SF"&data$data$Nombre=="Cta.Cte."])) {
    
    data$text <- "El Sector Financiero no tiene suficiente liquidez para esta operaci\u00F3n"
    
  } else {
    
    data$data$Variacion <- 0
    
    data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"] <- monto
    data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Dep.Vista"] <- -1*monto
    data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Circulante"] <- -1*monto
    data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.Vista"] <- -1*monto
    data <- ajustar_encaje(data)
    
    if((data$data$ValorFinal[data$data$Agente=="SF"&data$data$Nombre=="Circulante"] + 
       data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Circulante"])<0) {
      m1 <- abs(data$data$ValorFinal[data$data$Agente=="SF"&data$data$Nombre=="Circulante"] + 
        data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Circulante"])
      m2 <- min(m1,(data$data$ValorFinal[data$data$Agente=="SF"&data$data$Nombre=="Cta.Cte."] + 
                      data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Cta.Cte."]))
      data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Circulante"] <- 
        data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Circulante"] + m2
      data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Cta.Cte."] <- 
        data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Cta.Cte."] - m2
      data$data$Variacion[data$data$Agente=="BC"&data$data$Nombre=="Circulante"] <- 
        data$data$Variacion[data$data$Agente=="BC"&data$data$Nombre=="Circulante"] + m2
      data$data$Variacion[data$data$Agente=="BC"&data$data$Nombre=="Cta.Cte."] <- 
        data$data$Variacion[data$data$Agente=="BC"&data$data$Nombre=="Cta.Cte."] - m2
    }
    
    if((data$data$ValorFinal[data$data$Agente=="SF"&data$data$Nombre=="EM_Cta.Cte."] + 
        data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="EM_Cta.Cte."])<0) {
      m1 <- abs(data$data$ValorFinal[data$data$Agente=="SF"&data$data$Nombre=="EM_Cta.Cte."] + 
                  data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="EM_Cta.Cte."])
      data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="EM_Cta.Cte."] <- 
        data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="EM_Cta.Cte."] + m1
      data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Cta.Cte."] <- 
        data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Cta.Cte."] - m1
    }
    
    data$data$Variacion[data$data$Nombre=="Base Monetaria"] <- sum(data$data$Variacion[data$data$Agente=="BC"&
                                                                                         data$data$Nombre %in% c("Circulante","Cta.Cte.")])
    data$data$Variacion[data$data$Nombre=="M2"] <- data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data$data$Variacion[data$data$Nombre=="M3"] <- data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Plazo.Fijo"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data$data$Variacion[data$data$Nombre=="M3.Privado"] <- data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Plazo.Fijo"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data <- ajustar_resultado(data)
    data <- ajustar_usd(data)
    
    data$data$VarAcum <- data$data$VarAcum + data$data$Variacion
    data$data$ValorFinal <- data$data$ValorInicial + data$data$VarAcum
    
    data$data$Color <- ifelse(data$data$Variacion==0,"white",
                              ifelse(data$data$Variacion>0,"green","red"))
    data$data$Color2 <- ifelse(data$data$VarAcum==0,"white",
                               ifelse(data$data$VarAcum>0,"green","red"))
    
    data$text <- ""
    
  }
  data
}

extraer_USD <- function(monto) {
  
  if(monto<=0) {
    
    data$text <- "Ingrese un monto positivo, por favor"
    
  } else if(monto>(data$data$ValorFinal[data$data$Agente=="H"&data$data$Nombre=="Dep.USD"])) {
    
    data$text <- "El SPnF no tiene suficientes fondos para esta operaci\u00F3n"
    
  } else if(monto>(data$data$ValorFinal[data$data$Agente=="SF"&data$data$Nombre=="USD"] +
                   data$data$ValorFinal[data$data$Agente=="SF"&data$data$Nombre=="Cta.Cte.USD"] + 
                   data$data$ValorFinal[data$data$Agente=="SF"&data$data$Nombre=="EM_Cta.Cte.USD"])) {
    
    data$text <- "El Sector Financiero no tiene suficiente liquidez para esta operaci\u00F3n"
    
  } else {
    
    data$data$Variacion <- 0
    
    data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="USD"] <- monto
    data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Dep.USD"] <- -1*monto
    data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="USD"] <- -1*monto
    data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.USD"] <- -1*monto
    data <- ajustar_encaje(data,moneda = "d")
    
    if((data$data$ValorFinal[data$data$Agente=="SF"&data$data$Nombre=="USD"] + 
        data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="USD"])<0) {
      m1 <- abs(data$data$ValorFinal[data$data$Agente=="SF"&data$data$Nombre=="USD"] + 
                  data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="USD"])
      m2 <- min(m1,(data$data$ValorFinal[data$data$Agente=="SF"&data$data$Nombre=="Cta.Cte.USD"] + 
                      data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Cta.Cte.USD"]))
      data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="USD"] <- 
        data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="USD"] + m2
      data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Cta.Cte.USD"] <- 
        data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Cta.Cte.USD"] - m2
      data$data$Variacion[data$data$Agente=="BC"&data$data$Nombre=="Reservas"] <- 
        data$data$Variacion[data$data$Agente=="BC"&data$data$Nombre=="Reservas"] - m2
      data$data$Variacion[data$data$Agente=="BC"&data$data$Nombre=="Cta.Cte.USD"] <- 
        data$data$Variacion[data$data$Agente=="BC"&data$data$Nombre=="Cta.Cte.USD"] - m2
    }
    
    if((data$data$ValorFinal[data$data$Agente=="SF"&data$data$Nombre=="EM_Cta.Cte.USD"] + 
        data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="EM_Cta.Cte.USD"])<0) {
      m1 <- abs(data$data$ValorFinal[data$data$Agente=="SF"&data$data$Nombre=="EM_Cta.Cte.USD"] + 
               data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="EM_Cta.Cte.USD"])
      data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="EM_Cta.Cte.USD"] <- 
        data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="EM_Cta.Cte.USD"] + m1
      data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Cta.Cte.USD"] <- 
        data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Cta.Cte.USD"] - m1
    }
    
    data$data$Variacion[data$data$Nombre=="Base Monetaria"] <- sum(data$data$Variacion[data$data$Agente=="BC"&
                                                                                         data$data$Nombre %in% c("Circulante","Cta.Cte.")])
    data$data$Variacion[data$data$Nombre=="M2"] <- data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data$data$Variacion[data$data$Nombre=="M3"] <- data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Plazo.Fijo"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data$data$Variacion[data$data$Nombre=="M3.Privado"] <- data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Plazo.Fijo"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data <- ajustar_resultado(data)
    data <- ajustar_usd(data)
    
    data$data$VarAcum <- data$data$VarAcum + data$data$Variacion
    data$data$ValorFinal <- data$data$ValorInicial + data$data$VarAcum
    
    data$data$Color <- ifelse(data$data$Variacion==0,"white",
                              ifelse(data$data$Variacion>0,"green","red"))
    data$data$Color2 <- ifelse(data$data$VarAcum==0,"white",
                               ifelse(data$data$VarAcum>0,"green","red"))
    
    data$text <- ""
    
  }
  data
}

PF_hacer <- function(monto) {
  
  if(monto<=0) {
    
    data$text <- "Ingrese un monto positivo, por favor"
    
  } else if(monto>(data$data$ValorFinal[data$data$Agente=="H"&data$data$Nombre=="Dep.Vista"])) {
    
    data$text <- "El SPnF no tiene suficientes depósitos a la Vista para esta operaci\u00F3n"
    
  } else {
    
    data$data$Variacion <- 0
    
    data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Plazo.Fijo"] <- monto
    data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Dep.Vista"] <- -1*monto
    data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.Vista"] <- -1*monto
    data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Plazo.Fijo"] <- monto
    data <- ajustar_encaje(data)
    
    data$data$Variacion[data$data$Nombre=="Base Monetaria"] <- sum(data$data$Variacion[data$data$Agente=="BC"&
                                                                                         data$data$Nombre %in% c("Circulante","Cta.Cte.")])
    data$data$Variacion[data$data$Nombre=="M2"] <- data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data$data$Variacion[data$data$Nombre=="M3"] <- data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Plazo.Fijo"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data$data$Variacion[data$data$Nombre=="M3.Privado"] <- data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Plazo.Fijo"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data <- ajustar_resultado(data)
    data <- ajustar_usd(data)
    
    data$data$VarAcum <- data$data$VarAcum + data$data$Variacion
    data$data$ValorFinal <- data$data$ValorInicial + data$data$VarAcum
    
    data$data$Color <- ifelse(data$data$Variacion==0,"white",
                              ifelse(data$data$Variacion>0,"green","red"))
    data$data$Color2 <- ifelse(data$data$VarAcum==0,"white",
                               ifelse(data$data$VarAcum>0,"green","red"))
    
    if(data$data$ValorFinal[data$data$Agente=="SF"&data$data$Nombre=="Circulante"]<0) {
      data$text <- "El Sector Financiero tiene el Circulante en negativo. Consiga liquidez,
      por favor."
    } else {data$text <- ""}
    
  }
  data
}

PF_cancelar <- function(monto) {
  
  if(monto<=0) {
    
    data$text <- "Ingrese un monto positivo, por favor"
    
  } else if(monto>(data$data$ValorFinal[data$data$Agente=="H"&data$data$Nombre=="Plazo.Fijo"])) {
    
    data$text <- "No hay suficientes Plazos Fijos"
    
  } else {
    
    data$data$Variacion <- 0
    
    data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Plazo.Fijo"] <- -1*monto
    data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Dep.Vista"] <- monto
    data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.Vista"] <- monto
    data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Plazo.Fijo"] <- -1*monto
    data <- ajustar_encaje(data)
    
    data$data$Variacion[data$data$Nombre=="Base Monetaria"] <- sum(data$data$Variacion[data$data$Agente=="BC"&
                                                                                         data$data$Nombre %in% c("Circulante","Cta.Cte.")])
    data$data$Variacion[data$data$Nombre=="M2"] <- data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data$data$Variacion[data$data$Nombre=="M3"] <- data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Plazo.Fijo"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data$data$Variacion[data$data$Nombre=="M3.Privado"] <- data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Plazo.Fijo"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data <- ajustar_resultado(data)
    data <- ajustar_usd(data)
    
    data$data$VarAcum <- data$data$VarAcum + data$data$Variacion
    data$data$ValorFinal <- data$data$ValorInicial + data$data$VarAcum
    
    data$data$Color <- ifelse(data$data$Variacion==0,"white",
                              ifelse(data$data$Variacion>0,"green","red"))
    data$data$Color2 <- ifelse(data$data$VarAcum==0,"white",
                               ifelse(data$data$VarAcum>0,"green","red"))
    
    if(data$data$ValorFinal[data$data$Agente=="SF"&data$data$Nombre=="Circulante"]<0) {
      data$text <- "El Sector Financiero tiene el Circulante en negativo. Consiga liquidez,
      por favor."
    } else {data$text <- ""}
    
  }
  data
}

lebac_sp_sus <- function(monto) {
  
  if(monto<=0) {
    
    data$text <- "Ingrese un monto positivo, por favor"
    
  } else if(monto> (data$data$ValorFinal[data$data$Agente=="H"&data$data$Nombre=="Dep.Vista"])) {
    
    data$text <- "El SPnF no cuenta con suficientes dep\u00F3sitos a la vista para esta operaci\u00F3n."
    
  } else {
    
    data$data$Variacion <- 0
    
    data$data$Variacion[data$data$Agente=="BC"&data$data$Nombre=="LEBAC"] <- monto
    data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="LEBAC"] <- monto
    data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Dep.Vista"] <- -1*monto
    data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.Vista"] <- -1*monto
    data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Cta.Cte."] <- -1*monto
    data$data$Variacion[data$data$Agente=="BC"&data$data$Nombre=="Cta.Cte."] <- -1*monto
    data <- ajustar_encaje(data)
    
    data$data$Variacion[data$data$Nombre=="Base Monetaria"] <- sum(data$data$Variacion[data$data$Agente=="BC"&
                                                                                         data$data$Nombre %in% c("Circulante","Cta.Cte.")])
    data$data$Variacion[data$data$Nombre=="M2"] <- data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data$data$Variacion[data$data$Nombre=="M3"] <- data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Plazo.Fijo"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data$data$Variacion[data$data$Nombre=="M3.Privado"] <- data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Plazo.Fijo"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data <- ajustar_resultado(data)
    data <- ajustar_usd(data)
    
    data$data$VarAcum <- data$data$VarAcum + data$data$Variacion
    data$data$ValorFinal <- data$data$ValorInicial + data$data$VarAcum
    
    data$data$Color <- ifelse(data$data$Variacion==0,"white",
                              ifelse(data$data$Variacion>0,"green","red"))
    data$data$Color2 <- ifelse(data$data$VarAcum==0,"white",
                               ifelse(data$data$VarAcum>0,"green","red"))
    
    data$text <- ""
    
  }
  data
}

lebac_sp_can <- function(monto) {
  
  if(monto<=0) {
    
    data$text <- "Ingrese un monto positivo, por favor"
    
  } else if(monto> (data$data$ValorFinal[data$data$Agente=="H"&data$data$Nombre=="LEBAC"])) {
    
    data$text <- "El SPnF no cuenta con suficientes LEBAC para esta operaci\u00F3n."
    
  } else {
    
    data$data$Variacion <- 0
    
    data$data$Variacion[data$data$Agente=="BC"&data$data$Nombre=="LEBAC"] <- -1*monto
    data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="LEBAC"] <- -1*monto
    data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Dep.Vista"] <- monto
    data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.Vista"] <- monto
    data$data$Variacion[data$data$Agente=="BC"&data$data$Nombre=="Cta.Cte."] <- monto
    data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Cta.Cte."] <- monto
    data <- ajustar_encaje(data)
    
    data$data$Variacion[data$data$Nombre=="Base Monetaria"] <- sum(data$data$Variacion[data$data$Agente=="BC"&
                                                                                         data$data$Nombre %in% c("Circulante","Cta.Cte.")])
    data$data$Variacion[data$data$Nombre=="M2"] <- data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data$data$Variacion[data$data$Nombre=="M3"] <- data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Plazo.Fijo"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data$data$Variacion[data$data$Nombre=="M3.Privado"] <- data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Plazo.Fijo"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data <- ajustar_resultado(data)
    data <- ajustar_usd(data)
    
    data$data$VarAcum <- data$data$VarAcum + data$data$Variacion
    data$data$ValorFinal <- data$data$ValorInicial + data$data$VarAcum
    
    data$data$Color <- ifelse(data$data$Variacion==0,"white",
                              ifelse(data$data$Variacion>0,"green","red"))
    data$data$Color2 <- ifelse(data$data$VarAcum==0,"white",
                               ifelse(data$data$VarAcum>0,"green","red"))
    
    data$text <- ""
    
  }
  data
}

lebac_rm_sus <- function(monto) {
  
  if(monto<=0) {
    
    data$text <- "Ingrese un monto positivo, por favor"
    
  } else {
    
    data$data$Variacion <- 0
    
    data$data$Variacion[data$data$Agente=="BC"&data$data$Nombre=="LEBAC"] <- monto
    data$data$Variacion[data$data$Agente=="BC"&data$data$Nombre=="Reservas"] <- monto
    data$data$Variacion[data$data$Agente=="RM"&data$data$Nombre=="LEBAC"] <- monto
    data$data$Variacion[data$data$Agente=="RM"&data$data$Nombre=="Divisas"] <- monto
    
    data$data$Variacion[data$data$Nombre=="Base Monetaria"] <- sum(data$data$Variacion[data$data$Agente=="BC"&
                                                                                         data$data$Nombre %in% c("Circulante","Cta.Cte.")])
    data$data$Variacion[data$data$Nombre=="M2"] <- data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data$data$Variacion[data$data$Nombre=="M3"] <- data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Plazo.Fijo"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data$data$Variacion[data$data$Nombre=="M3.Privado"] <- data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Plazo.Fijo"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data <- ajustar_resultado(data)
    data <- ajustar_usd(data)
    
    data$data$VarAcum <- data$data$VarAcum + data$data$Variacion
    data$data$ValorFinal <- data$data$ValorInicial + data$data$VarAcum
    
    data$data$Color <- ifelse(data$data$Variacion==0,"white",
                              ifelse(data$data$Variacion>0,"green","red"))
    data$data$Color2 <- ifelse(data$data$VarAcum==0,"white",
                               ifelse(data$data$VarAcum>0,"green","red"))
    
    data$text <- ""
    
  }
  data
}

lebac_rm_can <- function(monto) {
  
  if(monto<=0) {
    
    data$text <- "Ingrese un monto positivo, por favor"
    
  } else if(monto>data$data$ValorFinal[data$data$Agente=="RM"&data$data$Nombre=="LEBAC"]) {
    
    data$text <- "El Resto del Mundo no dispone de tantas LEBAC"
    
  } else {
    
    data$data$Variacion <- 0
    
    data$data$Variacion[data$data$Agente=="BC"&data$data$Nombre=="LEBAC"] <- -1*monto
    data$data$Variacion[data$data$Agente=="BC"&data$data$Nombre=="Reservas"] <- -1*monto
    data$data$Variacion[data$data$Agente=="RM"&data$data$Nombre=="LEBAC"] <- -1*monto
    data$data$Variacion[data$data$Agente=="RM"&data$data$Nombre=="Divisas"] <- -1*monto
    
    data$data$Variacion[data$data$Nombre=="Base Monetaria"] <- sum(data$data$Variacion[data$data$Agente=="BC"&
                                                                                         data$data$Nombre %in% c("Circulante","Cta.Cte.")])
    data$data$Variacion[data$data$Nombre=="M2"] <- data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data$data$Variacion[data$data$Nombre=="M3"] <- data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Plazo.Fijo"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data$data$Variacion[data$data$Nombre=="M3.Privado"] <- data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Plazo.Fijo"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data <- ajustar_resultado(data)
    data <- ajustar_usd(data)
    
    data$data$VarAcum <- data$data$VarAcum + data$data$Variacion
    data$data$ValorFinal <- data$data$ValorInicial + data$data$VarAcum
    
    data$data$Color <- ifelse(data$data$Variacion==0,"white",
                              ifelse(data$data$Variacion>0,"green","red"))
    data$data$Color2 <- ifelse(data$data$VarAcum==0,"white",
                               ifelse(data$data$VarAcum>0,"green","red"))
    
    data$text <- ""
    
  }
  data
}

tp_em_p_sp <- function(monto) {
  
  if(monto<=0) {
    
    data$text <- "Ingrese un monto positivo, por favor"
    
  } else if(monto>data$data$ValorFinal[data$data$Agente=="H"&data$data$Nombre=="Dep.Vista"]) {
    
    data$text <- "El SPnF no dispone de suficientes dep\u00F3sitos a la vista"
    
  } else {
    
    data$data$Variacion <- 0
    
    data$data$Variacion[data$data$Agente=="T"&data$data$Nombre=="Tit.Pub.Pesos"] <- monto
    data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Tit.Pub.Pesos"] <- monto
    data$data$Variacion[data$data$Agente=="T"&data$data$Nombre=="Dep.Tesoro.Vista"] <- monto
    data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Dep.Vista"] <- -1*monto
    
    data$data$Variacion[data$data$Nombre=="Base Monetaria"] <- sum(data$data$Variacion[data$data$Agente=="BC"&
                                                                                         data$data$Nombre %in% c("Circulante","Cta.Cte.")])
    data$data$Variacion[data$data$Nombre=="M2"] <- data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data$data$Variacion[data$data$Nombre=="M3"] <- data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Plazo.Fijo"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data$data$Variacion[data$data$Nombre=="M3.Privado"] <- data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Plazo.Fijo"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data <- ajustar_resultado(data)
    data <- ajustar_usd(data)
    
    data$data$VarAcum <- data$data$VarAcum + data$data$Variacion
    data$data$ValorFinal <- data$data$ValorInicial + data$data$VarAcum
    
    data$data$Color <- ifelse(data$data$Variacion==0,"white",
                              ifelse(data$data$Variacion>0,"green","red"))
    data$data$Color2 <- ifelse(data$data$VarAcum==0,"white",
                               ifelse(data$data$VarAcum>0,"green","red"))
    
    data$text <- ""
    
  }
  data
}

tp_em_p_sf <- function(monto) {
  
  if(monto<=0) {
    
    data$text <- "Ingrese un monto positivo, por favor"
    
  } else if(monto>(data$data$ValorFinal[data$data$Agente=="SF"&data$data$Nombre=="Circulante"] +
                   data$data$ValorFinal[data$data$Agente=="SF"&data$data$Nombre=="Cta.Cte."])/data$encaje$VistaPesos) {
    
    data$text <- "El SF no dispone de suficientes liquidez"
    
  } else {
    
    data$data$Variacion <- 0
    
    m1 <- min(monto,data$data$ValorFinal[data$data$Agente=="SF"&data$data$Nombre=="Cta.Cte."]/data$encaje$VistaPesos)
    m2 <- monto-m1
    
    data$data$Variacion[data$data$Agente=="T"&data$data$Nombre=="Tit.Pub.Pesos"] <- monto
    data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Tit.Pub.Pesos"] <- monto
    data$data$Variacion[data$data$Agente=="T"&data$data$Nombre=="Dep.Tesoro.Vista"] <- monto
    data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.Vista"] <- monto
    data <- ajustar_encaje(data)

    data$data$Variacion[data$data$Nombre=="Base Monetaria"] <- sum(data$data$Variacion[data$data$Agente=="BC"&
                                                                                         data$data$Nombre %in% c("Circulante","Cta.Cte.")])
    data$data$Variacion[data$data$Nombre=="M2"] <- data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data$data$Variacion[data$data$Nombre=="M3"] <- data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Plazo.Fijo"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data$data$Variacion[data$data$Nombre=="M3.Privado"] <- data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Plazo.Fijo"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data <- ajustar_resultado(data)
    data <- ajustar_usd(data)
    
    data$data$VarAcum <- data$data$VarAcum + data$data$Variacion
    data$data$ValorFinal <- data$data$ValorInicial + data$data$VarAcum
    
    data$data$Color <- ifelse(data$data$Variacion==0,"white",
                              ifelse(data$data$Variacion>0,"green","red"))
    data$data$Color2 <- ifelse(data$data$VarAcum==0,"white",
                               ifelse(data$data$VarAcum>0,"green","red"))
    
    if(data$data$ValorFinal[data$data$Agente=="SF"&data$data$Nombre=="Circulante"]<0) {
      data$text <- "El Sector Financiero tiene el Circulante en negativo. Consiga liquidez,
      por favor."
    } else {data$text <- ""}
    
  }
  data
}

tp_em_p_rm <- function(monto) {
  
  if(monto<=0) {
    
    data$text <- "Ingrese un monto positivo, por favor"
    
  } else {
    
    data$data$Variacion <- 0
    
    data$data$Variacion[data$data$Agente=="T"&data$data$Nombre=="Tit.Pub.Pesos"] <- monto
    data$data$Variacion[data$data$Agente=="RM"&data$data$Nombre=="Tit.Pub.Pesos"] <- monto
    data$data$Variacion[data$data$Agente=="T"&data$data$Nombre=="Dep.Tesoro.BCRA"] <- monto
    data$data$Variacion[data$data$Agente=="BC"&data$data$Nombre=="Dep.Tesoro"] <- monto
    data$data$Variacion[data$data$Agente=="BC"&data$data$Nombre=="Reservas"] <- monto
    data$data$Variacion[data$data$Agente=="RM"&data$data$Nombre=="Divisas"] <- monto
    
    data$data$Variacion[data$data$Nombre=="Base Monetaria"] <- sum(data$data$Variacion[data$data$Agente=="BC"&
                                                                                         data$data$Nombre %in% c("Circulante","Cta.Cte.")])
    data$data$Variacion[data$data$Nombre=="M2"] <- data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data$data$Variacion[data$data$Nombre=="M3"] <- data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Plazo.Fijo"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data$data$Variacion[data$data$Nombre=="M3.Privado"] <- data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Plazo.Fijo"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data <- ajustar_resultado(data)
    data <- ajustar_usd(data)
    
    data$data$VarAcum <- data$data$VarAcum + data$data$Variacion
    data$data$ValorFinal <- data$data$ValorInicial + data$data$VarAcum
    
    data$data$Color <- ifelse(data$data$Variacion==0,"white",
                              ifelse(data$data$Variacion>0,"green","red"))
    data$data$Color2 <- ifelse(data$data$VarAcum==0,"white",
                               ifelse(data$data$VarAcum>0,"green","red"))
    
    if(data$data$ValorFinal[data$data$Agente=="SF"&data$data$Nombre=="Circulante"]<0) {
      data$text <- "El Sector Financiero tiene el Circulante en negativo. Consiga liquidez,
      por favor."
    } else {data$text <- ""}
    
  }
  data
}

tp_em_d_sp <- function(monto) {
  
  if(monto<=0) {
    
    data$text <- "Ingrese un monto positivo, por favor"
    
  } else if(monto>data$data$ValorFinal[data$data$Agente=="H"&data$data$Nombre=="Dep.USD"]) {
    
    data$text <- "El SPnF no dispone de suficientes dep\u00F3sitos en d\u00F3lares"
    
  } else {
    
    data$data$Variacion <- 0
    
    data$data$Variacion[data$data$Agente=="T"&data$data$Nombre=="Tit.Pub.USD"] <- monto
    data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Tit.Pub.USD"] <- monto
    data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Dep.USD"] <- -1*monto
    data$data$Variacion[data$data$Agente=="BC"&data$data$Nombre=="Cta.Cte.USD"] <- -1*monto
    data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Cta.Cte.USD"] <- -1*monto
    data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.USD"] <- -1*monto
    data$data$Variacion[data$data$Agente=="T"&data$data$Nombre=="Dep.Tesoro.BCRA"] <- monto
    data$data$Variacion[data$data$Agente=="BC"&data$data$Nombre=="Dep.Tesoro"] <- monto
    data <- ajustar_encaje(data,moneda = "d")
    
    data$data$Variacion[data$data$Nombre=="Base Monetaria"] <- sum(data$data$Variacion[data$data$Agente=="BC"&
                                                                                         data$data$Nombre %in% c("Circulante","Cta.Cte.")])
    data$data$Variacion[data$data$Nombre=="M2"] <- data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data$data$Variacion[data$data$Nombre=="M3"] <- data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Plazo.Fijo"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data$data$Variacion[data$data$Nombre=="M3.Privado"] <- data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Plazo.Fijo"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data <- ajustar_resultado(data)
    data <- ajustar_usd(data)
    
    data$data$VarAcum <- data$data$VarAcum + data$data$Variacion
    data$data$ValorFinal <- data$data$ValorInicial + data$data$VarAcum
    
    data$data$Color <- ifelse(data$data$Variacion==0,"white",
                              ifelse(data$data$Variacion>0,"green","red"))
    data$data$Color2 <- ifelse(data$data$VarAcum==0,"white",
                               ifelse(data$data$VarAcum>0,"green","red"))
    
    data$text <- ""
    
  }
  data
}

tp_em_d_rm <- function(monto) {
  
  if(monto<=0) {
    
    data$text <- "Ingrese un monto positivo, por favor"
    
  } else {
    
    data$data$Variacion <- 0
    
    data$data$Variacion[data$data$Agente=="T"&data$data$Nombre=="Tit.Pub.USD"] <- monto
    data$data$Variacion[data$data$Agente=="RM"&data$data$Nombre=="Tit.Pub.USD"] <- monto
    data$data$Variacion[data$data$Agente=="T"&data$data$Nombre=="Dep.Tesoro.BCRA"] <- monto
    data$data$Variacion[data$data$Agente=="BC"&data$data$Nombre=="Dep.Tesoro"] <- monto
    data$data$Variacion[data$data$Agente=="BC"&data$data$Nombre=="Reservas"] <- monto
    data$data$Variacion[data$data$Agente=="RM"&data$data$Nombre=="Divisas"] <- monto
    
    data$data$Variacion[data$data$Nombre=="Base Monetaria"] <- sum(data$data$Variacion[data$data$Agente=="BC"&
                                                                                         data$data$Nombre %in% c("Circulante","Cta.Cte.")])
    data$data$Variacion[data$data$Nombre=="M2"] <- data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data$data$Variacion[data$data$Nombre=="M3"] <- data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Plazo.Fijo"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data$data$Variacion[data$data$Nombre=="M3.Privado"] <- data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Plazo.Fijo"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data <- ajustar_resultado(data)
    data <- ajustar_usd(data)
    
    data$data$VarAcum <- data$data$VarAcum + data$data$Variacion
    data$data$ValorFinal <- data$data$ValorInicial + data$data$VarAcum
    
    data$data$Color <- ifelse(data$data$Variacion==0,"white",
                              ifelse(data$data$Variacion>0,"green","red"))
    data$data$Color2 <- ifelse(data$data$VarAcum==0,"white",
                               ifelse(data$data$VarAcum>0,"green","red"))
    
    if(data$data$ValorFinal[data$data$Agente=="SF"&data$data$Nombre=="Circulante"]<0) {
      data$text <- "El Sector Financiero tiene el Circulante en negativo. Consiga liquidez,
      por favor."
    } else {data$text <- ""}
    
  }
  data
}

tp_c_p_sp <- function(monto) {
  
  if(monto<=0) {
    
    data$text <- "Ingrese un monto positivo, por favor"
    
  } else if(monto>data$data$ValorFinal[data$data$Agente=="H"&data$data$Nombre=="Tit.Pub.Pesos"]) {
    
    data$text <- "El SPnF no dispone de tantos t\u00EDtulos"
    
  } else if(monto>(data$data$ValorFinal[data$data$Agente=="T"&data$data$Nombre=="Dep.Tesoro.Vista"] +
                   data$data$ValorFinal[data$data$Agente=="T"&data$data$Nombre=="Dep.Tesoro.BCRA"])) {
    
    data$text <- "El Tesoro no dispone de suficientes fondos"
    
  } else {
    
    data$data$Variacion <- 0
    
    m1 <- min(monto,data$data$ValorFinal[data$data$Agente=="T"&data$data$Nombre=="Dep.Tesoro.Vista"])
    m2 <- monto-m1
    
    data$data$Variacion[data$data$Agente=="T"&data$data$Nombre=="Tit.Pub.Pesos"] <- -1*monto
    data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Tit.Pub.Pesos"] <- -1*monto
    data$data$Variacion[data$data$Agente=="T"&data$data$Nombre=="Dep.Tesoro.Vista"] <- -1*m1
    data$data$Variacion[data$data$Agente=="T"&data$data$Nombre=="Dep.Tesoro.BCRA"] <- -1*m2
    data$data$Variacion[data$data$Agente=="BC"&data$data$Nombre=="Dep.Tesoro"] <- -1*m2
    data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Dep.Vista"] <- monto
    data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.Vista"] <- m2
    data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Cta.Cte."] <- m2
    data$data$Variacion[data$data$Agente=="BC"&data$data$Nombre=="Cta.Cte."] <- m2
    data <- ajustar_encaje(data)
    
    data$data$Variacion[data$data$Nombre=="Base Monetaria"] <- sum(data$data$Variacion[data$data$Agente=="BC"&
                                                                                         data$data$Nombre %in% c("Circulante","Cta.Cte.")])
    data$data$Variacion[data$data$Nombre=="M2"] <- data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data$data$Variacion[data$data$Nombre=="M3"] <- data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Plazo.Fijo"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data$data$Variacion[data$data$Nombre=="M3.Privado"] <- data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Plazo.Fijo"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data <- ajustar_resultado(data)
    data <- ajustar_usd(data)
    
    data$data$VarAcum <- data$data$VarAcum + data$data$Variacion
    data$data$ValorFinal <- data$data$ValorInicial + data$data$VarAcum
    
    data$data$Color <- ifelse(data$data$Variacion==0,"white",
                              ifelse(data$data$Variacion>0,"green","red"))
    data$data$Color2 <- ifelse(data$data$VarAcum==0,"white",
                               ifelse(data$data$VarAcum>0,"green","red"))
    
    data$text <- ""
    
  }
  data
}

tp_c_p_sf <- function(monto) {
  
  if(monto<=0) {
    
    data$text <- "Ingrese un monto positivo, por favor"
    
  } else if(monto>data$data$ValorFinal[data$data$Agente=="SF"&data$data$Nombre=="Tit.Pub.Pesos"]) {
    
    data$text <- "El SF no dispone de tantos t\u00EDtulos"
    
  } else if(monto>(data$data$ValorFinal[data$data$Agente=="T"&data$data$Nombre=="Dep.Tesoro.Vista"] +
                   data$data$ValorFinal[data$data$Agente=="T"&data$data$Nombre=="Dep.Tesoro.BCRA"])) {
    
    data$text <- "El Tesoro no dispone de suficientes fondos"
    
  } else {
    
    data$data$Variacion <- 0
    
    m1 <- min(monto,data$data$ValorFinal[data$data$Agente=="T"&data$data$Nombre=="Dep.Tesoro.Vista"])
    m2 <- monto-m1
    
    data$data$Variacion[data$data$Agente=="T"&data$data$Nombre=="Tit.Pub.Pesos"] <- -1*monto
    data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Tit.Pub.Pesos"] <- -1*monto
    data$data$Variacion[data$data$Agente=="T"&data$data$Nombre=="Dep.Tesoro.Vista"] <- -1*m1
    data$data$Variacion[data$data$Agente=="T"&data$data$Nombre=="Dep.Tesoro.BCRA"] <- -1*m2
    data$data$Variacion[data$data$Agente=="BC"&data$data$Nombre=="Dep.Tesoro"] <- -1*m2
    data$data$Variacion[data$data$Agente=="BC"&data$data$Nombre=="Cta.Cte."] <- m2
    data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Cta.Cte."] <- m2
    data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.Vista"] <- -1*m1
    data <- ajustar_encaje(data)
    
    data$data$Variacion[data$data$Nombre=="Base Monetaria"] <- sum(data$data$Variacion[data$data$Agente=="BC"&
                                                                                         data$data$Nombre %in% c("Circulante","Cta.Cte.")])
    data$data$Variacion[data$data$Nombre=="M2"] <- data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data$data$Variacion[data$data$Nombre=="M3"] <- data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Plazo.Fijo"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data$data$Variacion[data$data$Nombre=="M3.Privado"] <- data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Plazo.Fijo"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data <- ajustar_resultado(data)
    data <- ajustar_usd(data)
    
    data$data$VarAcum <- data$data$VarAcum + data$data$Variacion
    data$data$ValorFinal <- data$data$ValorInicial + data$data$VarAcum
    
    data$data$Color <- ifelse(data$data$Variacion==0,"white",
                              ifelse(data$data$Variacion>0,"green","red"))
    data$data$Color2 <- ifelse(data$data$VarAcum==0,"white",
                               ifelse(data$data$VarAcum>0,"green","red"))
    
    if(data$data$ValorFinal[data$data$Agente=="SF"&data$data$Nombre=="Circulante"]<0) {
      data$text <- "El Sector Financiero tiene el Circulante en negativo. Consiga liquidez,
      por favor."
    } else {data$text <- ""}
    
  }
  data
}

tp_c_p_rm <- function(monto) {
  
  if(monto<=0) {
    
    data$text <- "Ingrese un monto positivo, por favor"
    
  } else if(monto>data$data$ValorFinal[data$data$Agente=="RM"&data$data$Nombre=="Tit.Pub.Pesos"]) {
    
    data$text <- "El RM no dispone de tantos t\u00EDtulos"
    
  } else if(monto>(data$data$ValorFinal[data$data$Agente=="T"&data$data$Nombre=="Dep.Tesoro.Vista"] +
                   data$data$ValorFinal[data$data$Agente=="T"&data$data$Nombre=="Dep.Tesoro.BCRA"])) {
    
    data$text <- "El Tesoro no dispone de suficientes fondos"
    
  }else {
    
    data$data$Variacion <- 0
    
    m1 <- min(monto,data$data$ValorFinal[data$data$Agente=="T"&data$data$Nombre=="Dep.Tesoro.Vista"])
    m2 <- monto-m1
    
    data$data$Variacion[data$data$Agente=="T"&data$data$Nombre=="Tit.Pub.Pesos"] <- -1*monto
    data$data$Variacion[data$data$Agente=="RM"&data$data$Nombre=="Tit.Pub.Pesos"] <- -1*monto
    data$data$Variacion[data$data$Agente=="T"&data$data$Nombre=="Dep.Tesoro.BCRA"] <- -1*m2
    data$data$Variacion[data$data$Agente=="T"&data$data$Nombre=="Dep.Tesoro.Vista"] <- -1*m1
    data$data$Variacion[data$data$Agente=="BC"&data$data$Nombre=="Dep.Tesoro"] <- -1*m2
    data$data$Variacion[data$data$Agente=="BC"&data$data$Nombre=="Reservas"] <- -1*monto
    data$data$Variacion[data$data$Agente=="RM"&data$data$Nombre=="Divisas"] <- -1*monto
    data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.Vista"] <- -1*m1
    data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Cta.Cte."] <- -1*m1
    data$data$Variacion[data$data$Agente=="BC"&data$data$Nombre=="Cta.Cte."] <- -1*m1
    data <- ajustar_encaje(data)
    
    data$data$Variacion[data$data$Nombre=="Base Monetaria"] <- sum(data$data$Variacion[data$data$Agente=="BC"&
                                                                                         data$data$Nombre %in% c("Circulante","Cta.Cte.")])
    data$data$Variacion[data$data$Nombre=="M2"] <- data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data$data$Variacion[data$data$Nombre=="M3"] <- data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Plazo.Fijo"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data$data$Variacion[data$data$Nombre=="M3.Privado"] <- data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Plazo.Fijo"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data <- ajustar_resultado(data)
    data <- ajustar_usd(data)
    
    data$data$VarAcum <- data$data$VarAcum + data$data$Variacion
    data$data$ValorFinal <- data$data$ValorInicial + data$data$VarAcum
    
    data$data$Color <- ifelse(data$data$Variacion==0,"white",
                              ifelse(data$data$Variacion>0,"green","red"))
    data$data$Color2 <- ifelse(data$data$VarAcum==0,"white",
                               ifelse(data$data$VarAcum>0,"green","red"))
    
    if(data$data$ValorFinal[data$data$Agente=="SF"&data$data$Nombre=="Circulante"]<0) {
      data$text <- "El Sector Financiero tiene el Circulante en negativo. Consiga liquidez,
      por favor."
    } else {data$text <- ""}
    
  }
  data
}

tp_c_d_sp <- function(monto) {
  
  if(monto<=0) {
    
    data$text <- "Ingrese un monto positivo, por favor"
    
  } else if(monto>data$data$ValorFinal[data$data$Agente=="H"&data$data$Nombre=="Tit.Pub.USD"]) {
    
    data$text <- "El SP no dispone de tantos t\u00EDtulos"
    
  } else if(monto>(data$data$ValorFinal[data$data$Agente=="T"&data$data$Nombre=="Dep.Tesoro.Vista"] +
                   data$data$ValorFinal[data$data$Agente=="T"&data$data$Nombre=="Dep.Tesoro.BCRA"])) {
    
    data$text <- "El Tesoro no dispone de suficientes fondos"
    
  } else {
    
    data$data$Variacion <- 0
    
    m1 <- min(monto,data$data$ValorFinal[data$data$Agente=="T"&data$data$Nombre=="Dep.Tesoro.Vista"])
    m2 <- monto-m1
    
    data$data$Variacion[data$data$Agente=="T"&data$data$Nombre=="Tit.Pub.USD"] <- -1*monto
    data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Tit.Pub.USD"] <- -1*monto
    data$data$Variacion[data$data$Agente=="T"&data$data$Nombre=="Dep.Tesoro.Vista"] <- -1*m1
    data$data$Variacion[data$data$Agente=="T"&data$data$Nombre=="Dep.Tesoro.BCRA"] <- -1*m2
    data$data$Variacion[data$data$Agente=="BC"&data$data$Nombre=="Dep.Tesoro"] <- -1*m2
    data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.Vista"] <- -1*m1
    data$data$Variacion[data$data$Agente=="BC"&data$data$Nombre=="Cta.Cte."] <- -1*m1
    data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Cta.Cte."] <- -1*m1
    data <- ajustar_encaje(data)
    data$data$Variacion[data$data$Agente=="BC"&data$data$Nombre=="Cta.Cte.USD"] <- monto
    data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Dep.USD"] <- monto
    data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.USD"] <- monto
    data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Cta.Cte.USD"] <- monto
    data <- ajustar_encaje(data)
    
    data$data$Variacion[data$data$Nombre=="Base Monetaria"] <- sum(data$data$Variacion[data$data$Agente=="BC"&
                                                                                         data$data$Nombre %in% c("Circulante","Cta.Cte.")])
    data$data$Variacion[data$data$Nombre=="M2"] <- data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data$data$Variacion[data$data$Nombre=="M3"] <- data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Plazo.Fijo"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data$data$Variacion[data$data$Nombre=="M3.Privado"] <- data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Plazo.Fijo"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data <- ajustar_resultado(data)
    data <- ajustar_usd(data)
    
    data$data$VarAcum <- data$data$VarAcum + data$data$Variacion
    data$data$ValorFinal <- data$data$ValorInicial + data$data$VarAcum
    
    data$data$Color <- ifelse(data$data$Variacion==0,"white",
                              ifelse(data$data$Variacion>0,"green","red"))
    data$data$Color2 <- ifelse(data$data$VarAcum==0,"white",
                               ifelse(data$data$VarAcum>0,"green","red"))
    
    data$text <- ""
    
  }
  data
}

tp_c_d_rm <- function(monto) {
  
  if(monto<=0) {
    
    data$text <- "Ingrese un monto positivo, por favor"
    
  } else if(monto>data$data$ValorFinal[data$data$Agente=="RM"&data$data$Nombre=="Tit.Pub.USD"]) {
    
    data$text <- "El RM no dispone de tantos t\u00EDtulos"
    
  } else if(monto>(data$data$ValorFinal[data$data$Agente=="T"&data$data$Nombre=="Dep.Tesoro.Vista"] +
                   data$data$ValorFinal[data$data$Agente=="T"&data$data$Nombre=="Dep.Tesoro.BCRA"])) {
    
    data$text <- "El Tesoro no dispone de suficientes fondos"
    
  } else {
    
    data$data$Variacion <- 0
    
    m1 <- min(monto,data$data$ValorFinal[data$data$Agente=="T"&data$data$Nombre=="Dep.Tesoro.Vista"])
    m2 <- monto-m1
    
    data$data$Variacion[data$data$Agente=="T"&data$data$Nombre=="Tit.Pub.USD"] <- -1*monto
    data$data$Variacion[data$data$Agente=="RM"&data$data$Nombre=="Tit.Pub.USD"] <- -1*monto
    data$data$Variacion[data$data$Agente=="T"&data$data$Nombre=="Dep.Tesoro.BCRA"] <- -1*m2
    data$data$Variacion[data$data$Agente=="T"&data$data$Nombre=="Dep.Tesoro.Vista"] <- -1*m1
    data$data$Variacion[data$data$Agente=="BC"&data$data$Nombre=="Dep.Tesoro"] <- -1*m2
    data$data$Variacion[data$data$Agente=="BC"&data$data$Nombre=="Reservas"] <- -1*monto
    data$data$Variacion[data$data$Agente=="RM"&data$data$Nombre=="Divisas"] <- -1*monto
    data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.Vista"] <- -1*m1
    data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Cta.Cte."] <- -1*m1
    data$data$Variacion[data$data$Agente=="BC"&data$data$Nombre=="Cta.Cte."] <- -1*m1
    data <- ajustar_encaje(data)
    
    data$data$Variacion[data$data$Nombre=="Base Monetaria"] <- sum(data$data$Variacion[data$data$Agente=="BC"&
                                                                                         data$data$Nombre %in% c("Circulante","Cta.Cte.")])
    data$data$Variacion[data$data$Nombre=="M2"] <- data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data$data$Variacion[data$data$Nombre=="M3"] <- data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Plazo.Fijo"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data$data$Variacion[data$data$Nombre=="M3.Privado"] <- data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Plazo.Fijo"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data <- ajustar_resultado(data)
    data <- ajustar_usd(data)
    
    data$data$VarAcum <- data$data$VarAcum + data$data$Variacion
    data$data$ValorFinal <- data$data$ValorInicial + data$data$VarAcum
    
    data$data$Color <- ifelse(data$data$Variacion==0,"white",
                              ifelse(data$data$Variacion>0,"green","red"))
    data$data$Color2 <- ifelse(data$data$VarAcum==0,"white",
                               ifelse(data$data$VarAcum>0,"green","red"))
    
    if(data$data$ValorFinal[data$data$Agente=="SF"&data$data$Nombre=="Circulante"]<0) {
      data$text <- "El Sector Financiero tiene el Circulante en negativo. Consiga liquidez,
      por favor."
    } else {data$text <- ""}
    
  }
  data
}

tp_o_p_bc_sf <- function(monto) {
  
  if(monto<=0) {
    
    data$text <- "Ingrese un monto positivo, por favor"
    
  } else if(monto>data$data$ValorFinal[data$data$Agente=="SF"&data$data$Nombre=="Tit.Pub.Pesos"]) {
    
    data$text <- "El Sector Financiero no dispone de tantos t\u00EDtulos"
    
  } else {
    
    data$data$Variacion <- 0
    
    data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Tit.Pub.Pesos"] <- -1*monto
    data$data$Variacion[data$data$Agente=="BC"&data$data$Nombre=="Tit.Pub.Pesos"] <- monto
    data$data$Variacion[data$data$Agente=="BC"&data$data$Nombre=="Cta.Cte."] <- monto
    data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Cta.Cte."] <- monto
    
    data$data$Variacion[data$data$Nombre=="Base Monetaria"] <- sum(data$data$Variacion[data$data$Agente=="BC"&
                                                                                         data$data$Nombre %in% c("Circulante","Cta.Cte.")])
    data$data$Variacion[data$data$Nombre=="M2"] <- data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data$data$Variacion[data$data$Nombre=="M3"] <- data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Plazo.Fijo"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data$data$Variacion[data$data$Nombre=="M3.Privado"] <- data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Plazo.Fijo"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data <- ajustar_resultado(data)
    data <- ajustar_usd(data)
    
    data$data$VarAcum <- data$data$VarAcum + data$data$Variacion
    data$data$ValorFinal <- data$data$ValorInicial + data$data$VarAcum
    
    data$data$Color <- ifelse(data$data$Variacion==0,"white",
                              ifelse(data$data$Variacion>0,"green","red"))
    data$data$Color2 <- ifelse(data$data$VarAcum==0,"white",
                               ifelse(data$data$VarAcum>0,"green","red"))
    
    if(data$data$ValorFinal[data$data$Agente=="SF"&data$data$Nombre=="Circulante"]<0) {
      data$text <- "El Sector Financiero tiene el Circulante en negativo. Consiga liquidez,
      por favor."
    } else {data$text <- ""}
    
  }
  data
}

tp_o_p_bc_sp <- function(monto) {
  
  if(monto<=0) {
    
    data$text <- "Ingrese un monto positivo, por favor"
    
  } else if(monto>data$data$ValorFinal[data$data$Agente=="H"&data$data$Nombre=="Tit.Pub.Pesos"]) {
    
    data$text <- "El SPnF no dispone de tantos t\u00EDtulos"
    
  } else {
    
    data$data$Variacion <- 0
    
    data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Tit.Pub.Pesos"] <- -1*monto
    data$data$Variacion[data$data$Agente=="BC"&data$data$Nombre=="Tit.Pub.Pesos"] <- monto
    data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Dep.Vista"] <- monto
    data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.Vista"] <- monto
    data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Cta.Cte."] <- monto
    data$data$Variacion[data$data$Agente=="BC"&data$data$Nombre=="Cta.Cte."] <- monto
    data <- ajustar_encaje(data)
    
    data$data$Variacion[data$data$Nombre=="Base Monetaria"] <- sum(data$data$Variacion[data$data$Agente=="BC"&
                                                                                         data$data$Nombre %in% c("Circulante","Cta.Cte.")])
    data$data$Variacion[data$data$Nombre=="M2"] <- data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data$data$Variacion[data$data$Nombre=="M3"] <- data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Plazo.Fijo"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data$data$Variacion[data$data$Nombre=="M3.Privado"] <- data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Plazo.Fijo"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data <- ajustar_resultado(data)
    data <- ajustar_usd(data)
    
    data$data$VarAcum <- data$data$VarAcum + data$data$Variacion
    data$data$ValorFinal <- data$data$ValorInicial + data$data$VarAcum
    
    data$data$Color <- ifelse(data$data$Variacion==0,"white",
                              ifelse(data$data$Variacion>0,"green","red"))
    data$data$Color2 <- ifelse(data$data$VarAcum==0,"white",
                               ifelse(data$data$VarAcum>0,"green","red"))
    
    if(data$data$ValorFinal[data$data$Agente=="SF"&data$data$Nombre=="Circulante"]<0) {
      data$text <- "El Sector Financiero tiene el Circulante en negativo. Consiga liquidez,
      por favor."
    } else {data$text <- ""}
    
  }
  data
}

tp_o_p_bc_rm <- function(monto) {
  
  if(monto<=0) {
    
    data$text <- "Ingrese un monto positivo, por favor"
    
  } else if(monto>data$data$ValorFinal[data$data$Agente=="RM"&data$data$Nombre=="Tit.Pub.Pesos"]) {
    
    data$text <- "El Resto del Mundo no dispone de tantos t\u00EDtulos"
    
  } else {
    
    data$data$Variacion <- 0
    
    data$data$Variacion[data$data$Agente=="RM"&data$data$Nombre=="Tit.Pub.Pesos"] <- -1*monto
    data$data$Variacion[data$data$Agente=="BC"&data$data$Nombre=="Tit.Pub.Pesos"] <- monto
    data$data$Variacion[data$data$Agente=="RM"&data$data$Nombre=="Divisas"] <- -1*monto
    data$data$Variacion[data$data$Agente=="BC"&data$data$Nombre=="Reservas"] <- -1*monto
    
    data$data$Variacion[data$data$Nombre=="Base Monetaria"] <- sum(data$data$Variacion[data$data$Agente=="BC"&
                                                                                         data$data$Nombre %in% c("Circulante","Cta.Cte.")])
    data$data$Variacion[data$data$Nombre=="M2"] <- data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data$data$Variacion[data$data$Nombre=="M3"] <- data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Plazo.Fijo"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data$data$Variacion[data$data$Nombre=="M3.Privado"] <- data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Plazo.Fijo"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data <- ajustar_resultado(data)
    data <- ajustar_usd(data)
    
    data$data$VarAcum <- data$data$VarAcum + data$data$Variacion
    data$data$ValorFinal <- data$data$ValorInicial + data$data$VarAcum
    
    data$data$Color <- ifelse(data$data$Variacion==0,"white",
                              ifelse(data$data$Variacion>0,"green","red"))
    data$data$Color2 <- ifelse(data$data$VarAcum==0,"white",
                               ifelse(data$data$VarAcum>0,"green","red"))
    
    if(data$data$ValorFinal[data$data$Agente=="SF"&data$data$Nombre=="Circulante"]<0) {
      data$text <- "El Sector Financiero tiene el Circulante en negativo. Consiga liquidez,
      por favor."
    } else {data$text <- ""}
    
  }
  data
}

tp_o_p_sf_bc <- function(monto) {
  
  if(monto<=0) {
    
    data$text <- "Ingrese un monto positivo, por favor"
    
  } else if(monto>data$data$ValorFinal[data$data$Agente=="BC"&data$data$Nombre=="Tit.Pub.Pesos"]) {
    
    data$text <- "El Banco Central no dispone de tantos t\u00EDtulos"
    
  } else if(monto>data$data$ValorFinal[data$data$Agente=="SF"&data$data$Nombre=="Circulante"] +
            data$data$ValorFinal[data$data$Agente=="SF"&data$data$Nombre=="Cta.Cte."]) {
    
    data$text <- "El Sistema Finaciero no cuenta con suficiente liquidez"
    
  } else {
    
    data$data$Variacion <- 0
    
    data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Tit.Pub.Pesos"] <- monto
    data$data$Variacion[data$data$Agente=="BC"&data$data$Nombre=="Tit.Pub.Pesos"] <- -1*monto
    data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Cta.Cte."] <- -1*monto
    data$data$Variacion[data$data$Agente=="BC"&data$data$Nombre=="Cta.Cte."] <- -1*monto
    data <- ajustar_encaje(data)
    
    data$data$Variacion[data$data$Nombre=="Base Monetaria"] <- sum(data$data$Variacion[data$data$Agente=="BC"&
                                                                                         data$data$Nombre %in% c("Circulante","Cta.Cte.")])
    data$data$Variacion[data$data$Nombre=="M2"] <- data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data$data$Variacion[data$data$Nombre=="M3"] <- data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Plazo.Fijo"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data$data$Variacion[data$data$Nombre=="M3.Privado"] <- data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Plazo.Fijo"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data <- ajustar_resultado(data)
    data <- ajustar_usd(data)
    
    data$data$VarAcum <- data$data$VarAcum + data$data$Variacion
    data$data$ValorFinal <- data$data$ValorInicial + data$data$VarAcum
    
    data$data$Color <- ifelse(data$data$Variacion==0,"white",
                              ifelse(data$data$Variacion>0,"green","red"))
    data$data$Color2 <- ifelse(data$data$VarAcum==0,"white",
                               ifelse(data$data$VarAcum>0,"green","red"))
    
    if(data$data$ValorFinal[data$data$Agente=="SF"&data$data$Nombre=="Circulante"]<0) {
      data$text <- "El Sector Financiero tiene el Circulante en negativo. Consiga liquidez,
      por favor."
    } else {data$text <- ""}
    
  }
  data
}

tp_o_p_sf_sp <- function(monto) {
  
  if(monto<=0) {
    
    data$text <- "Ingrese un monto positivo, por favor"
    
  } else if(monto>data$data$ValorFinal[data$data$Agente=="H"&data$data$Nombre=="Tit.Pub.Pesos"]) {
    
    data$text <- "El SPnF no dispone de tantos t\u00EDtulos"
    
  } else if(monto>(data$data$ValorFinal[data$data$Agente=="SF"&data$data$Nombre=="Circulante"] +
                   data$data$ValorFinal[data$data$Agente=="SF"&data$data$Nombre=="Cta.Cte."])/data$encaje$VistaPesos) {
    
    data$text <- "El Sistema Finaciero no cuenta con suficiente liquidez"
    
  } else {
    
    data$data$Variacion <- 0
    
    data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Tit.Pub.Pesos"] <- monto
    data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Tit.Pub.Pesos"] <- -1*monto
    data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Dep.Vista"] <- monto
    data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.Vista"] <- monto
    data <- ajustar_encaje(data)
    
    data$data$Variacion[data$data$Nombre=="Base Monetaria"] <- sum(data$data$Variacion[data$data$Agente=="BC"&
                                                                                         data$data$Nombre %in% c("Circulante","Cta.Cte.")])
    data$data$Variacion[data$data$Nombre=="M2"] <- data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data$data$Variacion[data$data$Nombre=="M3"] <- data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Plazo.Fijo"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data$data$Variacion[data$data$Nombre=="M3.Privado"] <- data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Plazo.Fijo"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data <- ajustar_resultado(data)
    data <- ajustar_usd(data)
    
    data$data$VarAcum <- data$data$VarAcum + data$data$Variacion
    data$data$ValorFinal <- data$data$ValorInicial + data$data$VarAcum
    
    data$data$Color <- ifelse(data$data$Variacion==0,"white",
                              ifelse(data$data$Variacion>0,"green","red"))
    data$data$Color2 <- ifelse(data$data$VarAcum==0,"white",
                               ifelse(data$data$VarAcum>0,"green","red"))
    
    if(data$data$ValorFinal[data$data$Agente=="SF"&data$data$Nombre=="Circulante"]<0) {
      data$text <- "El Sector Financiero tiene el Circulante en negativo. Consiga liquidez,
      por favor."
    } else {data$text <- ""}
    
  }
  data
}

tp_o_p_sf_rm <- function(monto) {
  
  if(monto<=0) {
    
    data$text <- "Ingrese un monto positivo, por favor"
    
  } else if(monto>data$data$ValorFinal[data$data$Agente=="RM"&data$data$Nombre=="Tit.Pub.Pesos"]) {
    
    data$text <- "El Resto del Mundo no dispone de tantos t\u00EDtulos"
    
  } else if(monto>(data$data$ValorFinal[data$data$Agente=="SF"&data$data$Nombre=="USD"] +
                   data$data$ValorFinal[data$data$Agente=="SF"&data$data$Nombre=="Cta.Cte.USD"])) {
    
    data$text <- "El Sistema Finaciero no cuenta con suficiente liquidez en d\u00F3lares"
    
  } else {
    
    data$data$Variacion <- 0
    
    m1 <- min(monto,data$data$ValorFinal[data$data$Agente=="SF"&data$data$Nombre=="Cta.Cte.USD"])
    m2 <- monto-m1
    
    data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Tit.Pub.Pesos"] <- monto
    data$data$Variacion[data$data$Agente=="RM"&data$data$Nombre=="Tit.Pub.Pesos"] <- -1*monto
    data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="USD"] <- -1*m2
    data$data$Variacion[data$data$Agente=="RM"&data$data$Nombre=="Divisas"] <- -1*monto
    data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Cta.Cte.USD"] <- -1*m1
    data$data$Variacion[data$data$Agente=="BC"&data$data$Nombre=="Cta.Cte.USD"] <- -1*m2
    data$data$Variacion[data$data$Agente=="BC"&data$data$Nombre=="Reservas"] <- -1*m2
    
    data$data$Variacion[data$data$Nombre=="Base Monetaria"] <- sum(data$data$Variacion[data$data$Agente=="BC"&
                                                                                         data$data$Nombre %in% c("Circulante","Cta.Cte.")])
    data$data$Variacion[data$data$Nombre=="M2"] <- data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data$data$Variacion[data$data$Nombre=="M3"] <- data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Plazo.Fijo"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data$data$Variacion[data$data$Nombre=="M3.Privado"] <- data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Plazo.Fijo"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data <- ajustar_resultado(data)
    data <- ajustar_usd(data)
    
    data$data$VarAcum <- data$data$VarAcum + data$data$Variacion
    data$data$ValorFinal <- data$data$ValorInicial + data$data$VarAcum
    
    data$data$Color <- ifelse(data$data$Variacion==0,"white",
                              ifelse(data$data$Variacion>0,"green","red"))
    data$data$Color2 <- ifelse(data$data$VarAcum==0,"white",
                               ifelse(data$data$VarAcum>0,"green","red"))
    
    if(data$data$ValorFinal[data$data$Agente=="SF"&data$data$Nombre=="Circulante"]<0) {
      data$text <- "El Sector Financiero tiene el Circulante en negativo. Consiga liquidez,
      por favor."
    } else {data$text <- ""}
    
  }
  data
}

tp_o_p_sp_bc <- function(monto) {
  
  if(monto<=0) {
    
    data$text <- "Ingrese un monto positivo, por favor"
    
  } else if(monto>data$data$ValorFinal[data$data$Agente=="BC"&data$data$Nombre=="Tit.Pub.Pesos"]) {
    
    data$text <- "El Banco Central no dispone de tantos t\u00EDtulos"
    
  } else if(monto>data$data$ValorFinal[data$data$Agente=="H"&data$data$Nombre=="Dep.Vista"]) {
    
    data$text <- "El SpNF no cuenta con suficientes dep\u00F3sitos a la vista"
    
  } else {
    
    data$data$Variacion <- 0
    
    data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Tit.Pub.Pesos"] <- monto
    data$data$Variacion[data$data$Agente=="BC"&data$data$Nombre=="Tit.Pub.Pesos"] <- -1*monto
    data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Dep.Vista"] <- -1*monto
    data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.Vista"] <- -1*monto
    data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Cta.Cte."] <- -1*monto
    data$data$Variacion[data$data$Agente=="BC"&data$data$Nombre=="Cta.Cte."] <- -1*monto
    data <- ajustar_encaje(data)
    
    data$data$Variacion[data$data$Nombre=="Base Monetaria"] <- sum(data$data$Variacion[data$data$Agente=="BC"&
                                                                                         data$data$Nombre %in% c("Circulante","Cta.Cte.")])
    data$data$Variacion[data$data$Nombre=="M2"] <- data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data$data$Variacion[data$data$Nombre=="M3"] <- data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Plazo.Fijo"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data$data$Variacion[data$data$Nombre=="M3.Privado"] <- data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Plazo.Fijo"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data <- ajustar_resultado(data)
    data <- ajustar_usd(data)
    
    data$data$VarAcum <- data$data$VarAcum + data$data$Variacion
    data$data$ValorFinal <- data$data$ValorInicial + data$data$VarAcum
    
    data$data$Color <- ifelse(data$data$Variacion==0,"white",
                              ifelse(data$data$Variacion>0,"green","red"))
    data$data$Color2 <- ifelse(data$data$VarAcum==0,"white",
                               ifelse(data$data$VarAcum>0,"green","red"))
    
    if(data$data$ValorFinal[data$data$Agente=="SF"&data$data$Nombre=="Circulante"]<0) {
      data$text <- "El Sector Financiero tiene el Circulante en negativo. Consiga liquidez,
      por favor."
    } else {data$text <- ""}
    
  }
  data
}

tp_o_p_sp_sf <- function(monto) {
  
  if(monto<=0) {
    
    data$text <- "Ingrese un monto positivo, por favor"
    
  } else if(monto>data$data$ValorFinal[data$data$Agente=="SF"&data$data$Nombre=="Tit.Pub.Pesos"]) {
    
    data$text <- "El Sector Financiero no dispone de tantos t\u00EDtulos"
    
  } else if(monto>data$data$ValorFinal[data$data$Agente=="H"&data$data$Nombre=="Dep.Vista"]) {
    
    data$text <- "El SpNF no cuenta con suficientes dep\u00F3sitos a la vista"
    
  } else {
    
    data$data$Variacion <- 0
    
    data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Tit.Pub.Pesos"] <- monto
    data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Tit.Pub.Pesos"] <- -1*monto
    data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Dep.Vista"] <- -1*monto
    data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.Vista"] <- -1*monto
    data <- ajustar_encaje(data)
    
    data$data$Variacion[data$data$Nombre=="Base Monetaria"] <- sum(data$data$Variacion[data$data$Agente=="BC"&
                                                                                         data$data$Nombre %in% c("Circulante","Cta.Cte.")])
    data$data$Variacion[data$data$Nombre=="M2"] <- data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data$data$Variacion[data$data$Nombre=="M3"] <- data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Plazo.Fijo"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data$data$Variacion[data$data$Nombre=="M3.Privado"] <- data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Plazo.Fijo"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data <- ajustar_resultado(data)
    data <- ajustar_usd(data)
    
    data$data$VarAcum <- data$data$VarAcum + data$data$Variacion
    data$data$ValorFinal <- data$data$ValorInicial + data$data$VarAcum
    
    data$data$Color <- ifelse(data$data$Variacion==0,"white",
                              ifelse(data$data$Variacion>0,"green","red"))
    data$data$Color2 <- ifelse(data$data$VarAcum==0,"white",
                               ifelse(data$data$VarAcum>0,"green","red"))
    
    if(data$data$ValorFinal[data$data$Agente=="SF"&data$data$Nombre=="Circulante"]<0) {
      data$text <- "El Sector Financiero tiene el Circulante en negativo. Consiga liquidez,
      por favor."
    } else {data$text <- ""}
    
  }
  data
}

tp_o_p_sp_rm <- function(monto) {
  
  if(monto<=0) {
    
    data$text <- "Ingrese un monto positivo, por favor"
    
  } else if(monto>data$data$ValorFinal[data$data$Agente=="RM"&data$data$Nombre=="Tit.Pub.Pesos"]) {
    
    data$text <- "El Resto del Mundo no dispone de tantos t\u00EDtulos"
    
  } else if(monto>data$data$ValorFinal[data$data$Agente=="H"&data$data$Nombre=="Dep.USD"]) {
    
    data$text <- "El SpNF no cuenta con suficientes dep\u00F3sitos en d\u00F3lares"
    
  } else {
    
    data$data$Variacion <- 0
    
    data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Tit.Pub.Pesos"] <- monto
    data$data$Variacion[data$data$Agente=="RM"&data$data$Nombre=="Tit.Pub.Pesos"] <- -1*monto
    data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Dep.USD"] <- -1*monto
    data$data$Variacion[data$data$Agente=="RM"&data$data$Nombre=="Divisas"] <- -1*monto
    data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.USD"] <- -1*monto
    data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Cta.Cte.USD"] <- -1*monto
    data$data$Variacion[data$data$Agente=="BC"&data$data$Nombre=="Cta.Cte.USD"] <- -1*monto
    data$data$Variacion[data$data$Agente=="BC"&data$data$Nombre=="Reservas"] <- -1*monto
    data <- ajustar_encaje(data,moneda = "d")
    
    data$data$Variacion[data$data$Nombre=="Base Monetaria"] <- sum(data$data$Variacion[data$data$Agente=="BC"&
                                                                                         data$data$Nombre %in% c("Circulante","Cta.Cte.")])
    data$data$Variacion[data$data$Nombre=="M2"] <- data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data$data$Variacion[data$data$Nombre=="M3"] <- data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Plazo.Fijo"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data$data$Variacion[data$data$Nombre=="M3.Privado"] <- data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Plazo.Fijo"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data <- ajustar_resultado(data)
    data <- ajustar_usd(data)
    
    data$data$VarAcum <- data$data$VarAcum + data$data$Variacion
    data$data$ValorFinal <- data$data$ValorInicial + data$data$VarAcum
    
    data$data$Color <- ifelse(data$data$Variacion==0,"white",
                              ifelse(data$data$Variacion>0,"green","red"))
    data$data$Color2 <- ifelse(data$data$VarAcum==0,"white",
                               ifelse(data$data$VarAcum>0,"green","red"))
    
    if(data$data$ValorFinal[data$data$Agente=="SF"&data$data$Nombre=="Circulante"]<0) {
      data$text <- "El Sector Financiero tiene el Circulante en negativo. Consiga liquidez,
      por favor."
    } else {data$text <- ""}
    
  }
  data
}

tp_o_p_rm_bc <- function(monto) {
  
  if(monto<=0) {
    
    data$text <- "Ingrese un monto positivo, por favor"
    
  } else if(monto>data$data$ValorFinal[data$data$Agente=="BC"&data$data$Nombre=="Tit.Pub.Pesos"]) {
    
    data$text <- "El Banco Central no dispone de tantos t\u00EDtulos"
    
  } else {
    
    data$data$Variacion <- 0
    
    data$data$Variacion[data$data$Agente=="RM"&data$data$Nombre=="Tit.Pub.Pesos"] <- monto
    data$data$Variacion[data$data$Agente=="BC"&data$data$Nombre=="Tit.Pub.Pesos"] <- -1*monto
    data$data$Variacion[data$data$Agente=="RM"&data$data$Nombre=="Divisas"] <- monto
    data$data$Variacion[data$data$Agente=="BC"&data$data$Nombre=="Reservas"] <- monto
  
    data$data$Variacion[data$data$Nombre=="Base Monetaria"] <- sum(data$data$Variacion[data$data$Agente=="BC"&
                                                                                         data$data$Nombre %in% c("Circulante","Cta.Cte.")])
    data$data$Variacion[data$data$Nombre=="M2"] <- data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data$data$Variacion[data$data$Nombre=="M3"] <- data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Plazo.Fijo"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data$data$Variacion[data$data$Nombre=="M3.Privado"] <- data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Plazo.Fijo"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data <- ajustar_resultado(data)
    data <- ajustar_usd(data)
    
    data$data$VarAcum <- data$data$VarAcum + data$data$Variacion
    data$data$ValorFinal <- data$data$ValorInicial + data$data$VarAcum
    
    data$data$Color <- ifelse(data$data$Variacion==0,"white",
                              ifelse(data$data$Variacion>0,"green","red"))
    data$data$Color2 <- ifelse(data$data$VarAcum==0,"white",
                               ifelse(data$data$VarAcum>0,"green","red"))
    
    if(data$data$ValorFinal[data$data$Agente=="SF"&data$data$Nombre=="Circulante"]<0) {
      data$text <- "El Sector Financiero tiene el Circulante en negativo. Consiga liquidez,
      por favor."
    } else {data$text <- ""}
    
  }
  data
}

tp_o_p_rm_sf <- function(monto) {
  
  if(monto<=0) {
    
    data$text <- "Ingrese un monto positivo, por favor"
    
  } else if(monto>data$data$ValorFinal[data$data$Agente=="SF"&data$data$Nombre=="Tit.Pub.Pesos"]) {
    
    data$text <- "El Sistema Financiero no dispone de tantos t\u00EDtulos"
    
  } else {
    
    data$data$Variacion <- 0
    
    data$data$Variacion[data$data$Agente=="RM"&data$data$Nombre=="Tit.Pub.Pesos"] <- monto
    data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Tit.Pub.Pesos"] <- -1*monto
    data$data$Variacion[data$data$Agente=="RM"&data$data$Nombre=="Divisas"] <- monto
    data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Cta.Cte.USD"] <- monto
    data$data$Variacion[data$data$Agente=="BC"&data$data$Nombre=="Cta.Cte.USD"] <- monto
    data$data$Variacion[data$data$Agente=="BC"&data$data$Nombre=="Reservas"] <- monto
    
    data$data$Variacion[data$data$Nombre=="Base Monetaria"] <- sum(data$data$Variacion[data$data$Agente=="BC"&
                                                                                         data$data$Nombre %in% c("Circulante","Cta.Cte.")])
    data$data$Variacion[data$data$Nombre=="M2"] <- data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data$data$Variacion[data$data$Nombre=="M3"] <- data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Plazo.Fijo"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data$data$Variacion[data$data$Nombre=="M3.Privado"] <- data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Plazo.Fijo"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data <- ajustar_resultado(data)
    data <- ajustar_usd(data)
    
    data$data$VarAcum <- data$data$VarAcum + data$data$Variacion
    data$data$ValorFinal <- data$data$ValorInicial + data$data$VarAcum
    
    data$data$Color <- ifelse(data$data$Variacion==0,"white",
                              ifelse(data$data$Variacion>0,"green","red"))
    data$data$Color2 <- ifelse(data$data$VarAcum==0,"white",
                               ifelse(data$data$VarAcum>0,"green","red"))
    
    if(data$data$ValorFinal[data$data$Agente=="SF"&data$data$Nombre=="Circulante"]<0) {
      data$text <- "El Sector Financiero tiene el Circulante en negativo. Consiga liquidez,
      por favor."
    } else {data$text <- ""}
    
  }
  data
}

tp_o_p_rm_sp <- function(monto) {
  
  if(monto<=0) {
    
    data$text <- "Ingrese un monto positivo, por favor"
    
  } else if(monto>data$data$ValorFinal[data$data$Agente=="H"&data$data$Nombre=="Tit.Pub.Pesos"]) {
    
    data$text <- "El SPnF no dispone de tantos t\u00EDtulos"
    
  } else {
    
    data$data$Variacion <- 0
    
    data$data$Variacion[data$data$Agente=="RM"&data$data$Nombre=="Tit.Pub.Pesos"] <- monto
    data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Tit.Pub.Pesos"] <- -1*monto
    data$data$Variacion[data$data$Agente=="RM"&data$data$Nombre=="Divisas"] <- monto
    data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Dep.USD"] <- monto
    data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.USD"] <- monto
    data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Cta.Cte.USD"] <- monto
    data$data$Variacion[data$data$Agente=="BC"&data$data$Nombre=="Cta.Cte.USD"] <- monto
    data$data$Variacion[data$data$Agente=="BC"&data$data$Nombre=="Reservas"] <- monto
    data <- ajustar_encaje(data,moneda = "d")
    
    data$data$Variacion[data$data$Nombre=="Base Monetaria"] <- sum(data$data$Variacion[data$data$Agente=="BC"&
                                                                                         data$data$Nombre %in% c("Circulante","Cta.Cte.")])
    data$data$Variacion[data$data$Nombre=="M2"] <- data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data$data$Variacion[data$data$Nombre=="M3"] <- data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Plazo.Fijo"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data$data$Variacion[data$data$Nombre=="M3.Privado"] <- data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Plazo.Fijo"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data <- ajustar_resultado(data)
    data <- ajustar_usd(data)
    
    data$data$VarAcum <- data$data$VarAcum + data$data$Variacion
    data$data$ValorFinal <- data$data$ValorInicial + data$data$VarAcum
    
    data$data$Color <- ifelse(data$data$Variacion==0,"white",
                              ifelse(data$data$Variacion>0,"green","red"))
    data$data$Color2 <- ifelse(data$data$VarAcum==0,"white",
                               ifelse(data$data$VarAcum>0,"green","red"))
    
    if(data$data$ValorFinal[data$data$Agente=="SF"&data$data$Nombre=="Circulante"]<0) {
      data$text <- "El Sector Financiero tiene el Circulante en negativo. Consiga liquidez,
      por favor."
    } else {data$text <- ""}
    
  }
  data
}

tp_o_d_bc_sp <- function(monto) {
  
  if(monto<=0) {
    
    data$text <- "Ingrese un monto positivo, por favor"
    
  } else if(monto>data$data$ValorFinal[data$data$Agente=="H"&data$data$Nombre=="Tit.Pub.USD"]) {
    
    data$text <- "El SPnF no dispone de tantos t\u00EDtulos"
    
  } else {
    
    data$data$Variacion <- 0
    
    data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Tit.Pub.USD"] <- -1*monto
    data$data$Variacion[data$data$Agente=="BC"&data$data$Nombre=="Tit.Pub.USD"] <- monto
    data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Dep.Vista"] <- monto
    data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.Vista"] <- monto
    data$data$Variacion[data$data$Agente=="BC"&data$data$Nombre=="Cta.Cte."] <- monto
    data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Cta.Cte."] <- monto
    data <- ajustar_encaje(data)
    
    data$data$Variacion[data$data$Nombre=="Base Monetaria"] <- sum(data$data$Variacion[data$data$Agente=="BC"&
                                                                                         data$data$Nombre %in% c("Circulante","Cta.Cte.")])
    data$data$Variacion[data$data$Nombre=="M2"] <- data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data$data$Variacion[data$data$Nombre=="M3"] <- data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Plazo.Fijo"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data$data$Variacion[data$data$Nombre=="M3.Privado"] <- data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Plazo.Fijo"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data <- ajustar_resultado(data)
    data <- ajustar_usd(data)
    
    data$data$VarAcum <- data$data$VarAcum + data$data$Variacion
    data$data$ValorFinal <- data$data$ValorInicial + data$data$VarAcum
    
    data$data$Color <- ifelse(data$data$Variacion==0,"white",
                              ifelse(data$data$Variacion>0,"green","red"))
    data$data$Color2 <- ifelse(data$data$VarAcum==0,"white",
                               ifelse(data$data$VarAcum>0,"green","red"))
    
    if(data$data$ValorFinal[data$data$Agente=="SF"&data$data$Nombre=="Circulante"]<0) {
      data$text <- "El Sector Financiero tiene el Circulante en negativo. Consiga liquidez,
      por favor."
    } else {data$text <- ""}
    
  }
  data
}

tp_o_d_bc_rm <- function(monto) {
  
  if(monto<=0) {
    
    data$text <- "Ingrese un monto positivo, por favor"
    
  } else if(monto>data$data$ValorFinal[data$data$Agente=="RM"&data$data$Nombre=="Tit.Pub.USD"]) {
    
    data$text <- "El Resto del Mundo no dispone de tantos t\u00EDtulos"
    
  } else {
    
    data$data$Variacion <- 0
    
    data$data$Variacion[data$data$Agente=="RM"&data$data$Nombre=="Tit.Pub.USD"] <- -1*monto
    data$data$Variacion[data$data$Agente=="BC"&data$data$Nombre=="Tit.Pub.USD"] <- monto
    data$data$Variacion[data$data$Agente=="RM"&data$data$Nombre=="Divisas"] <- -1*monto
    data$data$Variacion[data$data$Agente=="BC"&data$data$Nombre=="Reservas"] <- -1*monto
    
    data$data$Variacion[data$data$Nombre=="Base Monetaria"] <- sum(data$data$Variacion[data$data$Agente=="BC"&
                                                                                         data$data$Nombre %in% c("Circulante","Cta.Cte.")])
    data$data$Variacion[data$data$Nombre=="M2"] <- data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data$data$Variacion[data$data$Nombre=="M3"] <- data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Plazo.Fijo"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data$data$Variacion[data$data$Nombre=="M3.Privado"] <- data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Plazo.Fijo"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data <- ajustar_resultado(data)
    data <- ajustar_usd(data)
    
    data$data$VarAcum <- data$data$VarAcum + data$data$Variacion
    data$data$ValorFinal <- data$data$ValorInicial + data$data$VarAcum
    
    data$data$Color <- ifelse(data$data$Variacion==0,"white",
                              ifelse(data$data$Variacion>0,"green","red"))
    data$data$Color2 <- ifelse(data$data$VarAcum==0,"white",
                               ifelse(data$data$VarAcum>0,"green","red"))
    
    if(data$data$ValorFinal[data$data$Agente=="SF"&data$data$Nombre=="Circulante"]<0) {
      data$text <- "El Sector Financiero tiene el Circulante en negativo. Consiga liquidez,
      por favor."
    } else {data$text <- ""}
    
  }
  data
}

tp_o_d_sp_bc <- function(monto) {
  
  if(monto<=0) {
    
    data$text <- "Ingrese un monto positivo, por favor"
    
  } else if(monto>data$data$ValorFinal[data$data$Agente=="BC"&data$data$Nombre=="Tit.Pub.USD"]) {
    
    data$text <- "El Banco Central no dispone de tantos t\u00EDtulos"
    
  } else if(monto>data$data$ValorFinal[data$data$Agente=="H"&data$data$Nombre=="Dep.Vista"]) {
    
    data$text <- "El SpNF no cuenta con suficientes dep\u00F3sitos a la vista"
    
  } else {
    
    data$data$Variacion <- 0
    
    data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Tit.Pub.USD"] <- monto
    data$data$Variacion[data$data$Agente=="BC"&data$data$Nombre=="Tit.Pub.USD"] <- -1*monto
    data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Dep.Vista"] <- -1*monto
    data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.Vista"] <- -1*monto
    data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Cta.Cte."] <- -1*monto
    data$data$Variacion[data$data$Agente=="BC"&data$data$Nombre=="Cta.Cte."] <- -1*monto
    data <- ajustar_encaje(data)
    
    data$data$Variacion[data$data$Nombre=="Base Monetaria"] <- sum(data$data$Variacion[data$data$Agente=="BC"&
                                                                                         data$data$Nombre %in% c("Circulante","Cta.Cte.")])
    data$data$Variacion[data$data$Nombre=="M2"] <- data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data$data$Variacion[data$data$Nombre=="M3"] <- data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Plazo.Fijo"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data$data$Variacion[data$data$Nombre=="M3.Privado"] <- data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Plazo.Fijo"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data <- ajustar_resultado(data)
    data <- ajustar_usd(data)
    
    data$data$VarAcum <- data$data$VarAcum + data$data$Variacion
    data$data$ValorFinal <- data$data$ValorInicial + data$data$VarAcum
    
    data$data$Color <- ifelse(data$data$Variacion==0,"white",
                              ifelse(data$data$Variacion>0,"green","red"))
    data$data$Color2 <- ifelse(data$data$VarAcum==0,"white",
                               ifelse(data$data$VarAcum>0,"green","red"))
    
    if(data$data$ValorFinal[data$data$Agente=="SF"&data$data$Nombre=="Circulante"]<0) {
      data$text <- "El Sector Financiero tiene el Circulante en negativo. Consiga liquidez,
      por favor."
    } else {data$text <- ""}
    
  }
  data
}

tp_o_d_sp_rm <- function(monto) {
  
  if(monto<=0) {
    
    data$text <- "Ingrese un monto positivo, por favor"
    
  } else if(monto>data$data$ValorFinal[data$data$Agente=="RM"&data$data$Nombre=="Tit.Pub.USD"]) {
    
    data$text <- "El Resto del Mundo no dispone de tantos t\u00EDtulos"
    
  } else if(monto>data$data$ValorFinal[data$data$Agente=="H"&data$data$Nombre=="Dep.USD"]) {
    
    data$text <- "El SpNF no cuenta con suficientes dep\u00F3sitos en d\u00F3lares"
    
  } else {
    
    data$data$Variacion <- 0
    
    data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Tit.Pub.USD"] <- monto
    data$data$Variacion[data$data$Agente=="RM"&data$data$Nombre=="Tit.Pub.USD"] <- -1*monto
    data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Dep.USD"] <- -1*monto
    data$data$Variacion[data$data$Agente=="RM"&data$data$Nombre=="Divisas"] <- -1*monto
    data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.USD"] <- -1*monto
    data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Cta.Cte.USD"] <- -1*monto
    data$data$Variacion[data$data$Agente=="BC"&data$data$Nombre=="Cta.Cte.USD"] <- -1*monto
    data$data$Variacion[data$data$Agente=="BC"&data$data$Nombre=="Reservas"] <- -1*monto
    data <- ajustar_encaje(data,moneda = "d")
    
    data$data$Variacion[data$data$Nombre=="Base Monetaria"] <- sum(data$data$Variacion[data$data$Agente=="BC"&
                                                                                         data$data$Nombre %in% c("Circulante","Cta.Cte.")])
    data$data$Variacion[data$data$Nombre=="M2"] <- data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data$data$Variacion[data$data$Nombre=="M3"] <- data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Plazo.Fijo"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data$data$Variacion[data$data$Nombre=="M3.Privado"] <- data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Plazo.Fijo"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data <- ajustar_resultado(data)
    data <- ajustar_usd(data)
    
    data$data$VarAcum <- data$data$VarAcum + data$data$Variacion
    data$data$ValorFinal <- data$data$ValorInicial + data$data$VarAcum
    
    data$data$Color <- ifelse(data$data$Variacion==0,"white",
                              ifelse(data$data$Variacion>0,"green","red"))
    data$data$Color2 <- ifelse(data$data$VarAcum==0,"white",
                               ifelse(data$data$VarAcum>0,"green","red"))
    
    if(data$data$ValorFinal[data$data$Agente=="SF"&data$data$Nombre=="Circulante"]<0) {
      data$text <- "El Sector Financiero tiene el Circulante en negativo. Consiga liquidez,
      por favor."
    } else {data$text <- ""}
    
  }
  data
}

tp_o_d_rm_bc <- function(monto) {
  
  if(monto<=0) {
    
    data$text <- "Ingrese un monto positivo, por favor"
    
  } else if(monto>data$data$ValorFinal[data$data$Agente=="BC"&data$data$Nombre=="Tit.Pub.USD"]) {
    
    data$text <- "El Banco Central no dispone de tantos t\u00EDtulos"
    
  } else {
    
    data$data$Variacion <- 0
    
    data$data$Variacion[data$data$Agente=="RM"&data$data$Nombre=="Tit.Pub.USD"] <- monto
    data$data$Variacion[data$data$Agente=="BC"&data$data$Nombre=="Tit.Pub.USD"] <- -1*monto
    data$data$Variacion[data$data$Agente=="RM"&data$data$Nombre=="Divisas"] <- monto
    data$data$Variacion[data$data$Agente=="BC"&data$data$Nombre=="Reservas"] <- monto
    
    data$data$Variacion[data$data$Nombre=="Base Monetaria"] <- sum(data$data$Variacion[data$data$Agente=="BC"&
                                                                                         data$data$Nombre %in% c("Circulante","Cta.Cte.")])
    data$data$Variacion[data$data$Nombre=="M2"] <- data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data$data$Variacion[data$data$Nombre=="M3"] <- data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Plazo.Fijo"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data$data$Variacion[data$data$Nombre=="M3.Privado"] <- data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Plazo.Fijo"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data <- ajustar_resultado(data)
    data <- ajustar_usd(data)
    
    data$data$VarAcum <- data$data$VarAcum + data$data$Variacion
    data$data$ValorFinal <- data$data$ValorInicial + data$data$VarAcum
    
    data$data$Color <- ifelse(data$data$Variacion==0,"white",
                              ifelse(data$data$Variacion>0,"green","red"))
    data$data$Color2 <- ifelse(data$data$VarAcum==0,"white",
                               ifelse(data$data$VarAcum>0,"green","red"))
    
    if(data$data$ValorFinal[data$data$Agente=="SF"&data$data$Nombre=="Circulante"]<0) {
      data$text <- "El Sector Financiero tiene el Circulante en negativo. Consiga liquidez,
      por favor."
    } else {data$text <- ""}
    
  }
  data
}

tp_o_d_rm_sp <- function(monto) {
  
  if(monto<=0) {
    
    data$text <- "Ingrese un monto positivo, por favor"
    
  } else if(monto>data$data$ValorFinal[data$data$Agente=="H"&data$data$Nombre=="Tit.Pub.USD"]) {
    
    data$text <- "El SPnF no dispone de tantos t\u00EDtulos"
    
  } else {
    
    data$data$Variacion <- 0
    
    data$data$Variacion[data$data$Agente=="RM"&data$data$Nombre=="Tit.Pub.USD"] <- monto
    data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Tit.Pub.USD"] <- -1*monto
    data$data$Variacion[data$data$Agente=="RM"&data$data$Nombre=="Divisas"] <- monto
    data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Dep.USD"] <- monto
    data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.USD"] <- monto
    data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Cta.Cte.USD"] <- monto
    data$data$Variacion[data$data$Agente=="BC"&data$data$Nombre=="Cta.Cte.USD"] <- monto
    data$data$Variacion[data$data$Agente=="BC"&data$data$Nombre=="Reservas"] <- monto
    data <- ajustar_encaje(data,moneda = "d")
    
    data$data$Variacion[data$data$Nombre=="Base Monetaria"] <- sum(data$data$Variacion[data$data$Agente=="BC"&
                                                                                         data$data$Nombre %in% c("Circulante","Cta.Cte.")])
    data$data$Variacion[data$data$Nombre=="M2"] <- data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data$data$Variacion[data$data$Nombre=="M3"] <- data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Plazo.Fijo"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data$data$Variacion[data$data$Nombre=="M3.Privado"] <- data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Plazo.Fijo"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data <- ajustar_resultado(data)
    data <- ajustar_usd(data)
    
    data$data$VarAcum <- data$data$VarAcum + data$data$Variacion
    data$data$ValorFinal <- data$data$ValorInicial + data$data$VarAcum
    
    data$data$Color <- ifelse(data$data$Variacion==0,"white",
                              ifelse(data$data$Variacion>0,"green","red"))
    data$data$Color2 <- ifelse(data$data$VarAcum==0,"white",
                               ifelse(data$data$VarAcum>0,"green","red"))
    
    if(data$data$ValorFinal[data$data$Agente=="SF"&data$data$Nombre=="Circulante"]<0) {
      data$text <- "El Sector Financiero tiene el Circulante en negativo. Consiga liquidez,
      por favor."
    } else {data$text <- ""}
    
  }
  data
}

em_l_b <- function(monto) {
  
  if(monto<=0) {
    
    data$text <- "Ingrese un monto positivo, por favor"
    
  } else if(monto>data$data$ValorFinal[data$data$Agente=="SF"&data$data$Nombre=="Tit.Pub.Pesos"]) {
    
    data$text <- "El SPnF no dispone de tantos t\u00EDtulos"
    
  } else if(monto>data$data$ValorFinal[data$data$Agente=="SF"&data$data$Nombre=="EM_LELIQ"]) {
    
    data$text <- "El SPnF no dispone de tantas LELIQ como Efectivo M\u00EDnimo"
    
  } else {
    
    data$data$Variacion <- 0
    
    data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Tit.Pub.Pesos"] <- -1*monto
    data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="EM_Tit.Pub.Pesos"] <- monto
    data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="EM_LELIQ"] <- -1*monto
    data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="LELIQ"] <- monto
    
    
    data$data$Variacion[data$data$Nombre=="Base Monetaria"] <- sum(data$data$Variacion[data$data$Agente=="BC"&
                                                                                         data$data$Nombre %in% c("Circulante","Cta.Cte.")])
    data$data$Variacion[data$data$Nombre=="M2"] <- data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data$data$Variacion[data$data$Nombre=="M3"] <- data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Plazo.Fijo"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data$data$Variacion[data$data$Nombre=="M3.Privado"] <- data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Plazo.Fijo"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data <- ajustar_resultado(data)
    data <- ajustar_usd(data)
    
    data$data$VarAcum <- data$data$VarAcum + data$data$Variacion
    data$data$ValorFinal <- data$data$ValorInicial + data$data$VarAcum
    
    data$data$Color <- ifelse(data$data$Variacion==0,"white",
                              ifelse(data$data$Variacion>0,"green","red"))
    data$data$Color2 <- ifelse(data$data$VarAcum==0,"white",
                               ifelse(data$data$VarAcum>0,"green","red"))
    
    if(data$data$ValorFinal[data$data$Agente=="SF"&data$data$Nombre=="Circulante"]<0) {
      data$text <- "El Sector Financiero tiene el Circulante en negativo. Consiga liquidez,
      por favor."
    } else {data$text <- ""}
    
  }
  data
}

sf_circ_cc <- function(monto) {
  
  if(monto<=0) {
    
    data$text <- "Ingrese un monto positivo, por favor"
    
  } else if(monto>data$data$ValorFinal[data$data$Agente=="SF"&data$data$Nombre=="Circulante"]) {
    
    data$text <- "El SPnF no dispone de tantos circulante"
    
  } else {
    
    data$data$Variacion <- 0
    
    data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Circulante"] <- -1*monto
    data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Cta.Cte."] <- monto
    data$data$Variacion[data$data$Agente=="BC"&data$data$Nombre=="Circulante"] <- -1*monto
    data$data$Variacion[data$data$Agente=="BC"&data$data$Nombre=="Cta.Cte."] <- monto
    
    
    data$data$Variacion[data$data$Nombre=="Base Monetaria"] <- sum(data$data$Variacion[data$data$Agente=="BC"&
                                                                                         data$data$Nombre %in% c("Circulante","Cta.Cte.")])
    data$data$Variacion[data$data$Nombre=="M2"] <- data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data$data$Variacion[data$data$Nombre=="M3"] <- data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Plazo.Fijo"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data$data$Variacion[data$data$Nombre=="M3.Privado"] <- data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Plazo.Fijo"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data <- ajustar_resultado(data)
    data <- ajustar_usd(data)
    
    data$data$VarAcum <- data$data$VarAcum + data$data$Variacion
    data$data$ValorFinal <- data$data$ValorInicial + data$data$VarAcum
    
    data$data$Color <- ifelse(data$data$Variacion==0,"white",
                              ifelse(data$data$Variacion>0,"green","red"))
    data$data$Color2 <- ifelse(data$data$VarAcum==0,"white",
                               ifelse(data$data$VarAcum>0,"green","red"))
    
    if(data$data$ValorFinal[data$data$Agente=="SF"&data$data$Nombre=="Circulante"]<0) {
      data$text <- "El Sector Financiero tiene el Circulante en negativo. Consiga liquidez,
      por favor."
    } else {data$text <- ""}
    
  }
  data
}

sf_cc_circ <- function(monto) {
  
  if(monto<=0) {
    
    data$text <- "Ingrese un monto positivo, por favor"
    
  } else if(monto>data$data$ValorFinal[data$data$Agente=="SF"&data$data$Nombre=="Cta.Cte."]) {
    
    data$text <- "El SPnF no dispone de saldo suficiente en la Cta.Cte."
    
  } else {
    
    data$data$Variacion <- 0
    
    data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Circulante"] <- monto
    data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Cta.Cte."] <- -1*monto
    data$data$Variacion[data$data$Agente=="BC"&data$data$Nombre=="Circulante"] <- monto
    data$data$Variacion[data$data$Agente=="BC"&data$data$Nombre=="Cta.Cte."] <- -1*monto
    
    
    data$data$Variacion[data$data$Nombre=="Base Monetaria"] <- sum(data$data$Variacion[data$data$Agente=="BC"&
                                                                                         data$data$Nombre %in% c("Circulante","Cta.Cte.")])
    data$data$Variacion[data$data$Nombre=="M2"] <- data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data$data$Variacion[data$data$Nombre=="M3"] <- data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Plazo.Fijo"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data$data$Variacion[data$data$Nombre=="M3.Privado"] <- data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Plazo.Fijo"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data <- ajustar_resultado(data)
    data <- ajustar_usd(data)
    
    data$data$VarAcum <- data$data$VarAcum + data$data$Variacion
    data$data$ValorFinal <- data$data$ValorInicial + data$data$VarAcum
    
    data$data$Color <- ifelse(data$data$Variacion==0,"white",
                              ifelse(data$data$Variacion>0,"green","red"))
    data$data$Color2 <- ifelse(data$data$VarAcum==0,"white",
                               ifelse(data$data$VarAcum>0,"green","red"))
    
    if(data$data$ValorFinal[data$data$Agente=="SF"&data$data$Nombre=="Circulante"]<0) {
      data$text <- "El Sector Financiero tiene el Circulante en negativo. Consiga liquidez,
      por favor."
    } else {data$text <- ""}
    
  }
  data
}

sf_usd_cc <- function(monto) {
  
  if(monto<=0) {
    
    data$text <- "Ingrese un monto positivo, por favor"
    
  } else if(monto>data$data$ValorFinal[data$data$Agente=="SF"&data$data$Nombre=="USD"]) {
    
    data$text <- "El SPnF no dispone de suficientes d\u00F3lares."
    
  } else {
    
    data$data$Variacion <- 0
    
    data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Cta.Cte.USD"] <- monto
    data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="USD"] <- -1*monto
    data$data$Variacion[data$data$Agente=="BC"&data$data$Nombre=="Cta.Cte.USD"] <- monto
    data$data$Variacion[data$data$Agente=="BC"&data$data$Nombre=="Reservas"] <- monto
    
    
    data$data$Variacion[data$data$Nombre=="Base Monetaria"] <- sum(data$data$Variacion[data$data$Agente=="BC"&
                                                                                         data$data$Nombre %in% c("Circulante","Cta.Cte.")])
    data$data$Variacion[data$data$Nombre=="M2"] <- data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data$data$Variacion[data$data$Nombre=="M3"] <- data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Plazo.Fijo"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data$data$Variacion[data$data$Nombre=="M3.Privado"] <- data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Plazo.Fijo"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data <- ajustar_resultado(data)
    data <- ajustar_usd(data)
    
    data$data$VarAcum <- data$data$VarAcum + data$data$Variacion
    data$data$ValorFinal <- data$data$ValorInicial + data$data$VarAcum
    
    data$data$Color <- ifelse(data$data$Variacion==0,"white",
                              ifelse(data$data$Variacion>0,"green","red"))
    data$data$Color2 <- ifelse(data$data$VarAcum==0,"white",
                               ifelse(data$data$VarAcum>0,"green","red"))
    
    if(data$data$ValorFinal[data$data$Agente=="SF"&data$data$Nombre=="Circulante"]<0) {
      data$text <- "El Sector Financiero tiene el Circulante en negativo. Consiga liquidez,
      por favor."
    } else {data$text <- ""}
    
  }
  data
}

sf_cc_usd <- function(monto) {
  
  if(monto<=0) {
    
    data$text <- "Ingrese un monto positivo, por favor"
    
  } else if(monto>data$data$ValorFinal[data$data$Agente=="SF"&data$data$Nombre=="USD"]) {
    
    data$text <- "El SPnF no dispone de saldo suficiente en la Cta.Cte. en USD"
    
  } else {
    
    data$data$Variacion <- 0
    
    data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Cta.Cte.USD"] <- -1*monto
    data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="USD"] <- monto
    data$data$Variacion[data$data$Agente=="BC"&data$data$Nombre=="Cta.Cte.USD"] <- -1*monto
    data$data$Variacion[data$data$Agente=="BC"&data$data$Nombre=="Reservas"] <- -1*monto
    
    
    data$data$Variacion[data$data$Nombre=="Base Monetaria"] <- sum(data$data$Variacion[data$data$Agente=="BC"&
                                                                                         data$data$Nombre %in% c("Circulante","Cta.Cte.")])
    data$data$Variacion[data$data$Nombre=="M2"] <- data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data$data$Variacion[data$data$Nombre=="M3"] <- data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Plazo.Fijo"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data$data$Variacion[data$data$Nombre=="M3.Privado"] <- data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Plazo.Fijo"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data <- ajustar_resultado(data)
    data <- ajustar_usd(data)
    
    data$data$VarAcum <- data$data$VarAcum + data$data$Variacion
    data$data$ValorFinal <- data$data$ValorInicial + data$data$VarAcum
    
    data$data$Color <- ifelse(data$data$Variacion==0,"white",
                              ifelse(data$data$Variacion>0,"green","red"))
    data$data$Color2 <- ifelse(data$data$VarAcum==0,"white",
                               ifelse(data$data$VarAcum>0,"green","red"))
    
    if(data$data$ValorFinal[data$data$Agente=="SF"&data$data$Nombre=="Circulante"]<0) {
      data$text <- "El Sector Financiero tiene el Circulante en negativo. Consiga liquidez,
      por favor."
    } else {data$text <- ""}
    
  }
  data
}

tiempo <- function(porcentajes) {
  
    data$data$Variacion <- 0
    
    data$data$Variacion[data$data$Agente=="BC"&data$data$Nombre=="Reservas"] <- round((porcentajes[1]/100)*
      data$data$ValorFinal[data$data$Agente=="BC"&data$data$Nombre=="Reservas"])
    data$data$Variacion[data$data$Agente=="BC"&data$data$Nombre=="Tit.Pub.Pesos"] <- round((porcentajes[2]/100)*
                                                                                        data$data$ValorFinal[data$data$Agente=="BC"&data$data$Nombre=="Tit.Pub.Pesos"])
    data$data$Variacion[data$data$Agente=="BC"&data$data$Nombre=="Tit.Pub.USD"] <- round(((1+porcentajes[3]/100)*(1+porcentajes[1]/100)*
                                                                                             data$data$ValorFinal[data$data$Agente=="BC"&data$data$Nombre=="Tit.Pub.USD"])-
                                                                                           data$data$ValorFinal[data$data$Agente=="BC"&data$data$Nombre=="Tit.Pub.USD"])
    data$data$Variacion[data$data$Agente=="BC"&data$data$Nombre=="LELIQ"] <- round((porcentajes[2]/100)*
                                                                                           data$data$ValorFinal[data$data$Agente=="BC"&data$data$Nombre=="LELIQ"])
    data$data$Variacion[data$data$Agente=="BC"&data$data$Nombre=="Cta.Cte.USD"] <- round((porcentajes[1]/100)*
                                                                                           data$data$ValorFinal[data$data$Agente=="BC"&data$data$Nombre=="Cta.Cte.USD"])
    data$data$Variacion[data$data$Agente=="BC"&data$data$Nombre=="LEBAC"] <- round((porcentajes[2]/100)*
                                                                                           data$data$ValorFinal[data$data$Agente=="BC"&data$data$Nombre=="LEBAC"])
    data$data$Variacion[data$data$Agente=="T"&data$data$Nombre=="Dep.USD"] <- round((porcentajes[1]/100)*
                                                                                     data$data$ValorFinal[data$data$Agente=="T"&data$data$Nombre=="Dep.USD"])
    data$data$Variacion[data$data$Agente=="T"&data$data$Nombre=="Plazo.Fijo"] <- round((porcentajes[2]/100)*
                                                                                      data$data$ValorFinal[data$data$Agente=="T"&data$data$Nombre=="Plazo.Fijo"])
    data$data$Variacion[data$data$Agente=="T"&data$data$Nombre=="Tit.Pub.Pesos"] <- round((porcentajes[2]/100)*
                                                                                      data$data$ValorFinal[data$data$Agente=="T"&data$data$Nombre=="Tit.Pub.Pesos"])
    data$data$Variacion[data$data$Agente=="T"&data$data$Nombre=="Tit.Pub.USD"] <- round(((1+porcentajes[3]/100)*(1+porcentajes[1]/100)*
                                                                                       data$data$ValorFinal[data$data$Agente=="T"&data$data$Nombre=="Tit.Pub.USD"])-
                                                                                      data$data$ValorFinal[data$data$Agente=="T"&data$data$Nombre=="Tit.Pub.USD"])
    data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="EM_LELIQ"] <- round((porcentajes[2]/100)*
                                                                                            data$data$ValorFinal[data$data$Agente=="SF"&data$data$Nombre=="EM_LELIQ"])
    data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="LELIQ"] <- round((porcentajes[2]/100)*
                                                                                        data$data$ValorFinal[data$data$Agente=="SF"&data$data$Nombre=="LELIQ"])
    data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Prestamos"] <- round((porcentajes[2]/100)*
                                                                                        data$data$ValorFinal[data$data$Agente=="SF"&data$data$Nombre=="Prestamos"])
    data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="EM_Tit.Pub.Pesos"] <- round((porcentajes[2]/100)*
                                                                                        data$data$ValorFinal[data$data$Agente=="SF"&data$data$Nombre=="EM_Tit.Pub.Pesos"])
    data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Tit.Pub.Pesos"] <- round((porcentajes[2]/100)*
                                                                                        data$data$ValorFinal[data$data$Agente=="SF"&data$data$Nombre=="Tit.Pub.Pesos"])
    data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Plazo.Fijo"] <- round((porcentajes[2]/100)*
                                                                                        data$data$ValorFinal[data$data$Agente=="SF"&data$data$Nombre=="Plazo.Fijo"])
    data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="EM_Cta.Cte.USD"] <- round((porcentajes[1]/100)*
                                                                                        data$data$ValorFinal[data$data$Agente=="SF"&data$data$Nombre=="EM_Cta.Cte.USD"])
    data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Cta.Cte.USD"] <- round((porcentajes[1]/100)*
                                                                                        data$data$ValorFinal[data$data$Agente=="SF"&data$data$Nombre=="Cta.Cte.USD"])
    data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="USD"] <- round((porcentajes[1]/100)*
                                                                                        data$data$ValorFinal[data$data$Agente=="SF"&data$data$Nombre=="USD"])
    data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.USD"] <- round((porcentajes[1]/100)*
                                                                                        data$data$ValorFinal[data$data$Agente=="SF"&data$data$Nombre=="Dep.USD"])
    data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Dep.USD"] <- round((porcentajes[1]/100)*
                                                                                       data$data$ValorFinal[data$data$Agente=="H"&data$data$Nombre=="Dep.USD"])
    data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="USD"] <- round((porcentajes[1]/100)*
                                                                                      data$data$ValorFinal[data$data$Agente=="H"&data$data$Nombre=="USD"])
    data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Plazo.Fijo"] <- round((porcentajes[2]/100)*
                                                                                      data$data$ValorFinal[data$data$Agente=="H"&data$data$Nombre=="Plazo.Fijo"])
    data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Tit.Pub.Pesos"] <- round((porcentajes[2]/100)*
                                                                                      data$data$ValorFinal[data$data$Agente=="H"&data$data$Nombre=="Tit.Pub.Pesos"])
    data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="LEBAC"] <- round((porcentajes[2]/100)*
                                                                                      data$data$ValorFinal[data$data$Agente=="H"&data$data$Nombre=="LEBAC"])
    data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Prestamos"] <- round((porcentajes[2]/100)*
                                                                                      data$data$ValorFinal[data$data$Agente=="H"&data$data$Nombre=="Prestamos"])
    data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Tit.Pub.USD"] <- round(((1+porcentajes[3]/100)*(1+porcentajes[1]/100)*
                                                                                           data$data$ValorFinal[data$data$Agente=="H"&data$data$Nombre=="Tit.Pub.USD"])-
                                                                                          data$data$ValorFinal[data$data$Agente=="H"&data$data$Nombre=="Tit.Pub.USD"])
    data$data$Variacion[data$data$Agente=="RM"&data$data$Nombre=="LEBAC"] <- round((porcentajes[2]/100)*
                                                                                        data$data$ValorFinal[data$data$Agente=="RM"&data$data$Nombre=="LEBAC"])
    data$data$Variacion[data$data$Agente=="RM"&data$data$Nombre=="Tit.Pub.Pesos"] <- round((porcentajes[2]/100)*
                                                                                     data$data$ValorFinal[data$data$Agente=="RM"&data$data$Nombre=="Tit.Pub.Pesos"])
    data$data$Variacion[data$data$Agente=="RM"&data$data$Nombre=="Tit.Pub.USD"] <- round(((1+porcentajes[3]/100)*(1+porcentajes[1]/100)*
                                                                                            data$data$ValorFinal[data$data$Agente=="RM"&data$data$Nombre=="Tit.Pub.USD"])-
                                                                                           data$data$ValorFinal[data$data$Agente=="RM"&data$data$Nombre=="Tit.Pub.USD"])
    data$data$Variacion[data$data$Agente=="RM"&data$data$Nombre=="Divisas"] <- round((porcentajes[1]/100)*
                                                                                     data$data$ValorFinal[data$data$Agente=="RM"&data$data$Nombre=="Divisas"])
    data$tc <- data$tc*(1+porcentajes[1]/100)
    
    
    data$data$Variacion[data$data$Nombre=="Base Monetaria"] <- sum(data$data$Variacion[data$data$Agente=="BC"&
                                                                                         data$data$Nombre %in% c("Circulante","Cta.Cte.")])
    data$data$Variacion[data$data$Nombre=="M2"] <- data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data$data$Variacion[data$data$Nombre=="M3"] <- data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="SF"&data$data$Nombre=="Plazo.Fijo"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data$data$Variacion[data$data$Nombre=="M3.Privado"] <- data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Dep.Vista"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Plazo.Fijo"] +
      data$data$Variacion[data$data$Agente=="H"&data$data$Nombre=="Circulante"]
    
    data <- ajustar_resultado(data)
    data <- ajustar_usd(data)
    
    data$data$VarAcum <- data$data$VarAcum + data$data$Variacion
    data$data$ValorFinal <- data$data$ValorInicial + data$data$VarAcum
    
    data$data$Color <- ifelse(data$data$Variacion==0,"white",
                              ifelse(data$data$Variacion>0,"green","red"))
    data$data$Color2 <- ifelse(data$data$VarAcum==0,"white",
                               ifelse(data$data$VarAcum>0,"green","red"))
    
    data$text <- ""
    
  data
}