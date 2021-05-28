library(ggplot2)

# coordenadas
ancho <- 8
largo <- 10
nombre_a1 <- 0
nombre_p1 <- ancho/2+0.1
nombre_a2 <- ancho+1
nombre_p2 <- 3*ancho/2+1+.1
nombre_a3 <- 2*ancho+2
nombre_p3 <- 5*ancho/2+2.1
valor_a1 <- nombre_a1+ancho/4
valor_p1 <- nombre_p1+ancho/4
valor_a2 <- nombre_a2+ancho/4
valor_p2 <- nombre_p2+ancho/4
valor_a3 <- nombre_a3+ancho/4
valor_p3 <- nombre_p3+ancho/4
var_a1 <- valor_a1+3*ancho/32
var_p1 <- valor_p1+3*ancho/32
var_a2 <- valor_a2+3*ancho/32
var_p2 <- valor_p2+3*ancho/32
var_a3 <- valor_a3+3*ancho/32
var_p3 <- valor_p3+3*ancho/32
tot_a1 <- nombre_a1+7*ancho/16-.1
tot_p1 <- nombre_p1+7*ancho/16
tot_a2 <- nombre_a2+7*ancho/16-.1
tot_p2 <- nombre_p2+7*ancho/16
tot_a3 <- nombre_a3+7*ancho/16-.1
tot_p3 <- nombre_p3+7*ancho/16


balancesheet <- function(data,input1=NA,input2=NA,monto=NA) {

  # actualizar data
  
  if(is.na(input1)) {

    data$data$ValorFinal <- data$data$ValorInicial
    data$data$Variacion <- 0
    data$data$VarAcum <- 0
    data$data$Color <- "white"
    data$data$Color2 <- "white"
    data$text <- ""

  } else if(input1=="Adelantos Transitorios") {

    if(input2=="Otorgar") {
      data <<- emision_at(monto)
    } else if(input2=="Cancelar") {
      data <<- cancelar_at(monto)
    }

  } else if(input1=="Gasto e Impuestos") {

    if(input2=="Gasto P\u00FAblico") {
      data <<- gasto(monto)
    } else if(input2=="Cobrar Impuestos a SF") {
      data <<- impuestos_sf(monto)
    } else if(input2=="Cobrar Impuestos a SPnF") {
      data <<- impuestos_sp(monto)
    }

  } else if(input1=="Leliq") {

    if(input2=="Suscribir") {
      data <<- emision_le(monto)
    } else if(input2=="Rescatar") {
      data <<- cancelar_le(monto)
    } 

  }  else if(input1=="Pr\u00E9stamos") {
    
    if(input2=="Otorgar") {
      data <<- prestamos_dar(monto)
    } else if(input2=="Cancelar") {
      data <<- prestamos_can(monto)
    }
    
  } else if(input1=="Balanza Comercial") {
    
    if(input2=="Super\u00E1vit") {
      data <<- trade_sup(monto)
    } else if(input2=="D\u00E9ficit") {
      data <<- trade_def(monto)
    }
    
  } else if(input1=="Reservas") {
    
    if(input2=="Comprar al SPnF") {
      data <<- res_c_sp(monto)
    } else if(input2=="Comprar al SF") {
      data <<- res_c_sf(monto)
    } else if(input2=="Comprar al G") {
      data <<- res_c_g(monto)
    } else if(input2=="Vender al SPnF") {
      data <<- res_v_sp(monto)
    } else if(input2=="Vender al SF") {
      data <<- res_v_sf(monto)
    } else if(input2=="Vender al G") {
      data <<- res_v_g(monto)
    }
    
  } else if(input1=="Dep\u00F3sitos") {
    
    if(input2=="Depositar Pesos") {
      data <<- dep_p(monto)
    } else if(input2=="Depositar USD") {
      data <<- dep_USD(monto)
    } else if(input2=="Extraer Pesos") {
      data <<- extraer_p(monto)
    } else if(input2=="Extraer USD") {
      data <<- extraer_USD(monto)
    } else if(input2=="Constituir Plazo Fijo") {
      data <<- PF_hacer(monto)
    } else if(input2=="Cancelar Plazo Fijo") {
      data <<- PF_cancelar(monto)
    } 
    
  } else if(input1=="LEBAC (*)") {
    
    if(input2=="Suscribir con SPnF") {
      data <<- lebac_sp_sus(monto)
    } else if(input2=="Suscribir con RM") {
      data <<- lebac_rm_sus(monto)
    } else if(input2=="Cancelar con SPnF") {
      data <<- lebac_sp_can(monto)
    } else if(input2=="Cancelar con RM") {
      data <<- lebac_rm_can(monto)
    }  
    
  } else if(input1=="Emitir Tit.Pub.") {
    
    if(input2=="En pesos con SPnF") {
      data <<- tp_em_p_sp(monto)
    } else if(input2=="En pesos con SF") {
      data <<- tp_em_p_sf(monto)
    } else if(input2=="En pesos con RM") {
      data <<- tp_em_p_rm(monto)
    } else if(input2=="En USD con SPnF") {
      data <<- tp_em_d_sp(monto)
    } else if(input2=="En USD con RM") {
      data <<- tp_em_d_rm(monto)
    }
    
  } else if(input1=="Cancelar Tit.Pub.") {
    
    if(input2=="En pesos con SPnF") {
      data <<- tp_c_p_sp(monto)
    } else if(input2=="En pesos con SF") {
      data <<- tp_c_p_sf(monto)
    } else if(input2=="En pesos con RM") {
      data <<- tp_c_p_rm(monto)
    } else if(input2=="En USD con SPnF") {
      data <<- tp_c_d_sp(monto)
    } else if(input2=="En USD con RM") {
      data <<- tp_c_d_rm(monto)
    }
    
  } else if(input1=="Operar Tit.Pub.Pesos") {
    
    if(input2=="BC compra al SF") {
      data <<- tp_o_p_bc_sf(monto)
    } else if(input2=="BC compra al SPnF") {
      data <<- tp_o_p_bc_sp(monto)
    } else if(input2=="BC compra al RM") {
      data <<- tp_o_p_bc_rm(monto)
    } else if(input2=="SF compra al BC") {
      data <<- tp_o_p_sf_bc(monto)
    } else if(input2=="SF compra al SPnF") {
      data <<- tp_o_p_sf_sp(monto)
    } else if(input2=="SF compra al RM") {
      data <<- tp_o_p_sf_rm(monto)
    } else if(input2=="SPnf compra al BC") {
      data <<- tp_o_p_sp_bc(monto)
    } else if(input2=="SPnf compra al SF") {
      data <<- tp_o_p_sp_sf(monto)
    } else if(input2=="SPnf compra al RM") {
      data <<- tp_o_p_sp_rm(monto)
    } else if(input2=="RM compra al BC") {
      data <<- tp_o_p_rm_bc(monto)
    } else if(input2=="RM compra al SF") {
      data <<- tp_o_p_rm_sf(monto)
    } else if(input2=="RM compra al SPnF") {
      data <<- tp_o_p_rm_sp(monto)
    }
    
  } else if(input1=="Operar Tit.Pub.USD") {
    
    if(input2=="BC compra al SPnF") {
      data <<- tp_o_d_bc_sp(monto)
    } else if(input2=="BC compra al RM") {
      data <<- tp_o_d_bc_rm(monto)
    } else if(input2=="SPnF compra al BC") {
      data <<- tp_o_d_sp_bc(monto)
    } else if(input2=="SPnF compra al RM") {
      data <<- tp_o_d_sp_rm(monto)
    } else if(input2=="RM compra al BC") {
      data <<- tp_o_d_rm_bc(monto)
    } else if(input2=="RM compra al SPnF") {
      data <<- tp_o_d_rm_sp(monto)
    }
    
  } else if(input1=="Efectivo Minimo") {
    
    if(input2=="Sustituir LELIQ por Bono") {
      data <<- em_l_b(monto)
    } 
    
  }

  # gráfico
  
  grafico <- ggplot() +

    # T
    geom_segment(aes(x=0,y=0,xend=ancho,yend=0),size=1) + #horizontal1
    geom_segment(aes(x=ancho+1,y=0,xend=2*ancho+1,yend=0),size=1) + #horizontal2
    geom_segment(aes(x=2*ancho+2,y=0,xend=3*ancho+2,yend=0),size=1) + #horizontal3
    geom_segment(aes(x=0,y=-largo-2,xend=ancho,yend=-largo-2),size=1) + #horizontal4
    geom_segment(aes(x=ancho+1,y=-largo-2,xend=2*ancho+1,yend=-largo-2),size=1) + #horizontal5
    geom_segment(aes(x=ancho/2,y=1,xend=ancho/2,yend=1-largo),size=1) + #vertical1
    geom_segment(aes(x=3*ancho/2+1,y=1,xend=3*ancho/2+1,yend=1-largo),size=1) + #vertical2
    geom_segment(aes(x=5*ancho/2+2,y=1,xend=5*ancho/2+2,yend=1-largo-4),size=1) + #vertical3
    geom_segment(aes(x=ancho/2,y=-largo-1,xend=ancho/2,yend=-2*largo-1),size=1) + #vertical4
    geom_segment(aes(x=3*ancho/2+1,y=-largo-1,xend=3*ancho/2+1,yend=-2*largo-1),size=1) + #vertical5
    geom_segment(aes(x=mean(var_a1,tot_a1)+0.25,y=0,xend=mean(var_a1,tot_a1)+0.25,yend=1-largo),size=0.5, linetype="dotted") + 
    geom_segment(aes(x=mean(var_p1,tot_p1)+0.25,y=0,xend=mean(var_p1,tot_p1)+0.25,yend=1-largo),size=0.5, linetype="dotted") +
    geom_segment(aes(x=mean(var_a2,tot_a2)+0.25,y=0,xend=mean(var_a2,tot_a2)+0.25,yend=1-largo),size=0.5, linetype="dotted") +
    geom_segment(aes(x=mean(var_p2,tot_p2)+0.25,y=0,xend=mean(var_p2,tot_p2)+0.25,yend=1-largo),size=0.5, linetype="dotted") +
    geom_segment(aes(x=mean(var_a3,tot_a3)+0.25,y=0,xend=mean(var_a3,tot_a3)+0.25,yend=1-largo-4),size=0.5, linetype="dotted") +
    geom_segment(aes(x=mean(var_p3,tot_p3)+0.25,y=0,xend=mean(var_p3,tot_p3)+0.25,yend=1-largo-4),size=0.5, linetype="dotted") +
    geom_segment(aes(x=mean(var_a1,tot_a1)+0.25,y=-largo-2,xend=mean(var_a1,tot_a1)+0.25,yend=-2*largo-1),size=0.5, linetype="dotted") + 
    geom_segment(aes(x=mean(var_p1,tot_p1)+0.25,y=-largo-2,xend=mean(var_p1,tot_p1)+0.25,yend=-2*largo-1),size=0.5, linetype="dotted") +
    geom_segment(aes(x=mean(var_a2,tot_a2)+0.25,y=-largo-2,xend=mean(var_a2,tot_a2)+0.25,yend=-2*largo-1),size=0.5, linetype="dotted") +
    geom_segment(aes(x=mean(var_p2,tot_p2)+0.25,y=-largo-2,xend=mean(var_p2,tot_p2)+0.25,yend=-2*largo-1),size=0.5, linetype="dotted") +

    # Títulos
    geom_label(aes(x=ancho/2, y=2, label="Banco Central"), size=5) + #titulo1
    geom_label(aes(x=3*ancho/2+1, y=2, label="Gobierno"), size=5) + #titulo2
    geom_label(aes(x=5*ancho/2+2, y=2, label="Sector Financiero"), size=5) + #titulo3
    geom_label(aes(x=ancho/2, y=-largo, label="Sector Privado no Financiero"), size=5) + #titulo4
    geom_label(aes(x=3*ancho/2+1, y=-largo, label="Resto del Mundo"), size=5) + #titulo5
    geom_text(aes(x=ancho/4,y=0.5,label="A")) + #activo1
    geom_text(aes(x=5*ancho/4+1,y=0.5,label="A")) + #activo2
    geom_text(aes(x=9*ancho/4+2,y=0.5,label="A")) + #activo3
    geom_text(aes(x=ancho/4,y=-largo-1.5,label="A")) + #activo4
    geom_text(aes(x=5*ancho/4+1,y=-largo-1.5,label="A")) + #activo5
    geom_text(aes(x=3*ancho/4,y=0.5,label="P")) + #pasivo1
    geom_text(aes(x=7*ancho/4+1,y=0.5,label="P")) + #pasivo2
    geom_text(aes(x=11*ancho/4+2,y=0.5,label="P")) + #pasivo3
    geom_text(aes(x=3*ancho/4,y=-largo-1.5,label="P")) + #pasivo4
    geom_text(aes(x=7*ancho/4+1,y=-largo-1.5,label="P")) + #pasivo5
    geom_text(aes(x=var_a1,y=-0.5,label="Var.")) +
    geom_text(aes(x=var_a2,y=-0.5,label="Var.")) +
    geom_text(aes(x=var_a3,y=-0.5,label="Var.")) +
    geom_text(aes(x=var_p1,y=-0.5,label="Var.")) +
    geom_text(aes(x=var_p2,y=-0.5,label="Var.")) +
    geom_text(aes(x=var_p3,y=-0.5,label="Var.")) +
    geom_text(aes(x=var_a1,y=-largo-2.5,label="Var.")) +
    geom_text(aes(x=var_a2,y=-largo-2.5,label="Var.")) +
    geom_text(aes(x=var_p1,y=-largo-2.5,label="Var.")) +
    geom_text(aes(x=var_p2,y=-largo-2.5,label="Var.")) +
    geom_text(aes(x=nombre_p3+1,y=-largo-4.5,label="Var.")) +

    geom_text(aes(x=tot_a1,y=-0.5,label="Acum.")) +
    geom_text(aes(x=tot_a2,y=-0.5,label="Acum.")) +
    geom_text(aes(x=tot_a3,y=-0.5,label="Acum.")) +
    geom_text(aes(x=tot_p1,y=-0.5,label="Acum.")) +
    geom_text(aes(x=tot_p2,y=-0.5,label="Acum.")) +
    geom_text(aes(x=tot_p3,y=-0.5,label="Acum.")) +
    geom_text(aes(x=tot_a1,y=-largo-2.5,label="Acum.")) +
    geom_text(aes(x=tot_a2,y=-largo-2.5,label="Acum.")) +
    geom_text(aes(x=tot_p1,y=-largo-2.5,label="Acum.")) +
    geom_text(aes(x=tot_p2,y=-largo-2.5,label="Acum.")) +
    geom_text(aes(x=nombre_p3+2,y=-largo-4.5,label="Acum.")) +
    
    # Mensajes
    geom_text(aes(x=2*ancho/2,y=-ancho-10,label=data$text),color="red",size=5) +

    # Agregados
    geom_text(aes(x=nombre_a3,y=-ancho-7.5,label="BASE MONETARIA ="),hjust=0) +
    geom_text(aes(x=tot_a3,y=-ancho-7.5,label=data$data$ValorFinal[data$data$Nombre=="Base Monetaria"])) +
    geom_text(aes(x=nombre_p3+1,y=-ancho-7.5,label=data$data$Variacion[data$data$Nombre=="Base Monetaria"]),
              color=data$data$Color[data$data$Nombre=="Base Monetaria"]) +
    geom_text(aes(x=nombre_p3+2,y=-ancho-7.5,label=data$data$VarAcum[data$data$Nombre=="Base Monetaria"]),
              color=data$data$Color2[data$data$Nombre=="Base Monetaria"]) +
    geom_text(aes(x=nombre_a3,y=-ancho-8.5,label="M2 ="),hjust=0) +
    geom_text(aes(x=tot_a3,y=-ancho-8.5,label=data$data$ValorFinal[data$data$Nombre=="M2"])) +
    geom_text(aes(x=nombre_p3+1,y=-ancho-8.5,label=data$data$Variacion[data$data$Nombre=="M2"]),
              color=data$data$Color[data$data$Nombre=="M2"]) +
    geom_text(aes(x=nombre_p3+2,y=-ancho-8.5,label=data$data$VarAcum[data$data$Nombre=="M2"]),
              color=data$data$Color2[data$data$Nombre=="M2"]) +
    geom_text(aes(x=nombre_a3,y=-ancho-10.5,label="M3 ="),hjust=0) +
    geom_text(aes(x=tot_a3,y=-ancho-10.5,label=data$data$ValorFinal[data$data$Nombre=="M3"])) +
    geom_text(aes(x=nombre_p3+1,y=-ancho-10.5,label=data$data$Variacion[data$data$Nombre=="M3"]),
              color=data$data$Color[data$data$Nombre=="M3"]) +
    geom_text(aes(x=nombre_p3+2,y=-ancho-10.5,label=data$data$VarAcum[data$data$Nombre=="M3"]),
              color=data$data$Color2[data$data$Nombre=="M3"]) +
    geom_text(aes(x=nombre_a3,y=-ancho-9.5,label="M3 Privado ="),hjust=0) +
    geom_text(aes(x=tot_a3,y=-ancho-9.5,label=data$data$ValorFinal[data$data$Nombre=="M3.Privado"])) +
    geom_text(aes(x=nombre_p3+1,y=-ancho-9.5,label=data$data$Variacion[data$data$Nombre=="M3.Privado"]),
              color=data$data$Color[data$data$Nombre=="M3.Privado"]) +
    geom_text(aes(x=nombre_p3+2,y=-ancho-9.5,label=data$data$VarAcum[data$data$Nombre=="M3.Privado"]),
              color=data$data$Color2[data$data$Nombre=="M3.Privado"]) +

    # BCRA
    #bcra_activo1
    geom_text(aes(x=nombre_a1,y=-1.5,label="Reservas"),size = 4,hjust=0) +
    geom_text(aes(x=tot_a1,y=-1.5,label=data$data$VarAcum[data$data$Nombre=="Reservas"]),size = 4,
              color=data$data$Color2[data$data$Nombre=="Reservas"]) +
    geom_text(aes(x=valor_a1,y=-1.5,label=data$data$ValorFinal[data$data$Nombre=="Reservas"]),size = 4,hjust=1) +
    geom_text(aes(x=var_a1,y=-1.5,label=data$data$Variacion[data$data$Nombre=="Reservas"]),size = 4,
              color=data$data$Color[data$data$Nombre=="Reservas"]) +

    #bcra_activo2
    geom_text(aes(x=nombre_a1,y=-2.5,label="ATs"),size = 4,hjust=0) +
    geom_text(aes(x=tot_a1,y=-2.5,label=data$data$VarAcum[data$data$Nombre=="ATs"&data$data$Agente=="BC"]),
              size = 4,color=data$data$Color2[data$data$Nombre=="ATs"&data$data$Agente=="BC"]) +
    geom_text(aes(x=valor_a1,y=-2.5,label=data$data$ValorFinal[data$data$Nombre=="ATs"&data$data$Agente=="BC"]),size = 4,hjust=1) +
    geom_text(aes(x=var_a1,y=-2.5,label=data$data$Variacion[data$data$Nombre=="ATs"&data$data$Agente=="BC"]),size = 4,
              color=data$data$Color[data$data$Nombre=="ATs"&data$data$Agente=="BC"]) +
    
    #bcra_activo3
    geom_text(aes(x=nombre_a1,y=-3.5,label="Tit.Pesos (**)"),size = 4,hjust=0) +
    geom_text(aes(x=tot_a1,y=-3.5,label=data$data$VarAcum[data$data$Nombre=="Tit.Pub.Pesos"&data$data$Agente=="BC"]),
              size = 4,color=data$data$Color2[data$data$Nombre=="Tit.Pub.Pesos"&data$data$Agente=="BC"]) +
    geom_text(aes(x=valor_a1,y=-3.5,label=data$data$ValorFinal[data$data$Nombre=="Tit.Pub.Pesos"&data$data$Agente=="BC"]),size = 4,hjust=1) +
    geom_text(aes(x=var_a1,y=-3.5,label=data$data$Variacion[data$data$Nombre=="Tit.Pub.Pesos"&data$data$Agente=="BC"]),size = 4,
              color=data$data$Color[data$data$Nombre=="Tit.Pub.Pesos"&data$data$Agente=="BC"]) +
    
    #bcra_activo4
    geom_text(aes(x=nombre_a1,y=-4.5,label="Tit.USD (**)"),size = 4,hjust=0) +
    geom_text(aes(x=tot_a1,y=-4.5,label=data$data$VarAcum[data$data$Nombre=="Tit.Pub.USD"&data$data$Agente=="BC"]),
              size = 4,color=data$data$Color2[data$data$Nombre=="Tit.Pub.USD"&data$data$Agente=="BC"]) +
    geom_text(aes(x=valor_a1,y=-4.5,label=data$data$ValorFinal[data$data$Nombre=="Tit.Pub.USD"&data$data$Agente=="BC"]),size = 4,hjust=1) +
    geom_text(aes(x=var_a1,y=-4.5,label=data$data$Variacion[data$data$Nombre=="Tit.Pub.USD"&data$data$Agente=="BC"]),size = 4,
              color=data$data$Color[data$data$Nombre=="Tit.Pub.USD"&data$data$Agente=="BC"]) +

    geom_text(aes(x=nombre_p1,y=-1.5,label="Base Monetaria"),size = 4,hjust=0) +
    #bcra_pasivo1
    geom_text(aes(x=nombre_p1,y=-2.5,label="  Circulante"),size = 3,hjust=0) +
    geom_text(aes(x=tot_p1,y=-2.5,label=data$data$VarAcum[data$data$Nombre=="Circulante"&data$data$Agente=="BC"]),
              size = 3,color=data$data$Color2[data$data$Nombre=="Circulante"&data$data$Agente=="BC"]) +
    geom_text(aes(x=valor_p1,y=-2.5,label=data$data$ValorFinal[data$data$Nombre=="Circulante"&data$data$Agente=="BC"]),size = 3,hjust=1) +
    geom_text(aes(x=var_p1,y=-2.5,label=data$data$Variacion[data$data$Nombre=="Circulante"&data$data$Agente=="BC"]),size = 3,
              color=data$data$Color[data$data$Nombre=="Circulante"&data$data$Agente=="BC"]) +

    #bcra_pasivo2
    geom_text(aes(x=nombre_p1,y=-3.5,label="  Cta.Cte."),size = 3,hjust=0) +
    geom_text(aes(x=tot_p1,y=-3.5,label=data$data$VarAcum[data$data$Nombre=="Cta.Cte."&data$data$Agente=="BC"]),
              size = 3,color=data$data$Color2[data$data$Nombre=="Cta.Cte."&data$data$Agente=="BC"]) +
    geom_text(aes(x=valor_p1,y=-3.5,label=data$data$ValorFinal[data$data$Nombre=="Cta.Cte."&data$data$Agente=="BC"]),size = 3,hjust=1) +
    geom_text(aes(x=var_p1,y=-3.5,label=data$data$Variacion[data$data$Nombre=="Cta.Cte."&data$data$Agente=="BC"]),size = 3,
              color=data$data$Color[data$data$Nombre=="Cta.Cte."&data$data$Agente=="BC"]) +

    #pintar base monetaria
    geom_polygon(aes(x=c(ancho/2,ancho+0.2,ancho+0.2,ancho/2),y=c(-1,-1,-4,-4)),fill="lightgrey",alpha=0.3) +

    #bcra_pasivo3
    geom_text(aes(x=nombre_p1,y=-4.5,label="Dep.Tesoro"),size = 4,hjust=0) +
    geom_text(aes(x=tot_p1,y=-4.5,label=data$data$VarAcum[data$data$Nombre=="Dep.Tesoro"&data$data$Agente=="BC"]),
              size = 4,color=data$data$Color2[data$data$Nombre=="Dep.Tesoro"&data$data$Agente=="BC"]) +
    geom_text(aes(x=valor_p1,y=-4.5,label=data$data$ValorFinal[data$data$Nombre=="Dep.Tesoro"&data$data$Agente=="BC"]),size = 4,hjust=1) +
    geom_text(aes(x=var_p1,y=-4.5,label=data$data$Variacion[data$data$Nombre=="Dep.Tesoro"&data$data$Agente=="BC"]),size = 4,
              color=data$data$Color[data$data$Nombre=="Dep.Tesoro"&data$data$Agente=="BC"]) +

    #bcra_pasivo4
    geom_text(aes(x=nombre_p1,y=-5.5,label="LELIQ/Pases"),size = 4,hjust=0) +
    geom_text(aes(x=tot_p1,y=-5.5,label=data$data$VarAcum[data$data$Nombre=="LELIQ"&data$data$Agente=="BC"]),
              size = 4,color=data$data$Color2[data$data$Nombre=="LELIQ"&data$data$Agente=="BC"]) +
    geom_text(aes(x=valor_p1,y=-5.5,label=data$data$ValorFinal[data$data$Nombre=="LELIQ"&data$data$Agente=="BC"]),size = 4,hjust=1) +
    geom_text(aes(x=var_p1,y=-5.5,label=data$data$Variacion[data$data$Nombre=="LELIQ"&data$data$Agente=="BC"]),size = 4,
              color=data$data$Color[data$data$Nombre=="LELIQ"&data$data$Agente=="BC"]) +
    
    #bcra_pasivo5
    geom_text(aes(x=nombre_p1,y=-6.5,label="Cta.Cte.USD"),size = 4,hjust=0) +
    geom_text(aes(x=tot_p1,y=-6.5,label=data$data$VarAcum[data$data$Nombre=="Cta.Cte.USD"&data$data$Agente=="BC"]),
              size = 4,color=data$data$Color2[data$data$Nombre=="Cta.Cte.USD"&data$data$Agente=="BC"]) +
    geom_text(aes(x=valor_p1,y=-6.5,label=data$data$ValorFinal[data$data$Nombre=="Cta.Cte.USD"&data$data$Agente=="BC"]),size = 4,hjust=1) +
    geom_text(aes(x=var_p1,y=-6.5,label=data$data$Variacion[data$data$Nombre=="Cta.Cte.USD"&data$data$Agente=="BC"]),size = 4,
              color=data$data$Color[data$data$Nombre=="Cta.Cte.USD"&data$data$Agente=="BC"]) +
    
    #bcra_pasivo6
    geom_text(aes(x=nombre_p1,y=-7.5,label="LEBAC (*)"),size = 4,hjust=0) +
    geom_text(aes(x=tot_p1,y=-7.5,label=data$data$VarAcum[data$data$Nombre=="LEBAC"&data$data$Agente=="BC"]),
              size = 4,color=data$data$Color2[data$data$Nombre=="LEBAC"&data$data$Agente=="BC"]) +
    geom_text(aes(x=valor_p1,y=-7.5,label=data$data$ValorFinal[data$data$Nombre=="LEBAC"&data$data$Agente=="BC"]),size = 4,hjust=1) +
    geom_text(aes(x=var_p1,y=-7.5,label=data$data$Variacion[data$data$Nombre=="LEBAC"&data$data$Agente=="BC"]),size = 4,
              color=data$data$Color[data$data$Nombre=="LEBAC"&data$data$Agente=="BC"]) +

    # Tesoro
    #Tesoro_activo1
    geom_text(aes(x=nombre_a2,y=-1.5,label="Dep.BCRA"),size = 4,hjust=0) +
    geom_text(aes(x=tot_a2,y=-1.5,label=data$data$VarAcum[data$data$Nombre=="Dep.Tesoro.BCRA"]),size = 4,
              color=data$data$Color2[data$data$Nombre=="Dep.Tesoro.BCRA"]) +
    geom_text(aes(x=valor_a2,y=-1.5,label=data$data$ValorFinal[data$data$Nombre=="Dep.Tesoro.BCRA"]),size = 4,hjust=1) +
    geom_text(aes(x=var_a2,y=-1.5,label=data$data$Variacion[data$data$Nombre=="Dep.Tesoro.BCRA"]),size = 4,
              color=data$data$Color[data$data$Nombre=="Dep.Tesoro.BCRA"]) +

    #Tesoro_activo2
    geom_text(aes(x=nombre_a2,y=-2.5,label="Dep.Vista"),size = 4,hjust=0) +
    geom_text(aes(x=tot_a2,y=-2.5,label=data$data$VarAcum[data$data$Nombre=="Dep.Tesoro.Vista"]),size = 4,
              color=data$data$Color2[data$data$Nombre=="Dep.Tesoro.Vista"]) +
    geom_text(aes(x=valor_a2,y=-2.5,label=data$data$ValorFinal[data$data$Nombre=="Dep.Tesoro.Vista"]),size = 4,hjust=1) +
    geom_text(aes(x=var_a2,y=-2.5,label=data$data$Variacion[data$data$Nombre=="Dep.Tesoro.Vista"]),size = 4,
              color=data$data$Color[data$data$Nombre=="Dep.Tesoro.Vista"]) +

    #Tesoro_activo3
    geom_text(aes(x=nombre_a2,y=-3.5,label="Dep.P\u00FAblicos"),size = 4,hjust=0) +
    geom_text(aes(x=tot_a2,y=-3.5,label=data$data$VarAcum[data$data$Nombre=="Dep.Pub.Vista"]),size = 4,
              color=data$data$Color2[data$data$Nombre=="Dep.Pub.Vista"]) +
    geom_text(aes(x=valor_a2,y=-3.5,label=data$data$ValorFinal[data$data$Nombre=="Dep.Pub.Vista"]),size = 4,hjust=1) +
    geom_text(aes(x=var_a2,y=-3.5,label=data$data$Variacion[data$data$Nombre=="Dep.Pub.Vista"]),size = 4,
              color=data$data$Color[data$data$Nombre=="Dep.Pub.Vista"]) +
    
    #Tesoro_activo4
    geom_text(aes(x=nombre_a2,y=-4.5,label="Plazo.Fijo"),size = 4,hjust=0) +
    geom_text(aes(x=tot_a2,y=-4.5,label=data$data$VarAcum[data$data$Nombre=="Plazo.Fijo"&data$data$Agente=="T"]),size = 4,
              color=data$data$Color2[data$data$Nombre=="Plazo.Fijo"&data$data$Agente=="T"]) +
    geom_text(aes(x=valor_a2,y=-4.5,label=data$data$ValorFinal[data$data$Nombre=="Plazo.Fijo"&data$data$Agente=="T"]),size = 4,hjust=1) +
    geom_text(aes(x=var_a2,y=-4.5,label=data$data$Variacion[data$data$Nombre=="Plazo.Fijo"&data$data$Agente=="T"]),size = 4,
              color=data$data$Color[data$data$Nombre=="Plazo.Fijo"&data$data$Agente=="T"]) +
    
    #Tesoro_activo5
    geom_text(aes(x=nombre_a2,y=-5.5,label="Dep.USD"),size = 4,hjust=0) +
    geom_text(aes(x=tot_a2,y=-5.5,label=data$data$VarAcum[data$data$Nombre=="Dep.USD"&data$data$Agente=="T"]),size = 4,
              color=data$data$Color2[data$data$Nombre=="Dep.USD"&data$data$Agente=="T"]) +
    geom_text(aes(x=valor_a2,y=-5.5,label=data$data$ValorFinal[data$data$Nombre=="Dep.USD"&data$data$Agente=="T"]),size = 4,hjust=1) +
    geom_text(aes(x=var_a2,y=-5.5,label=data$data$Variacion[data$data$Nombre=="Dep.USD"&data$data$Agente=="T"]),size = 4,
              color=data$data$Color[data$data$Nombre=="Dep.USD"&data$data$Agente=="T"]) +

    #Tesoro_pasivo1
    geom_text(aes(x=nombre_p2,y=-1.5,label="ATs"),size = 4,hjust=0) +
    geom_text(aes(x=tot_p2,y=-1.5,label=data$data$VarAcum[data$data$Nombre=="ATs"&data$data$Agente=="T"]),
              size = 4,color=data$data$Color2[data$data$Nombre=="ATs"&data$data$Agente=="T"]) +
    geom_text(aes(x=valor_p2,y=-1.5,label=data$data$ValorFinal[data$data$Nombre=="ATs"&data$data$Agente=="T"]),size = 4,hjust=1) +
    geom_text(aes(x=var_p2,y=-1.5,label=data$data$Variacion[data$data$Nombre=="ATs"&data$data$Agente=="T"]),size = 4,
              color=data$data$Color[data$data$Nombre=="ATs"&data$data$Agente=="T"]) +
    
    #Tesoro_pasivo2
    geom_text(aes(x=nombre_p2,y=-2.5,label="Tit.Pesos (**)"),size = 4,hjust=0) +
    geom_text(aes(x=tot_p2,y=-2.5,label=data$data$VarAcum[data$data$Nombre=="Tit.Pub.Pesos"&data$data$Agente=="T"]),
              size = 4,color=data$data$Color2[data$data$Nombre=="Tit.Pub.Pesos"&data$data$Agente=="T"]) +
    geom_text(aes(x=valor_p2,y=-2.5,label=data$data$ValorFinal[data$data$Nombre=="Tit.Pub.Pesos"&data$data$Agente=="T"]),size = 4,hjust=1) +
    geom_text(aes(x=var_p2,y=-2.5,label=data$data$Variacion[data$data$Nombre=="Tit.Pub.Pesos"&data$data$Agente=="T"]),size = 4,
              color=data$data$Color[data$data$Nombre=="Tit.Pub.Pesos"&data$data$Agente=="T"]) +
    
    #Tesoro_pasivo3
    geom_text(aes(x=nombre_p2,y=-3.5,label="Tit.USD (**)"),size = 4,hjust=0) +
    geom_text(aes(x=tot_p2,y=-3.5,label=data$data$VarAcum[data$data$Nombre=="Tit.Pub.USD"&data$data$Agente=="T"]),
              size = 4,color=data$data$Color2[data$data$Nombre=="Tit.Pub.USD"&data$data$Agente=="T"]) +
    geom_text(aes(x=valor_p2,y=-3.5,label=data$data$ValorFinal[data$data$Nombre=="Tit.Pub.USD"&data$data$Agente=="T"]),size = 4,hjust=1) +
    geom_text(aes(x=var_p2,y=-3.5,label=data$data$Variacion[data$data$Nombre=="Tit.Pub.USD"&data$data$Agente=="T"]),size = 4,
              color=data$data$Color[data$data$Nombre=="Tit.Pub.USD"&data$data$Agente=="T"]) +

    # Sistema Financiero
    geom_text(aes(x=nombre_a3,y=-1.5,label="Efectivo M\u00EDnimo"),size = 4,hjust=0) +
    #SF_activo1
    geom_text(aes(x=nombre_a3,y=-2.5,label="  Cta.Cte."),size = 3,hjust=0) +
    geom_text(aes(x=tot_a3,y=-2.5,label=data$data$VarAcum[data$data$Nombre=="EM_Cta.Cte."&data$data$Agente=="SF"]),size = 3,
              color=data$data$Color2[data$data$Nombre=="EM_Cta.Cte."&data$data$Agente=="SF"]) +
    geom_text(aes(x=valor_a3,y=-2.5,label=data$data$ValorFinal[data$data$Nombre=="EM_Cta.Cte."&data$data$Agente=="SF"]),size = 3,hjust=1) +
    geom_text(aes(x=var_a3,y=-2.5,label=data$data$Variacion[data$data$Nombre=="EM_Cta.Cte."&data$data$Agente=="SF"]),size = 3,
              color=data$data$Color[data$data$Nombre=="EM_Cta.Cte."&data$data$Agente=="SF"]) +
    
    #SF_activo2
    geom_text(aes(x=nombre_a3,y=-3.5,label="  LELIQ"),size = 3,hjust=0) +
    geom_text(aes(x=tot_a3,y=-3.5,label=data$data$VarAcum[data$data$Nombre=="EM_LELIQ"&data$data$Agente=="SF"]),size = 3,
              color=data$data$Color2[data$data$Nombre=="EM_LELIQ"&data$data$Agente=="SF"]) +
    geom_text(aes(x=valor_a3,y=-3.5,label=data$data$ValorFinal[data$data$Nombre=="EM_LELIQ"&data$data$Agente=="SF"]),size = 3,hjust=1) +
    geom_text(aes(x=var_a3,y=-3.5,label=data$data$Variacion[data$data$Nombre=="EM_LELIQ"&data$data$Agente=="SF"]),size = 3,
              color=data$data$Color[data$data$Nombre=="EM_LELIQ"&data$data$Agente=="SF"]) +
    
    #SF_activo3
    geom_text(aes(x=nombre_a3,y=-4.5,label="  Tit.Pesos (**)"),size = 3,hjust=0) +
    geom_text(aes(x=tot_a3,y=-4.5,label=data$data$VarAcum[data$data$Nombre=="EM_Tit.Pub.Pesos"&data$data$Agente=="SF"]),size = 3,
              color=data$data$Color2[data$data$Nombre=="EM_Tit.Pub.Pesos"&data$data$Agente=="SF"]) +
    geom_text(aes(x=valor_a3,y=-4.5,label=data$data$ValorFinal[data$data$Nombre=="EM_Tit.Pub.Pesos"&data$data$Agente=="SF"]),size = 3,hjust=1) +
    geom_text(aes(x=var_a3,y=-4.5,label=data$data$Variacion[data$data$Nombre=="EM_Tit.Pub.Pesos"&data$data$Agente=="SF"]),size = 3,
              color=data$data$Color[data$data$Nombre=="EM_Tit.Pub.Pesos"&data$data$Agente=="SF"]) +
    
    #SF_activo4
    geom_text(aes(x=nombre_a3,y=-5.5,label="  Cta.Cte.USD"),size = 3,hjust=0) +
    geom_text(aes(x=tot_a3,y=-5.5,label=data$data$VarAcum[data$data$Nombre=="EM_Cta.Cte.USD"&data$data$Agente=="SF"]),size = 3,
              color=data$data$Color2[data$data$Nombre=="EM_Cta.Cte.USD"&data$data$Agente=="SF"]) +
    geom_text(aes(x=valor_a3,y=-5.5,label=data$data$ValorFinal[data$data$Nombre=="EM_Cta.Cte.USD"&data$data$Agente=="SF"]),size = 3,hjust=1) +
    geom_text(aes(x=var_a3,y=-5.5,label=data$data$Variacion[data$data$Nombre=="EM_Cta.Cte.USD"&data$data$Agente=="SF"]),size = 3,
              color=data$data$Color[data$data$Nombre=="EM_Cta.Cte.USD"&data$data$Agente=="SF"]) +
    
    #pintar efectivo minimo
    geom_polygon(aes(x=c(nombre_a3,2.5*ancho+2,2.5*ancho+2,nombre_a3),y=c(-1,-1,-6,-6)),fill="lightgrey",alpha=0.3) +
    
    #SF_activo5
    geom_text(aes(x=nombre_a3,y=-6.5,label="Circulante"),size = 4,hjust=0) +
    geom_text(aes(x=tot_a3,y=-6.5,label=data$data$VarAcum[data$data$Nombre=="Circulante"&data$data$Agente=="SF"]),size = 4,
              color=data$data$Color2[data$data$Nombre=="Circulante"&data$data$Agente=="SF"]) +
    geom_text(aes(x=valor_a3,y=-6.5,label=data$data$ValorFinal[data$data$Nombre=="Circulante"&data$data$Agente=="SF"]),size = 4,hjust=1) +
    geom_text(aes(x=var_a3,y=-6.5,label=data$data$Variacion[data$data$Nombre=="Circulante"&data$data$Agente=="SF"]),size = 4,
              color=data$data$Color[data$data$Nombre=="Circulante"&data$data$Agente=="SF"]) +

    #SF_activo6
    geom_text(aes(x=nombre_a3,y=-7.5,label="Cta.Cte."),size = 4,hjust=0) +
    geom_text(aes(x=tot_a3,y=-7.5,label=data$data$VarAcum[data$data$Nombre=="Cta.Cte."&data$data$Agente=="SF"]),size = 4,
              color=data$data$Color2[data$data$Nombre=="Cta.Cte."&data$data$Agente=="SF"]) +
    geom_text(aes(x=valor_a3,y=-7.5,label=data$data$ValorFinal[data$data$Nombre=="Cta.Cte."&data$data$Agente=="SF"]),size = 4,hjust=1) +
    geom_text(aes(x=var_a3,y=-7.5,label=data$data$Variacion[data$data$Nombre=="Cta.Cte."&data$data$Agente=="SF"]),size = 4,
              color=data$data$Color[data$data$Nombre=="Cta.Cte."&data$data$Agente=="SF"]) +

    #SF_activo7
    geom_text(aes(x=nombre_a3,y=-8.5,label="LELIQ/Pases"),size = 4,hjust=0) +
    geom_text(aes(x=tot_a3,y=-8.5,label=data$data$VarAcum[data$data$Nombre=="LELIQ"&data$data$Agente=="SF"]),size = 4,
              color=data$data$Color2[data$data$Nombre=="LELIQ"&data$data$Agente=="SF"]) +
    geom_text(aes(x=valor_a3,y=-8.5,label=data$data$ValorFinal[data$data$Nombre=="LELIQ"&data$data$Agente=="SF"]),size = 4,hjust=1) +
    geom_text(aes(x=var_a3,y=-8.5,label=data$data$Variacion[data$data$Nombre=="LELIQ"&data$data$Agente=="SF"]),size = 4,
              color=data$data$Color[data$data$Nombre=="LELIQ"&data$data$Agente=="SF"]) +
    
    #SF_activo8
    geom_text(aes(x=nombre_a3,y=-9.5,label="Pr\u00E9stamos"),size = 4,hjust=0) +
    geom_text(aes(x=tot_a3,y=-9.5,label=data$data$VarAcum[data$data$Nombre=="Prestamos"&data$data$Agente=="SF"]),size = 4,
              color=data$data$Color2[data$data$Nombre=="Prestamos"&data$data$Agente=="SF"]) +
    geom_text(aes(x=valor_a3,y=-9.5,label=data$data$ValorFinal[data$data$Nombre=="Prestamos"&data$data$Agente=="SF"]),size = 4,hjust=1) +
    geom_text(aes(x=var_a3,y=-9.5,label=data$data$Variacion[data$data$Nombre=="Prestamos"&data$data$Agente=="SF"]),size = 4,
              color=data$data$Color[data$data$Nombre=="Prestamos"&data$data$Agente=="SF"]) +
    
    #SF_activo9
    geom_text(aes(x=nombre_a3,y=-10.5,label="Cta.Cte.USD"),size = 4,hjust=0) +
    geom_text(aes(x=tot_a3,y=-10.5,label=data$data$VarAcum[data$data$Nombre=="Cta.Cte.USD"&data$data$Agente=="SF"]),size = 4,
              color=data$data$Color2[data$data$Nombre=="Cta.Cte.USD"&data$data$Agente=="SF"]) +
    geom_text(aes(x=valor_a3,y=-10.5,label=data$data$ValorFinal[data$data$Nombre=="Cta.Cte.USD"&data$data$Agente=="SF"]),size = 4,hjust=1) +
    geom_text(aes(x=var_a3,y=-10.5,label=data$data$Variacion[data$data$Nombre=="Cta.Cte.USD"&data$data$Agente=="SF"]),size = 4,
              color=data$data$Color[data$data$Nombre=="Cta.Cte.USD"&data$data$Agente=="SF"]) +
    
    #SF_activo10
    geom_text(aes(x=nombre_a3,y=-11.5,label="USD"),size = 4,hjust=0) +
    geom_text(aes(x=tot_a3,y=-11.5,label=data$data$VarAcum[data$data$Nombre=="USD"&data$data$Agente=="SF"]),size = 4,
              color=data$data$Color2[data$data$Nombre=="USD"&data$data$Agente=="SF"]) +
    geom_text(aes(x=valor_a3,y=-11.5,label=data$data$ValorFinal[data$data$Nombre=="USD"&data$data$Agente=="SF"]),size = 4,hjust=1) +
    geom_text(aes(x=var_a3,y=-11.5,label=data$data$Variacion[data$data$Nombre=="USD"&data$data$Agente=="SF"]),size = 4,
              color=data$data$Color[data$data$Nombre=="USD"&data$data$Agente=="SF"]) +
    
    #SF_activo11
    geom_text(aes(x=nombre_a3,y=-12.5,label="Tit.Pesos (**)"),size = 4,hjust=0) +
    geom_text(aes(x=tot_a3,y=-12.5,label=data$data$VarAcum[data$data$Nombre=="Tit.Pub.Pesos"&data$data$Agente=="SF"]),size = 4,
              color=data$data$Color2[data$data$Nombre=="Tit.Pub.Pesos"&data$data$Agente=="SF"]) +
    geom_text(aes(x=valor_a3,y=-12.5,label=data$data$ValorFinal[data$data$Nombre=="Tit.Pub.Pesos"&data$data$Agente=="SF"]),size = 4,hjust=1) +
    geom_text(aes(x=var_a3,y=-12.5,label=data$data$Variacion[data$data$Nombre=="Tit.Pub.Pesos"&data$data$Agente=="SF"]),size = 4,
              color=data$data$Color[data$data$Nombre=="Tit.Pub.Pesos"&data$data$Agente=="SF"]) +

    #SF_pasivo1
    geom_text(aes(x=nombre_p3,y=-1.5,label="Dep.Vista"),size = 4,hjust=0) +
    geom_text(aes(x=tot_p3,y=-1.5,label=data$data$VarAcum[data$data$Nombre=="Dep.Vista"&data$data$Agente=="SF"]),
              size = 4,color=data$data$Color2[data$data$Nombre=="Dep.Vista"&data$data$Agente=="SF"]) +
    geom_text(aes(x=valor_p3,y=-1.5,label=data$data$ValorFinal[data$data$Nombre=="Dep.Vista"&data$data$Agente=="SF"]),size = 4,hjust=1) +
    geom_text(aes(x=var_p3,y=-1.5,label=data$data$Variacion[data$data$Nombre=="Dep.Vista"&data$data$Agente=="SF"]),size = 4,
              color=data$data$Color[data$data$Nombre=="Dep.Vista"&data$data$Agente=="SF"]) +
    
    #SF_pasivo2
    geom_text(aes(x=nombre_p3,y=-2.5,label="Plazo.Fijo"),size = 4,hjust=0) +
    geom_text(aes(x=tot_p3,y=-2.5,label=data$data$VarAcum[data$data$Nombre=="Plazo.Fijo"&data$data$Agente=="SF"]),
              size = 4,color=data$data$Color2[data$data$Nombre=="Plazo.Fijo"&data$data$Agente=="SF"]) +
    geom_text(aes(x=valor_p3,y=-2.5,label=data$data$ValorFinal[data$data$Nombre=="Plazo.Fijo"&data$data$Agente=="SF"]),size = 4,hjust=1) +
    geom_text(aes(x=var_p3,y=-2.5,label=data$data$Variacion[data$data$Nombre=="Plazo.Fijo"&data$data$Agente=="SF"]),size = 4,
              color=data$data$Color[data$data$Nombre=="Plazo.Fijo"&data$data$Agente=="SF"]) +
    
    #SF_pasivo3
    geom_text(aes(x=nombre_p3,y=-3.5,label="Dep.USD"),size = 4,hjust=0) +
    geom_text(aes(x=tot_p3,y=-3.5,label=data$data$VarAcum[data$data$Nombre=="Dep.USD"&data$data$Agente=="SF"]),
              size = 4,color=data$data$Color2[data$data$Nombre=="Dep.USD"&data$data$Agente=="SF"]) +
    geom_text(aes(x=valor_p3,y=-3.5,label=data$data$ValorFinal[data$data$Nombre=="Dep.USD"&data$data$Agente=="SF"]),size = 4,hjust=1) +
    geom_text(aes(x=var_p3,y=-3.5,label=data$data$Variacion[data$data$Nombre=="Dep.USD"&data$data$Agente=="SF"]),size = 4,
              color=data$data$Color[data$data$Nombre=="Dep.USD"&data$data$Agente=="SF"]) +

    # Hogares
    #hogares_activo1
    geom_text(aes(x=nombre_a1,y=-ancho-5.5,label="Circulante"),size = 4,hjust=0) +
    geom_text(aes(x=tot_a1,y=-ancho-5.5,label=data$data$VarAcum[data$data$Nombre=="Circulante"&data$data$Agente=="H"]),size = 4,
              color=data$data$Color2[data$data$Nombre=="Circulante"&data$data$Agente=="H"]) +
    geom_text(aes(x=valor_a1,y=-ancho-5.5,label=data$data$ValorFinal[data$data$Nombre=="Circulante"&data$data$Agente=="H"]),size = 4,hjust=1) +
    geom_text(aes(x=var_a1,y=-ancho-5.5,label=data$data$Variacion[data$data$Nombre=="Circulante"&data$data$Agente=="H"]),size = 4,
              color=data$data$Color[data$data$Nombre=="Circulante"&data$data$Agente=="H"]) +

    #hogares_activo2
    geom_text(aes(x=nombre_a1,y=-ancho-6.5,label="Dep.Vista"),size = 4,hjust=0) +
    geom_text(aes(x=tot_a1,y=-ancho-6.5,label=data$data$VarAcum[data$data$Nombre=="Dep.Vista"&data$data$Agente=="H"]),size = 4,
              color=data$data$Color2[data$data$Nombre=="Dep.Vista"&data$data$Agente=="H"]) +
    geom_text(aes(x=valor_a1,y=-ancho-6.5,label=data$data$ValorFinal[data$data$Nombre=="Dep.Vista"&data$data$Agente=="H"]),size = 4,hjust=1) +
    geom_text(aes(x=var_a1,y=-ancho-6.5,label=data$data$Variacion[data$data$Nombre=="Dep.Vista"&data$data$Agente=="H"]),size = 4,
              color=data$data$Color[data$data$Nombre=="Dep.Vista"&data$data$Agente=="H"]) +
    
    #hogares_activo3
    geom_text(aes(x=nombre_a1,y=-ancho-7.5,label="Plazo.Fijo"),size = 4,hjust=0) +
    geom_text(aes(x=tot_a1,y=-ancho-7.5,label=data$data$VarAcum[data$data$Nombre=="Plazo.Fijo"&data$data$Agente=="H"]),size = 4,
              color=data$data$Color2[data$data$Nombre=="Plazo.Fijo"&data$data$Agente=="H"]) +
    geom_text(aes(x=valor_a1,y=-ancho-7.5,label=data$data$ValorFinal[data$data$Nombre=="Plazo.Fijo"&data$data$Agente=="H"]),size = 4,hjust=1) +
    geom_text(aes(x=var_a1,y=-ancho-7.5,label=data$data$Variacion[data$data$Nombre=="Plazo.Fijo"&data$data$Agente=="H"]),size = 4,
              color=data$data$Color[data$data$Nombre=="Plazo.Fijo"&data$data$Agente=="H"]) +
    
    #hogares_activo4
    geom_text(aes(x=nombre_a1,y=-ancho-8.5,label="Dep.USD"),size = 4,hjust=0) +
    geom_text(aes(x=tot_a1,y=-ancho-8.5,label=data$data$VarAcum[data$data$Nombre=="Dep.USD"&data$data$Agente=="H"]),size = 4,
              color=data$data$Color2[data$data$Nombre=="Dep.USD"&data$data$Agente=="H"]) +
    geom_text(aes(x=valor_a1,y=-ancho-8.5,label=data$data$ValorFinal[data$data$Nombre=="Dep.USD"&data$data$Agente=="H"]),size = 4,hjust=1) +
    geom_text(aes(x=var_a1,y=-ancho-8.5,label=data$data$Variacion[data$data$Nombre=="Dep.USD"&data$data$Agente=="H"]),size = 4,
              color=data$data$Color[data$data$Nombre=="Dep.USD"&data$data$Agente=="H"]) +
    
    #hogares_activo5
    geom_text(aes(x=nombre_a1,y=-ancho-9.5,label="USD (**)"),size = 4,hjust=0) +
    geom_text(aes(x=tot_a1,y=-ancho-9.5,label=data$data$VarAcum[data$data$Nombre=="USD"&data$data$Agente=="H"]),size = 4,
              color=data$data$Color2[data$data$Nombre=="USD"&data$data$Agente=="H"]) +
    geom_text(aes(x=valor_a1,y=-ancho-9.5,label=data$data$ValorFinal[data$data$Nombre=="USD"&data$data$Agente=="H"]),size = 4,hjust=1) +
    geom_text(aes(x=var_a1,y=-ancho-9.5,label=data$data$Variacion[data$data$Nombre=="USD"&data$data$Agente=="H"]),size = 4,
              color=data$data$Color[data$data$Nombre=="USD"&data$data$Agente=="H"]) +
    
    #hogares_activo6
    geom_text(aes(x=nombre_a1,y=-ancho-10.5,label="LEBAC (*)"),size = 4,hjust=0) +
    geom_text(aes(x=tot_a1,y=-ancho-10.5,label=data$data$VarAcum[data$data$Nombre=="LEBAC"&data$data$Agente=="H"]),size = 4,
              color=data$data$Color2[data$data$Nombre=="LEBAC"&data$data$Agente=="H"]) +
    geom_text(aes(x=valor_a1,y=-ancho-10.5,label=data$data$ValorFinal[data$data$Nombre=="LEBAC"&data$data$Agente=="H"]),size = 4,hjust=1) +
    geom_text(aes(x=var_a1,y=-ancho-10.5,label=data$data$Variacion[data$data$Nombre=="LEBAC"&data$data$Agente=="H"]),size = 4,
              color=data$data$Color[data$data$Nombre=="LEBAC"&data$data$Agente=="H"]) +
    
    #hogares_activo7
    geom_text(aes(x=nombre_a1,y=-ancho-11.5,label="Tit.Pesos (**)"),size = 4,hjust=0) +
    geom_text(aes(x=tot_a1,y=-ancho-11.5,label=data$data$VarAcum[data$data$Nombre=="Tit.Pub.Pesos"&data$data$Agente=="H"]),size = 4,
              color=data$data$Color2[data$data$Nombre=="Tit.Pub.Pesos"&data$data$Agente=="H"]) +
    geom_text(aes(x=valor_a1,y=-ancho-11.5,label=data$data$ValorFinal[data$data$Nombre=="Tit.Pub.Pesos"&data$data$Agente=="H"]),size = 4,hjust=1) +
    geom_text(aes(x=var_a1,y=-ancho-11.5,label=data$data$Variacion[data$data$Nombre=="Tit.Pub.Pesos"&data$data$Agente=="H"]),size = 4,
              color=data$data$Color[data$data$Nombre=="Tit.Pub.Pesos"&data$data$Agente=="H"]) +
    
    #hogares_activo8
    geom_text(aes(x=nombre_a1,y=-ancho-12.5,label="Tit.USD (**)"),size = 4,hjust=0) +
    geom_text(aes(x=tot_a1,y=-ancho-12.5,label=data$data$VarAcum[data$data$Nombre=="Tit.Pub.USD"&data$data$Agente=="H"]),size = 4,
              color=data$data$Color2[data$data$Nombre=="Tit.Pub.USD"&data$data$Agente=="H"]) +
    geom_text(aes(x=valor_a1,y=-ancho-12.5,label=data$data$ValorFinal[data$data$Nombre=="Tit.Pub.USD"&data$data$Agente=="H"]),size = 4,hjust=1) +
    geom_text(aes(x=var_a1,y=-ancho-12.5,label=data$data$Variacion[data$data$Nombre=="Tit.Pub.USD"&data$data$Agente=="H"]),size = 4,
              color=data$data$Color[data$data$Nombre=="Tit.Pub.USD"&data$data$Agente=="H"]) +
    
    #hogares_pasivo1
    geom_text(aes(x=nombre_p1,y=-ancho-5.5,label="Pr\u00E9stamos"),size = 4,hjust=0) +
    geom_text(aes(x=tot_p1,y=-ancho-5.5,label=data$data$VarAcum[data$data$Nombre=="Prestamos"&data$data$Agente=="H"]),size = 4,
              color=data$data$Color2[data$data$Nombre=="Prestamos"&data$data$Agente=="H"]) +
    geom_text(aes(x=valor_p1,y=-ancho-5.5,label=data$data$ValorFinal[data$data$Nombre=="Prestamos"&data$data$Agente=="H"]),size = 4,hjust=1) +
    geom_text(aes(x=var_p1,y=-ancho-5.5,label=data$data$Variacion[data$data$Nombre=="Prestamos"&data$data$Agente=="H"]),size = 4,
              color=data$data$Color[data$data$Nombre=="Prestamos"&data$data$Agente=="H"]) +

    # Resto del Mundo
    #RM_activo1
    geom_text(aes(x=nombre_a2,y=-ancho-5.5,label="LEBAC (*)"),size = 4,hjust=0) +
    geom_text(aes(x=tot_a2,y=-ancho-5.5,label=data$data$VarAcum[data$data$Nombre=="LEBAC"&data$data$Agente=="RM"]),
              size = 4,color=data$data$Color2[data$data$Nombre=="LEBAC"&data$data$Agente=="RM"]) +
    geom_text(aes(x=valor_a2,y=-ancho-5.5,label=data$data$ValorFinal[data$data$Nombre=="LEBAC"&data$data$Agente=="RM"]),size = 4,hjust=1) +
    geom_text(aes(x=var_a2,y=-ancho-5.5,label=data$data$Variacion[data$data$Nombre=="LEBAC"&data$data$Agente=="RM"]),size = 4,
              color=data$data$Color[data$data$Nombre=="LEBAC"&data$data$Agente=="RM"]) +
    
    #RM_activo2
    geom_text(aes(x=nombre_a2,y=-ancho-6.5,label="Tit.Pesos (**)"),size = 4,hjust=0) +
    geom_text(aes(x=tot_a2,y=-ancho-6.5,label=data$data$VarAcum[data$data$Nombre=="Tit.Pub.Pesos"&data$data$Agente=="RM"]),
              size = 4,color=data$data$Color2[data$data$Nombre=="Tit.Pub.Pesos"&data$data$Agente=="RM"]) +
    geom_text(aes(x=valor_a2,y=-ancho-6.5,label=data$data$ValorFinal[data$data$Nombre=="Tit.Pub.Pesos"&data$data$Agente=="RM"]),size = 4,hjust=1) +
    geom_text(aes(x=var_a2,y=-ancho-6.5,label=data$data$Variacion[data$data$Nombre=="Tit.Pub.Pesos"&data$data$Agente=="RM"]),size = 4,
              color=data$data$Color[data$data$Nombre=="Tit.Pub.Pesos"&data$data$Agente=="RM"]) +
    
    #RM_activo3
    geom_text(aes(x=nombre_a2,y=-ancho-7.5,label="Tit.USD (**)"),size = 4,hjust=0) +
    geom_text(aes(x=tot_a2,y=-ancho-7.5,label=data$data$VarAcum[data$data$Nombre=="Tit.Pub.USD"&data$data$Agente=="RM"]),
              size = 4,color=data$data$Color2[data$data$Nombre=="Tit.Pub.USD"&data$data$Agente=="RM"]) +
    geom_text(aes(x=valor_a2,y=-ancho-7.5,label=data$data$ValorFinal[data$data$Nombre=="Tit.Pub.USD"&data$data$Agente=="RM"]),size = 4,hjust=1) +
    geom_text(aes(x=var_a2,y=-ancho-7.5,label=data$data$Variacion[data$data$Nombre=="Tit.Pub.USD"&data$data$Agente=="RM"]),size = 4,
              color=data$data$Color[data$data$Nombre=="Tit.Pub.USD"&data$data$Agente=="RM"]) +
    
    #RM_pasivo1
    geom_text(aes(x=nombre_p2,y=-ancho-5.5,label="Divisas"),size = 4,hjust=0) +
    geom_text(aes(x=tot_p2,y=-ancho-5.5,label=data$data$VarAcum[data$data$Nombre=="Divisas"&data$data$Agente=="RM"]),
              size = 4,color=data$data$Color2[data$data$Nombre=="Divisas"&data$data$Agente=="RM"]) +
    geom_text(aes(x=valor_p2,y=-ancho-5.5,label=data$data$ValorFinal[data$data$Nombre=="Divisas"&data$data$Agente=="RM"]),size = 4,hjust=1) +
    geom_text(aes(x=var_p2,y=-ancho-5.5,label=data$data$Variacion[data$data$Nombre=="Divisas"&data$data$Agente=="RM"]),size = 4,
              color=data$data$Color[data$data$Nombre=="Divisas"&data$data$Agente=="RM"]) +

    # Configuración
    theme(panel.background = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank())

  grafico
}
