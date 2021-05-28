# En Construcción

1. **Adelantos Transitorios**. Financiamiento del Banco Central al Gobierno. Se permiten
dos operaciones con este instrumento:
    1. Otorgar
    2. Cancelar
    
2. **Leliq y Pases**. Instrumentos de esterilización del Banco Central (se los considera
de manera conjunta). Sólo pueden ser suscritos por el Sector Financiero. Se permiten
dos operaciones con este instrumento:
    1. Suscribir
    2. Rescatar 

3. **Reservas**. Activos en moneda extranjera del Banco Central. Se permite comprar y
vender reservas. Dependiendo de quién sea la contraparte, se obtiene un total de seis
operaciones posibles:
   1. El Banco Central compra Reservas
       1. Al Gobierno
       2. Al Sector Financiero
       3. Al Sector Privado no Financiero
   2. El Banco Central vende Reservas
       1. Al Gobierno
       2. Al Sector Financiero
       3. Al Sector Privado no Financiero
       
4. **Gasto e Impuestos**. Se consideran tres operaciones distintas. El Gasto Público se modela como una transferencia desde el Gobierno al Sector Privado no Financiero. Por su
parte, los Impuestos son una transferencia que recibe el Gobierno, habiendo dos posibilidades:
    1. Impuestos cobrados al Sector Financiero
    2. Impuestos cobrados al Sector Privado no Financiero

5. **Títulos Públicos**. Son instrumentos de deuda que el Gobierno emite para obtener financiamiento. Pueden estar denominados en Pesos o en Dólares, ademas de contar con un mercado secundario. Dada la cantidad de operaciones posibles (28 en total), se las agrupa de la siguiente manera:
    1. Emitir Títulos Públicos
        1. Denominados en Pesos
            1. Al Sector Financiero
            2. Al Sector Privado no Financiero
            3. Al Resto del Mundo
        2. Denominados en Dólares
            1. Al Sector Privado no Financiero
            2. Al Resto del Mundo
    2. Cancelar Títulos Públicos
        1. Denominados en Pesos
            1. Al Sector Financiero
            2. Al Sector Privado no Financiero
            3. Al Resto del Mundo
        2. Denominados en Dólares
            1. Al Sector Privado no Financiero
            2. Al Resto del Mundo
    3. Operar Títulos Públicos denominados en Pesos
        1. Banco Central compra al Sector Financiero
        2. Banco Central compra al Sector Privado no Financiero
        3. Banco Central compra al Resto del Mundo
        4. Sector Financiero compra al Banco Central
        5. Sector Financiero compra al SPnF
        6. Sector Financiero compra al Resto del Mundo
        7. Sector Privado no Financiero compra al Banco Central
        8. Sector Privado no Financiero compra al Sector Financiero
        9. Sector Privado no Financiero compra al Resto del Mundo
        10. Resto del Mundo compra al Banco Central
        11. Resto del Mundo compra al Sector Financiero
        12. Resto del Mundo compra al Sector Privado no Financiero
    4. Operar Títulos Públicos denominados en Dólares
        1. Banco Central compra al Sector Privado no Financiero
        2. Banco Central compra al Resto del Mundo
        3. Sector Privado no Financiero compra al Banco Central
        4. Sector Privado no Financiero compra al Resto del Mundo
        5. Resto del Mundo compra al Banco Central
        6. Resto del Mundo compra al Sector Privado no Financiero

6. **Depósitos**. El Sector Privado no Financiero tiene Depósitos a la Vista (en pesos y en dólares) y a Plazo Fijo (sólo en pesos) en el Sector Financiero. Se permite depositar y extraer efectivo desde los Depósitos a la Vista. Los Plazos Fijos sólo pueden consituirse a partir de un Depósito a la Vista. Si bien hay depósitos del sector público, sólo se puede usarlos para recibir o transferir dinero. En total, se incluyen seis operaciones:
    1. Depositar Pesos constituyendo un Depósito a la Vista en pesos
    2. Depositar dólares constituyendo un Depósito a la Vista en dólares
    3. Extraer Pesos de un Depósito a la Vista en pesos
    4. Extraer dólares de un Depósito a la Vista en dólares
    5. Constituir un Plazo Fijo en pesos a partir de un Depósito a la Vista en pesos
    6. Cancelar un Plazo Fijo en pesos recibiendo los fondos en un Depósito a la Vista en pesos

7. **Préstamos**. Del Sector Financiero al Sector Privado no Financiero. Se permiten
dos operaciones con este instrumento:
    1. Otorgar
    2. Cancelar

8. **Balanza Comercial**. Se modela como una transferencia entre un Depósito a la Vista en Dólares del Sector Privado no Financiero y el Resto del Mundo. Dependiendo del sentido de la transferencia, se tiene alguna de las siguientes operaciones:
    1. Superávit Comercial
    2. Déficit Comercial

9. **LEBAC**. Eran el principal instrumento de esterilización empleado por el Banco Central entre 2002 y 2018. Se las incluye para poder simular políticas que no se pueden realizar con los LELIQ y Pases actuales. En particular, se consideran las siguientes 4 operaciones:
    1. Suscribir con el Sector Privado no Financiero
    2. Sucribir con el Resto del Mundo
    3. Cancelar con el Sector Privado no Financiero
    4. Cancelar con el Resto del Mundo
