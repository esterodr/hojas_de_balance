# En Construcción

A continuación, se describe cada una de las cuentas que se incluyen en el ejercicio, indicando la fuente de donde se obtienen sus valores.

## Banco Central

### Activos

**Reservas**: Stock de Reservas Internacionales extraído del Informe Monetario Diario que publica diariamente el BCRA. Se presenta su valor en Pesos y en Dólares.

**ATs**: Adelantos Transitorios. El monto se obtiene del Balance Semanal publicado por el BCRA.

**Tit.Pesos**: Títulos Públicos denominados en pesos que han sido adquiridos por el BCRA, ya sea mediante suscripción directa o en el mercado secundario. Esta información no es pública, por lo que **el monto que aquí se indica es arbitrario** y puede estar lejos del real.

**Tit.USD**: Títulos Públicos denominados en dólares que han sido adquiridos por el BCRA, ya sea mediante suscripción directa o en el mercado secundario. Esta información no es pública, por lo que **el monto que aquí se indica es arbitrario** y puede estar lejos del real.

### Pasivos

**Circulante**: Billetes y Monedas en poder del público o de las entidades financieras. Es uno de los componentes de la Base Monetaria. El valor se obtiene del Informe Monetario Diario que publica diariamente el BCRA.

**Cta.Cte.**: Depósitos en Pesos de las Entidades Financieras. Parte o la totalidad de los mismos sirven para la integración de Efectivo Mínimo. Es uno de los componentes de la Base Monetaria. El valor se obtiene del Informe Monetario Diario que publica diariamente el BCRA.

**Dep.Tesoro.**: Cuenta del Tesoro en el BCRA. Se la considera fuera de la Base Monetaria. La información se obtiene de la Información Diaria sobre Reservas Internacionales y Principales Pasivos del BCRA) que publica diariamente el BCRA.

**LELIQ/Pases**: Corresponde al stock total de LELIQ más el stock total de Pases Pasivos en Pesos. Los valores se obtienen del Informe Monetario Diario que publica diariamente el BCRA.

**Cta.Cte.USD**: Corresponde a la fila "Cuentas Corrientes en Otras Monedas" del Balance Semanal publicado por el BCRA. Se considera que las Entidades Financieras depositan en esta cuenta la integración de efectivo mínimo por los depósitos en dólares, como también todo exceso de dólares que no conservan como efectivo. En particular se asume que ante un nuevo depósito en dólares, el **80%** queda depositado aquí.

**LEBAC**: Instrumento de esterilización utilizado entre 2002 y 2018. Si bien ya no existe más, se lo incluye para simular la adquisición de estos instrumentos por parte del Sector Privado no Financiero y por el Resto del Mundo, algo no permitido con las actuales LELIQ.

**Resultado**: Se contabiliza el resultado en pesos de las variaciones patrimoniales que se puedan producir. En particular, las operaciones que generarán un resultado serán la variación del tipo de cambio y el devengamiento de intereses, las cuales se pueden simular mediante la opción "Transcurso del Tiempo".

## Gobierno

### Activos

**Dep.BCRA**: Cuenta del Tesoro en el BCRA. La información se obtiene de la Información Diaria sobre Reservas Internacionales y Principales Pasivos del BCRA) que publica diariamente el BCRA.

**Dep.Vista**: Por falta de información desagregada, se crea esta cuenta para diferenciarla de la cuenta "Dep.Públicos". Los fondos aquí disponibles pueden ser gastados libremente por el Gobierno. Por ejemplo, se supone que la recaudación impositiva se deposita aquí. Por simplicidad, se supone que el valor inicial de esta cuenta es 0.

**Dep.Públicos**: Por falta de información desagregada, se crea esta cuenta para diferenciarla de la cuenta "Dep.Vista". Aquí están todos los depósitos a la vista del Sector Público (en todos sus niveles) que se informan en el Informe Monetario Diario que publica diariamente el BCRA. Los montos aquí depositados no se los considera para ninguna operación, sólo se los incluye para mantener la consistencia con el total de pasivos del Sistema Financiero.

**Plazo.Fijo**: Depósitos a Plazo del Sector Público (en todos sus niveles). La información se extrae del Informe Monetario Diario que publica diariamente el BCRA. Los montos aquí depositados no se los considera para ninguna operación, sólo se los incluye para mantener la consistencia con el total de pasivos del Sistema Financiero.

**Dep.USD**: Depósitos en Dólares del Sector Público (en todos sus niveles). La información se extrae del Informe Monetario Diario que publica diariamente el BCRA. 

### Pasivos

**ATs**: Adelantos Transitorios. El monto se obtiene del Balance Semanal publicado por el BCRA.

**Tit.Pesos**: Total de Deuda Pública denominada en Pesos. Si bien periódicamente se publica este valor, no siempre es posible desagregarla por tipo de acreedor. Para mantener la consistencia con las de más Hojas de Balance, **el monto que aquí se indica es arbitrario** y puede estar lejos del real.

**Tit.USD**: Total de Deuda Pública denominada en Dólares. Si bien periódicamente se publica este valor, no siempre es posible desagregarla por tipo de acreedor. Para mantener la consistencia con las de más Hojas de Balance, **el monto que aquí se indica es arbitrario** y puede estar lejos del real.

**Resultado**: Se contabiliza el resultado en pesos de las variaciones patrimoniales que se puedan producir. Los impuestos y el gasto tienen un efecto en el Resultado. También la variación del tipo de cambio y el devengamiento de intereses, las cuales se pueden simular mediante la opción "Transcurso del Tiempo".

## Sector Financiero

### Activos

**Efectivo Mínimo**: De acuerdo a la regulación vigente, la integración de Efectivo Mínimo puede realizarse con distintos instrumentos: depósitos en Cta.Cte., LELIQ o Títulos Públicos. Es imposible realizar una estimación muy precisa en base a datos públicos, por lo que se supone que la **integración inicial** de efectivo mínimo se realiza de la siguiente forma:
-- El 100% de los fondos que inicialmente se encuentran depositados en la Cta.Cte. del BCRA (esto es, lo que surge del Informe Monetario Diario que publica diariamente el BCRA).
-- Un monto de LELIQ equivalente al 20% del total de Plazos Fijos, cuyo valor se obtiene del Informe Monetario Diario que publica diariamente el BCRA.
-- Un monto de Títulos Públicos equivalente al 4% del total de Depósitos (a la Vista + Plazos Fijos), cuyo valor se obtiene del Informe Monetario Diario que publica diariamente el BCRA.
-- El 100% de los fondos que inicialmente se encuentran depositados en la Cta.Cte. en USD del BCRA (esto es, la fila "Cuentas Corrientes en Otras Monedas" del Balance Semanal publicado por el BCRA).
A medida que se van realizando operaciones, estas proporciones no seguirán siendo válidas.

**Circulante**:

**Cta.Cte.**:

**LELIQ/Pases**:

**Préstamos**:

**Cta.Cte.USD**:

**USD**:

**Tit.Pesos**:

### Pasivos

**Dep.Vista**:

**Plazo.Fijo**:

**Dep.USD**:

**Resultado**:

[Volver](README.md)