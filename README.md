# Portafolios óptimos de Markowitz para N activos

Este proyecto generaliza el análisis original de dos acciones a **N activos** mediante el modelo media-varianza de Markowitz. Está preparado para los CSV originales de Walmex y GFNorte y permite añadir más activos sin modificar el código.

## Estructura

- `analisis_portafolios.R`: carga y alinea datos, calcula rendimientos, medias, matrices de covarianza/correlación, simula portafolios long-only y obtiene el portafolio de mínima varianza mediante programación cuadrática.
- `data/`: un CSV por activo. Cada archivo debe contener columnas `Date` y `Price`; el nombre del archivo se usa como nombre del activo.
- `output/`: resultados generados por el script.

## Requisitos

R (4.1 o posterior recomendado) y paquetes `quadprog` y `ggplot2`.

```r
install.packages(c("quadprog", "ggplot2"))
```

## Ejecutar

Desde la carpeta raíz del proyecto, en R o RStudio:

```r
source("analisis_portafolios.R")
```

Los datos de ejemplo tienen fecha `mm/dd/yyyy` y precios mensuales. Para añadir un activo, agrega por ejemplo `data/femsa.csv` con columnas `Date,Price`. El script hace un `inner join` por fecha: solo utiliza fechas disponibles para todos los activos, evitando emparejar precios de meses distintos.

## Configuración principal

Al inicio de `analisis_portafolios.R` puedes cambiar:

- `FREQUENCY`: `daily`, `weekly`, `monthly` o `annual`; determina la anualización de la tasa libre de riesgo y el número de periodos por año.
- `RISK_FREE_ANNUAL`: tasa anual expresada como decimal (por ejemplo, `0.0628` equivale a 6.28%). Actualízala para el periodo de estudio.
- `N_RANDOM_PORTFOLIOS`: número de combinaciones aleatorias de pesos a evaluar.
- `ALLOW_SHORT_SELLING`: se deja en `FALSE`; la implementación actual es long-only (pesos no negativos y suma de pesos igual a 1).

## Resultados

- `rendimientos.csv`: rendimientos simples por activo y fecha.
- `estadisticas_activos.csv`: rendimiento medio y volatilidad por periodo.
- `matriz_covarianzas.csv` y `matriz_correlaciones.csv`: matrices usadas en el cálculo de riesgo.
- `portafolios_simulados.csv`: nube de portafolios aleatorios, rendimientos, varianza, volatilidad, Sharpe y pesos.
- `portafolios_optimos.csv`: portafolio de mínima varianza exacto y mejor Sharpe encontrado por simulación.
- `pesos_optimos.csv`: asignaciones de los portafolios destacados.
- `frontera_eficiente_aproximada.csv` y `frontera_eficiente.png`: aproximación visual de la frontera eficiente.

## Matemática

Con el vector de pesos `w`, el vector de medias `mu` y la matriz de covarianzas `Sigma`:

- Rendimiento esperado: `w' mu`
- Varianza: `w' Sigma w`
- Volatilidad: `sqrt(w' Sigma w)`
- Sharpe por periodo: `(rendimiento esperado - tasa libre de riesgo por periodo) / volatilidad`

La matriz de covarianzas captura las relaciones entre **todos los pares** de activos; por eso el riesgo no se obtiene sumando únicamente varianzas individuales. La optimización de mínima varianza se resuelve con `quadprog` bajo `sum(w)=1` y `w>=0`. El máximo Sharpe es una búsqueda aproximada sobre los portafolios simulados, no una garantía del óptimo global.

## Consideraciones importantes

- El script calcula rendimientos simples y usa su media y covarianza muestral.
- La periodicidad debe coincidir con los datos. Los archivos de ejemplo contienen observaciones mensuales; no selecciones `daily` para esos archivos.
- La tasa libre de riesgo se convierte de anual efectiva a la frecuencia elegida. El valor predeterminado es ilustrativo, no una cotización vigente.
- Este modelo no incorpora costos, impuestos, restricciones regulatorias, límites por emisor, liquidez ni pasivos de una aseguradora. Esas restricciones deben modelarse por separado antes de usarlo para decisiones reales.
- Los CSV de entrada deben compartir suficientes fechas; el script no rellena precios faltantes.
