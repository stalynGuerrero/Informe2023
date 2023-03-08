

# Estimación del modelo de área 

El modelo de Fay Herriot FH, propuesto por Fay y Herriot (1979), es un modelo estadístico de área y es el más comúnmente utilizado, cabe tener en cuenta, que dentro de la metodología de estimación en áreas pequeñas, los modelos de área son los de mayor aplicación, ya que lo más factible es no contar con la información a nivel de individuo, pero si encontrar no solo los datos a nivel de área, sino también información auxiliar asociada a estos datos. Este modelo lineal mixto, fue el primero en incluir efectos aleatorios a nivel de área, lo que implica que la mayoría de la información que se introduce al modelo corresponde a agregaciaciones usualmente, departamentos, regiones, provincias, municipios entre otros, donde las estimaciones que se logran con el modelo se obtienen sobre estas agregaciones o subpoblaciones.

Además, el modelo FH utiliza información auxiliar para mejorar la precisión de las estimaciones en áreas pequeñas. Esta información auxiliar puede ser de diferentes tipos, como censos poblacionales, encuestas de hogares, registros administrativos, entre otros. La inclusión de esta información auxiliar se realiza a través de un modelo lineal mixto, en el que se consideran tanto efectos fijos como aleatorios a nivel de área. Los efectos fijos representan la relación entre la variable de interés y las variables auxiliares, mientras que los efectos aleatorios capturan la variabilidad no explicada por estas variables. De esta forma, el modelo FH permite obtener estimaciones precisas y confiables para subpoblaciones o áreas pequeñas, lo que resulta de gran utilidad para la toma de decisiones en diferentes ámbitos, como políticas públicas, planificación urbana, entre otros.


## Lectura de librerías


```r
library(survey)
library(srvyr)
library(TeachingSampling)
library(stringr)
library(magrittr)
library(sae)
library(ggplot2)
library(emdi)
library(patchwork)
library(readxl)
library(tidyverse)
select <- dplyr::select
id_dominio <- "id_dominio"
```



## Lectura de bases de datos 

Para la lecturas de las insumos se emplean los siguientes comando de R. La función `readRDS()` es utilizada para cargar objetos creados previamente y guardados en archivos RDS. Los archivos RDS contienen objetos de R guardados de forma binaria, lo que permite guardar y recuperar objetos de R con facilidad.

En este caso, se cargan tres objetos diferentes: `base_FH, `statelevel_predictors` y `DEE_Mun`.


```r
# Estimación Directa + FGV
base_FH <- readRDS('../Data/base_FH.Rds')

# Variables predicadoras 
statelevel_predictors <- readRDS("../Data/statelevel_predictors_df.rds")
DEE_Mun <- readRDS('../Data/DEE_Mun.Rds') 
```

## Consolidando las bases de datos. 

La función `select()` del paquete `dplyr` se utiliza para seleccionar columnas de un `data.frame`. En este caso, se seleccionan las columnas `id_dominio`, `Rd`, `hat_var`, `n_eff_FGV` y `n` de la base de datos `base_FH`.

Luego, se utiliza la función `full_join()` de `dplyr` para unir los datos de la base de datos `base_FH` con los datos de la base de datos `statelevel_predictors` por la columna `id_dominio`. La opción by se utiliza para especificar la columna por la cual se realizará la unión.

Por último, se utiliza la función `left_join()` de `dplyr` para unir los datos de la base de datos `base_completa` con los datos de la base de datos `DEE_Mun` por la columna `id_dominio`. Al igual que en el caso anterior, la opción `by` se utiliza para especificar la columna por la cual se realizará la unión.


```r
base_completa <-
  base_FH %>% select(id_dominio, Rd, hat_var, n_eff_FGV,n) %>%
  full_join(statelevel_predictors, by = id_dominio)

base_completa <- left_join(base_completa, DEE_Mun, by = id_dominio)
```

Fialmente se almacena la base consolidada en un archivo `.rds`


```r
saveRDS(base_completa, 'Data/base_completa.Rds')
```

## Estimando el modelo de área con transformación arcoseno

El código corresponde a la estimación del modelo de Fay Herriot (FH) utilizando el método de mínimos cuadrados restringidos (REML) y transformación de los datos mediante la función `arcsin`.

En la especificación del modelo se definen las variables predictoras fijas y la variable dependiente (Rd), que es la estimación directa en una determinada área (dominio). Además, se especifica la variable auxiliar para la estimación de varianza (`hat_var`).

Se utiliza la base de datos completa (`base_completa`) que incluye la información de las variables predictoras a nivel de dominio, información auxiliar y el tamaño de muestra efectivo para cada dominio (`eff_smpsize`).

Se especifica que el método de estimación de la varianza es mediante bootstrapping (`mse_type = "boot"`) con un número de replicaciones igual a 500 (`B = 500`).

El modelo también incluye efectos aleatorios a nivel de dominio, lo que se indica mediante la especificación de los dominios (`domains`).

Finalmente, se especifica que se desea la transformación de los datos mediante la función arcsin (`transformation = "arcsin"`) y su backtransformation mediante el método bias-corrected (`backtransformation = "bc"`). Se utiliza el parámetro `MSE = TRUE` para obtener la estimación de la varianza del error de muestreo (MSE) del estimador.



```r
fh_arcsin <- fh(
  fixed = Rd ~ 0 + P45_TUVO_EMPLEO  +  P46_ACTIVIDAD_PORPAGA  +
    P47_AYUDO_SINPAGA  +  P30_DONDE_NACE  +  P41_ANOS_UNIVERSITARIOS  +
    P38_ANOEST  +  P44_PAIS_VIVIA_CODIGO  +  P50_DISPUESTO_TRABAJAR  +
    P51_TRABAJO_ANTES  +  P40_SEGRADUO  +  P29_EDAD_ANOS_CUMPLIDOS  +
    P35_SABE_LEER  +  P27_SEXO  +  H25C_TOTAL  +  P45R1_CONDICION_ACTIVIDAD_OCUPADO  +
    P45R1_CONDICION_ACTIVIDAD_DISCAPACITADO  +  P45R1_CONDICION_ACTIVIDAD_1erTrabajo  +
    P45R1_CONDICION_ACTIVIDAD_EDUCACION  + P54R1_CATOCUP_SINCOM +
    P27_SEXO_JEFE + P29_EDAD_JEFE + ZONA_Rur + H31_HACINAMIENTO + H15_PROCEDENCIA_AGUA +
    H17_ALUMBRADO + H12_SANITARIO + V03_PAREDES + V04_TECHO + V05_PISO +
    H32_GRADSAN + H35_GRUPSEC +
    luces_nocturnas_promedio  +
    cubrimiento_cultivo_suma +
    cubrimiento_urbano_suma +
    accesibilidad_hospitales_promedio +
    accesibilidad_hosp_caminado_promedio,
  vardir = "hat_var",
  combined_data = base_completa %>% data.frame(),
  domains = id_dominio,
  method = "reml",
  transformation = "arcsin",
  backtransformation = "bc",
  eff_smpsize = "n_eff_FGV",
  MSE = TRUE,
  mse_type = "boot",
  B = 500
)
```
leer el modelo compilado previamente.

```r
fh_arcsin <- readRDS('../Data/fh_arcsin.Rds')
```

## Estimaciones de los dominios a partir del modelo. 

Lo primero que hace el código es obtener los valores de gamma ($\gamma$), recordemos que el estimador de FH esta dado como una combinación convexa entre la estimación directa y la estimación sintética, es decir, $\tilde{\theta}_{d}^{FH}  =  \hat{\gamma_d}\hat{\theta}^{DIR}_{d}+(1-\hat{\gamma_d})\boldsymbol{x_d}^{T}\hat{\boldsymbol{\beta}}$. Luego, se utiliza la función `estimators()` para calcular las predicciones de las estimaciones directas y sus errores estándar, tanto de forma puntual como mediante un intervalo de confianza. Finalmente, se juntan los resultados con información auxiliar sobre los dominios, como su tamaño muestral y el valor del  Gamma, y se imprime una tabla con las primeras 20 filas de la tabla de resultados.


```r
data_Gamma <- fh_arcsin$model$gamma %>% 
  transmute(id_dominio = Domain, Gamma)

estimaciones <- estimators(fh_arcsin,
                           indicator = "All",
                           MSE = TRUE,
                           CV = TRUE) %>%
  as.data.frame() %>%
  rename(id_dominio = Domain) %>%
  left_join(base_completa %>%
              transmute(id_dominio,
                        n = ifelse(is.na(n), 0, n)),
            by = id_dominio) %>%
  left_join(data_Gamma, by = id_dominio) %>%
dplyr::select(id_dominio, everything())
tba(head(estimaciones, 20))
```

<table class="table table-striped lightable-classic" style="width: auto !important; margin-left: auto; margin-right: auto; font-family: Arial Narrow; width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> id_dominio </th>
   <th style="text-align:right;"> Direct </th>
   <th style="text-align:right;"> Direct_MSE </th>
   <th style="text-align:right;"> Direct_CV </th>
   <th style="text-align:right;"> FH </th>
   <th style="text-align:right;"> FH_MSE </th>
   <th style="text-align:right;"> FH_CV </th>
   <th style="text-align:right;"> n </th>
   <th style="text-align:right;"> Gamma </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> 0101 </td>
   <td style="text-align:right;"> 0.4147 </td>
   <td style="text-align:right;"> 0.0005 </td>
   <td style="text-align:right;"> 0.0534 </td>
   <td style="text-align:right;"> 0.4142 </td>
   <td style="text-align:right;"> 0.0004 </td>
   <td style="text-align:right;"> 0.0496 </td>
   <td style="text-align:right;"> 2951 </td>
   <td style="text-align:right;"> 0.1207 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 0201 </td>
   <td style="text-align:right;"> 0.4526 </td>
   <td style="text-align:right;"> 0.0053 </td>
   <td style="text-align:right;"> 0.1613 </td>
   <td style="text-align:right;"> 0.5390 </td>
   <td style="text-align:right;"> 0.0017 </td>
   <td style="text-align:right;"> 0.0766 </td>
   <td style="text-align:right;"> 221 </td>
   <td style="text-align:right;"> 0.0128 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 0202 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> 0.6466 </td>
   <td style="text-align:right;"> 0.0051 </td>
   <td style="text-align:right;"> 0.1104 </td>
   <td style="text-align:right;"> 86 </td>
   <td style="text-align:right;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 0203 </td>
   <td style="text-align:right;"> 0.7138 </td>
   <td style="text-align:right;"> 0.0101 </td>
   <td style="text-align:right;"> 0.1406 </td>
   <td style="text-align:right;"> 0.7917 </td>
   <td style="text-align:right;"> 0.0029 </td>
   <td style="text-align:right;"> 0.0682 </td>
   <td style="text-align:right;"> 86 </td>
   <td style="text-align:right;"> 0.0056 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 0204 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> 0.7472 </td>
   <td style="text-align:right;"> 0.0053 </td>
   <td style="text-align:right;"> 0.0972 </td>
   <td style="text-align:right;"> 51 </td>
   <td style="text-align:right;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 0205 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> 0.7119 </td>
   <td style="text-align:right;"> 0.0104 </td>
   <td style="text-align:right;"> 0.1431 </td>
   <td style="text-align:right;"> 34 </td>
   <td style="text-align:right;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 0206 </td>
   <td style="text-align:right;"> 0.5527 </td>
   <td style="text-align:right;"> 0.0133 </td>
   <td style="text-align:right;"> 0.2088 </td>
   <td style="text-align:right;"> 0.5870 </td>
   <td style="text-align:right;"> 0.0041 </td>
   <td style="text-align:right;"> 0.1087 </td>
   <td style="text-align:right;"> 65 </td>
   <td style="text-align:right;"> 0.0052 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 0208 </td>
   <td style="text-align:right;"> 0.8122 </td>
   <td style="text-align:right;"> 0.0097 </td>
   <td style="text-align:right;"> 0.1210 </td>
   <td style="text-align:right;"> 0.6703 </td>
   <td style="text-align:right;"> 0.0060 </td>
   <td style="text-align:right;"> 0.1156 </td>
   <td style="text-align:right;"> 74 </td>
   <td style="text-align:right;"> 0.0044 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 0210 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> 0.7081 </td>
   <td style="text-align:right;"> 0.0108 </td>
   <td style="text-align:right;"> 0.1465 </td>
   <td style="text-align:right;"> 16 </td>
   <td style="text-align:right;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 0301 </td>
   <td style="text-align:right;"> 0.5668 </td>
   <td style="text-align:right;"> 0.0039 </td>
   <td style="text-align:right;"> 0.1100 </td>
   <td style="text-align:right;"> 0.5305 </td>
   <td style="text-align:right;"> 0.0013 </td>
   <td style="text-align:right;"> 0.0683 </td>
   <td style="text-align:right;"> 264 </td>
   <td style="text-align:right;"> 0.0170 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 0302 </td>
   <td style="text-align:right;"> 0.7561 </td>
   <td style="text-align:right;"> 0.0059 </td>
   <td style="text-align:right;"> 0.1017 </td>
   <td style="text-align:right;"> 0.6841 </td>
   <td style="text-align:right;"> 0.0040 </td>
   <td style="text-align:right;"> 0.0921 </td>
   <td style="text-align:right;"> 123 </td>
   <td style="text-align:right;"> 0.0085 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 0303 </td>
   <td style="text-align:right;"> 0.6078 </td>
   <td style="text-align:right;"> 0.0047 </td>
   <td style="text-align:right;"> 0.1125 </td>
   <td style="text-align:right;"> 0.6168 </td>
   <td style="text-align:right;"> 0.0012 </td>
   <td style="text-align:right;"> 0.0572 </td>
   <td style="text-align:right;"> 206 </td>
   <td style="text-align:right;"> 0.0139 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 0304 </td>
   <td style="text-align:right;"> 0.6450 </td>
   <td style="text-align:right;"> 0.0051 </td>
   <td style="text-align:right;"> 0.1110 </td>
   <td style="text-align:right;"> 0.7021 </td>
   <td style="text-align:right;"> 0.0025 </td>
   <td style="text-align:right;"> 0.0717 </td>
   <td style="text-align:right;"> 176 </td>
   <td style="text-align:right;"> 0.0121 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 0305 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> 0.6842 </td>
   <td style="text-align:right;"> 0.0053 </td>
   <td style="text-align:right;"> 0.1066 </td>
   <td style="text-align:right;"> 51 </td>
   <td style="text-align:right;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 0401 </td>
   <td style="text-align:right;"> 0.5419 </td>
   <td style="text-align:right;"> 0.0021 </td>
   <td style="text-align:right;"> 0.0840 </td>
   <td style="text-align:right;"> 0.5609 </td>
   <td style="text-align:right;"> 0.0012 </td>
   <td style="text-align:right;"> 0.0627 </td>
   <td style="text-align:right;"> 481 </td>
   <td style="text-align:right;"> 0.0319 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 0402 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> 0.7460 </td>
   <td style="text-align:right;"> 0.0047 </td>
   <td style="text-align:right;"> 0.0916 </td>
   <td style="text-align:right;"> 75 </td>
   <td style="text-align:right;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 0403 </td>
   <td style="text-align:right;"> 0.6788 </td>
   <td style="text-align:right;"> 0.0083 </td>
   <td style="text-align:right;"> 0.1345 </td>
   <td style="text-align:right;"> 0.6959 </td>
   <td style="text-align:right;"> 0.0036 </td>
   <td style="text-align:right;"> 0.0859 </td>
   <td style="text-align:right;"> 108 </td>
   <td style="text-align:right;"> 0.0072 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 0404 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> 0.5999 </td>
   <td style="text-align:right;"> 0.0057 </td>
   <td style="text-align:right;"> 0.1257 </td>
   <td style="text-align:right;"> 68 </td>
   <td style="text-align:right;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 0405 </td>
   <td style="text-align:right;"> 0.5383 </td>
   <td style="text-align:right;"> 0.0050 </td>
   <td style="text-align:right;"> 0.1309 </td>
   <td style="text-align:right;"> 0.5777 </td>
   <td style="text-align:right;"> 0.0026 </td>
   <td style="text-align:right;"> 0.0889 </td>
   <td style="text-align:right;"> 221 </td>
   <td style="text-align:right;"> 0.0136 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 0407 </td>
   <td style="text-align:right;"> 0.7513 </td>
   <td style="text-align:right;"> 0.0124 </td>
   <td style="text-align:right;"> 0.1482 </td>
   <td style="text-align:right;"> 0.6447 </td>
   <td style="text-align:right;"> 0.0046 </td>
   <td style="text-align:right;"> 0.1050 </td>
   <td style="text-align:right;"> 67 </td>
   <td style="text-align:right;"> 0.0042 </td>
  </tr>
</tbody>
</table>

De la Tabla es posible notar que las estimaciones de FH existen para todos los dominios. 

guardar estimaciones


```r
saveRDS(estimaciones, '../Data/estimaciones.Rds')
```

