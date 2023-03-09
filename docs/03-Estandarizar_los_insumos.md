

# Estandarizar insumos 

El modelo Fay-Herriot utiliza información auxiliar, en forma de covariables, para mejorar la precisión de la estimación. Este conjunto de sintaxis en R tiene como objetivo preparar los datos necesarios para la aplicación del modelo Fay-Herriot para la estimación directa, utilizando la transformación arcoseno y la Función Generalizada de Varianza. Para lograr esto, se utilizarán diferentes fuentes de información (encuestas, registros administrativos, imágenes satelitales, shapefile y el Censo). Por esta razón, es fundamental la estandarización de las bases de datos, que consiste en tener una uniformidad de la estructura, formato y contenido de los datos, lo que permite su comparación y combinación sin errores o confusiones. La estandarización de bases de datos es esencial para la integración de datos provenientes de diferentes fuentes, lo que facilita la toma de decisiones durante el proceso y la obtención de resultados precisos y confiables. En resumen, la estandarización de bases de datos es crucial para garantizar la calidad y confiabilidad de la información utilizada en los procesos de análisis.

## Lectura de librerias 

El código presenta una serie de librerías de `R` que se utilizan para la preparación de los datos. La librería `tidyverse` proporciona un conjunto de paquetes para la procesamiento y visualización de datos. `magrittr` se utiliza para facilitar la lectura del código, mientras que `stringr` es una librería que proporciona herramientas para la manipulación de cadenas de caracteres. `readxl` es una librería utilizada para leer archivos de Excel. `tmap` y `sp` se utilizan para la visualización de mapas, mientras que `sf` es una librería que proporciona herramientas para la manipulación de datos espaciales. En conjunto, estas librerías permiten la estandarización de bases de datos y la posterior integración de información proveniente de diferentes fuentes.



```r
library(tidyverse)
library(magrittr)
library(stringr)
library(readxl)
library(tmap)
library(sp)
library(sf)
```

## Lectura de encuesta. 

El código cargará un archivo `encuestaDOM.Rds` que contiene los datos de la encuesta realizada. Luego, utiliza la función `mutate()` del paquete `dplyr` para crear dos nuevas columnas `id_dominio` e `id_region` que representan el identificador del municipio y la región respectivamente. La función `str_pad()` del paquete `stringr` se utiliza para agregar ceros a la izquierda de los números, lo que asegura que los identificadores tengan el mismo número de dígitos. El operador `%<>%` se utiliza para encadenar las funciones y guardar el resultado en la misma variable `encuestaDOM`.


```r
encuestaDOM <-  readRDS("../Data/encuestaDOM.Rds") 
encuestaDOM %<>% 
  mutate(id_dominio = str_pad(string = id_municipio, width = 4, pad = "0"),
         id_region = str_pad(string = orden_region, width = 2, pad = "0"))
```

## Lectura de información auxiliar (covariable)

Este código carga el archivo `auxiliar_org.Rds` que contiene la información *Censal* y luego agrega una columna llamada `id_dominio`, la cual consiste en el valor de la columna `id_municipio` rellenada con ceros a la izquierda para completar 4 dígitos. La función utilizada para rellenar con ceros es `str_pad` de la librería `stringr`.


```r
auxiliar_org <-  readRDS("../Data/auxiliar_org.Rds") %>%
   mutate(id_dominio = str_pad(
    string = id_municipio,
    width = 4,
    pad = "0"
  ))
```

El código tiene como objetivo crear una base de datos auxiliar a partir de la información satelital y otra de la información del Censo. Primero se carga la información del archivo `auxiliar_satelital` y se estandarizan las variables numéricas mediante la función `scale()` Luego se realiza una manipulación de la variable `ENLACE` y se crea una nueva variable `id_dominio` mediante la función `str_pad()`. Después se realiza una unión interna de las bases de datos `auxiliar_org` y `auxiliar_satelital` por la variable `id_dominio`, para crear la base de datos `statelevel_predictors`, la cual servirá de información auxilia


```r
auxiliar_satelital <-  readRDS("../Data/auxiliar_satelital.Rds") %>%
  mutate_if(is.numeric, function(x)as.numeric(scale(x))) %>%  mutate(
    ENLACE = substring(ENLACE, 3),
    id_dominio = str_pad(
      string = ENLACE,
      width = 4,
      pad = "0"
    )
  )

statelevel_predictors <- inner_join(auxiliar_org, auxiliar_satelital, 
                                    by = "id_dominio")
```

## Otra fuente de información 
El fragmento de código presenta la carga de un archivo Excel que contiene datos del DEE agrupados por municipios. Los datos son modificados mediante la función `mutate()` para incluir un identificador de dominio y se elimina la columna `Cod`.


```r
DEE_Mun <- read_xlsx('../Data/datos del DEE ONE agrupdos por municipios.xlsx') %>% 
  mutate(id_dominio = str_pad(
    string = Cod,
    width = 4,
    pad = "0"
  ), Cod =NULL)
```
## Definiendo el identificador de dominio en la shapefile.  

Este código carga un archivo de `shapefile` y lo convierte en un objeto de la clase `sf` utilizando la función `read_sf()` de la librería `sf`. Posteriormente, se realiza una mutación en la cual se crea una nueva variable llamada `id_dominio` que corresponde a los cuatro últimos dígitos de la variable `ENLACE` que se encuentra en el `shapefile`. Esto se logra mediante la función `substring()` y la función `str_pad()` de la librería `stringr`. Finalmente, se selecciona la variable `id_dominio` y `geometry` del objeto `sf` utilizando la función `select`.


```r
Shapefile <- read_sf( "../shapefiles2010/MUNCenso2010.shp" ) %>% 
  mutate(
    ENLACE = substring(ENLACE,3),
    id_dominio = str_pad(
      string = ENLACE,
      width = 4,
      pad = "0"
    )
  )  %>% select(id_dominio, geometry)  
```

## Validando id_dominio

La validación del identificador `id_dominio` es importante para asegurarnos de que todas las bases de datos que se utilizarán en el modelo tienen una correspondencia adecuada entre ellas. En el código presentado se realizan tres validaciones utilizando la función `full_join()` y la función `distinct()`. En la primera validación, se comparan los identificadores de `encuestaDOM` con los de `statelevel_predictors`. En la segunda validación, se comparan los identificadores de `encuestaDOM` con los de `DEE_Mun`. Finalmente, en la tercera validación, se comparan los identificadores de `encuestaDOM` con los de `Shapefile`. En todas las validaciones se utilizó la función `head()` para mostrar las primeras 10 filas y la función `tba()` para visualizar los resultados en forma de tabla. De esta forma, se puede verificar que el identificador `id_dominio` es consistente en todas las bases de datos utilizadas.


```r
encuestaDOM %>% distinct(id_dominio, DES_PROVINCIA) %>%
  full_join(statelevel_predictors %>% distinct(id_dominio)) %>% 
  head(10) %>% tba()
```

<table class="table table-striped lightable-classic" style="width: auto !important; margin-left: auto; margin-right: auto; font-family: Arial Narrow; width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> id_dominio </th>
   <th style="text-align:left;"> DES_PROVINCIA </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> 0101 </td>
   <td style="text-align:left;"> DISTRITO NACIONAL </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 3201 </td>
   <td style="text-align:left;"> SANTO DOMINGO </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 3202 </td>
   <td style="text-align:left;"> SANTO DOMINGO </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 3203 </td>
   <td style="text-align:left;"> SANTO DOMINGO </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 3204 </td>
   <td style="text-align:left;"> SANTO DOMINGO </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 3205 </td>
   <td style="text-align:left;"> SANTO DOMINGO </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 3206 </td>
   <td style="text-align:left;"> SANTO DOMINGO </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 3207 </td>
   <td style="text-align:left;"> SANTO DOMINGO </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2501 </td>
   <td style="text-align:left;"> SANTIAGO </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2502 </td>
   <td style="text-align:left;"> SANTIAGO </td>
  </tr>
</tbody>
</table>

```r
encuestaDOM %>% distinct(id_dominio, DES_PROVINCIA) %>%
  full_join(DEE_Mun %>% distinct(id_dominio, Des))%>% 
  head(10) %>% tba()
```

<table class="table table-striped lightable-classic" style="width: auto !important; margin-left: auto; margin-right: auto; font-family: Arial Narrow; width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> id_dominio </th>
   <th style="text-align:left;"> DES_PROVINCIA </th>
   <th style="text-align:left;"> Des </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> 0101 </td>
   <td style="text-align:left;"> DISTRITO NACIONAL </td>
   <td style="text-align:left;"> Santo Domingo de Guzmán </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 3201 </td>
   <td style="text-align:left;"> SANTO DOMINGO </td>
   <td style="text-align:left;"> Santo Domingo Este </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 3202 </td>
   <td style="text-align:left;"> SANTO DOMINGO </td>
   <td style="text-align:left;"> Santo Domingo Oeste </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 3203 </td>
   <td style="text-align:left;"> SANTO DOMINGO </td>
   <td style="text-align:left;"> Santo Domingo Norte </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 3204 </td>
   <td style="text-align:left;"> SANTO DOMINGO </td>
   <td style="text-align:left;"> Boca Chica </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 3205 </td>
   <td style="text-align:left;"> SANTO DOMINGO </td>
   <td style="text-align:left;"> San Antonio de Guerra </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 3206 </td>
   <td style="text-align:left;"> SANTO DOMINGO </td>
   <td style="text-align:left;"> Los Alcarrizos </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 3207 </td>
   <td style="text-align:left;"> SANTO DOMINGO </td>
   <td style="text-align:left;"> Pedro Brand </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2501 </td>
   <td style="text-align:left;"> SANTIAGO </td>
   <td style="text-align:left;"> Santiago </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2502 </td>
   <td style="text-align:left;"> SANTIAGO </td>
   <td style="text-align:left;"> Bisonó </td>
  </tr>
</tbody>
</table>

```r
encuestaDOM %>% distinct(id_dominio, DES_PROVINCIA) %>%
  full_join(Shapefile %>% data.frame() %>% select(id_dominio))%>% 
  head(10) %>% tba()
```

<table class="table table-striped lightable-classic" style="width: auto !important; margin-left: auto; margin-right: auto; font-family: Arial Narrow; width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> id_dominio </th>
   <th style="text-align:left;"> DES_PROVINCIA </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> 0101 </td>
   <td style="text-align:left;"> DISTRITO NACIONAL </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 3201 </td>
   <td style="text-align:left;"> SANTO DOMINGO </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 3202 </td>
   <td style="text-align:left;"> SANTO DOMINGO </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 3203 </td>
   <td style="text-align:left;"> SANTO DOMINGO </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 3204 </td>
   <td style="text-align:left;"> SANTO DOMINGO </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 3205 </td>
   <td style="text-align:left;"> SANTO DOMINGO </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 3206 </td>
   <td style="text-align:left;"> SANTO DOMINGO </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 3207 </td>
   <td style="text-align:left;"> SANTO DOMINGO </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2501 </td>
   <td style="text-align:left;"> SANTIAGO </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2502 </td>
   <td style="text-align:left;"> SANTIAGO </td>
  </tr>
</tbody>
</table>

## Guardar archivos  

Las instrucciones presentadas permiten guardar las bases de datos que se utilizarán en el modelo Fay-Herriot para la estimación directa. Las funciones `saveRDS()` y `st_write()` se utilizan para guardar las bases en formatos compatibles con el software R, permitiendo su posterior uso en otras sesiones de trabajo. Es importante guardar las bases de datos una vez que se han validado los identificadores, para asegurarse de que no hay errores en el proceso de identificación de las observaciones.


```r
saveRDS(encuestaDOM, file = "../Data/encuestaDOM.Rds")
saveRDS(statelevel_predictors, file = "../Data/statelevel_predictors_df.rds")
saveRDS(DEE_Mun, file = "../Data/DEE_Mun.Rds")
st_write(obj = Shapefile,"../shapefiles2010/DOM.shp")
```

## Agregados censales (municipios y región). 

Se importaron las bases de agregados censales por municipios y por región. Se utilizó la función `readRDS()` para importar la base `encuestaDOMRegion.Rds` que contiene información del Censo por región y la función `transmute()` para seleccionar y renombrar las variables de interés. La base resultante `agregado_region` se guardó en un archivo `.rds`.


```r
agregado_region <- readRDS("../Data/encuestaDOMRegion.Rds") %>% 
  transmute(pp_region = hh_depto,
           id_region = str_pad(string = grupo_region, width = 2,
                             pad = "0"))
tba(agregado_region)
```

<table class="table table-striped lightable-classic" style="width: auto !important; margin-left: auto; margin-right: auto; font-family: Arial Narrow; width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:right;"> pp_region </th>
   <th style="text-align:left;"> id_region </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:right;"> 3339410 </td>
   <td style="text-align:left;"> 01 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 3246032 </td>
   <td style="text-align:left;"> 02 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 1692085 </td>
   <td style="text-align:left;"> 03 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 1167754 </td>
   <td style="text-align:left;"> 04 </td>
  </tr>
</tbody>
</table>


```r
saveRDS(agregado_region, file = "../Data/agregado_persona_region.rds")
```

Además, se cargó la base `personas_dominio.Rds` que contiene la cantidad de personas por dominio y se utilizó la función `mutate()` para crear la variable `id_region` y transformar la variable `total_pp` en `pp_dominio`. Finalmente, se eliminaron las variables `total_pp` e `id_municipio.` La base resultante se guardó en un archivo `.rds`.


```r
readRDS("../Data/personas_dominio.Rds") %>% 
  mutate( id_region = str_pad(string = orden_region, width = 2,
                              pad = "0"),
          orden_region = NULL,
          pp_dominio = total_pp,
          total_pp  = NULL,
          id_municipio = NULL) %>% 
  saveRDS(file = "../Data/agregado_persona_dominio.rds")  
```

