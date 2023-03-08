

# Gráfico comparativo de las estimaciones.

## Lectura de librerías


```r
library(plotly)
library(dplyr)
library(tidyr)
library(forcats)
library(survey)
library(srvyr)
```

## Lecturas de bases de datos. 

El bloque de código carga tres archivos de datos en R:

  -   `encuestaDOM.Rds`: es utilizado para obtener las estimaciones directas de la región.
  -   `estimacionesPre.Rds`: contiene estimaciones previas, sin embargo solo son seleccionadas las columnas `id_region`, `id_dominio` y `W_i` que son útiles para la construcción del gráfico.
  -   `TablaFinal.Rds`: contiene las estimaciones directas, sintéticas, Fay Harriot  y Fay Harriot con benchmarking. Primero se carga el archivo y luego se realiza una unión interna (`inner_join()`) con el archivo `estimacionesPre` por la variable `id_dominio`.


```r
encuestaDOM <-  readRDS("../Data/encuestaDOM.Rds")
estimacionesPre <- readRDS('../Data/estimacionesBench.Rds') %>% 
  dplyr::select(id_region, id_dominio, W_i)

TablaFinal <- readRDS('../Data/TablaFinal.Rds') %>% 
  inner_join(estimacionesPre, by = "id_dominio")
```

Este bloque de código calcula las estimaciones agregadas por región a partir de la tabla `TablaFinal`, que contiene las estimaciones sintéticas y de Fay Harriot para cada dominio, junto con su peso `W_i`. Primero, la tabla se agrupa por `id_region`. Luego, para cada región, se calcula la suma ponderada de las estimaciones sintéticas (`sintetico_back`) y de Fay Harriot (`FH_RBench` y `FayHerriot`) utilizando los pesos `W_i`. Las tres estimaciones se presentan en la tabla final `estimaciones_agregada`.


```r
estimaciones_agregada <- TablaFinal %>% data.frame() %>% 
  group_by(id_region) %>% 
  summarize(
    Sintetico=sum(W_i*sintetico_back),
    FH_bench=sum(W_i*FH_RBench),
    FH = sum(W_i*FayHerriot)
  )
tba(estimaciones_agregada)
```

<table class="table table-striped lightable-classic" style="width: auto !important; margin-left: auto; margin-right: auto; font-family: Arial Narrow; width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> id_region </th>
   <th style="text-align:right;"> Sintetico </th>
   <th style="text-align:right;"> FH_bench </th>
   <th style="text-align:right;"> FH </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> 01 </td>
   <td style="text-align:right;"> 0.4355 </td>
   <td style="text-align:right;"> 0.4331 </td>
   <td style="text-align:right;"> 0.4355 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 02 </td>
   <td style="text-align:right;"> 0.5340 </td>
   <td style="text-align:right;"> 0.5231 </td>
   <td style="text-align:right;"> 0.5339 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 03 </td>
   <td style="text-align:right;"> 0.5656 </td>
   <td style="text-align:right;"> 0.5566 </td>
   <td style="text-align:right;"> 0.5656 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 04 </td>
   <td style="text-align:right;"> 0.4791 </td>
   <td style="text-align:right;"> 0.4685 </td>
   <td style="text-align:right;"> 0.4791 </td>
  </tr>
</tbody>
</table>

En este bloque de código se realizan las siguientes acciones:

  -   Se agrega ceros a la izquierda de las variables `upm` y `estrato` para que tengan una longitud de 9 y 5 dígitos, respectivamente.
  -   Se crea un objeto `disenoDOM` con el diseño de la encuesta, utilizando la función `as_survey_design()` de la librería survey. Se especifica que el estrato es la variable `estrato`, la unidad primaria de muestreo (UPM) es la variable `upm` y el peso muestral es la variable `factor_anual`. Además, se utiliza el argumento `nest=T` para indicar que las UPM están anidadas.
  -   Se estima el indicador directo para cada región. Para ello,  se agrupan `id_region` y se utiliza la función `summarise()` para calcular el tamaño de la muestra (`n`) y la razón de diseño (`Rd`) utilizando la función `survey_ratio()` de la librería `survey`.


```r
encuestaDOM <-
  encuestaDOM %>%
  mutate(
     upm = str_pad(string = upm,width = 9,pad = "0"),
    estrato = str_pad(string = estrato,width = 5,pad = "0"),
    factor_anual = factor_expansion / 4
  )

#Creación de objeto diseno--------------------------------------- 
disenoDOM <- encuestaDOM %>%
  as_survey_design(
    strata = estrato,
    ids = upm,
    weights = factor_anual,
    nest=T
  )

#Cálculo del indicador ----------------
indicador_dir <-
  disenoDOM %>% group_by(id_region) %>%
  filter(ocupado == 1 & pet == 1) %>%
  summarise(
    n = unweighted(n()),
    Rd = survey_ratio(
      numerator = orden_sector == 2 ,
      denominator = 1,
      vartype = c("se", "ci"),
      deff = F
    )
  )
```

Este bloque de código realiza lo siguiente:

  -   Primero, se une el objeto estimaciones_agregada que contiene las estimaciones de los métodos sintético, FH benchmark y FH, con el objeto `indicador_dir` que contiene el indicador directo para cada región.
  -   Luego, se seleccionan las columnas necesarias para el gráfico y se transforma el formato de las columnas usando la función` gather()` para tener los valores de las estimaciones en una sola columna y el nombre del método en otra columna. Además, se cambia el nombre de los métodos para que sean más descriptivos.
Finalmente, se crean los límites del intervalo de confianza para el indicador directo y se guardan en el objeto `lims_IC`.


```r
data_plot <- left_join(estimaciones_agregada, 
                       indicador_dir, by = 'id_region')  %>% data.frame()

temp <- data_plot %>% select(-Rd_low, -Rd_upp,-Rd_se) %>%
  gather(key = "Estimacion",value = "value", -n,-id_region) %>% 
  mutate(Estimacion = case_when(Estimacion == "FH" ~ "Fay Harriot",
                                Estimacion == "FH_bench" ~ "FH bench",
                                Estimacion == "Rd"~ "Directo",
                                      TRUE ~ Estimacion))
lims_IC <-  data_plot %>%
  select(n,id_region,value = Rd, Rd_low, Rd_upp) %>% 
  mutate(Estimacion = "Directo")
```

Este bloque de código grafica las estimaciones de los diferentes métodos, junto con los intervalos de confianza de las estimaciones directas. La función` ggplot()` de la librería `ggplot2` se utiliza para crear el gráfico, y se utilizan diferentes capas para agregar diferentes elementos al gráfico.


```r
p <- ggplot(temp,
            aes(
              x = fct_reorder2(id_region, id_region, n),
              y = value,
              shape = Estimacion,
              color = Estimacion
            )) +
  geom_errorbar(
    data = lims_IC,
    aes(ymin = Rd_low ,
        ymax = Rd_upp, x = id_region),
    width = 0.2,
    size = 1
  )  +
  geom_jitter(size = 3)+
  labs(x = "Región")
ggplotly(p)
```

```{=html}
<div id="htmlwidget-3dcbba4c1e5984f3f40a" style="width:672px;height:480px;" class="plotly html-widget"></div>
<script type="application/json" data-for="htmlwidget-3dcbba4c1e5984f3f40a">{"x":{"data":[{"x":[1,2,3,4],"y":[0.433065959562755,0.523064539343195,0.556554418885329,0.468528753635787],"text":["id_region: 01<br />value: 0.4330660<br />Estimacion: Directo<br />Estimacion: Directo<br />Rd_low: 0.4126795<br />Rd_upp: 0.4534524","id_region: 02<br />value: 0.5230645<br />Estimacion: Directo<br />Estimacion: Directo<br />Rd_low: 0.5041898<br />Rd_upp: 0.5419393","id_region: 03<br />value: 0.5565544<br />Estimacion: Directo<br />Estimacion: Directo<br />Rd_low: 0.5280040<br />Rd_upp: 0.5851048","id_region: 04<br />value: 0.4685288<br />Estimacion: Directo<br />Estimacion: Directo<br />Rd_low: 0.4349671<br />Rd_upp: 0.5020904"],"type":"scatter","mode":"lines","opacity":1,"line":{"color":"transparent"},"error_y":{"array":[0.0203864568185464,0.0188747405188321,0.0285503731928917,0.0335616296172846],"arrayminus":[0.0203864568185464,0.0188747405188321,0.0285503731928917,0.0335616296172846],"type":"data","width":8.00000000000001,"symmetric":false,"color":"rgba(248,118,109,1)"},"name":"Directo","legendgroup":"Directo","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[1.09988806527108,1.6814863467589,3.05324433427304,4.22267993763089],"y":[0.433068963354325,0.523066561814737,0.556559610478534,0.46852797952971],"text":["fct_reorder2(id_region, id_region, n): 01<br />value: 0.4330660<br />Estimacion: Directo<br />Estimacion: Directo","fct_reorder2(id_region, id_region, n): 02<br />value: 0.5230645<br />Estimacion: Directo<br />Estimacion: Directo","fct_reorder2(id_region, id_region, n): 03<br />value: 0.5565544<br />Estimacion: Directo<br />Estimacion: Directo","fct_reorder2(id_region, id_region, n): 04<br />value: 0.4685288<br />Estimacion: Directo<br />Estimacion: Directo"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(248,118,109,1)","opacity":1,"size":11.3385826771654,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(248,118,109,1)"}},"hoveron":"points","name":"Directo","legendgroup":"Directo","showlegend":false,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[1.37164073996246,2.36086857672781,2.94972082097083,3.87763158474118],"y":[0.43553171024257,0.533902765134124,0.565617111653576,0.479087065331264],"text":["fct_reorder2(id_region, id_region, n): 01<br />value: 0.4355284<br />Estimacion: Fay Harriot<br />Estimacion: Fay Harriot","fct_reorder2(id_region, id_region, n): 02<br />value: 0.5338985<br />Estimacion: Fay Harriot<br />Estimacion: Fay Harriot","fct_reorder2(id_region, id_region, n): 03<br />value: 0.5656195<br />Estimacion: Fay Harriot<br />Estimacion: Fay Harriot","fct_reorder2(id_region, id_region, n): 04<br />value: 0.4790926<br />Estimacion: Fay Harriot<br />Estimacion: Fay Harriot"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(124,174,0,1)","opacity":1,"size":11.3385826771654,"symbol":"triangle-up","line":{"width":1.88976377952756,"color":"rgba(124,174,0,1)"}},"hoveron":"points","name":"Fay Harriot","legendgroup":"Fay Harriot","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[1.18356576673687,2.04564237184823,2.81799682918936,3.93806802257895],"y":[0.433059249569666,0.523058448812578,0.556545695673142,0.468530713318628],"text":["fct_reorder2(id_region, id_region, n): 01<br />value: 0.4330660<br />Estimacion: FH bench<br />Estimacion: FH bench","fct_reorder2(id_region, id_region, n): 02<br />value: 0.5230645<br />Estimacion: FH bench<br />Estimacion: FH bench","fct_reorder2(id_region, id_region, n): 03<br />value: 0.5565544<br />Estimacion: FH bench<br />Estimacion: FH bench","fct_reorder2(id_region, id_region, n): 04<br />value: 0.4685288<br />Estimacion: FH bench<br />Estimacion: FH bench"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(0,191,196,1)","opacity":1,"size":11.3385826771654,"symbol":"square","line":{"width":1.88976377952756,"color":"rgba(0,191,196,1)"}},"hoveron":"points","name":"FH bench","legendgroup":"FH bench","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[0.97651881724596,2.05296538677067,3.31601100135595,4.13312988970429],"y":[0.435508328429408,0.533957570855761,0.56557161692897,0.479154529570541],"text":["fct_reorder2(id_region, id_region, n): 01<br />value: 0.4355052<br />Estimacion: Sintetico<br />Estimacion: Sintetico","fct_reorder2(id_region, id_region, n): 02<br />value: 0.5339611<br />Estimacion: Sintetico<br />Estimacion: Sintetico","fct_reorder2(id_region, id_region, n): 03<br />value: 0.5655792<br />Estimacion: Sintetico<br />Estimacion: Sintetico","fct_reorder2(id_region, id_region, n): 04<br />value: 0.4791488<br />Estimacion: Sintetico<br />Estimacion: Sintetico"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(199,124,255,1)","opacity":1,"size":11.3385826771654,"symbol":"cross-thin-open","line":{"width":1.88976377952756,"color":"rgba(199,124,255,1)"}},"hoveron":"points","name":"Sintetico","legendgroup":"Sintetico","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null}],"layout":{"margin":{"t":26.2283105022831,"r":7.30593607305936,"b":40.1826484018265,"l":48.9497716894977},"plot_bgcolor":"rgba(255,255,255,1)","paper_bgcolor":"rgba(255,255,255,1)","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"xaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[0.4,4.6],"tickmode":"array","ticktext":["01","02","03","04"],"tickvals":[1,2,3,4],"categoryorder":"array","categoryarray":["01","02","03","04"],"nticks":null,"ticks":"outside","tickcolor":"rgba(51,51,51,1)","ticklen":3.65296803652968,"tickwidth":0,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716895},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(235,235,235,1)","gridwidth":0,"zeroline":false,"anchor":"y","title":{"text":"Región","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187}},"hoverformat":".2f"},"yaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[0.404058238277508,0.593726056544922],"tickmode":"array","ticktext":["0.45","0.50","0.55"],"tickvals":[0.45,0.5,0.55],"categoryorder":"array","categoryarray":["0.45","0.50","0.55"],"nticks":null,"ticks":"outside","tickcolor":"rgba(51,51,51,1)","ticklen":3.65296803652968,"tickwidth":0,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716895},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(235,235,235,1)","gridwidth":0,"zeroline":false,"anchor":"x","title":{"text":"value","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187}},"hoverformat":".2f"},"shapes":[{"type":"rect","fillcolor":"transparent","line":{"color":"rgba(51,51,51,1)","width":0,"linetype":"solid"},"yref":"paper","xref":"paper","x0":0,"x1":1,"y0":0,"y1":1}],"showlegend":true,"legend":{"bgcolor":"rgba(255,255,255,1)","bordercolor":"transparent","borderwidth":0,"font":{"color":"rgba(0,0,0,1)","family":"","size":11.689497716895},"title":{"text":"Estimacion","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187}}},"hovermode":"closest","barmode":"relative"},"config":{"doubleClick":"reset","modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false},"source":"A","attrs":{"1644bf124b4":{"x":{},"y":{},"shape":{},"colour":{},"ymin":{},"ymax":{},"type":"scatter"},"16447ab76bcc":{"x":{},"y":{},"shape":{},"colour":{}}},"cur_data":"1644bf124b4","visdat":{"1644bf124b4":["function (y) ","x"],"16447ab76bcc":["function (y) ","x"]},"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.2,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script>
```

La capa `aes()` define la estética de la gráfica. En este caso, se utiliza la variable `id_region` en el eje x, la variable `value` en el eje y, y se utiliza el color y la forma de los puntos para distinguir entre los diferentes métodos de estimación. Se utiliza la función `fct_reorder2()` para ordenar las regiones de manera ascendente según su tamaño muestral `n`.

La capa `geom_errorbar()` se utiliza para agregar los intervalos de confianza de las estimaciones directas. Se utiliza el conjunto de datos `lims_IC` que contiene los límites de los intervalos de confianza. Se utiliza `ymin` y `ymax` para definir los límites inferior y superior de las barras de error, respectivamente, y `x` para definir la ubicación en el eje x de cada intervalo.

La capa `geom_jitter()` se utiliza para agregar los puntos de las estimaciones de cada método, con un poco de ruido para que no se superpongan.

Finalmente, la capa `labs()` se utiliza para agregar etiquetas a los ejes del gráfico. En este caso, se utiliza `Región` para el eje x. El objeto p contiene el gráfico creado.
