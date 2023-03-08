

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


# Resultados finales 
<table class="table table-striped lightable-classic" style="width: auto !important; margin-left: auto; margin-right: auto; font-family: Arial Narrow; width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> Codico del municipio </th>
   <th style="text-align:right;"> Estimacion </th>
   <th style="text-align:right;"> Error estandar </th>
   <th style="text-align:right;"> CV(%) </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> 3201 </td>
   <td style="text-align:right;"> 0.4191 </td>
   <td style="text-align:right;"> 0.0197 </td>
   <td style="text-align:right;"> 4.6813 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 0601 </td>
   <td style="text-align:right;"> 0.5803 </td>
   <td style="text-align:right;"> 0.0286 </td>
   <td style="text-align:right;"> 4.8231 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 0101 </td>
   <td style="text-align:right;"> 0.4118 </td>
   <td style="text-align:right;"> 0.0205 </td>
   <td style="text-align:right;"> 4.9604 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2902 </td>
   <td style="text-align:right;"> 0.6007 </td>
   <td style="text-align:right;"> 0.0306 </td>
   <td style="text-align:right;"> 4.9792 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 0604 </td>
   <td style="text-align:right;"> 0.7061 </td>
   <td style="text-align:right;"> 0.0361 </td>
   <td style="text-align:right;"> 5.0126 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2101 </td>
   <td style="text-align:right;"> 0.4601 </td>
   <td style="text-align:right;"> 0.0238 </td>
   <td style="text-align:right;"> 5.0894 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2501 </td>
   <td style="text-align:right;"> 0.4045 </td>
   <td style="text-align:right;"> 0.0218 </td>
   <td style="text-align:right;"> 5.2862 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 3203 </td>
   <td style="text-align:right;"> 0.4911 </td>
   <td style="text-align:right;"> 0.0262 </td>
   <td style="text-align:right;"> 5.3144 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 0901 </td>
   <td style="text-align:right;"> 0.4940 </td>
   <td style="text-align:right;"> 0.0282 </td>
   <td style="text-align:right;"> 5.5985 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 1301 </td>
   <td style="text-align:right;"> 0.4825 </td>
   <td style="text-align:right;"> 0.0280 </td>
   <td style="text-align:right;"> 5.6759 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 0303 </td>
   <td style="text-align:right;"> 0.6069 </td>
   <td style="text-align:right;"> 0.0353 </td>
   <td style="text-align:right;"> 5.7247 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2201 </td>
   <td style="text-align:right;"> 0.5756 </td>
   <td style="text-align:right;"> 0.0338 </td>
   <td style="text-align:right;"> 5.7837 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 1401 </td>
   <td style="text-align:right;"> 0.6128 </td>
   <td style="text-align:right;"> 0.0363 </td>
   <td style="text-align:right;"> 5.8071 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 3202 </td>
   <td style="text-align:right;"> 0.4018 </td>
   <td style="text-align:right;"> 0.0237 </td>
   <td style="text-align:right;"> 5.8530 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 1801 </td>
   <td style="text-align:right;"> 0.4920 </td>
   <td style="text-align:right;"> 0.0299 </td>
   <td style="text-align:right;"> 5.9545 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 0903 </td>
   <td style="text-align:right;"> 0.5912 </td>
   <td style="text-align:right;"> 0.0362 </td>
   <td style="text-align:right;"> 5.9966 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 3204 </td>
   <td style="text-align:right;"> 0.5210 </td>
   <td style="text-align:right;"> 0.0317 </td>
   <td style="text-align:right;"> 6.0463 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2205 </td>
   <td style="text-align:right;"> 0.6241 </td>
   <td style="text-align:right;"> 0.0386 </td>
   <td style="text-align:right;"> 6.0891 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 3101 </td>
   <td style="text-align:right;"> 0.6474 </td>
   <td style="text-align:right;"> 0.0405 </td>
   <td style="text-align:right;"> 6.1558 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2801 </td>
   <td style="text-align:right;"> 0.5660 </td>
   <td style="text-align:right;"> 0.0357 </td>
   <td style="text-align:right;"> 6.1716 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 1809 </td>
   <td style="text-align:right;"> 0.5407 </td>
   <td style="text-align:right;"> 0.0342 </td>
   <td style="text-align:right;"> 6.1973 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 0401 </td>
   <td style="text-align:right;"> 0.5519 </td>
   <td style="text-align:right;"> 0.0352 </td>
   <td style="text-align:right;"> 6.2693 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2507 </td>
   <td style="text-align:right;"> 0.5230 </td>
   <td style="text-align:right;"> 0.0345 </td>
   <td style="text-align:right;"> 6.4628 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 1403 </td>
   <td style="text-align:right;"> 0.6948 </td>
   <td style="text-align:right;"> 0.0464 </td>
   <td style="text-align:right;"> 6.5464 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2701 </td>
   <td style="text-align:right;"> 0.5550 </td>
   <td style="text-align:right;"> 0.0372 </td>
   <td style="text-align:right;"> 6.5591 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 3001 </td>
   <td style="text-align:right;"> 0.5319 </td>
   <td style="text-align:right;"> 0.0357 </td>
   <td style="text-align:right;"> 6.5605 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2402 </td>
   <td style="text-align:right;"> 0.6418 </td>
   <td style="text-align:right;"> 0.0445 </td>
   <td style="text-align:right;"> 6.8000 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 3207 </td>
   <td style="text-align:right;"> 0.5537 </td>
   <td style="text-align:right;"> 0.0379 </td>
   <td style="text-align:right;"> 6.8072 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 0203 </td>
   <td style="text-align:right;"> 0.7790 </td>
   <td style="text-align:right;"> 0.0540 </td>
   <td style="text-align:right;"> 6.8164 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 0301 </td>
   <td style="text-align:right;"> 0.5220 </td>
   <td style="text-align:right;"> 0.0362 </td>
   <td style="text-align:right;"> 6.8324 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 1303 </td>
   <td style="text-align:right;"> 0.5712 </td>
   <td style="text-align:right;"> 0.0405 </td>
   <td style="text-align:right;"> 6.9534 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 0602 </td>
   <td style="text-align:right;"> 0.6824 </td>
   <td style="text-align:right;"> 0.0485 </td>
   <td style="text-align:right;"> 6.9648 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2301 </td>
   <td style="text-align:right;"> 0.4703 </td>
   <td style="text-align:right;"> 0.0339 </td>
   <td style="text-align:right;"> 7.0455 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2803 </td>
   <td style="text-align:right;"> 0.6341 </td>
   <td style="text-align:right;"> 0.0463 </td>
   <td style="text-align:right;"> 7.1557 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 0304 </td>
   <td style="text-align:right;"> 0.6908 </td>
   <td style="text-align:right;"> 0.0503 </td>
   <td style="text-align:right;"> 7.1690 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 1903 </td>
   <td style="text-align:right;"> 0.6294 </td>
   <td style="text-align:right;"> 0.0470 </td>
   <td style="text-align:right;"> 7.3152 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2502 </td>
   <td style="text-align:right;"> 0.5207 </td>
   <td style="text-align:right;"> 0.0395 </td>
   <td style="text-align:right;"> 7.4240 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2104 </td>
   <td style="text-align:right;"> 0.5942 </td>
   <td style="text-align:right;"> 0.0450 </td>
   <td style="text-align:right;"> 7.4516 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 0607 </td>
   <td style="text-align:right;"> 0.7440 </td>
   <td style="text-align:right;"> 0.0570 </td>
   <td style="text-align:right;"> 7.5098 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2901 </td>
   <td style="text-align:right;"> 0.5697 </td>
   <td style="text-align:right;"> 0.0445 </td>
   <td style="text-align:right;"> 7.6338 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 0201 </td>
   <td style="text-align:right;"> 0.5304 </td>
   <td style="text-align:right;"> 0.0413 </td>
   <td style="text-align:right;"> 7.6649 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2203 </td>
   <td style="text-align:right;"> 0.7384 </td>
   <td style="text-align:right;"> 0.0576 </td>
   <td style="text-align:right;"> 7.6702 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2105 </td>
   <td style="text-align:right;"> 0.5204 </td>
   <td style="text-align:right;"> 0.0411 </td>
   <td style="text-align:right;"> 7.7788 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 1302 </td>
   <td style="text-align:right;"> 0.5569 </td>
   <td style="text-align:right;"> 0.0445 </td>
   <td style="text-align:right;"> 7.8213 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 1701 </td>
   <td style="text-align:right;"> 0.5159 </td>
   <td style="text-align:right;"> 0.0410 </td>
   <td style="text-align:right;"> 7.8224 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2404 </td>
   <td style="text-align:right;"> 0.6179 </td>
   <td style="text-align:right;"> 0.0494 </td>
   <td style="text-align:right;"> 7.8341 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 0802 </td>
   <td style="text-align:right;"> 0.5935 </td>
   <td style="text-align:right;"> 0.0476 </td>
   <td style="text-align:right;"> 7.8415 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2401 </td>
   <td style="text-align:right;"> 0.5233 </td>
   <td style="text-align:right;"> 0.0421 </td>
   <td style="text-align:right;"> 7.8830 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 3206 </td>
   <td style="text-align:right;"> 0.4019 </td>
   <td style="text-align:right;"> 0.0319 </td>
   <td style="text-align:right;"> 7.8988 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 1808 </td>
   <td style="text-align:right;"> 0.6207 </td>
   <td style="text-align:right;"> 0.0503 </td>
   <td style="text-align:right;"> 7.9337 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 1802 </td>
   <td style="text-align:right;"> 0.6460 </td>
   <td style="text-align:right;"> 0.0530 </td>
   <td style="text-align:right;"> 8.0387 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2002 </td>
   <td style="text-align:right;"> 0.6042 </td>
   <td style="text-align:right;"> 0.0496 </td>
   <td style="text-align:right;"> 8.0415 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 1101 </td>
   <td style="text-align:right;"> 0.3579 </td>
   <td style="text-align:right;"> 0.0295 </td>
   <td style="text-align:right;"> 8.0516 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2001 </td>
   <td style="text-align:right;"> 0.6262 </td>
   <td style="text-align:right;"> 0.0526 </td>
   <td style="text-align:right;"> 8.2306 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2506 </td>
   <td style="text-align:right;"> 0.4441 </td>
   <td style="text-align:right;"> 0.0375 </td>
   <td style="text-align:right;"> 8.2796 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2903 </td>
   <td style="text-align:right;"> 0.5864 </td>
   <td style="text-align:right;"> 0.0499 </td>
   <td style="text-align:right;"> 8.3275 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2202 </td>
   <td style="text-align:right;"> 0.7496 </td>
   <td style="text-align:right;"> 0.0634 </td>
   <td style="text-align:right;"> 8.3277 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2904 </td>
   <td style="text-align:right;"> 0.5980 </td>
   <td style="text-align:right;"> 0.0510 </td>
   <td style="text-align:right;"> 8.3437 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 1203 </td>
   <td style="text-align:right;"> 0.5434 </td>
   <td style="text-align:right;"> 0.0464 </td>
   <td style="text-align:right;"> 8.3509 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 0904 </td>
   <td style="text-align:right;"> 0.5929 </td>
   <td style="text-align:right;"> 0.0507 </td>
   <td style="text-align:right;"> 8.3852 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 3003 </td>
   <td style="text-align:right;"> 0.7079 </td>
   <td style="text-align:right;"> 0.0607 </td>
   <td style="text-align:right;"> 8.3883 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 1404 </td>
   <td style="text-align:right;"> 0.6815 </td>
   <td style="text-align:right;"> 0.0587 </td>
   <td style="text-align:right;"> 8.4406 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 1805 </td>
   <td style="text-align:right;"> 0.6977 </td>
   <td style="text-align:right;"> 0.0607 </td>
   <td style="text-align:right;"> 8.5232 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 0403 </td>
   <td style="text-align:right;"> 0.6847 </td>
   <td style="text-align:right;"> 0.0598 </td>
   <td style="text-align:right;"> 8.5881 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 1402 </td>
   <td style="text-align:right;"> 0.5864 </td>
   <td style="text-align:right;"> 0.0526 </td>
   <td style="text-align:right;"> 8.7892 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2304 </td>
   <td style="text-align:right;"> 0.4873 </td>
   <td style="text-align:right;"> 0.0440 </td>
   <td style="text-align:right;"> 8.8239 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 1001 </td>
   <td style="text-align:right;"> 0.5727 </td>
   <td style="text-align:right;"> 0.0514 </td>
   <td style="text-align:right;"> 8.8394 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2503 </td>
   <td style="text-align:right;"> 0.6934 </td>
   <td style="text-align:right;"> 0.0629 </td>
   <td style="text-align:right;"> 8.8843 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 0405 </td>
   <td style="text-align:right;"> 0.5684 </td>
   <td style="text-align:right;"> 0.0513 </td>
   <td style="text-align:right;"> 8.8851 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2403 </td>
   <td style="text-align:right;"> 0.6014 </td>
   <td style="text-align:right;"> 0.0548 </td>
   <td style="text-align:right;"> 8.9258 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 1503 </td>
   <td style="text-align:right;"> 0.5498 </td>
   <td style="text-align:right;"> 0.0501 </td>
   <td style="text-align:right;"> 8.9318 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 1504 </td>
   <td style="text-align:right;"> 0.6833 </td>
   <td style="text-align:right;"> 0.0630 </td>
   <td style="text-align:right;"> 9.0315 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 1505 </td>
   <td style="text-align:right;"> 0.7065 </td>
   <td style="text-align:right;"> 0.0654 </td>
   <td style="text-align:right;"> 9.0654 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 0402 </td>
   <td style="text-align:right;"> 0.7341 </td>
   <td style="text-align:right;"> 0.0684 </td>
   <td style="text-align:right;"> 9.1628 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2905 </td>
   <td style="text-align:right;"> 0.7170 </td>
   <td style="text-align:right;"> 0.0675 </td>
   <td style="text-align:right;"> 9.2020 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 0302 </td>
   <td style="text-align:right;"> 0.6732 </td>
   <td style="text-align:right;"> 0.0630 </td>
   <td style="text-align:right;"> 9.2143 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 0406 </td>
   <td style="text-align:right;"> 0.7824 </td>
   <td style="text-align:right;"> 0.0738 </td>
   <td style="text-align:right;"> 9.2856 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 3205 </td>
   <td style="text-align:right;"> 0.4671 </td>
   <td style="text-align:right;"> 0.0441 </td>
   <td style="text-align:right;"> 9.3953 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 0503 </td>
   <td style="text-align:right;"> 0.6934 </td>
   <td style="text-align:right;"> 0.0667 </td>
   <td style="text-align:right;"> 9.4203 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2702 </td>
   <td style="text-align:right;"> 0.5450 </td>
   <td style="text-align:right;"> 0.0526 </td>
   <td style="text-align:right;"> 9.4588 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 0704 </td>
   <td style="text-align:right;"> 0.8148 </td>
   <td style="text-align:right;"> 0.0785 </td>
   <td style="text-align:right;"> 9.4735 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 3103 </td>
   <td style="text-align:right;"> 0.8061 </td>
   <td style="text-align:right;"> 0.0781 </td>
   <td style="text-align:right;"> 9.5311 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 1807 </td>
   <td style="text-align:right;"> 0.5279 </td>
   <td style="text-align:right;"> 0.0518 </td>
   <td style="text-align:right;"> 9.6104 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 1902 </td>
   <td style="text-align:right;"> 0.6461 </td>
   <td style="text-align:right;"> 0.0640 </td>
   <td style="text-align:right;"> 9.7082 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 0204 </td>
   <td style="text-align:right;"> 0.7352 </td>
   <td style="text-align:right;"> 0.0726 </td>
   <td style="text-align:right;"> 9.7218 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 3002 </td>
   <td style="text-align:right;"> 0.6026 </td>
   <td style="text-align:right;"> 0.0606 </td>
   <td style="text-align:right;"> 9.8296 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2305 </td>
   <td style="text-align:right;"> 0.5244 </td>
   <td style="text-align:right;"> 0.0527 </td>
   <td style="text-align:right;"> 9.8332 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2504 </td>
   <td style="text-align:right;"> 0.5118 </td>
   <td style="text-align:right;"> 0.0515 </td>
   <td style="text-align:right;"> 9.8617 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 1901 </td>
   <td style="text-align:right;"> 0.5399 </td>
   <td style="text-align:right;"> 0.0546 </td>
   <td style="text-align:right;"> 9.9006 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2103 </td>
   <td style="text-align:right;"> 0.4002 </td>
   <td style="text-align:right;"> 0.0404 </td>
   <td style="text-align:right;"> 9.9262 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2601 </td>
   <td style="text-align:right;"> 0.6008 </td>
   <td style="text-align:right;"> 0.0611 </td>
   <td style="text-align:right;"> 9.9594 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2703 </td>
   <td style="text-align:right;"> 0.6194 </td>
   <td style="text-align:right;"> 0.0631 </td>
   <td style="text-align:right;"> 9.9876 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2603 </td>
   <td style="text-align:right;"> 0.6122 </td>
   <td style="text-align:right;"> 0.0625 </td>
   <td style="text-align:right;"> 9.9990 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 3102 </td>
   <td style="text-align:right;"> 0.7093 </td>
   <td style="text-align:right;"> 0.0725 </td>
   <td style="text-align:right;"> 10.0564 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 1201 </td>
   <td style="text-align:right;"> 0.3941 </td>
   <td style="text-align:right;"> 0.0409 </td>
   <td style="text-align:right;"> 10.1563 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 0701 </td>
   <td style="text-align:right;"> 0.6123 </td>
   <td style="text-align:right;"> 0.0634 </td>
   <td style="text-align:right;"> 10.1962 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2802 </td>
   <td style="text-align:right;"> 0.4911 </td>
   <td style="text-align:right;"> 0.0515 </td>
   <td style="text-align:right;"> 10.2721 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 1806 </td>
   <td style="text-align:right;"> 0.5701 </td>
   <td style="text-align:right;"> 0.0601 </td>
   <td style="text-align:right;"> 10.3223 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 1501 </td>
   <td style="text-align:right;"> 0.5392 </td>
   <td style="text-align:right;"> 0.0570 </td>
   <td style="text-align:right;"> 10.3570 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 0408 </td>
   <td style="text-align:right;"> 0.7528 </td>
   <td style="text-align:right;"> 0.0793 </td>
   <td style="text-align:right;"> 10.3658 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 1702 </td>
   <td style="text-align:right;"> 0.5278 </td>
   <td style="text-align:right;"> 0.0559 </td>
   <td style="text-align:right;"> 10.4131 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 0603 </td>
   <td style="text-align:right;"> 0.6444 </td>
   <td style="text-align:right;"> 0.0690 </td>
   <td style="text-align:right;"> 10.4884 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 0801 </td>
   <td style="text-align:right;"> 0.4319 </td>
   <td style="text-align:right;"> 0.0464 </td>
   <td style="text-align:right;"> 10.4972 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 0407 </td>
   <td style="text-align:right;"> 0.6344 </td>
   <td style="text-align:right;"> 0.0677 </td>
   <td style="text-align:right;"> 10.4994 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2107 </td>
   <td style="text-align:right;"> 0.4417 </td>
   <td style="text-align:right;"> 0.0476 </td>
   <td style="text-align:right;"> 10.6130 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 0305 </td>
   <td style="text-align:right;"> 0.6732 </td>
   <td style="text-align:right;"> 0.0729 </td>
   <td style="text-align:right;"> 10.6587 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2102 </td>
   <td style="text-align:right;"> 0.5302 </td>
   <td style="text-align:right;"> 0.0575 </td>
   <td style="text-align:right;"> 10.6641 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 0606 </td>
   <td style="text-align:right;"> 0.6388 </td>
   <td style="text-align:right;"> 0.0696 </td>
   <td style="text-align:right;"> 10.6788 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 0501 </td>
   <td style="text-align:right;"> 0.5157 </td>
   <td style="text-align:right;"> 0.0563 </td>
   <td style="text-align:right;"> 10.6996 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 0902 </td>
   <td style="text-align:right;"> 0.5921 </td>
   <td style="text-align:right;"> 0.0655 </td>
   <td style="text-align:right;"> 10.8437 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 0206 </td>
   <td style="text-align:right;"> 0.5776 </td>
   <td style="text-align:right;"> 0.0638 </td>
   <td style="text-align:right;"> 10.8699 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 1804 </td>
   <td style="text-align:right;"> 0.5700 </td>
   <td style="text-align:right;"> 0.0634 </td>
   <td style="text-align:right;"> 10.9026 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 0605 </td>
   <td style="text-align:right;"> 0.6672 </td>
   <td style="text-align:right;"> 0.0747 </td>
   <td style="text-align:right;"> 10.9664 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 0202 </td>
   <td style="text-align:right;"> 0.6362 </td>
   <td style="text-align:right;"> 0.0714 </td>
   <td style="text-align:right;"> 11.0369 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2508 </td>
   <td style="text-align:right;"> 0.4093 </td>
   <td style="text-align:right;"> 0.0462 </td>
   <td style="text-align:right;"> 11.0654 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 1102 </td>
   <td style="text-align:right;"> 0.4651 </td>
   <td style="text-align:right;"> 0.0531 </td>
   <td style="text-align:right;"> 11.1586 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2106 </td>
   <td style="text-align:right;"> 0.4415 </td>
   <td style="text-align:right;"> 0.0514 </td>
   <td style="text-align:right;"> 11.4504 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 1304 </td>
   <td style="text-align:right;"> 0.4789 </td>
   <td style="text-align:right;"> 0.0563 </td>
   <td style="text-align:right;"> 11.5236 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 0208 </td>
   <td style="text-align:right;"> 0.6595 </td>
   <td style="text-align:right;"> 0.0775 </td>
   <td style="text-align:right;"> 11.5607 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 0502 </td>
   <td style="text-align:right;"> 0.5588 </td>
   <td style="text-align:right;"> 0.0671 </td>
   <td style="text-align:right;"> 11.7577 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2206 </td>
   <td style="text-align:right;"> 0.8007 </td>
   <td style="text-align:right;"> 0.0965 </td>
   <td style="text-align:right;"> 11.8583 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 1601 </td>
   <td style="text-align:right;"> 0.5702 </td>
   <td style="text-align:right;"> 0.0687 </td>
   <td style="text-align:right;"> 11.8589 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 1506 </td>
   <td style="text-align:right;"> 0.5548 </td>
   <td style="text-align:right;"> 0.0676 </td>
   <td style="text-align:right;"> 11.9359 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 1502 </td>
   <td style="text-align:right;"> 0.6285 </td>
   <td style="text-align:right;"> 0.0772 </td>
   <td style="text-align:right;"> 12.0334 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 1002 </td>
   <td style="text-align:right;"> 0.5425 </td>
   <td style="text-align:right;"> 0.0667 </td>
   <td style="text-align:right;"> 12.1009 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2303 </td>
   <td style="text-align:right;"> 0.5484 </td>
   <td style="text-align:right;"> 0.0685 </td>
   <td style="text-align:right;"> 12.2159 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 0411 </td>
   <td style="text-align:right;"> 0.7268 </td>
   <td style="text-align:right;"> 0.0903 </td>
   <td style="text-align:right;"> 12.2206 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 0207 </td>
   <td style="text-align:right;"> 0.6864 </td>
   <td style="text-align:right;"> 0.0868 </td>
   <td style="text-align:right;"> 12.4458 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 0404 </td>
   <td style="text-align:right;"> 0.5903 </td>
   <td style="text-align:right;"> 0.0754 </td>
   <td style="text-align:right;"> 12.5699 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 0409 </td>
   <td style="text-align:right;"> 0.7034 </td>
   <td style="text-align:right;"> 0.0904 </td>
   <td style="text-align:right;"> 12.6515 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2505 </td>
   <td style="text-align:right;"> 0.5465 </td>
   <td style="text-align:right;"> 0.0715 </td>
   <td style="text-align:right;"> 12.8127 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 0209 </td>
   <td style="text-align:right;"> 0.7456 </td>
   <td style="text-align:right;"> 0.0986 </td>
   <td style="text-align:right;"> 13.0068 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 1803 </td>
   <td style="text-align:right;"> 0.5815 </td>
   <td style="text-align:right;"> 0.0788 </td>
   <td style="text-align:right;"> 13.2856 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 1003 </td>
   <td style="text-align:right;"> 0.5552 </td>
   <td style="text-align:right;"> 0.0785 </td>
   <td style="text-align:right;"> 13.9060 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2602 </td>
   <td style="text-align:right;"> 0.6421 </td>
   <td style="text-align:right;"> 0.0929 </td>
   <td style="text-align:right;"> 14.1715 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2509 </td>
   <td style="text-align:right;"> 0.5278 </td>
   <td style="text-align:right;"> 0.0764 </td>
   <td style="text-align:right;"> 14.1854 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 0205 </td>
   <td style="text-align:right;"> 0.7005 </td>
   <td style="text-align:right;"> 0.1019 </td>
   <td style="text-align:right;"> 14.3111 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 0702 </td>
   <td style="text-align:right;"> 0.7351 </td>
   <td style="text-align:right;"> 0.1080 </td>
   <td style="text-align:right;"> 14.4632 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 0210 </td>
   <td style="text-align:right;"> 0.6967 </td>
   <td style="text-align:right;"> 0.1037 </td>
   <td style="text-align:right;"> 14.6450 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 1005 </td>
   <td style="text-align:right;"> 0.5748 </td>
   <td style="text-align:right;"> 0.0856 </td>
   <td style="text-align:right;"> 14.6475 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 0706 </td>
   <td style="text-align:right;"> 0.7822 </td>
   <td style="text-align:right;"> 0.1179 </td>
   <td style="text-align:right;"> 14.8284 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2204 </td>
   <td style="text-align:right;"> 0.6252 </td>
   <td style="text-align:right;"> 0.0947 </td>
   <td style="text-align:right;"> 14.8985 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2302 </td>
   <td style="text-align:right;"> 0.4087 </td>
   <td style="text-align:right;"> 0.0652 </td>
   <td style="text-align:right;"> 15.6126 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 0504 </td>
   <td style="text-align:right;"> 0.5568 </td>
   <td style="text-align:right;"> 0.0899 </td>
   <td style="text-align:right;"> 15.8200 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2306 </td>
   <td style="text-align:right;"> 0.4035 </td>
   <td style="text-align:right;"> 0.0655 </td>
   <td style="text-align:right;"> 15.8646 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 1602 </td>
   <td style="text-align:right;"> 0.6280 </td>
   <td style="text-align:right;"> 0.1020 </td>
   <td style="text-align:right;"> 15.9795 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 1202 </td>
   <td style="text-align:right;"> 0.3644 </td>
   <td style="text-align:right;"> 0.0608 </td>
   <td style="text-align:right;"> 16.3099 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2003 </td>
   <td style="text-align:right;"> 0.6520 </td>
   <td style="text-align:right;"> 0.1086 </td>
   <td style="text-align:right;"> 16.3151 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 0410 </td>
   <td style="text-align:right;"> 0.5266 </td>
   <td style="text-align:right;"> 0.0886 </td>
   <td style="text-align:right;"> 16.5560 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 1004 </td>
   <td style="text-align:right;"> 0.5422 </td>
   <td style="text-align:right;"> 0.1032 </td>
   <td style="text-align:right;"> 18.7286 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2108 </td>
   <td style="text-align:right;"> 0.6988 </td>
   <td style="text-align:right;"> 0.1367 </td>
   <td style="text-align:right;"> 19.2503 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 0705 </td>
   <td style="text-align:right;"> 0.6216 </td>
   <td style="text-align:right;"> 0.1232 </td>
   <td style="text-align:right;"> 19.4951 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 0703 </td>
   <td style="text-align:right;"> 0.6370 </td>
   <td style="text-align:right;"> 0.1347 </td>
   <td style="text-align:right;"> 20.8065 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 1006 </td>
   <td style="text-align:right;"> 0.5964 </td>
   <td style="text-align:right;"> 0.1340 </td>
   <td style="text-align:right;"> 22.1069 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 0505 </td>
   <td style="text-align:right;"> 0.5411 </td>
   <td style="text-align:right;"> 0.1256 </td>
   <td style="text-align:right;"> 22.7386 </td>
  </tr>
</tbody>
</table>

