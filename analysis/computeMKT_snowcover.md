En primer lugar leemos los datos

``` r
################################################################
# Load packages 
library('wq')
```

    ## Loading required package: zoo
    ## 
    ## Attaching package: 'zoo'
    ## 
    ## The following objects are masked from 'package:base':
    ## 
    ##     as.Date, as.Date.numeric

``` r
library('reshape2')
################################################################

################################################################
# Read data
snow <- read.csv(file=paste(di, '/data/snow_sn.csv', sep=''), header=T, sep=';')
################################################################
```

A continuación creamos un bucle para obtener de cada indicador la tendencia y la pendiente. Aplicamos la técnica de Mann-Kendall-Theil-Sen. El bucle realiza las siguientes operaciones:

-   Crea una variable con los indicadores para el bucle.
-   Realiza un subset de datos con los valores del indicador para cada pixel. Obtenemos una serie de datos por cada pixel.
-   Calculamos la tendencia y la pendiente
-   Guardamos los resultamos como csv (uno por cada indicador, ver `./data/`)

``` r
################################################################
# Loop to compute the MKT by indicator

# Define name of indicators (see variables names)
indicadores <- c('scd', 'scod', 'scmd', 'scmc' )

# subset by indicator
for (j in indicadores){ 
  # Create a subset by indicator
  subdf <- snow[, names(snow) %in% c('nie_malla_modi_id', 'ano', j )]
  
  # Manipule data. Transpose 
  # la funcion aggregate es porque tenemos duplicados 
  aux <- dcast(subdf, ano ~ nie_malla_modi_id, value.var = j, fun.aggregate = mean) 

  # Create a zoo object 
  auxts <- zoo(aux[-1],aux[,1])
  
  # Compute Mann Kendall and Theil 
  theil <- mannKen(as.ts(auxts))
  
  # Prepare data output
  theil <- as.data.frame(theil)
  theil$nie_malla_modi_id <- rownames(theil)
  
  # Round the output
  theil$sen.slope <- round(theil$sen.slope, 3)
  theil$sen.slope.pct <- round(theil$sen.slope.pct,2)
  theil$p.value <- round(theil$p.value, 5)
  theil$varS <- round(theil$varS, 2)
  theil$tau <- round(theil$tau, 3)
  
  # Assign output to an dataframe
  assign(j, theil)
  
  # Write table 
  write.table(assign(j, theil), file=paste(di, '/data/', j, '.csv', sep=''), row.names=FALSE, sep=',')
  
  # Check Results. Get the pixel with NA in pvalue. Save results
  aux_na <- theil[!complete.cases(theil),]
  aux_na$var <- rep(j,nrow(aux_na))
  write.table(aux_na[,c(8:9)], file=paste(di, '/data/na_', j, '.csv', sep=''), row.names=FALSE, sep=',')
    
} 
################################################################
```