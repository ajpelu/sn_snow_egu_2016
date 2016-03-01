En primer lugar leemos los datos

``` r
################################################################
# Load packages 
library("wq")
```

    ## Loading required package: zoo

    ## 
    ## Attaching package: 'zoo'

    ## The following objects are masked from 'package:base':
    ## 
    ##     as.Date, as.Date.numeric

``` r
library("zoo")
library("reshape2")
library("stringr")
library("dplyr")
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
################################################################

################################################################
# Read data

# Get file names
myfiles <- list.files(path = paste(di, "/data/raw/", sep=""), pattern = "\\.csv$")

# Loop to read files 
for (j in myfiles){ 
  aux <- read.csv(file=paste(di, "/data/raw/", j, sep= ""),
              header = TRUE,
              sep = ',')
  name_aux <- str_replace(j, "\\..*", "") 

  assign(name_aux, aux)
}

# Join Wimmed data
join_aux1 <- inner_join(PnXpxXyear, PorcPnXpxXyear, by=c('id','year'))
join_aux2 <- inner_join(PXpxXyear, TXpxXyear, by=c('id','year'))
join_aux3 <- inner_join(join_aux1, join_aux2, by=c('id','year'))

# Set rigth names
names(join_aux3)[1] <- "nie_malla_modi_id"
names(snow_sn)[3] <- "year"

# Join with snow data
mydf <- inner_join(snow_sn, join_aux3, by=c("nie_malla_modi_id", "year"))

# remove temp files
rm(join_aux1, join_aux2, join_aux3)
```

A continuación creamos un bucle para obtener de cada indicador la tendencia y la pendiente. Aplicamos la técnica de Mann-Kendall-Theil-Sen. El bucle realiza las siguientes operaciones:

-   Crea una variable con los indicadores para el bucle.
-   Realiza un subset de datos con los valores del indicador para cada pixel. Obtenemos una serie de datos por cada pixel.
-   Calculamos la tendencia y la pendiente
-   Guardamos los resultamos como csv (uno por cada indicador, ver `./data/derived/`)

``` r
# Loop to compute the MKT by indicator

# Define name of indicators (see variables names)
indicadores <- c("scd", "scod", "scmd", "scmc", "pre", "pre_snow", "pre_snow_per", "temp")

# subset by indicator
for (j in indicadores){ 
  # Create a subset by indicator
  subdf <- mydf[, names(mydf) %in% c("nie_malla_modi_id", "year", j )]
  
  # Manipule data. Transpose 
  # la funcion aggregate es porque tenemos duplicados 
  aux <- dcast(subdf, year ~ nie_malla_modi_id, value.var = j, fun.aggregate = mean) 

  # Create a zoo object 
  auxts <- zoo(aux[-1],aux[,1])
  
  # Compute Mann Kendall and Theil 
  theil <- mannKen(as.ts(auxts))
  
  # Prepare data output
  theil <- as.data.frame(theil)
  
  theil_df <- theil %>% 
    mutate(nie_malla_modi_id = as.numeric(rownames(theil))) %>%
    mutate(sen_slope = round(sen.slope, 3),
           sen_slope_per = round(sen.slope.pct, 2),
           p_value = round(p.value, 5),
           tau = round(tau, 3)) %>%
    dplyr::select(nie_malla_modi_id, sen_slope, sen_slope_per, p_value, tau)
  
  # As data.frame
  theil_df <- as.data.frame(theil_df)
  
  # Rename variables adding dataframe name
  
  # Create vector of new names 
  new_names <- c(paste(names(theil_df), j ,sep='_'))
  
  theil_rename <- theil_df %>% rename_(.dots = setNames(names(.), new_names)) %>%
    dplyr::select(nie_malla_modi_id = starts_with("nie_malla_modi_id"), everything())
  
  # Write table 
  write.table(assign(j, theil_rename), file=paste(di, "/data/derived/", j, ".csv", sep=""), row.names=FALSE, sep=',')
  
  # Check Results. Get the pixel with NA in pvalue. Save results
  aux_na <- theil_df[!complete.cases(theil_df),]
  aux_na$var <- rep(j,nrow(aux_na))
  write.table(aux_na[,c("nie_malla_modi_id","var")], file=paste(di, "/data/derived/na_", j, ".csv", sep=""), row.names=FALSE, sep=',')
} 
```
