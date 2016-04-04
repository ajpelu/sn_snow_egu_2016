Mann-Kendal Seil Slope analysis for annual and monthly data
-----------------------------------------------------------

### Annual data

En primer lugar leemos los datos

``` r
################################################################
# Load packages 
library("wq")
```

    ## Loading required package: zoo
    ## 
    ## Attaching package: 'zoo'
    ## 
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
    ## 
    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag
    ## 
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

Monthly data
------------

-   Read data
-   Aggregate data (by season):
    -   wi = m01, m02, m03
    -   sp = m04, m05, m06
    -   su = m07; m08; m09
    -   au = m10, m11, m12
-   Compute MKTS by month and by season

### Read an prepare data

``` r
my_var <- c('P_n', 'Pre', 'T_m')  

for (i in my_var){ 

# Get file names
myfiles_month <- list.files(path = paste(di, "/data/raw_month/", sep=""), pattern = paste0("(.*", i, ")"))

# Create a auxiliar file to store all monthly data by variable
superauxiliar <- c() 

# Loop to read files 
for (j in myfiles_month){ 
  aux <- read.csv(file=paste(di, "/data/raw_month/", j, sep= ""),
              header = TRUE,
              sep = ',')
  name_aux <- str_replace(j, "\\..*", "") 
  
  ### Create a variable with the name of the month 
  aux$month <- paste0('m', str_extract_all(j, "\\d+")) 
  
  
  #aux_name <- str_replace(j, "(.*month)","")
  #aux_month <- as.numeric(str_replace(aux_name, "\\..*", ""))
  #aux$month <-  aux_month
  
  # Change name of some variables
  names(aux)[1] <- 'nie_malla_modi_id'
  names(aux)[3] <- 'year' 
  
  # Select only pixel matching snow_cover 
  aux <- aux[aux$nie_malla_modi_id %in% snow_sn$nie_malla_modi_id, ]
  
  # Remove data from 2015 and 2000  
  aux <- aux %>% 
    filter(year < 2015) %>%
    filter(year > 2000)

  # Store as object 
  superauxiliar <- rbind(superauxiliar, aux)
  name_superauxiliar <- i 
  assign(name_superauxiliar, superauxiliar)
}
 
}


# Aggregate value by season: 
# winter = 1,2,3 
# spring = 4,5,6 
# summer = 7,8,9
# autumn = 10,11,12 

# Temperature data 
temp_monthly <- as.data.frame(dcast(T_m, nie_malla_modi_id + year ~ month, value.var = "T_m"))

temp_season4 <- temp_monthly %>% 
  group_by(nie_malla_modi_id, year) %>% 
  summarise(wi = mean(m01,m02,m03),
         sp = mean(m04,m05,m06),
         su = mean(m07,m08,m09),
         au = mean(m10,m11,m12)) 

# Join data 

tempSeas <- temp_monthly %>% 
  inner_join(temp_season4, by=c("nie_malla_modi_id","year"))
## 


# Precipitation data
pre_monthly <- as.data.frame(dcast(Pre, nie_malla_modi_id + year ~ month, value.var = "Pre"))

pre_season4 <- pre_monthly %>% 
  group_by(nie_malla_modi_id, year) %>% 
  summarise(wi = sum(m01,m02,m03),
         sp = sum(m04,m05,m06),
         su = sum(m07,m08,m09),
         au = sum(m10,m11,m12)) 

# Join data 
preSeas <- pre_monthly %>% 
  inner_join(pre_season4, by=c("nie_malla_modi_id","year"))
## 




# Precipitation data
pn_monthly <- as.data.frame(dcast(P_n, nie_malla_modi_id + year ~ month, value.var = "P_n"))

pn_season4 <- pn_monthly %>% 
  group_by(nie_malla_modi_id, year) %>% 
  summarise(wi = sum(m01,m02,m03),
         sp = sum(m04,m05,m06),
         su = sum(m07,m08,m09),
         au = sum(m10,m11,m12)) 

# Join data 
pnSeas <- pn_monthly %>% 
  inner_join(pn_season4, by=c("nie_malla_modi_id","year"))
## 
```

#### Trend of temperatures by month and by season

``` r
# Vector of variables 
## Get names of variables 
indicadores_temp <- names(tempSeas)
## Remove year and nie_modi_id
indicadores_temp <- indicadores_temp[-c(1,2)]


# 
mydf <- tempSeas

# subset by indicator
for (j in indicadores_temp){ 
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
  new_names <- c(paste(names(theil_df), paste0('temp', j) , sep='_'))
  
  theil_rename <- theil_df %>% rename_(.dots = setNames(names(.), new_names)) %>%
    dplyr::select(nie_malla_modi_id = starts_with("nie_malla_modi_id"), everything())
  
  # Write table 
  write.table(assign(j, theil_rename), file=paste(di, "/data/derived/temp", j, ".csv", sep=""), row.names=FALSE, sep=',')
  
  # Check Results. Get the pixel with NA in pvalue. Save results
  aux_na <- theil_df[!complete.cases(theil_df),]
  aux_na$var <- rep(j,nrow(aux_na))
  write.table(aux_na[,c("nie_malla_modi_id","var")], file=paste(di, "/data/derived/na_temp", j, ".csv", sep=""), row.names=FALSE, sep=',')
}
```

#### Trend of precipitation (cummulative) by month and by season

``` r
# Vector of variables 
## Get names of variables 
indicadores_pre <- names(preSeas)
## Remove year and nie_modi_id
indicadores_pre <- indicadores_pre[-c(1,2)]


# 
mydf <- preSeas

# subset by indicator
for (j in indicadores_pre){ 
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
  new_names <- c(paste(names(theil_df), paste0('pre', j) , sep='_'))
  
  theil_rename <- theil_df %>% rename_(.dots = setNames(names(.), new_names)) %>%
    dplyr::select(nie_malla_modi_id = starts_with("nie_malla_modi_id"), everything())
  
  # Write table 
  write.table(assign(j, theil_rename), file=paste(di, "/data/derived/pre", j, ".csv", sep=""), row.names=FALSE, sep=',')
  
  # Check Results. Get the pixel with NA in pvalue. Save results
  aux_na <- theil_df[!complete.cases(theil_df),]
  aux_na$var <- rep(j,nrow(aux_na))
  write.table(aux_na[,c("nie_malla_modi_id","var")], file=paste(di, "/data/derived/na_pre", j, ".csv", sep=""), row.names=FALSE, sep=',')
}
```

#### Trend of precipitation snow (cummulative) by month and by season

``` r
# Vector of variables 
## Get names of variables 
indicadores_pn <- names(pnSeas)
## Remove year and nie_modi_id
indicadores_pn <- indicadores_pn[-c(1,2)]


# 
mydf <- pnSeas

# subset by indicator
for (j in indicadores_pn){ 
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
  new_names <- c(paste(names(theil_df), paste0('pn', j) , sep='_'))
  
  theil_rename <- theil_df %>% rename_(.dots = setNames(names(.), new_names)) %>%
    dplyr::select(nie_malla_modi_id = starts_with("nie_malla_modi_id"), everything())
  
  # Write table 
  write.table(assign(j, theil_rename), file=paste(di, "/data/derived/pn", j, ".csv", sep=""), row.names=FALSE, sep=',')
  
  # Check Results. Get the pixel with NA in pvalue. Save results
  aux_na <- theil_df[!complete.cases(theil_df),]
  aux_na$var <- rep(j,nrow(aux_na))
  write.table(aux_na[,c("nie_malla_modi_id","var")], file=paste(di, "/data/derived/na_pn", j, ".csv", sep=""), row.names=FALSE, sep=',')
}
```
