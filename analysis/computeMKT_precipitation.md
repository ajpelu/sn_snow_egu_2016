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
raw_pre <- read.csv(file=paste(di, '/data/PXpxXyear.csv', sep=''), header=T, sep=',')

# Remove NaN
prec <- raw_pre[which(!is.na(raw_pre$pre)),]

# Rename 
names(prec) <- c('nie_malla_modi_id', 'pre', 'year')
                
################################################################
```

A continuación computamos la tendencia y la pendiente. Aplicamos la técnica de Mann-Kendall-Theil-Sen. El bucle realiza las siguientes operaciones:

``` r
# Prepare data. Transpose 
aux <- dcast(prec, year ~ nie_malla_modi_id, value.var = 'pre') 

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

# Write table 
write.table(theil, file=paste(di, '/data/pre_trend.csv', sep=''), row.names=FALSE, sep=',')
################################################################
```

``` r
# Select only data within SN using scod file 
scod <- read.csv(file=paste(di, '/data/scod.csv', sep=''), header=T, sep=',')
pre_sn <- theil[which(theil$nie_malla_modi_id %in% scod$nie_malla_modi_id), ]


summary(pre_sn)
```

    ##    sen.slope       sen.slope.pct         p.value             S           
    ##  Min.   :-7.0000   Min.   :-1.44000   Min.   :0.3223   Min.   :-21.0000  
    ##  1st Qu.:-1.9300   1st Qu.:-0.38000   1st Qu.:0.8431   1st Qu.: -3.0000  
    ##  Median :-0.5115   Median :-0.09000   Median :0.9212   Median : -1.0000  
    ##  Mean   :-0.1930   Mean   :-0.09057   Mean   :0.9085   Mean   : -0.9754  
    ##  3rd Qu.: 1.4840   3rd Qu.: 0.21000   3rd Qu.:1.0000   3rd Qu.:  1.0000  
    ##  Max.   : 8.9320   Max.   : 1.18000   Max.   :1.0000   Max.   : 11.0000  
    ##       varS            miss        tau            nie_malla_modi_id 
    ##  Min.   :407.3   Min.   :0   Min.   :-0.200000   Length:7994       
    ##  1st Qu.:408.3   1st Qu.:0   1st Qu.:-0.029000   Class :character  
    ##  Median :408.3   Median :0   Median :-0.010000   Mode  :character  
    ##  Mean   :408.3   Mean   :0   Mean   :-0.009338                     
    ##  3rd Qu.:408.3   3rd Qu.:0   3rd Qu.: 0.010000                     
    ##  Max.   :408.3   Max.   :0   Max.   : 0.105000
