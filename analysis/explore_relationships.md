Prepare Data
------------

-   Read data of Mann-Kendal Sen-Slope for each pixels and each indicator: snow-cover related (scd, scod, scmd, scmc) and hydrological model (pre, pre\_snow, pre\_snow\_per, temp). See :red\_circle: (`$TODO`: include link to analysis)
-   Read data of topographic variable :red\_circle: (`$TODO`: Document script)
-   Create two dataframes:
-   Full Dataframe with all variables and all pixels
-   Dataframe with all variables and filter by pixels above 1900 *m.a.s.l.*

``` r
# Read data

# Define name of indicators (see variables names)
indicadores <- c("scd", "scod", "scmd", "scmc", "pre", "pre_snow", "pre_snow_per", "temp")

# Loop to read files 
for (j in indicadores){ 
  aux <- read.csv(file=paste(di, "/data/derived/", j, ".csv", sep= ""),
              header = TRUE,
              sep = ',')
  assign(j, aux)
}


# Read Topographic data 
rawtopo <- read.csv(file=paste(di, "/data/topo_nie_malla_modis.csv", sep=""),
                 header=TRUE,
                 sep = ",") 
```

Explore Snow-cover related indicators
-------------------------------------

We explore the pattern of the trend (*tau*) of the snow-cover indicators (see figure 1).

By exploring the relationship between the trend of two indicators of snow cover (scod: snow cover onset date; and scmd: snow cover melting date) we can describe the temporal evolution of snow cover in Sierra Nevada from 2000-2014. This relationship is evaluated at pixel scale (figure 1c) and we have four potential scenarios:

-   Expansion
-   Contraction
-   Shift (delay and advance)

**Figure 1** ![Figure 1.](/images/snow_cover_profile.png)

We applied this exploratory analysis for all pixels and for all pixels above 1900 *m asl*.

<img src="explore_relationships_files/figure-markdown_github/unnamed-chunk-1-1.png" alt="Figure 2"  />
<p class="caption">
Figure 2
</p>

<img src="explore_relationships_files/figure-markdown_github/unnamed-chunk-2-1.png" alt="Figure 3"  />
<p class="caption">
Figure 3
</p>

A total of 70.31 % of pixels (of all above 1900) showed a positive trend in snow cover onset date (late onset) and a negative trend in snow cover melting date (earlier melting date). It means that of 70.31 % of the pixels above 1900 m, have suffered a trend to rectraction of the snow cover period in the last years (Figure 3).
