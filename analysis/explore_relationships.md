Prepare Data
------------

-   Read data of Mann-Kendal Sen-Slope for each pixels and each indicator: snow-cover related (scd, scod, scmd, scmc) and hydrological model (annual pre, pre\_snow, pre\_snow\_per, temp; and seasonal pre, pn and temp).
-   Read data of topographic variable :red\_circle: (`$TODO`: Document script)
-   Create two dataframes:
-   Full Dataframe with all variables and all pixels
-   Dataframe with all variables and filter by pixels above 1250 *m.a.s.l.*

``` r
# Read Trend analysis data
# Define name of indicators (see variables names)

indicadores <- c("scd", "scod", "scmd", "scmc",
                 "pre", "pre_snow", "pre_snow_per", "temp", 
                 "pnau", "pnsp", "pnsu", "pnwi",
                 "preau", "presp", "presu", "prewi",
                 "tempau", "tempsp", "tempsu", "tempwi")

# Loop to read files 
for (j in indicadores){ 
  aux <- read.csv(file=paste(di, "/data/derived/", j, ".csv", sep= ""),
              header = TRUE,
              sep = ',')
  assign(j, aux)
}
# --

# Define pixels of interes 
pixels_interes <- pre$nie_malla_modi_id

# Read Topographic data 
rawtopo <- read.csv(file=paste(di, "/data/topo_nie_malla_modis.csv", sep=""),
                    header=TRUE,
                    sep = ",") 

# function to convert radian to degree 
rad2deg <- function(rad) {(rad * 180) / (pi)} 


topo <- rawtopo %>% 
  filter(id %in% pixels_interes) %>% 
  mutate(nie_malla_modi_id = id, 
         slope50mean_deg = rad2deg(slope50mean),
         slope50median_deg = rad2deg(slope50median),
         aspect50mean_deg = rad2deg(aspect50mean),
         aspect50median_deg = rad2deg(aspect50median)) %>%
  dplyr::select(nie_malla_modi_id, dem50mean, dem50median, slope50mean_deg, 
                slope50median_deg, aspect50mean_deg, aspect50median_deg) 

## Create interval variables (250 m) for dem; and classify aspect into 8 categories 
topo <- topo %>% 
  mutate(dem50mean_group = cut(dem50mean, 
                         breaks = seq(from=0, to=3500, by=250),
                         labels = c("0-250", "251-500","501-750","751-1000",
                                    "1001-1250","1251-1500","1501-1750","1751-2000",
                                    "2001-2250","2251-2500","2501-2750", "2751-3000",
                                    "3001-3250", "3251-3500")),
         aspect50mean_deg_group = cut(aspect50mean_deg, 
                                      breaks= c(22.5, 67.5, 112.5, 157.5, 202.5, 247.5, 292.5, 337.5, 359.5),
                                      labels = c("N", "NE", "E", "SE", "S", "SW", "W", "NW")))


# --

# Read spatial data and Get lat/long
centroides <- rgdal::readOGR(dsn=paste(di, "/data/geoinfo", sep=""),
                             layer = "centroides_selected", verbose = FALSE)
# Select only attributes of interest and rename them
centroides <- centroides[c("id")]

# Create lat/lng by id 
xycentroides <- cbind(centroides@data, coordinates(centroides))
names(xycentroides) <- c("nie_malla_modi_id", "lon","lat")

xycentroides <- filter(xycentroides, nie_malla_modi_id %in% pixels_interes)
# -- 

# Hydrological basin 
basin <- read.csv(file=paste(di, "/data/derived/pixel_region.csv", sep=""),
                    header=TRUE,
                    sep = ",") 
# --


# Create un dataframe con todos los datos
fulldf <- topo %>% 
  inner_join(scd,  by=c("nie_malla_modi_id")) %>% 
  inner_join(scod,  by=c("nie_malla_modi_id")) %>% 
  inner_join(scmd,  by=c("nie_malla_modi_id")) %>% 
  inner_join(scmc,  by=c("nie_malla_modi_id")) %>%
  inner_join(pre,  by=c("nie_malla_modi_id")) %>% 
  inner_join(pre_snow,  by=c("nie_malla_modi_id")) %>% 
  inner_join(pre_snow_per,  by=c("nie_malla_modi_id")) %>% 
  inner_join(temp,  by=c("nie_malla_modi_id")) %>% 
  inner_join(pnau,  by=c("nie_malla_modi_id")) %>% 
  inner_join(pnsp,  by=c("nie_malla_modi_id")) %>% 
  inner_join(pnsu,  by=c("nie_malla_modi_id")) %>% 
  inner_join(pnwi,  by=c("nie_malla_modi_id")) %>% 
  inner_join(preau,  by=c("nie_malla_modi_id")) %>% 
  inner_join(presp,  by=c("nie_malla_modi_id")) %>% 
  inner_join(presu,  by=c("nie_malla_modi_id")) %>% 
  inner_join(prewi,  by=c("nie_malla_modi_id")) %>% 
  inner_join(tempau,  by=c("nie_malla_modi_id")) %>% 
  inner_join(tempsp,  by=c("nie_malla_modi_id")) %>% 
  inner_join(tempsu,  by=c("nie_malla_modi_id")) %>% 
  inner_join(tempwi,  by=c("nie_malla_modi_id")) %>% 
  inner_join(xycentroides, by="nie_malla_modi_id") %>%
  inner_join(basin, by="nie_malla_modi_id")


# Create subset of pixels above 1250 
fulldf1250 <- fulldf %>% 
  filter(dem50mean > 1250)  
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

![Figure 2](explore_relationships_files/figure-markdown_github/unnamed-chunk-1-1.png)

![Figure 3](explore_relationships_files/figure-markdown_github/unnamed-chunk-2-1.png)

``` r
aux <- round(((df %>% filter(tau_scod > 0) %>% filter(tau_scmd < 0) %>% count()) / df %>% count())*100, 2)
```

A total of 63.55 % of pixels (of all above 1250) showed a positive trend in snow cover onset date (late onset) and a negative trend in snow cover melting date (earlier melting date). It means that of 63.55 % of the pixels above 1250 m, have suffered a trend to rectraction of the snow cover period in the last years (Figure 3).

:red\_circle: `TODO$:` Hacer mapa de esto

We also can explore this relationship by elevation (Figure 4), and we obtanied that the *retraction pattern* is more evident at low elevations

![](explore_relationships_files/figure-markdown_github/unnamed-chunk-5-1.png)

``` r
# Compute percentage of pixels (tau scod > 0, tau_scmd < 0)   
aux <- df %>% group_by(dem50mean_group) %>% 
  summarise(total=n())

aux1 <- df %>% group_by(dem50mean_group) %>% 
  filter(tau_scod > 0) %>% 
  filter(tau_scmd < 0) %>% 
  summarise(n=n()) %>%
  inner_join(aux, by="dem50mean_group") %>%
  mutate(freq = round((n / total)*100,2))
pander(aux1, caption = '% pixels scod tau_scod > 0 & tau_scmd < 0')
```

<table style="width:53%;">
<caption>% pixels scod tau_scod &gt; 0 &amp; tau_scmd &lt; 0</caption>
<colgroup>
<col width="25%" />
<col width="5%" />
<col width="11%" />
<col width="11%" />
</colgroup>
<thead>
<tr class="header">
<th align="center">dem50mean_group</th>
<th align="center">n</th>
<th align="center">total</th>
<th align="center">freq</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="center">1251-1500</td>
<td align="center">419</td>
<td align="center">1235</td>
<td align="center">33.93</td>
</tr>
<tr class="even">
<td align="center">1501-1750</td>
<td align="center">910</td>
<td align="center">1377</td>
<td align="center">66.09</td>
</tr>
<tr class="odd">
<td align="center">1751-2000</td>
<td align="center">964</td>
<td align="center">1211</td>
<td align="center">79.60</td>
</tr>
<tr class="even">
<td align="center">2001-2250</td>
<td align="center">738</td>
<td align="center">977</td>
<td align="center">75.54</td>
</tr>
<tr class="odd">
<td align="center">2251-2500</td>
<td align="center">456</td>
<td align="center">704</td>
<td align="center">64.77</td>
</tr>
<tr class="even">
<td align="center">2501-2750</td>
<td align="center">278</td>
<td align="center">470</td>
<td align="center">59.15</td>
</tr>
<tr class="odd">
<td align="center">2751-3000</td>
<td align="center">227</td>
<td align="center">314</td>
<td align="center">72.29</td>
</tr>
<tr class="even">
<td align="center">3001-3250</td>
<td align="center">64</td>
<td align="center">95</td>
<td align="center">67.37</td>
</tr>
<tr class="odd">
<td align="center">3251-3500</td>
<td align="center">5</td>
<td align="center">7</td>
<td align="center">71.43</td>
</tr>
</tbody>
</table>

``` r
aux2 <- df %>% group_by(dem50mean_group) %>% 
  filter(tau_scod > 0.25) %>% 
  filter(tau_scmd < -0.25) %>% 
  summarise(n=n()) %>%
  inner_join(aux, by="dem50mean_group") %>%
  mutate(freq = round((n / total)*100,2))
pander(aux2, caption = '% pixels scod tau_scod > 0.25 & tau_scmd < -0.25')
```

<table style="width:53%;">
<caption>% pixels scod tau_scod &gt; 0.25 &amp; tau_scmd &lt; -0.25</caption>
<colgroup>
<col width="25%" />
<col width="5%" />
<col width="11%" />
<col width="11%" />
</colgroup>
<thead>
<tr class="header">
<th align="center">dem50mean_group</th>
<th align="center">n</th>
<th align="center">total</th>
<th align="center">freq</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="center">1251-1500</td>
<td align="center">12</td>
<td align="center">1235</td>
<td align="center">0.97</td>
</tr>
<tr class="even">
<td align="center">1501-1750</td>
<td align="center">136</td>
<td align="center">1377</td>
<td align="center">9.88</td>
</tr>
<tr class="odd">
<td align="center">1751-2000</td>
<td align="center">225</td>
<td align="center">1211</td>
<td align="center">18.58</td>
</tr>
<tr class="even">
<td align="center">2001-2250</td>
<td align="center">75</td>
<td align="center">977</td>
<td align="center">7.68</td>
</tr>
<tr class="odd">
<td align="center">2251-2500</td>
<td align="center">36</td>
<td align="center">704</td>
<td align="center">5.11</td>
</tr>
<tr class="even">
<td align="center">2501-2750</td>
<td align="center">29</td>
<td align="center">470</td>
<td align="center">6.17</td>
</tr>
<tr class="odd">
<td align="center">2751-3000</td>
<td align="center">3</td>
<td align="center">314</td>
<td align="center">0.96</td>
</tr>
</tbody>
</table>

Table of relationships
======================

``` r
indicadores <- c("pre", "pre_snow", "pre_snow_per", "temp", 
                 "pnau", "pnsp", "pnsu", "pnwi",
                 "preau", "presp", "presu", "prewi",
                 "tempau", "tempsp", "tempsu", "tempwi")

d <- data.frame(variable = indicadores, 
                scd = c(1,1,1,1, rep(0,12)),
                scod = c(rep(0,4),1,1,0,0,1,1,0,0,1,1,0,0),
                scmd = c(rep(0,5),1,1,0,0,1,1,0,0,1,1,0))
pander(d, caption = 'Potential relationships')
```

<table style="width:46%;">
<caption>Potential relationships</caption>
<colgroup>
<col width="18%" />
<col width="8%" />
<col width="9%" />
<col width="9%" />
</colgroup>
<thead>
<tr class="header">
<th align="center">variable</th>
<th align="center">scd</th>
<th align="center">scod</th>
<th align="center">scmd</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="center">pre</td>
<td align="center">1</td>
<td align="center">0</td>
<td align="center">0</td>
</tr>
<tr class="even">
<td align="center">pre_snow</td>
<td align="center">1</td>
<td align="center">0</td>
<td align="center">0</td>
</tr>
<tr class="odd">
<td align="center">pre_snow_per</td>
<td align="center">1</td>
<td align="center">0</td>
<td align="center">0</td>
</tr>
<tr class="even">
<td align="center">temp</td>
<td align="center">1</td>
<td align="center">0</td>
<td align="center">0</td>
</tr>
<tr class="odd">
<td align="center">pnau</td>
<td align="center">0</td>
<td align="center">1</td>
<td align="center">0</td>
</tr>
<tr class="even">
<td align="center">pnsp</td>
<td align="center">0</td>
<td align="center">1</td>
<td align="center">1</td>
</tr>
<tr class="odd">
<td align="center">pnsu</td>
<td align="center">0</td>
<td align="center">0</td>
<td align="center">1</td>
</tr>
<tr class="even">
<td align="center">pnwi</td>
<td align="center">0</td>
<td align="center">0</td>
<td align="center">0</td>
</tr>
<tr class="odd">
<td align="center">preau</td>
<td align="center">0</td>
<td align="center">1</td>
<td align="center">0</td>
</tr>
<tr class="even">
<td align="center">presp</td>
<td align="center">0</td>
<td align="center">1</td>
<td align="center">1</td>
</tr>
<tr class="odd">
<td align="center">presu</td>
<td align="center">0</td>
<td align="center">0</td>
<td align="center">1</td>
</tr>
<tr class="even">
<td align="center">prewi</td>
<td align="center">0</td>
<td align="center">0</td>
<td align="center">0</td>
</tr>
<tr class="odd">
<td align="center">tempau</td>
<td align="center">0</td>
<td align="center">1</td>
<td align="center">0</td>
</tr>
<tr class="even">
<td align="center">tempsp</td>
<td align="center">0</td>
<td align="center">1</td>
<td align="center">1</td>
</tr>
<tr class="odd">
<td align="center">tempsu</td>
<td align="center">0</td>
<td align="center">0</td>
<td align="center">1</td>
</tr>
<tr class="even">
<td align="center">tempwi</td>
<td align="center">0</td>
<td align="center">0</td>
<td align="center">0</td>
</tr>
</tbody>
</table>

First we explore the relationship between topographic variables and snow-cover related variables.

Notas
-----

(see Chen et al. 2015) \* Dd &lt;- duration of snow cover \* Do &lt;- day of onset \* De &lt;- day of end

Analysis of the spatio-temporal pattern of this variables

### Elevation pattern

``` r
# Compute mean, sd, se of tau variables by elevation interval and plot them 
taus <- fulldf1900 %>% 
  select(contains("tau"), dem50mean_group) 
taus <- melt(taus, id=c("dem50mean_group"))

taus <- taus %>% 
  group_by(dem50mean_group, variable) %>% 
  summarise(mean = mean(value),
            sd = sd(value),
            se=sd(value)/sqrt(length(value)))

ggplot(taus, aes(x=dem50mean_group, y=mean, group=variable)) +
  geom_line() + geom_errorbar(aes(ymax = mean + se, ymin= mean - se), width=.15) +
  geom_point(size=3, shape=21, fill="white") +
  facet_wrap(~variable) +
  theme_bw() + xlab('elevation') + ylab('tau (average value)') +
  theme(axis.text.x=element_text(angle = -90, hjust = 0))


# Explore trends of temperatures and snow-cover indicators potentially related with temperature
df <- taus %>% filter(variable %in% c("tau_scod","tau_scmd", "tau_scd", "tau_temp"))

ggplot(df, aes(x=dem50mean_group, y=mean, group=variable)) +
  geom_line(aes(col=variable)) + 
  geom_errorbar(aes(col=variable, ymax = mean + se, ymin= mean - se), width=.15) +
  geom_point(aes(col=variable), size=3, shape=21, fill="white") + 
  theme_bw() + xlab('elevation') + ylab('tau (average value)') +
  theme(axis.text.x=element_text(angle = -90, hjust = 0))
 

# Explore trends of temperatures and snow-cover indicators potentially related with temperature
df <- taus %>% filter(variable %in% c("tau_scod","tau_scmd", "tau_scd", "tau_pre_snow"))

ggplot(df, aes(x=dem50mean_group, y=mean, group=variable)) +
  geom_line(aes(col=variable)) + 
  geom_errorbar(aes(col=variable, ymax = mean + se, ymin= mean - se), width=.15) +
  geom_point(aes(col=variable), size=3, shape=21, fill="white") + 
  theme_bw() + xlab('elevation') + ylab('tau (average value)') +
  theme(axis.text.x=element_text(angle = -90, hjust = 0))


####

# Compute mean, sd, se of tau variables by elevation interval and plot them 
sen <- fulldf1900 %>% 
  select(contains("sen_slope"), aspect50mean_deg) %>%
  select(-contains("slope_per")) %>% 
  mutate(aspect50mean_deg_group = cut(aspect50mean_deg, 
    breaks= c(22.5, 67.5, 112.5, 157.5, 202.5, 247.5, 292.5, 337.5, 359.5),
    labels = c("N", "NE", "E", "SE", "S", "SW", "W", "NW")))

taus <- fulldf1900 %>% 
  select(contains("tau"), aspect50mean_deg) %>%
  mutate(aspect50mean_deg_group = cut(aspect50mean_deg, 
    breaks= c(22.5, 67.5, 112.5, 157.5, 202.5, 247.5, 292.5, 337.5, 359.5),
    labels = c("N", "NE", "E", "SE", "S", "SW", "W", "NW")))

plot(taus$tau_scmd~taus$aspect50mean_deg_group, col='grey')
abline(lm(taus$tau_scod~taus$aspect50mean_deg))

plot(taus$tau_scd~taus$aspect50mean_deg, col='grey')
abline(lm(taus$tau_scd~taus$aspect50mean_deg))









sen <- melt(sen, id=c("dem50mean_group"))

sen <- sen %>% 
  group_by(dem50mean_group, variable) %>% 
  summarise(mean = mean(value),
            sd = sd(value),
            se=sd(value)/sqrt(length(value)))

ggplot(sen, aes(x=dem50mean_group, y=mean, group=variable)) +
  geom_line() + geom_errorbar(aes(ymax = mean + se, ymin= mean - se), width=.15) +
  geom_point(size=3, shape=21, fill="white") +
  facet_wrap(~variable) +
  theme_bw() + xlab('elevation') + ylab('tau (average value)') +
  theme(axis.text.x=element_text(angle = -90, hjust = 0))






)## Analysis of the correlation between taus. 

### Relation between *scod* and *scmd* 



## Explore Slopes 
```

``` r
# Exploratory for pixels >1900
ggplot(fulldf1900, aes(x=sen_slope_scod, y=sen_slope_scmd)) +
  geom_point(alpha=0.5) + 
  xlim(-7.5,7.5) + ylim(-7.5,7) + 
  geom_vline(xintercept=0) + geom_hline(yintercept=0) + 
  theme_bw() + 
  labs(title= 'scod vs. scmd (>1900 masl)',
       x= 'Slope (days) of Snow cover onset date',
       y='Slope (days) of Snow cover melting date') 

# by elevation group 
ggplot(fulldf1900, aes(x=sen_slope_scod, y=sen_slope_scmd)) + 
  geom_point(alpha=0.5) + 
  xlim(-7.5,7.5) + ylim(-7.5,7) +
  geom_vline(xintercept=0) +
  geom_hline(yintercept=0) +
  labs(title= 'scod vs. scmd (>1900 masl)',
       x= 'Slope (days) of Snow cover onset date',
       y='Slope (days) of Snow cover melting date') +
  theme_bw() + 
  theme(strip.background = element_rect(fill = "white")) +
  facet_wrap(~dem50mean_group)
```
