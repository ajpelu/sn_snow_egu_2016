## Script's list 

# Prepare data

## Auxiliar data 
* [SQL queries](./analysis/get_auxiliar_data.md) to get topographic attributes and coordinates of pixels. 
* [Script](./analysis/get_region_by_pixel.md) to obtain the hydrological basin of each pixel

# Analysis 
## Mann-Kendall Seil Sen Slope Analysis 
This [script](./analysis/computeMK_all.md) computes Mann-Kendall trend test and Sen's slope estimator for each variable. We computed the MKTS for different variables at different temporal scales. 

* For **snow-cover** related indicators (i.e.: scd; scod; scmd; scmc; all derived from MODIS) we used annual data. 
* For variables dervied from **WiMMed**, we used three temporal scales: annual, monthly and seasonal data. 

