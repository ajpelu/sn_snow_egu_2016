## Get data for pixels 

### Coordinates for pixels 

```sql 
SELECT 
  nie_malla_modis.id, 
  ST_X(nie_malla_modis.centroide), 
  ST_Y(nie_malla_modis.centroide) 
FROM 
  public.nie_malla_modis,
  (SELECT DISTINCT
    nie_indicadors.nie_malla_modi_id
   FROM 
    public.nie_indicadors) as aux
WHERE 
  nie_malla_modis.id = aux.nie_malla_modi_id; 
```

* Save data as `./data/geoinfo/latlong_nie_malla_modi_id.csv` 

### Topographic data for pixels 

We have two tables:

* `mde` (mde_celdas_20m): a spatial table with the grid of DEM (20 m). EPSG: 23030
* `nie` (nie_malla_modis): a spatial table with the grid of MODIS snow attributes 

First we select all the pixel of the mde within nie table 

```sql
CREATE TABLE nie_malla_modis_mde20 AS
SELECT 
  mde_celdas_20m.id as mde_id, 
  nie_malla_modis.id as nie_malla_modis_id
FROM 
  "Proyecto_NDVI".mde_celdas_20m,  
  public.nie_malla_modis
WHERE 
  ST_Within(ST_Transform(mde_celdas_20m.geom, 4326), nie_malla_modis.the_geom);
```

Then we select the topographic variables of interest 

```sql 
SELECT 
  nie_malla_modis_mde20.nie_malla_modis_id, 
  mde_20_v_top.altura, 
  mde_20_v_top.twi, 
  mde_20_v_top.pendiente, 
  mde_20_v_top.rad_total, 
  mde_20_v_top.rad_prim, 
  mde_20_v_top.rad_inv, 
  mde_20_v_top.rad_ver, 
  mde_20_v_top.rad_oto
FROM 
  public.nie_malla_modis_mde20, 
  "Proyecto_NDVI".mde_20_v_top
WHERE 
  nie_malla_modis_mde20.mde_id = mde_20_v_top.id_mde;
```

* Save data as `./data/topo_ni_malla_modi_id.csv` 

