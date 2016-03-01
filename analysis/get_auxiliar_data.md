## Get data for pixels 

### Topographic data for pixels 

```sql 
SELECT 
  iv_malla_modis_nie_malla_modis.nie_malla_modi_id, 
  aux.nie_malla_modi_id, 
  iv_malla_modis_nie_malla_modis.iv_malla_modi_id, 
  iv_malla_modis_mde_celdas_20m.id_modis, 
  iv_malla_modis_mde_celdas_20m.id_mde_20m, 
  mde_20_v_top.altura, 
  mde_20_v_top.twi, 
  mde_20_v_top.pendiente, 
  mde_20_v_top.rad_total, 
  mde_20_v_top.rad_prim, 
  mde_20_v_top.rad_inv, 
  mde_20_v_top.rad_oto, 
  mde_20_v_top.rad_ver, 
  mde_20_v_top.id_mde
FROM 
  public.iv_malla_modis_nie_malla_modis, 
  "Proyecto_NDVI".iv_malla_modis_mde_celdas_20m, 
  "Proyecto_NDVI".mde_20_v_top,
  (SELECT DISTINCT
  	nie_indicadors.nie_malla_modi_id
  	FROM 
  	public.nie_indicadors) as aux
WHERE 
  aux.nie_malla_modi_id = iv_malla_modis_nie_malla_modis.nie_malla_modi_id AND
  iv_malla_modis_mde_celdas_20m.id_mde_20m = mde_20_v_top.id_mde AND
  iv_malla_modis_mde_celdas_20m.id_modis = iv_malla_modis_nie_malla_modis.iv_malla_modi_id ORDER BY aux.nie_malla_modi_id;
```

* Save data as `./data/topo_ni_malla_modi_id.csv` 

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



