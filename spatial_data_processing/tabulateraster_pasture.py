from rasterstats import zonal_stats
import pandas as pd

tabulated_geojson = zonal_stats("~/output/spatial_output/countries_tnc_intersect.gpkg", "~/data/global_cropland/pasture_equalarea.vrt", stats = "count min mean max median sum", nodata = float('-inf'), geojson_out = True)

output_properties = [x['properties'] for x in tabulated_geojson]
outputdf = pd.DataFrame(output_properties)

outputdf.to_csv("~/output/spatial_output/global_count_pasture.csv")
