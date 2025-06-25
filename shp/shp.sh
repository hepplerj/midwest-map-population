for file in *.shp; do
  ogr2ogr -f GeoJSON "${file%.shp}.geojson" "$file"
done
