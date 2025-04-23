# decreasing-homicide-waves

| Database | Purpose | Location; Creator; Use |
|:-----------------------|:-----------------------|:-----------------------|
| hexhom.csv | Geographic information about each hexagon **geometry**, **year**, **homicides for that year (hom_ct)**, **building count (bldg_cnt)**, **homicide rate (hom_rt)** | Box; UChicago; base_analysis |
| CHI Community Areas.shp | Current neighborhoods in Chicago, where they're located. Many columns but columns of importance are **COMMUNITY** and **geometry** (and maybe shape area and shape length) | data/Community Areas Shapefiles/CHI Community Areas.shp; Unknown to Libby; base_analysis |
| cluster_assignments.csv | extended cluster column (this is based on trajectory analysis starting in 1870 vs the regular cluster column which was based on the analysis starting in 1940) | Slack; David; TBD |
| Hydro_20250423.csv | Chicago Waterways | [Chicago Data Portal](https://data.cityofchicago.org/Parks-Recreation/Waterways/eg9f-z3t6) |
| Parks_Aug2012.zip | Chicago Parks | Once City of Chicago, now [Big Ten Data Alliance](https://geo.btaa.org/catalog/5msb-wbxn)?  |
| parks_water_together.geojson | Chicago Waterways & Parks | Libby created from above two files; to use to recognize low areas we don't necessarily care about  |



Affordable_Rental_Housing_Developments_20250212.csv: Data from Chicago Housing Data Portal, link is https://data.cityofchicago.org/Community-Economic-Development/Affordable-Rental-Housing-Developments/s6ha-ppgi/about_data

hom_decrease.ipynb: initial examination of homicide waves by longest gap in waves and zoomed in on housing projects as well
hom_decrease_2.ipynb: examination of wave intensity, 1-5-10 year dropoff and increases in homicide; also code producing *.html

viz/ *.html: the only important ones you should care about:

    - `merged_viz.html`: dropoffs and increases by 1-5-10 years and by hex clusters
    - `increase_decade_merged.html`: maximum increase by decade and by hex clusters
    - `decrease_decade_merged.html`: maximum decrease by decade and by hex clusters
