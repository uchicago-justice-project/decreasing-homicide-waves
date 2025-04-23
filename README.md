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

.
├── Affordable_Rental_Housing_Developments_20250212.csv
├── Affordable_Rental_Housing_Developments_20250212.csv:Zone.Identifier
├── IndividualHomicides_1965_2022.csv
├── IndividualHomicides_1965_2022.csv:Zone.Identifier
├── Jada_Hexagons
│   ├── HexHom2024.R
│   ├── HexHom2024.R:Zone.Identifier
│   ├── HexHom2024.R:Zone.Identifier:Zone.Identifier
│   ├── hex_hom_bldg.dbf
│   ├── hex_hom_bldg.dbf:Zone.Identifier
│   ├── hex_hom_bldg.dbf:Zone.Identifier:Zone.Identifier
│   ├── hex_hom_bldg.prj
│   ├── hex_hom_bldg.prj:Zone.Identifier
│   ├── hex_hom_bldg.shp
│   ├── hex_hom_bldg.shp:Zone.Identifier
│   ├── hex_hom_bldg.shp:Zone.Identifier:Zone.Identifier
│   ├── hex_hom_bldg.shx
│   ├── hex_hom_bldg.shx:Zone.Identifier
│   └── hex_hom_bldg.shx:Zone.Identifier:Zone.Identifier
├── README.md
├── alt.LayerChart(...).json
├── cha_projects.geojson
├── cha_projects.geojson:Zone.Identifier
├── chicago_hexagons_with_wards_join.shp
├── chicago_hexagons_with_wards_join.shp:Zone.Identifier
├── chicago_hexagons_with_wards_join.shx
├── chicago_hexagons_with_wards_join.shx:Zone.Identifier
├── cluster_assignments.csv
├── cluster_assignments.csv:Zone.Identifier
├── comm_area_shapefiles
│   ├── CHI Community Areas.cpg
│   ├── CHI Community Areas.cpg:Zone.Identifier
│   ├── CHI Community Areas.cpg:Zone.Identifier:Zone.Identifier
│   ├── CHI Community Areas.dbf
│   ├── CHI Community Areas.dbf:Zone.Identifier
│   ├── CHI Community Areas.prj
│   ├── CHI Community Areas.prj:Zone.Identifier
│   ├── CHI Community Areas.sbn
│   ├── CHI Community Areas.sbn:Zone.Identifier
│   ├── CHI Community Areas.sbn:Zone.Identifier:Zone.Identifier
│   ├── CHI Community Areas.sbx
│   ├── CHI Community Areas.sbx:Zone.Identifier
│   ├── CHI Community Areas.shp
│   ├── CHI Community Areas.shp:Zone.Identifier
│   ├── CHI Community Areas.shp:Zone.Identifier:Zone.Identifier
│   ├── CHI Community Areas.shx
│   ├── CHI Community Areas.shx:Zone.Identifier
│   └── CHI Community Areas.shx:Zone.Identifier:Zone.Identifier
├── dir_dist
│   ├── 1870_2022.gif
│   ├── 1874_1930.gif
│   ├── 1874_1964.gif
│   ├── 1940_1964.gif
│   ├── 1940_2022.gif
│   ├── dist_1870.png
│   ├── dist_1871.png
│   ├── dist_1872.png
│   ├── dist_1873.png
│   ├── dist_1874.png
│   ├── dist_1875.png
│   ├── dist_1876.png
│   ├── dist_1877.png
│   ├── dist_1878.png
│   ├── dist_1879.png
│   ├── dist_1880.png
│   ├── dist_1881.png
│   ├── dist_1882.png
│   ├── dist_1883.png
│   ├── dist_1884.png
│   ├── dist_1885.png
│   ├── dist_1886.png
│   ├── dist_1887.png
│   ├── dist_1888.png
│   ├── dist_1889.png
│   ├── dist_1890.png
│   ├── dist_1891.png
│   ├── dist_1892.png
│   ├── dist_1893.png
│   ├── dist_1894.png
│   ├── dist_1895.png
│   ├── dist_1896.png
│   ├── dist_1897.png
│   ├── dist_1898.png
│   ├── dist_1899.png
│   ├── dist_1900.png
│   ├── dist_1901.png
│   ├── dist_1902.png
│   ├── dist_1903.png
│   ├── dist_1904.png
│   ├── dist_1905.png
│   ├── dist_1906.png
│   ├── dist_1907.png
│   ├── dist_1908.png
│   ├── dist_1909.png
│   ├── dist_1910.png
│   ├── dist_1911.png
│   ├── dist_1912.png
│   ├── dist_1913.png
│   ├── dist_1914.png
│   ├── dist_1915.png
│   ├── dist_1916.png
│   ├── dist_1917.png
│   ├── dist_1918.png
│   ├── dist_1919.png
│   ├── dist_1920.png
│   ├── dist_1921.png
│   ├── dist_1922.png
│   ├── dist_1923.png
│   ├── dist_1924.png
│   ├── dist_1925.png
│   ├── dist_1926.png
│   ├── dist_1927.png
│   ├── dist_1928.png
│   ├── dist_1929.png
│   ├── dist_1930.png
│   ├── dist_1931.png
│   ├── dist_1932.png
│   ├── dist_1933.png
│   ├── dist_1934.png
│   ├── dist_1935.png
│   ├── dist_1936.png
│   ├── dist_1937.png
│   ├── dist_1938.png
│   ├── dist_1939.png
│   ├── dist_1940.png
│   ├── dist_1941.png
│   ├── dist_1942.png
│   ├── dist_1943.png
│   ├── dist_1944.png
│   ├── dist_1945.png
│   ├── dist_1946.png
│   ├── dist_1947.png
│   ├── dist_1948.png
│   ├── dist_1949.png
│   ├── dist_1950.png
│   ├── dist_1951.png
│   ├── dist_1952.png
│   ├── dist_1953.png
│   ├── dist_1954.png
│   ├── dist_1955.png
│   ├── dist_1956.png
│   ├── dist_1957.png
│   ├── dist_1958.png
│   ├── dist_1959.png
│   ├── dist_1960.png
│   ├── dist_1961.png
│   ├── dist_1962.png
│   ├── dist_1963.png
│   ├── dist_1964.png
│   ├── dist_1965.png
│   ├── dist_1966.png
│   ├── dist_1967.png
│   ├── dist_1968.png
│   ├── dist_1969.png
│   ├── dist_1970.png
│   ├── dist_1971.png
│   ├── dist_1972.png
│   ├── dist_1973.png
│   ├── dist_1974.png
│   ├── dist_1975.png
│   ├── dist_1976.png
│   ├── dist_1977.png
│   ├── dist_1978.png
│   ├── dist_1979.png
│   ├── dist_1980.png
│   ├── dist_1981.png
│   ├── dist_1982.png
│   ├── dist_1983.png
│   ├── dist_1984.png
│   ├── dist_1985.png
│   ├── dist_1986.png
│   ├── dist_1987.png
│   ├── dist_1988.png
│   ├── dist_1989.png
│   ├── dist_1990.png
│   ├── dist_1991.png
│   ├── dist_1992.png
│   ├── dist_1993.png
│   ├── dist_1994.png
│   ├── dist_1995.png
│   ├── dist_1996.png
│   ├── dist_1997.png
│   ├── dist_1998.png
│   ├── dist_1999.png
│   ├── dist_2000.png
│   ├── dist_2001.png
│   ├── dist_2002.png
│   ├── dist_2003.png
│   ├── dist_2004.png
│   ├── dist_2005.png
│   ├── dist_2006.png
│   ├── dist_2007.png
│   ├── dist_2008.png
│   ├── dist_2009.png
│   ├── dist_2010.png
│   ├── dist_2011.png
│   ├── dist_2012.png
│   ├── dist_2013.png
│   ├── dist_2014.png
│   ├── dist_2015.png
│   ├── dist_2016.png
│   ├── dist_2017.png
│   ├── dist_2018.png
│   ├── dist_2019.png
│   ├── dist_2020.png
│   ├── dist_2021.png
│   └── dist_2022.png
├── gap_outliers.csv
├── hexhom.csv
├── hexhom.csv:Zone.Identifier
├── hom_decrease.ipynb
├── hom_decrease.ipynb:Zone.Identifier
├── hom_decrease_2.ipynb
├── hom_decrease_3.ipynb
├── homcide-locations_1940-1964.csv
├── homcide-locations_1940-1964.csv:Zone.Identifier
├── homicide-locations_1870-1930.csv
├── homicide-locations_1870-1930.csv:Zone.Identifier
├── homicides_1940-1965_geocoded_v3.shp
├── homicides_1940-1965_geocoded_v3.shp:Zone.Identifier
├── homicides_1940-1965_geocoded_v3.shx
├── homicides_1940-1965_geocoded_v3.shx:Zone.Identifier
├── locs_homicide_post_65.geojson
├── locs_homicide_post_65.geojson:Zone.Identifier
├── map_data.csv
├── map_projects.csv
├── projects_gap_outliers_wonky.csv
├── south_chicago.geojson
├── south_chicago.geojson:Zone.Identifier
├── viz
│   ├── base.png
│   ├── concat_decade_merged.html
│   ├── decrease_decade_1870.html
│   ├── decrease_decade_1880.html
│   ├── decrease_decade_1890.html
│   ├── decrease_decade_1900.html
│   ├── decrease_decade_1910.html
│   ├── decrease_decade_1920.html
│   ├── decrease_decade_1930.html
│   ├── decrease_decade_1940.html
│   ├── decrease_decade_1950.html
│   ├── decrease_decade_1960.html
│   ├── decrease_decade_1970.html
│   ├── decrease_decade_1980.html
│   ├── decrease_decade_1990.html
│   ├── decrease_decade_2000.html
│   ├── decrease_decade_merged.html
│   ├── dropoff_10year.html
│   ├── dropoff_1year.html
│   ├── dropoff_5year.html
│   ├── increase_10year.html
│   ├── increase_1year.html
│   ├── increase_5year.html
│   ├── increase_decade_1870.html
│   ├── increase_decade_1880.html
│   ├── increase_decade_1890.html
│   ├── increase_decade_1900.html
│   ├── increase_decade_1910.html
│   ├── increase_decade_1920.html
│   ├── increase_decade_1930.html
│   ├── increase_decade_1940.html
│   ├── increase_decade_1950.html
│   ├── increase_decade_1960.html
│   ├── increase_decade_1970.html
│   ├── increase_decade_1980.html
│   ├── increase_decade_1990.html
│   ├── increase_decade_2000.html
│   ├── increase_decade_merged.html
│   ├── jsons
│   │   ├── concat_10year.json
│   │   ├── concat_1year.json
│   │   ├── concat_5year.json
│   │   ├── concat_decade_1870_json.json
│   │   ├── concat_decade_1880_json.json
│   │   ├── concat_decade_1890_json.json
│   │   ├── concat_decade_1900_json.json
│   │   ├── concat_decade_1910_json.json
│   │   ├── concat_decade_1920_json.json
│   │   ├── concat_decade_1930_json.json
│   │   ├── concat_decade_1940_json.json
│   │   ├── concat_decade_1950_json.json
│   │   ├── concat_decade_1960_json.json
│   │   ├── concat_decade_1970_json.json
│   │   ├── concat_decade_1980_json.json
│   │   ├── concat_decade_1990_json.json
│   │   ├── concat_decade_2000_json.json
│   │   ├── concat_decade_merged.html
│   │   ├── decrease_decade_1870.json
│   │   ├── decrease_decade_1880.json
│   │   ├── decrease_decade_1890.json
│   │   ├── decrease_decade_1900.json
│   │   ├── decrease_decade_1910.json
│   │   ├── decrease_decade_1920.json
│   │   ├── decrease_decade_1930.json
│   │   ├── decrease_decade_1940.json
│   │   ├── decrease_decade_1950.json
│   │   ├── decrease_decade_1960.json
│   │   ├── decrease_decade_1970.json
│   │   ├── decrease_decade_1980.json
│   │   ├── decrease_decade_1990.json
│   │   ├── decrease_decade_2000.json
│   │   ├── increase_decade_1870.json
│   │   ├── increase_decade_1880.json
│   │   ├── increase_decade_1890.json
│   │   ├── increase_decade_1900.json
│   │   ├── increase_decade_1910.json
│   │   ├── increase_decade_1920.json
│   │   ├── increase_decade_1930.json
│   │   ├── increase_decade_1940.json
│   │   ├── increase_decade_1950.json
│   │   ├── increase_decade_1960.json
│   │   ├── increase_decade_1970.json
│   │   ├── increase_decade_1980.json
│   │   ├── increase_decade_1990.json
│   │   └── increase_decade_2000.json
│   ├── merged_viz.html
│   ├── outliers.png
│   ├── outliers_timeseries.png
│   ├── post_dep_outliers.png
│   ├── post_dep_outliers_timeseries.png
│   ├── projects_longest_gaps.png
│   ├── projects_longest_gaps_outliers.png
│   ├── projects_longest_gaps_real.png
│   ├── projects_outliers_timeseries.png
│   ├── wave_amplitude_outliers.png
│   └── wave_amplitude_outliers_timeseries.png
├── viz1.json
├── wave_amp_outliers.csv
└── wave_metrics.csv
