# decreasing-homicide-waves

| Database | Purpose | Location; Creator | Key Use |
|:-----------------------|:-----------------------|:-----------------------|:-------|
| hex_hom_bldg.shp| Geographic information about each hexagon **geometry**, **year**, **homicides for that year (hom_ct)**, **building count (bldg_cnt)**, **homicide rate (hom_rt)** | Jada_Hexagons | Any analysis with hexagon base |
| CHI Community Areas.shp | Current neighborhoods in Chicago, where they're located. Many columns but columns of importance are **COMMUNITY** and **geometry** (and maybe shape area and shape length) | comm_area_shapefiles | neighborhood-level analysis |
| cluster_assignments.csv | extended cluster column (this is based on trajectory analysis starting in 1870 vs the regular cluster column which was based on the analysis starting in 1940) | Slack; David; TBD | To answer key questions regarding clusters in Hexagons |
| Affordable_Rental_Housing_Developments_20250212.csv | Chicago Affordable Housing | [Data from Chicago Housing Data Portal](https://data.cityofchicago.org/Community-Economic-Development/Affordable-Rental-Housing-Developments/s6ha-ppgi/about_data) | Used as an initial attempt to find patterns with affordable housing & homicides |

## Files & Purposes

### hom_decrease.ipynb
initial examination of homicide waves by longest gap in waves and zoomed in on housing projects as well

### hom_decrease_2.ipynb
examination of wave intensity, 1-5-10 year dropoff and increases in homicide; also code producing *.html

### Viz
viz/ *.html: the only important ones you should care about: <br>
    <li> **merged_viz.html**: dropoffs and increases by 1-5-10 years and by hex clusters <br>
    <li> **increase_decade_merged.html**: maximum increase by decade and by hex clusters <br>
    <li> **decrease_decade_merged.html**: maximum decrease by decade and by hex clusters <br>
 
## Everything

```
# IGNORE // IMPORTANT

├── Affordable_Rental_Housing_Developments_20250212.csv # IGNORE 
├── IndividualHomicides_1965_2022.csv # IMPORTANT 
├── Jada_Hexagons 
│   ├── HexHom2024.R  # IGNORE 
│   ├── hex_hom_bldg.dbf # IMPORTANT 
│   ├── hex_hom_bldg.prj # IMPORTANT 
│   ├── hex_hom_bldg.shp # IMPORTANT 
│   └── hex_hom_bldg.shx # IMPORTANT 
├── README.md # IMPORTANT 
├── alt.LayerChart(...).json # VIZ SUPPLEMENT
├── cha_projects.geojson # IGNORE // IMPORTANT
├── chicago_hexagons_with_wards_join.shp # IGNORE // IMPORTANT
├── chicago_hexagons_with_wards_join.shx # IGNORE // IMPORTANT
├── cluster_assignments.csv # IMPORTANT
├── comm_area_shapefiles 
│   ├── CHI Community Areas.cpg # IMPORTANT
│   ├── CHI Community Areas.dbf # IMPORTANT
│   ├── CHI Community Areas.prj # IMPORTANT
│   ├── CHI Community Areas.sbn # IMPORTANT
│   ├── CHI Community Areas.sbx # IMPORTANT
│   ├── CHI Community Areas.shp # IMPORTANT
│   └──  CHI Community Areas.shx # IMPORTANT
├── dir_dist
│   ├── 1870_2022.gif # IMPORTANT
│   ├── 1874_1930.gif # IMPORTANT
│   ├── 1874_1964.gif # IMPORTANT
│   ├── 1940_1964.gif # IMPORTANT
│   ├── 1940_2022.gif # IMPORTANT
│   ├── dist_1870.png  # IGNORE 
│   ├── dist_1871.png  # IGNORE 
│   ├── dist_1872.png  # IGNORE 
│   ├── dist_1873.png  # IGNORE 
│   ├── dist_1874.png  # IGNORE 
│   ├── dist_1875.png  # IGNORE 
│   ├── dist_1876.png  # IGNORE 
│   ├── dist_1877.png  # IGNORE 
│   ├── dist_1878.png  # IGNORE 
│   ├── dist_1879.png  # IGNORE 
│   ├── dist_1880.png  # IGNORE 
│   ├── dist_1881.png  # IGNORE 
│   ├── dist_1882.png  # IGNORE 
│   ├── dist_1883.png  # IGNORE 
│   ├── dist_1884.png  # IGNORE 
│   ├── dist_1885.png  # IGNORE 
│   ├── dist_1886.png  # IGNORE 
│   ├── dist_1887.png  # IGNORE 
│   ├── dist_1888.png  # IGNORE 
│   ├── dist_1889.png  # IGNORE 
│   ├── dist_1890.png  # IGNORE 
│   ├── dist_1891.png  # IGNORE 
│   ├── dist_1892.png  # IGNORE 
│   ├── dist_1893.png  # IGNORE 
│   ├── dist_1894.png  # IGNORE 
│   ├── dist_1895.png  # IGNORE 
│   ├── dist_1896.png  # IGNORE 
│   ├── dist_1897.png  # IGNORE 
│   ├── dist_1898.png  # IGNORE 
│   ├── dist_1899.png  # IGNORE 
│   ├── dist_1900.png  # IGNORE 
│   ├── dist_1901.png  # IGNORE 
│   ├── dist_1902.png  # IGNORE 
│   ├── dist_1903.png  # IGNORE 
│   ├── dist_1904.png  # IGNORE 
│   ├── dist_1905.png  # IGNORE 
│   ├── dist_1906.png  # IGNORE 
│   ├── dist_1907.png  # IGNORE 
│   ├── dist_1908.png  # IGNORE 
│   ├── dist_1909.png  # IGNORE 
│   ├── dist_1910.png  # IGNORE 
│   ├── dist_1911.png  # IGNORE 
│   ├── dist_1912.png  # IGNORE 
│   ├── dist_1913.png  # IGNORE 
│   ├── dist_1914.png  # IGNORE 
│   ├── dist_1915.png  # IGNORE 
│   ├── dist_1916.png  # IGNORE 
│   ├── dist_1917.png  # IGNORE 
│   ├── dist_1918.png  # IGNORE 
│   ├── dist_1919.png  # IGNORE 
│   ├── dist_1920.png  # IGNORE 
│   ├── dist_1921.png  # IGNORE 
│   ├── dist_1922.png  # IGNORE 
│   ├── dist_1923.png  # IGNORE 
│   ├── dist_1924.png  # IGNORE 
│   ├── dist_1925.png  # IGNORE 
│   ├── dist_1926.png  # IGNORE 
│   ├── dist_1927.png  # IGNORE 
│   ├── dist_1928.png  # IGNORE 
│   ├── dist_1929.png  # IGNORE 
│   ├── dist_1930.png  # IGNORE 
│   ├── dist_1931.png  # IGNORE 
│   ├── dist_1932.png  # IGNORE 
│   ├── dist_1933.png  # IGNORE 
│   ├── dist_1934.png  # IGNORE 
│   ├── dist_1935.png  # IGNORE 
│   ├── dist_1936.png  # IGNORE 
│   ├── dist_1937.png  # IGNORE 
│   ├── dist_1938.png  # IGNORE 
│   ├── dist_1939.png  # IGNORE 
│   ├── dist_1940.png  # IGNORE 
│   ├── dist_1941.png  # IGNORE 
│   ├── dist_1942.png  # IGNORE 
│   ├── dist_1943.png  # IGNORE 
│   ├── dist_1944.png  # IGNORE 
│   ├── dist_1945.png  # IGNORE 
│   ├── dist_1946.png  # IGNORE 
│   ├── dist_1947.png  # IGNORE 
│   ├── dist_1948.png  # IGNORE 
│   ├── dist_1949.png  # IGNORE 
│   ├── dist_1950.png  # IGNORE 
│   ├── dist_1951.png  # IGNORE 
│   ├── dist_1952.png  # IGNORE 
│   ├── dist_1953.png  # IGNORE 
│   ├── dist_1954.png  # IGNORE 
│   ├── dist_1955.png  # IGNORE 
│   ├── dist_1956.png  # IGNORE 
│   ├── dist_1957.png  # IGNORE 
│   ├── dist_1958.png  # IGNORE 
│   ├── dist_1959.png  # IGNORE 
│   ├── dist_1960.png  # IGNORE 
│   ├── dist_1961.png  # IGNORE 
│   ├── dist_1962.png  # IGNORE 
│   ├── dist_1963.png  # IGNORE 
│   ├── dist_1964.png  # IGNORE 
│   ├── dist_1965.png  # IGNORE 
│   ├── dist_1966.png  # IGNORE 
│   ├── dist_1967.png  # IGNORE 
│   ├── dist_1968.png  # IGNORE 
│   ├── dist_1969.png  # IGNORE 
│   ├── dist_1970.png  # IGNORE 
│   ├── dist_1971.png  # IGNORE 
│   ├── dist_1972.png  # IGNORE 
│   ├── dist_1973.png  # IGNORE 
│   ├── dist_1974.png  # IGNORE 
│   ├── dist_1975.png  # IGNORE 
│   ├── dist_1976.png  # IGNORE 
│   ├── dist_1977.png  # IGNORE 
│   ├── dist_1978.png  # IGNORE 
│   ├── dist_1979.png  # IGNORE 
│   ├── dist_1980.png  # IGNORE 
│   ├── dist_1981.png  # IGNORE 
│   ├── dist_1982.png  # IGNORE 
│   ├── dist_1983.png  # IGNORE 
│   ├── dist_1984.png  # IGNORE 
│   ├── dist_1985.png  # IGNORE 
│   ├── dist_1986.png  # IGNORE 
│   ├── dist_1987.png  # IGNORE 
│   ├── dist_1988.png  # IGNORE 
│   ├── dist_1989.png  # IGNORE 
│   ├── dist_1990.png  # IGNORE 
│   ├── dist_1991.png  # IGNORE 
│   ├── dist_1992.png  # IGNORE 
│   ├── dist_1993.png  # IGNORE 
│   ├── dist_1994.png  # IGNORE 
│   ├── dist_1995.png  # IGNORE 
│   ├── dist_1996.png  # IGNORE 
│   ├── dist_1997.png  # IGNORE 
│   ├── dist_1998.png  # IGNORE 
│   ├── dist_1999.png  # IGNORE 
│   ├── dist_2000.png  # IGNORE 
│   ├── dist_2001.png  # IGNORE 
│   ├── dist_2002.png  # IGNORE 
│   ├── dist_2003.png  # IGNORE 
│   ├── dist_2004.png  # IGNORE 
│   ├── dist_2005.png  # IGNORE 
│   ├── dist_2006.png  # IGNORE 
│   ├── dist_2007.png  # IGNORE 
│   ├── dist_2008.png  # IGNORE 
│   ├── dist_2009.png  # IGNORE 
│   ├── dist_2010.png  # IGNORE 
│   ├── dist_2011.png  # IGNORE 
│   ├── dist_2012.png  # IGNORE 
│   ├── dist_2013.png  # IGNORE 
│   ├── dist_2014.png  # IGNORE 
│   ├── dist_2015.png  # IGNORE 
│   ├── dist_2016.png  # IGNORE 
│   ├── dist_2017.png  # IGNORE 
│   ├── dist_2018.png  # IGNORE 
│   ├── dist_2019.png  # IGNORE 
│   ├── dist_2020.png  # IGNORE 
│   ├── dist_2021.png  # IGNORE 
│   └── dist_2022.png  # IGNORE 
├── gap_outliers.csv 
├── hexhom.csv
├── hom_decrease.ipynb
├── hom_decrease_2.ipynb
├── hom_decrease_3.ipynb
├── homcide-locations_1940-1964.csv
├── homicide-locations_1870-1930.csv
├── homicides_1940-1965_geocoded_v3.shp
├── homicides_1940-1965_geocoded_v3.shx
├── locs_homicide_post_65.geojson
├── map_data.csv
├── map_projects.csv
├── projects_gap_outliers_wonky.csv
├── south_chicago.geojson
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
```
