


*** Homicide Hexagons ***
  
install.packages("sf")
install.packages("sp")
install.packages("rgdal")
install.packages("rgeos")
install.packages("fastmap")
install.packages("ggplot2")
install.packages("raster")
install.packages("base")
install.packages("dplyr")
install.packages("httr")

install.packages("lubridate")
library(lubridate)


### installing packages
library(sp)
library(rgdal)
library(rgeos)
library(fastmap)
library(sf)
library(ggplot2)
library(raster)
library(base)
library(dplyr)
library(httr)

# Install the required packages if not already installed
install.packages("showtext")
install.packages("sysfonts")

# Load libraries
library(showtext)
library(sysfonts)
library(ggplot2)

hexhom <- st_read("/Users/robertvargas/Library/CloudStorage/Box-Box/Lab/Projects/Homicide/hexhom.csv")

hexhomshp <- st_read("/Users/robertvargas/Library/CloudStorage/Box-Box/Lab/Projects/Homicide/Chicago Homicides 1880_2017/Jada_Hexagons")
shapefile <- hexhomshp
hex_data <- hexhom
merged_data <- shapefile %>%
  left_join(hex_data, by = "GRID_ID")  # Assuming GRID_ID is the common key
merged_data <- merged_data %>%
  mutate(highlight = ifelse(GRID_ID %in% highlight_hexagons, TRUE, FALSE))

write.csv(hexhom, "/Users/robertvargas/Library/CloudStorage/Box-Box/Lab/Projects/Homicide/hexhom.csv", row.names = FALSE)

# Load the GeoJSON file
geo_data <- st_read("/Users/robertvargas/Library/CloudStorage/Box-Box/Lab/Projects/CheckerBUrbanization/LandTranShapefiles/chicago_with_geometries.geojson")
date <- geo_data$Date.of.Purchase
geo <- geo_data$geometry

geo_data <- geo_data %>%
  mutate(
    CleanDate = mdy(date),    # Convert character to Date (MM/DD/YYYY format)
    Year = as.numeric(year(CleanDate))    # Extract year and ensure numeric format
  )

Year <- geo_data$YEAR

geo_data <- geo_data %>%
  filter(Year >= 1805 | is.na(Year))  # Keep rows where Year >= 1805 or Year is NA

geo_data <- geo_data %>%
  filter(Year <= 2000 | is.na(Year))  # Keep rows where Year <= 2000 or Year is NA


# Plot the county map
ggplot(data = geo_data) +
  geom_sf(aes(fill = Year), color = NA, size = 0) +  # Remove borders
  scale_fill_viridis_c(
    option = "magma",
    name = "Year Sold",
    direction = 1,  # Reverse the direction of the color scale
    breaks = seq(1805, 1885, by = 10)  # Adjust breaks for spacing
  ) +
  theme_void() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 12, family = "serif"),  # Change to Garamond
    legend.text = element_text(size = 10, family = "serif"),
    plot.title = element_text(size = 16, family = "serif", face = "bold"),
    plot.subtitle = element_text(size = 14, family = "serif")
  ) +
  labs(
    title = "Map of First Land Sales in Cook County 1805-1885",
  )



### GGROUP BY DECADE WITH CATEGORICAL ENTRIES BY DECADE AND NOT GRADIENT.

# Step 1: Group by decade
geo_data <- geo_data %>%
  mutate(
    Decade = floor(Year / 10) * 10  # Calculate the decade
  )

# Step 2: Summarize the count of sales per decade
sales_by_decade <- geo_data %>%
  group_by(Decade) %>%
  summarise(SalesCount = n()) %>%
  ungroup()

# Step 3: Join summarized data back to the original dataset
geo_data <- geo_data %>%
  left_join(sales_by_decade, by = "Decade")

# Step 4: Create the plot
ggplot(data = geo_data %>% filter(!is.na(Decade))) +  # Remove NA values for Decade
  geom_sf(aes(fill = as.factor(Decade)), color = NA, size = 0) +  # Map Decade as a factor for discrete colors
  scale_fill_manual(
    values = c(
      "1800" = "#800080",  # Dark purple
      "1810" = "#993399",  # Medium purple
      "1820" = "#B266B2",  # Light purple
      "1830" = "#CC99CC",  # Very light purple
      "1840" = "#FFCC99",  # Light orange
      "1850" = "#FFB266",  # Medium-light orange
      "1860" = "#FF9933",  # Medium orange
      "1870" = "#FF8000",  # Bright orange
      "1880" = "#FF6600"   # Deeper orange
    ),
    name = "Decade"
  ) +
  theme_void() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 12, family = "serif"),  # Change to Garamond
    legend.text = element_text(size = 10, family = "serif"),
    plot.title = element_text(size = 16, family = "serif", face = "bold"),
    plot.subtitle = element_text(size = 14, family = "serif")
  ) +
  labs(
    title = "Map of First Land Sales by Decade in Cook County (1800s)",
    subtitle = "Count of sales grouped by decade"
  )

#### CREATING THE SVG FILES
# Unique decades in the dataset
decades <- unique(geo_data$Decade)
decades <- decades[!is.na(decades)]  # Remove NA values if present

# Define colors for each decade
decade_colors <- c(
  "1800" = "#800080",  # Dark purple
  "1810" = "#993399",  # Medium purple
  "1820" = "#B266B2",  # Light purple
  "1830" = "#CC99CC",  # Very light purple
  "1840" = "#FFCC99",  # Light orange
  "1850" = "#FFB266",  # Medium-light orange
  "1860" = "#FF9933",  # Medium orange
  "1870" = "#FF8000",  # Bright orange
  "1880" = "#FF6600"   # Deeper orange
)

setwd("~/Downloads")


# Loop through each decade and generate/export individual plots
for (decade in decades) {
  # Filter data for the current decade
  decade_data <- geo_data %>% filter(Decade == decade)
  
  # Create the plot for the current decade
  plot <- ggplot(data = decade_data) +
    geom_sf(aes(fill = as.factor(Decade)), color = NA, size = 0) +  # Map Decade as a factor for discrete colors
    scale_fill_manual(
      values = decade_colors[as.character(decade)],  # Use the specific color for the decade
      name = "Decade"
    ) +
    theme_void() +
    theme(
      legend.position = "none",  # Remove legend for individual plots
      plot.title = element_text(size = 16, family = "serif", face = "bold"),
      plot.subtitle = element_text(size = 14, family = "serif")
    ) +
    labs(
      title = paste("Land Sales in Cook County -", decade, "Decade")
    )
  
  # Save the plot as an SVG file
  file_name <- paste0("land_sales_decade_", decade, ".svg")
  ggsave(filename = file_name, plot = plot, width = 8, height = 6, dpi = 300, device = "svg")
  
  print(paste("Saved:", file_name))
}






### Plotting the homicide hexagons

grid <- hexhom$GRID_ID
homyear <- hexhom$year
homct <- hexhom$hom_ct

homyear <- as.numeric(homyear)
homct <- as.numeric(homct)

# Art experiments
ggplot(hexhom, aes(x = homyear, y = grid, fill = homct)) +
  geom_tile(color = "NA", size = 0.1) +  # Optional: Adjust size for clarity
  scale_fill_gradient(low = "red", high = "darkblue", name = "Homicides") +
  labs(
    title = "",  # Remove the title
    x = NULL,    # Remove x-axis label
    y = NULL     # Remove y-axis label
  ) +
  theme_void() +
  theme(
    axis.text = element_blank(),         # Remove axis text
    axis.ticks = element_blank(),        # Remove axis ticks
    axis.title = element_blank(),        # Remove axis titles
    panel.grid.major = element_blank(),  # Remove major gridlines
    panel.grid.minor = element_blank(),  # Remove minor gridlines
    legend.position = "right"            # Keep legend
  )

### Removing the white column from the middle
# Filter out missing years and reindex homyear
hexhom_clean <- hexhom %>%
  filter(!is.na(homyear)) %>%  # Remove rows with missing homyear
  mutate(homyear = as.numeric(as.factor(homyear)))  # Reindex years to remove gaps

# Plot with the cleaned data
ggplot(hexhom_clean, aes(x = homyear, y = grid, fill = homct)) +
  geom_tile(color = NA, size = 0.1) +  # Optional: Adjust size for clarity
  scale_fill_gradient(low = "red", high = "darkblue", name = "Homicides") +
  labs(
    title = "",  # Remove the title
    x = NULL,    # Remove x-axis label
    y = NULL     # Remove y-axis label
  ) +
  theme_void() +
  theme(
    axis.text = element_blank(),         # Remove axis text
    axis.ticks = element_blank(),        # Remove axis ticks
    axis.title = element_blank(),        # Remove axis titles
    panel.grid.major = element_blank(),  # Remove major gridlines
    panel.grid.minor = element_blank(),  # Remove minor gridlines
    legend.position = "right"            # Keep legend
  )

#### Plotted by Decade (which will make for easier SVG files)

GRID_ID <- hexhom_clean$GRID_ID
hexhom$hom_ct <- as.numeric(hexhom$hom_ct)
hom_ct <- as.numeric(hexhom$hom_ct)



# Ensure the 'year' and 'hom_ct' columns are numeric
hexhom$year <- as.numeric(hexhom$year)
hexhom$hom_ct <- as.numeric(hexhom$hom_ct)

# Verify the structure of the dataset
str(hexhom)


# Create a new column for the decade
hexhom <- hexhom %>%
  filter(!is.na(year)) %>%  # Ensure no missing years
  mutate(decade = floor(year / 10) * 10)  # Group years into decades

# Summarize homicide counts by hexagon and decade
hexhom_aggregated <- hexhom %>%
  group_by(GRID_ID, decade) %>%
  summarise(
    hom_ct_sum = sum(hom_ct, na.rm = TRUE),  # Sum homicides for each hexagon-decade
    .groups = "drop"
  )

# Aggregate total homicides across all hexagons for each decade
total_homicides_per_decade <- hexhom_aggregated %>%
  group_by(decade) %>%
  summarise(
    total_hom_ct = sum(hom_ct_sum, na.rm = TRUE),  # Total homicides per decade
    .groups = "drop"
  )

# Ensure GRID_ID is ordered for plotting
hexhom_aggregated <- hexhom_aggregated %>%
  mutate(GRID_ID = factor(GRID_ID, levels = unique(GRID_ID)))

#Histogram
ggplot(hexhom_aggregated, aes(x = hom_ct_sum)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black", alpha = 0.7) +  # Adjust binwidth as needed
  labs(
    title = "Histogram of Homicide Counts per Hexagon-Decade",
    x = "Total Homicides (per Hexagon-Decade)",
    y = "Frequency"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )

# Plot # Define categories for homicide counts
hexhom_aggregated <- hexhom_aggregated %>%
  mutate(
    hom_ct_category = cut(
      hom_ct_sum,
      breaks = c(-Inf, 0, 5, 10, 20, 30, Inf),  # Define 6 categories
      labels = c("0", "1-5", "6-10", "11-20", "21-30", "31+")
    )
  )

# Plot without Labels
#Blue_Green
ggplot(hexhom_aggregated, aes(x = factor(decade), y = GRID_ID, fill = hom_ct_category)) +
  geom_tile(color = NA, width = 1.2, height = 1.2) +  # No gridlines
  scale_fill_manual(
    values = c("#f0f9e8", "#bae4bc", "#7bccc4", "#43a2ca", "#0868ac", "#084081"),  # Custom color palette
    name = NULL  # Remove legend title
  ) +
  theme_void() +  # Completely void theme for an abstract look
  theme(
    legend.position = "none",  # Remove legend
    plot.margin = margin(0, 0, 0, 0)  # Remove plot margins
  )

#Plot with labels

ggplot(hexhom_aggregated, aes(x = factor(decade), y = GRID_ID, fill = hom_ct_category)) +
  geom_tile(color = NA) +  # No gridlines
  scale_fill_manual(
    values = c("#f0f9e8", "#bae4bc", "#7bccc4", "#43a2ca", "#0868ac", "#084081"),  # Custom color palette
    name = "Homicides"  # Legend title
  ) +
  labs(
    title = "Homicides by Hexagon and Decade",
    x = "Decade",
    y = "Hexagon (GRID_ID)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",  # Add legend on the right
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),  # Center the title
    axis.text.x = element_text(size = 10, angle = 45, hjust = 1),  # Rotate x-axis labels
    axis.text.y = element_text(size = 8),  # Adjust y-axis text size
    axis.title.x = element_text(size = 12),  # X-axis label size
    axis.title.y = element_text(size = 12)   # Y-axis label size
  )


#### Joining Hexagons to Community Areas
commarea <- st_read("/Users/robertvargas/Library/CloudStorage/Box-Box/Lab/Data/Chicago Data/Shapefiles/Community Areas Shapefiles/CHI Community Areas.shp")
hexhomshp <- st_read("/Users/robertvargas/Library/CloudStorage/Box-Box/Lab/Projects/Homicide/Chicago Homicides 1880_2017/Jada_Hexagons")

# Ensure both shapefiles have the same CRS (Coordinate Reference System)
commarea <- st_transform(commarea, st_crs(hexhomshp))

# Perform the spatial join to assign community area information to each hexagon
hexhomshp_joined <- st_join(hexhomshp, commarea, join = st_within)

# Check the resulting data to ensure the join worked
head(hexhomshp_joined)

# Save the resulting shapefile with community area information
st_write(hexhomshp_joined, "Hexagons_with_Community_Areas.shp", delete_dsn = TRUE)



# Prepare the data by ordering hexagons based on their community area number
hexhomshp_joined <- hexhomshp_joined %>%
  arrange(AREA_NUMBE, COMMUNITY) %>%  # Sort by community area number
  mutate(
    COMMUNITY_LABEL = paste(AREA_NUMBE, COMMUNITY, sep = ": "),  # Create label with number and name
    COMMUNITY_LABEL = factor(COMMUNITY_LABEL, levels = unique(COMMUNITY_LABEL))  # Maintain order
  )

# Ensure community area information is merged into hexhom_aggregated
hexhom_aggregated <- hexhom_aggregated %>%
  left_join(
    hexhomshp_joined %>%
      select(GRID_ID, AREA_NUMBE, COMMUNITY) %>%
      distinct(),
    by = "GRID_ID"
  ) %>%
  arrange(AREA_NUMBE, COMMUNITY) %>%  # Sort by community area number
  mutate(
    COMMUNITY_LABEL = paste(AREA_NUMBE, COMMUNITY, sep = ": "),  # Create label with number and name
    COMMUNITY_LABEL = factor(COMMUNITY_LABEL, levels = unique(COMMUNITY_LABEL))  # Maintain order
  )

# Create the plot
ggplot(hexhom_aggregated, aes(x = factor(decade), y = COMMUNITY_LABEL, fill = hom_ct_category)) +
  geom_tile(color = NA) +  # No gridlines
  scale_fill_manual(
    values = c("#f0f9e8", "#bae4bc", "#7bccc4", "#43a2ca", "#0868ac", "#084081"),  # Custom color palette
    name = "Homicides"  # Legend title
  ) +
  labs(
    title = "Homicides by Hexagon and Decade",
    x = "Decade",
    y = "Community Area (Hexagons)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",  # Add legend on the right
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),  # Center the title
    axis.text.x = element_text(size = 10, angle = 45, hjust = 1),  # Rotate x-axis labels
    axis.text.y = element_text(size = 8),  # Adjust y-axis text size
    axis.title.x = element_text(size = 12),  # X-axis label size
    axis.title.y = element_text(size = 12)   # Y-axis label size
  )
