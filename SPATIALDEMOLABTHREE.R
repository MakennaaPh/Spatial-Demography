#April 15, 2026 | Makenna Phillips
#Dot Density-------------------------------------------------------------
# import libraries

library(tidycensus)
library(tidyverse)
library(sf)

jeff_race <- get_decennial(
  geography = "tract",
  state = "TX",
  county = "Jefferson",
  variables = c(Hispanic = "P2_002N", White = "P2_005N", Black = "P2_006N", Asian = "P2_008N"),
  year = 2020,
  geometry = TRUE
)

jeff_outline <- get_decennial(
  geography = "county",
  state = "TX",
  county = "Jefferson",
  year = 2020,
  geometry = TRUE
)

jeff_race_proj <- st_transform(jeff_race, 6580) %>% st_make_valid()
jeff_outline_proj <- st_transform(jeff_outline, 6580) %>% st_make_valid()

groups <- c("Hispanic", "White", "Black", "Asian")
all_dots <- list()

for (g in groups) {
  group_df <- jeff_race_proj %>% filter(variable == g, value > 0)
  if (nrow(group_df) > 0) {
    dots <- st_sample(group_df, size = pmax(1, round(group_df$value / 100)))
    all_dots[[g]] <- st_sf(variable = g, geometry = dots)
  }
}
jeff_dots <- bind_rows(all_dots)

ggplot() +
  geom_sf(data = jeff_race_proj, color = "grey95", fill = "white", size = 0.1) +
  geom_sf(data = jeff_dots, aes(color = variable), size = 0.4, alpha = 0.8) +
  geom_sf(data = jeff_outline_proj, fill = NA, color = "black", size = 0.8) + 
  scale_color_manual(values = c(
    "Black" = "#5A2653", "White" = "#9acd32",
    "Hispanic" = "#E69F00", "Asian" = "#0072B2"
  )) +
  coord_sf(expand = FALSE) + 
  theme_void() +
  labs(title = "Race and Ethnicity Dot Density Map",
       subtitle = "Jefferson County, TX (1 dot = 100 people)",
       color = "Group")

ggsave("Figure_3_Dot_Density.png", bg = "white", width = 8, height = 6)


















































Figure 1. 

#Graduated Symbols--------------------------------------------------------------
library(tmap)

young_data <- get_acs(
  geography = "tract",
  variables = "B01001_001", 
  state = "TX",
  county = "Jefferson",
  year = 2020,
  geometry = TRUE
)

# 2. Fetch the county-level geometry for the outline
jeff_outline <- get_acs(
  geography = "county",
  variables = "B01001_001",
  state = "TX",
  county = "Jefferson",
  year = 2020,
  geometry = TRUE
)

# 3. Layer the map: Tracts -> Symbols -> County Outline
tmap_mode("plot")
tm_young <- tm_shape(young_data) +
  tm_polygons(col = "grey95", border.col = "white") +
  tm_shape(young_data) +
  tm_bubbles(size = "estimate", 
             col = "#5A2653", 
             alpha = 0.6,
             title.size = "Population Size") +
  tm_shape(jeff_outline) +
  tm_borders(col = "black", lwd = 2) + # This adds the thick county outline
  tm_layout(frame = FALSE, 
            main.title = "Youth Distribution in Jefferson County",
            inner.margins = c(0.05, 0.05, 0.05, 0.05),
            legend.outside = TRUE)

tmap_save(tm_young, "Figure_4_Young_Graduated.png", width = 8, height = 6)
















Figure 2.

#Choropleth--------------------------------------------------------------
library(sf)

jeff_income <- get_acs(
  geography = "tract",
  variables = "B19013_001",
  state = "TX",
  county = "Jefferson",
  year = 2020,
  geometry = TRUE
)

jeff_county_outline <- get_acs(
  geography = "county",
  variables = "B19013_001",
  state = "TX",
  county = "Jefferson",
  year = 2020,
  geometry = TRUE
)

jeff_income_proj <- st_transform(jeff_income, 6580)
jeff_county_proj <- st_transform(jeff_county_outline, 6580)

ggplot(jeff_income_proj) +
 
  geom_sf(aes(fill = estimate), color = "white", size = 0.05) +
  geom_sf(data = jeff_county_proj, fill = NA, color = "black", size = 0.8) +
  scale_fill_gradient(
    low = "#5A2653", 
    high = "#9acd32", 
    na.value = "grey80", 
    labels = scales::dollar,
    name = "Annual Income"
  ) +
  coord_sf(expand = FALSE) +
  theme_void() +
  labs(
    title = "Median Household Income by Census Tract",
    subtitle = "Jefferson County, TX (2020 ACS 5-Year Estimates)"
  )

ggsave("Figure_5_Income_Choropleth.png", bg = "white", width = 8, height = 6)




Figure 3.

Geography of Interest 
The study area for this lab is Jefferson County, Texas, located along the Gulf Coast in the southeastern corner of Texas. Anchored by the cities of Beaumont and Port Arthur, the county is an important hub for the global petrochemical industry and maritime trade. This specific location was chosen because its population is concentrated in distinct urban clusters separated by large unpopulated marshlands and refineries, making it a good candidate for spatial analysis and density mapping.

Prompt 1: Race and Ethnicity Dot Density Map 
The dot density map (Figure 1) visualizes the racial and ethnic distribution of Jefferson County using 2020 Decennial Census data at the census tract level. To accurately represent the population, the map utilizes a scale of 1 dot = 100 people. The visualization utilizes a color palette of green for the White population and purple for the Black population, with orange and blue respectively for Hispanic and Asian groups, respectively. By adding a black county outline, the map clearly contains the residential clusters, revealing that Black and Hispanic populations are heavily concentrated in the industrial urban cores of the Port Arthur area and central Beaumont, while the White population is more widely distributed in the western suburban reaches.

Prompt 2: Graduated Symbol Map of Young Persons
Using the tmap package a graduated symbol map (Figure 2) was created to display the distribution of young people under the age of five (Variable B01001_001 from the 2020 ACS 5-year estimates). The map utilizes purple bubbles to represent the total youth population per census tract. A black outline was layered over the map to define the legal boundaries of Jefferson County. Visualization shows that the highest concentrations of young children are found in the northern residential neighborhoods of Beaumont. In contrast, the southern half of the county, which is largely comprised of refineries and wetlands, shows almost no youth population, highlighting the divide between industrial land use and residential life.

Prompt 3: Income Choropleth Analysis and Justification 
The final visualization (Figure 3) is a choropleth map of Median Household Income (Variable B19013_001) from the 2020 ACS 5-year estimates. This dataset was selected because the median provides a more reliable measure of a neighborhood's economic health than a simple mean, which can be skewed by the high-income outliers. I chose this as the geographic subunit because it is the most granular level that maintains high data reliability for income in a county of this size. The color scheme is a custom gradient transitioning from purple at the lower income levels to green at the higher levels, maintaining the visual identity established in previous labs and assignments. A continuous legend is placed on the right side of the frame to provide a reference for income values. 

