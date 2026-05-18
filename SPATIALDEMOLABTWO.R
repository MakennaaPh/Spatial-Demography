#April 7, 2026 | Makenna Phillips
# Exploratory Data Analysis-- Learn basics of ggplot, tidyverse environment
#Beaumont, Texas as future study environment
# Libraries used: Tidyverse, Tidycensus, Scales
#-------------------------------------------------------------------------
#Import Libraries
library(tidycensus)
library(tidyverse)
library(scales)

# PROMPT 1: Educational Attainment
# -------------------------------------------------------------------------
tx_edu <- get_acs(
  geography = "county",
  variables = "DP02_0068P", 
  state = "TX",
  year = 2019,
  survey = "acs5"
)

highest <- tx_edu %>% arrange(desc(estimate)) %>% slice(1)
lowest <- tx_edu %>% arrange(estimate) %>% slice(1)
median_val <- median(tx_edu$estimate, na.rm = TRUE)

cat("--- Educational Attainment Report (TX) ---\n")
cat("Highest %:", highest$NAME, "-", highest$estimate, "%\n")
cat("Lowest %:", lowest$NAME, "-", lowest$estimate, "%\n")
cat("Median for Texas counties:", median_val, "%\n\n")

# PROMPT 2: Margin of Error (MOE)
# Variable Chosen: DP03_0025 (Mean Travel Time to Work in Minutes)
# -------------------------------------------------------------------------
beaumont_counties <- c("Jefferson", "Orange", "Hardin", "Chambers", "Liberty")

commute_data <- get_acs(
  geography = "county",
  variables = "DP03_0025", 
  state = "TX",
  year = 2019
) %>%
  filter(str_detect(NAME, paste(beaumont_counties, collapse="|"))) %>%
  mutate(NAME = str_remove(NAME, " County, Texas"))

commute_plot <- ggplot(commute_data, aes(x = estimate, y = reorder(NAME, estimate))) +
  geom_errorbarh(aes(xmin = estimate - moe, xmax = estimate + moe), height = 0.3, color = "#9acd32") +
  geom_point(size = 4, color = "#5A2653") + 
  theme_minimal() +
  labs(
    title = "Mean Commute Times: Beaumont-Port Arthur Region",
    subtitle = "Average travel time to work (minutes) with 90% MOE",
    x = "Minutes",
    y = "",
    caption = "Source: ACS Variable DP03_0025"
  )

ggsave("Figure_1_Commute_MOE.png", commute_plot, width = 8, height = 5)

# PROMPT 3: Population Pyramid
# Jefferson County, Texas
# -------------------------------------------------------------------------
jeff_pop_data <- get_acs(
  geography = "county",
  table = "B01001",
  state = "TX",
  county = "Jefferson",
  year = 2019,
  survey = "acs5"
) %>%
  filter(!variable %in% c("B01001_001", "B01001_002", "B01001_026")) %>%
  mutate(
    sex = ifelse(as.numeric(str_sub(variable, -3)) <= 25, "Male", "Female"),
    estimate_pyramid = ifelse(sex == "Male", -estimate, estimate)
  )

pop_pyramid <- ggplot(jeff_pop_data, aes(x = estimate_pyramid, y = variable, fill = sex)) +
  geom_col(width = 0.8) +
  scale_fill_manual(values = c("Male" = "#9acd32", "Female" = "#5A2653")) +
  theme_bw() + 
  scale_x_continuous(labels = abs) + 
  theme(
    axis.text.y = element_blank(), 
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white")
  ) + 
  labs(
    title = "Population Pyramid: Jefferson County, TX",
    subtitle = "Age and Sex Distribution (2015-2019 ACS)",
    x = "Population Count",
    y = "Age Cohorts (Youngest at Base)",
    fill = "Sex"
  )

png("Figure_2_Jefferson_Pyramid.png", width = 800, height = 600, res = 100)
print(pop_pyramid)
dev.off()