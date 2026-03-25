# ============================================================
# Assignment 7 Solutions: Spatial Data I
# Applied Quantitative Methods II, UC3M
# ============================================================

# Load libraries
library(sf)
library(spData)
library(dplyr)
library(ggplot2)

# Load dataset
data(world)

# ============================================================
# 1. Inspecting an sf object
# ============================================================

# a) Inspect the structure of the world dataset
class(world)
# Returns both "sf" and "data.frame":
# An sf object is a data frame with a geometry column (sfc)
# that stores spatial shapes (polygons, points, lines).
# The geometry column is "sticky", meaning dplyr operations
# retain spatial information automatically.

names(world)

nrow(world)

# b) Check the coordinate reference system (CRS)
st_crs(world)
# EPSG:4326 (WGS84 - World Geodetic System 1984)
# Coordinates are in longitude/latitude (decimal degrees)
# This is the global standard used by GPS and web maps.

# c) Inspect geometry types
unique(st_geometry_type(world))
# MULTIPOLYGON:
# A collection of polygons representing countries with
# multiple land areas (e.g., USA, France overseas territories).

# d) Quick base-R map of GDP per capita

pdf("world_gdp_base.pdf")
plot(world["gdpPercap"])
dev.off()

# Display inline as well
plot(world["gdpPercap"], main = "GDP per capita by country")
# Reveals global inequality:
# High: Europe, North America, Australia/NZ
# Low: Sub-Saharan Africa, parts of Asia

# ============================================================
# 2. Attribute operations
# ============================================================

# a) Filter to African countries
africa <- filter(world, continent == "Africa")

nrow(africa)

plot(africa["gdpPercap"], main = "GDP per capita -- Africa")
# Dataset contains 51 African countries (slightly below UN count)

# b) Add population in millions and summarise GDP by continent

world <- world %>%
  mutate(pop_millions = pop / 1e6)

gdp_by_continent <- world %>%
  group_by(continent) %>%
  summarise(mean_gdpPercap = mean(gdpPercap, na.rm = TRUE))

print(st_drop_geometry(gdp_by_continent))
# summarise() unions geometries by group
# Use st_drop_geometry() to remove spatial data for tabular output

# c) Top 5 African countries by GDP per capita

africa_sorted <- africa %>%
  arrange(desc(gdpPercap)) %>%
  select(name_long, gdpPercap)

print(head(st_drop_geometry(africa_sorted), 5))
# Top countries driven by oil (Equatorial Guinea, Libya, Gabon)
# or resources/institutions (Botswana)

# ============================================================
# 3. Simple visualization with ggplot2
# ============================================================

# a) Choropleth map of world GDP per capita

ggplot(world) +
  geom_sf(aes(fill = gdpPercap)) +
  scale_fill_viridis_c(
    option = "plasma",
    na.value = "grey80",
    name = "GDP per capita"
  ) +
  theme_void() +
  labs(title = "GDP per capita by country")

ggsave("world_gdp.pdf", width = 10, height = 5)
# Pattern: wealthy (Europe, North America, Oceania),
# middle (East Asia), low (Africa, South Asia)

# b) Africa map with magma palette

ggplot(africa) +
  geom_sf(aes(fill = gdpPercap)) +
  scale_fill_viridis_c(
    option = "magma",
    na.value = "grey80",
    name = "GDP per capita"
  ) +
  theme_void() +
  labs(title = "GDP per capita -- Africa")

ggsave("africa_gdp.pdf", width = 7, height = 6)
# Variation within Africa:
# Higher in North & Southern Africa, lower in Central/West

# c) Africa map with country borders

ggplot(africa) +
  geom_sf(aes(fill = gdpPercap), color = "white", linewidth = 0.3) +
  scale_fill_viridis_c(
    option = "magma",
    na.value = "grey80",
    name = "GDP per capita"
  ) +
  theme_void() +
  labs(title = "GDP per capita -- Africa (with borders)")

ggsave("africa_gdp_borders.pdf", width = 7, height = 6)
# Adding borders improves readability and country distinction

# ------------------------------------------------------------------------------

# ============================================================
# Part 2: Point Data and Spatial Joins
# ============================================================

library(sf)
library(dplyr)
library(ggplot2)
library(tidyr)

# Load world data (if not already loaded)
library(spData)
data(world)

# Load conflict data (make sure file path is correct)
events <- read.csv("conflict_events.csv")

# ============================================================
# 2.1 Converting tabular data to sf
# ============================================================

# a) Convert to sf object
events_sf <- st_as_sf(events,
                      coords = c("longitude", "latitude"),
                      crs = 4326)

class(events_sf)
st_crs(events_sf)

# st_as_sf() converts a data frame into a spatial object.
# coords specifies which columns contain coordinates.
# crs = 4326 means WGS84 (lat/lon in degrees).

# b) Number of events and counts by type
nrow(events_sf)

table(events_sf$event_type)

# state-based event types are the most common

# c) Map of events over world

ggplot() +
  geom_sf(data = world, fill = "grey90", color = "white") +
  geom_sf(data = events_sf, aes(color = event_type), size = 0.7) +
  theme_void() +
  labs(title = "Conflict Events Worldwide")

ggsave("events_map.pdf", width = 10, height = 5)

# Since the database only has data about conflicts in Africa, central Africa
# and the Horn of Africa are especially the regions with the most conflicts

# ============================================================
# 2.2 Spatial join: events to countries
# ============================================================

# a) Ensure same CRS
st_crs(world)
st_crs(events_sf)

# Spatial join
events_joined <- st_join(events_sf, world)

nrow(events_joined)
# Should equal number of events

# st_join() assigns each point the attributes of the polygon
# it falls within (based on spatial location).

# b) Check unmatched events
sum(is.na(events_joined$name_long))

# Fraction unmatched
mean(is.na(events_joined$name_long))

# Possible reasons might be:
# - The point is situated in the ocean
# - There might be errors with the coordinates or the precission of the borders
# of countries close to the sea

# c) Count events and fatalities by country

events_country <- events_joined %>%
  filter(!is.na(name_long)) %>%
  group_by(name_long) %>%
  summarise(
    n_events = n(),
    total_fatalities = sum(fatalities, na.rm = TRUE)
  ) %>%
  arrange(desc(n_events))

# Print top 10
print(head(st_drop_geometry(events_country), 10))

# Taking into account that the database provided only contains conflict events
# within Africa, the list is somewhat consistent with my knowledge, although 
# coding specificities might lead events without media presence to be coded as 
# conflicts anyways, thus the list does not necessarily need to match with 
# the best-known conflicts in the continent.

# ============================================================
# 2.3 Choropleth of conflict intensity
# ============================================================

# a) Join counts back to world

events_country_df <- st_drop_geometry(events_country)

world_events <- world %>%
  left_join(events_country_df, by = "name_long") %>%
  mutate(n_events = replace_na(n_events, 0),
         total_fatalities = replace_na(total_fatalities, 0))

nrow(world_events) == nrow(world)

# b) Choropleth map (raw counts)

ggplot(world_events) +
  geom_sf(aes(fill = n_events)) +
  scale_fill_distiller(palette = "Reds", direction = 1,
                       name = "Number of events") +
  theme_void() +
  labs(title = "Conflict Events by Country")

ggsave("conflict_map.pdf", width = 10, height = 5)

# The new map does mostly match the patterns observed in the map from question
# 2.1c

# c) Choropleth with log transformation

ggplot(world_events) +
  geom_sf(aes(fill = log1p(n_events))) +
  scale_fill_distiller(palette = "YlOrRd", direction = 1,
                       name = "Log(events + 1)") +
  theme_void() +
  labs(title = "Conflict Events (Log Scale)")

ggsave("conflict_log_map.pdf", width = 10, height = 5)

# The log transformation reduces skewness and helps visualize
# variation among countries with fewer events present.