#Week 4 homework - global gender inequality
#Read in GII data, join to world countries, make a column of
#the difference in inequality between 2010 and 2019, then map it

#install.packages(c("countrycode", "tmap", "janitor"))

#Load Packages (ignore any error messages about being built under a
#different R version):
library(sf)
library(tidyverse)
library(janitor)
library(countrycode)
library(tmap)

#working directory should be the week4 folder (or open the .Rproj in it)

##########################
# read in the data
##########################

#gender inequality - "All composite indices and components time series (1990-2023)"
#from https://hdr.undp.org/data-center/documentation-and-downloads
HDR <- read_csv("HDR25_Composite_indices_complete_time_series.csv",
                locale = locale(encoding = "latin1"),
                na = c("", "NA"))

#spatial data of the World - World Countries (Generalized) geojson from ArcGIS Hub
World <- st_read("World_Countries_Generalized.geojson")

##########################
# select and tidy the GII
##########################

#gii_2010 and gii_2019 are the gender inequality index columns
#(see the metadata file that sits under the dataset on the UNDP page)
HDRcols <- HDR %>%
  clean_names() %>%
  dplyr::select(iso3, country, gii_2010, gii_2019) %>%
  # remove the regional/aggregate rows, these have iso3 codes like ZZA.VHHD
  filter(str_detect(iso3, "^[A-Z]{3}$")) %>%
  # difference in inequality between 2010 and 2019
  # negative = inequality has gone down (got better)
  mutate(gii_diff = gii_2019 - gii_2010)

##########################
# join
##########################

#the world data uses a 2 character ISO code, the GII uses 3 character ISO codes
#so use countrycode to make a 3 character code in the world data first
World <- World %>%
  clean_names() %>%
  mutate(iso3 = countrycode(iso, origin = "iso2c", destination = "iso3c"))

#always use a join type - left join keeps all the countries in the spatial data
Join_HDI <- World %>%
  left_join(.,
            HDRcols,
            by = c("iso3" = "iso3"))

#check which countries didn't match
Join_HDI %>%
  st_drop_geometry() %>%
  filter(is.na(gii_diff)) %>%
  dplyr::select(country.x, iso, iso3) %>%
  head(20)

##########################
# map
##########################

tmap_mode("plot")
# change the above to "view" for interactive

#most values are between -0.15 and 0.05 (check with summary(Join_HDI$gii_diff))
#so set the breaks by hand, with 0 as the middle of the diverging colours
#the - in front of the palette flips it: blue = inequality went down (better)
tm1 <- Join_HDI %>%
  filter(country.x != "Antarctica") %>%
  tm_shape(.) +
  tm_polygons(fill = "gii_diff",
              col = "grey40",
              lwd = 0.2,
              fill.scale = tm_scale_intervals(values = "-brewer.rd_bu",
                                              breaks = c(-0.55, -0.15, -0.1, -0.05, 0, 0.05, 0.3),
                                              midpoint = 0,
                                              value.na = "grey90",
                                              label.na = "No data"),
              fill.legend = tm_legend(title = "GII change\n2010-2019",
                                      position = tm_pos_in("left", "bottom"))) +
  tm_title("Change in the Gender Inequality Index (2019 minus 2010)") +
  tm_credits("Data: UNDP Human Development Report 2025, Esri World Countries (Generalized)",
             size = 0.5,
             position = tm_pos_in("right", "bottom")) +
  tm_layout(frame = FALSE)

tm1

#device = ragg::agg_png because the default png device needs XQuartz on a Mac
#if you have XQuartz installed you can remove the device argument
tmap_save(tm1, "gii_diff_map.png", width = 10, height = 6, units = "in", dpi = 300,
          device = ragg::agg_png)

#write out the joined data so it can be shared on GitHub
Join_HDI %>%
  st_write(., "World_GII_diff.geojson", delete_dsn = TRUE)
