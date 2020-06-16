library(ggplot2)
library(ggmap)
library(maps)
library(httr)
library(jsonlite)
library(sp)
library(sf)
library(tmap)
library(stringr)
library(dplyr)
library(purrr)
library(readr)
library(lubridate)


setwd("~/Documents/Stories/Moving_Violations_DC_Covid-19")
## These are the six URL endpoints for the Open Data DC API calls to the February - April moving violations
# datasets of 2019 and 2020
dcMovingViolationsURLs <- c("https://opendata.arcgis.com/datasets/186e3b6cf45f44b1ac0fe750518e3cab_3.geojson", 
                            "https://opendata.arcgis.com/datasets/6ceb38b8e24a464a94434c7d39934ebd_2.geojson",
                            "https://opendata.arcgis.com/datasets/c3e91eed970149e6a41853ddadf36394_1.geojson",
                            "https://opendata.arcgis.com/datasets/878e5e25b4fe47bbbbd3a37c77285a63_3.geojson",
                            "https://opendata.arcgis.com/datasets/0e38e123d4414d37905d0bd64af456ad_2.geojson",
                            "https://opendata.arcgis.com/datasets/a03b8a80a06e4451951497dee78959ab_1.geojson")

# This function calls the API endpoint, throws an error if the GET request is not 200, and returns the JSON
# of the data parsed with the jsonlite package.
getMovingViolations <- function(url) {
  Sys.sleep(5)
  server_response <- GET(url = url, user_agent("Adrian Nesta adriannesta@gmail.com This is for a blog post on Covid-19 impacts on MPD moving violations"))
  if (http_error(server_response)) {
    stop("The request to Open Data DC has failed")
  }
  Sys.sleep(5)
  data <- fromJSON(content(server_response, as = "text", encoding = "UTF-8"))
  return(data)
}

# This empty list and empty caracter vector will be where the list of data frames and list of 
# data frame names will be populated from the for loop below using the getMovingViolations function
dcMovingViolationsDFs <- vector(mode = "list", length = length(dcMovingViolationsURLs))
dcMovingViolationsDFNames <- vector(mode = "character", length = length(dcMovingViolationsURLs))
Sys.sleep(15)

# Looping through the list of Open Data DC URLs and adding the data and names to the empty list and
# vector created above
for (i in seq_along(dcMovingViolationsURLs)) {
  movingViolationDF <- getMovingViolations(dcMovingViolationsURLs[i])
  dcMovingViolationsDFs[[i]] <- as_tibble(movingViolationDF$features$properties)
  dcMovingViolationsDFNames[i] <- movingViolationDF$name
  
}
Sys.sleep(15)
# Removing extra data set from loop
rm(movingViolationDF)

# Adding names from the character vector populated with data frame names from the API to the list
# of data frames
dcMovingViolationsDFs <- set_names(dcMovingViolationsDFs, dcMovingViolationsDFNames)

# Calculating the overall year-over-year percent change for February - April between 2019 and 2020
# This is at the end of the first paragraph.

getOverallPercentChange <- function(oldDF, newDF) {
  return(((nrow(newDF) - nrow(oldDF)) / nrow(oldDF)) * 100)
}

febYoYPercentChange <- getOverallPercentChange(dcMovingViolationsDFs[["Moving_Violations_Issued_in_February_2019"]], dcMovingViolationsDFs[["Moving_Violations_Issued_in_February_2020"]])
# February had a 2.9% decrease
marYoYPercentChange <- getOverallPercentChange(dcMovingViolationsDFs[["Moving_Violations_Issued_in_March_2019"]], dcMovingViolationsDFs[["Moving_Violations_Issued_in_March_2020"]])
# March had a 37.9% increase
aprYoYPercentChange <- getOverallPercentChange(dcMovingViolationsDFs[["Moving_Violations_Issued_in_April_2019"]], dcMovingViolationsDFs[["Moving_Violations_Issued_in_April_2020"]])
# April had a 17.4% increase


# This is generally cleaning up the address column to remove unwanted characters/phrases and try and parse
# out a street number, street name, and directional sign.
dcMovingViolationsDFs <- dcMovingViolationsDFs %>% map(~ {
  mutate(., LOCATION = str_squish(str_remove_all(str_to_upper(LOCATION), "BLK|BLOCK|OF"))) %>% 
  mutate(., cleanedAddress = paste(str_to_title(str_extract(LOCATION, "\\d{3,}\\s\\w+\\s\\w+\\s\\w+")), "Washington DC")) %>% 
  mutate(., cleanedAddress = str_replace_all(cleanedAddress, "NA Washington DC", NA_character_)) %>% 
  mutate(., cleanedAddress = str_replace_all(cleanedAddress, " Se ", " SE ")) %>%
  mutate(., cleanedAddress = str_replace_all(cleanedAddress, " Sw ", " SW ")) %>%
  mutate(., cleanedAddress = str_replace_all(cleanedAddress, " Ne ", " NE ")) %>%
  mutate(., cleanedAddress = str_replace_all(cleanedAddress, " Nw ", " NW "))  
})

## This function will write out json from each data frame in our list of addresses that both don't already
# have lat/long values and are not NA for the new cleaned address column

Sys.sleep(10)
addressesToGeocode <- function(df) {
  return(df %>% 
    filter((is.na(LATITUDE) & is.na(LONGITUDE) & !is.na(cleanedAddress))) %>% 
    select(cleanedAddress) %>%
    unlist() %>% 
    unname() %>% 
    toJSON())
  }

# Write out these JSON files which will then be geocoded with Geocodio
walk2(dcMovingViolationsDFs, names(dcMovingViolationsDFs), ~{
  write(addressesToGeocode(.x), paste0(.y, ".json"))
})

Sys.sleep(5)

## Using Bash script to hide the Geocodio API
## This will geocode the missing lat long rows that had addresses that were able to be parsed using 
## Geocodio bulk geocoding.
# system("bash ../../Bash_Scripts/DC_Moving_Violations_Geocode.sh")

# Reading in the Geocodio results.
geocodedMovingViolationsJSONFiles <- list.files(path = ".", pattern = "*_Results.json", full.names = T)
geocodedMovingViolationsJSONFilesOrdered <- as.character(sort(factor(geocodedMovingViolationsJSONFiles, 
                                                                     levels = c(geocodedMovingViolationsJSONFiles[2], 
                                                                                geocodedMovingViolationsJSONFiles[6], 
                                                                                geocodedMovingViolationsJSONFiles[4], 
                                                                                geocodedMovingViolationsJSONFiles[1], 
                                                                                geocodedMovingViolationsJSONFiles[5], 
                                                                                geocodedMovingViolationsJSONFiles[3]))))
Sys.sleep(15)

# This nested map call will get the lat/lon values for the addresses that Geocodio was able to geocode.
listOfCoords <- map(
  map(
    geocodedMovingViolationsJSONFilesOrdered, ~ read_json(.x, simplifyVector = T)
  ), function(x) {
    map_df(x$results$response$results, function(y) {
      y$location
    })
  }  
)
Sys.sleep(5)
# A copy of the list of data frames is needed for in place replacement.
copy <- dcMovingViolationsDFs

# This loop will replace every lat/lon value in each data frame that does not already have values and does have
# a cleaned address value with it's the lat/long coordinates that geocodio was able to prodce 
for (i in seq_along(dcMovingViolationsDFs)) {
  dcMovingViolationsDFs[[names(copy)[i]]][(is.na(copy[[i]]$LATITUDE) & is.na(copy[[i]]$LONGITUDE) & !is.na(copy[[i]]$cleanedAddress)),][["LATITUDE"]] <- listOfCoords[[i]][["lat"]]
  dcMovingViolationsDFs[[names(copy)[i]]][(is.na(copy[[i]]$LATITUDE) & is.na(copy[[i]]$LONGITUDE) & !is.na(copy[[i]]$cleanedAddress)),][["LONGITUDE"]] <- listOfCoords[[i]][["lng"]]
}
# Remove the copy
rm(copy)
Sys.sleep(10)

# Read in the dc shapfile and arrange it in descending WARD order
dcWards <- st_read("/home/adrian/Documents/US_County_Shapfile_Population/9fd1d3b6-e8c5-4158-a4db-03cad2c1beca2020328-1-ammg2.x7wow5.shp")
dcWards <- dcWards %>% arrange(WARD)

## There were still some addresses that showed up rather frequently with no lat/lon values. I exported a 
# csv to geocode these by hand with Google Maps.
map_df(dcMovingViolationsDFs, function(x) {
      x %>% 
      filter(is.na(LATITUDE) & is.na(LONGITUDE)) %>% 
      group_by(LOCATION) %>% 
      tally() %>% 
      arrange(desc(n)) %>% 
      top_n(15) %>% 
      select(LOCATION) %>% 
      bind_rows()

}) %>% unique() %>% write_csv("evenMoreAddressesToHandGeocode.csv") 

# This function takes all the cleanable addresses and populates the lat/long columns with their corresponding
# coordinates.
cleanCoordinates <- function(dataframe) {
  dataframe <- dataframe %>% 
    mutate(LATITUDE = case_when(LOCATION == "I395 SW 250 FEET AFTER EXIT 4 E/B" ~ 38.882188,
                                LOCATION == "DC295 SE 0.4 MILES S/O E/2 S/B (WZ)" ~ 38.842975,
                                LOCATION == "DC295 SW .05 MILE S/O EXIT 1 N/B" ~ 38.822317,
                                LOCATION == "S DAKOTA AVE SE/B @ BLADENSBURG RD" ~ 38.925303,
                                LOCATION == "INTERSECTION  WASHINGTON CIR NW  @*" ~ 38.902401,
                                LOCATION == "INTERSECTION  10TH ST NW  @FLORIDA*" ~ 38.920517,
                                LOCATION == "MONROE ST E/B 16TH ST NE" ~ 38.932962,
                                LOCATION == "TAFT BRIDGE CONNECTICUT AVE NW SO*" ~ 38.921144,
                                LOCATION == "INTERSECTION 14TH ST NW @U ST NW" ~ 38.917079,
                                LOCATION == "TAFT BRIDGE CONNECTICUT AVE NW NO*" ~ 38.921144,
                                LOCATION == "INTERSECTION 17TH ST NW @CORCORA*" ~ 38.911874,
                                LOCATION == "INTERSECTION 9TH ST NW @K ST NW" ~ 38.902525,
                                LOCATION == "INTERSECTION WASHINGTON CIR NW @*" ~ 38.902401,
                                LOCATION == "INTERSECTION BRANCH AVE SE @FRAN*" ~ 38.856475,
                                LOCATION == "INTERSECTION M ST NW @WISCONSIN *" ~ 38.905133,
                                LOCATION == "INTERSECTION BRANCH AVE SE @ERIE*" ~ 38.857285,
                                LOCATION == "INTERSECTION 18TH ST NE @EVARTS *" ~ 38.924517,
                                LOCATION == "INTERSECTION 20TH ST NW @L ST NW" ~ 38.903751,
                                LOCATION == "INT O & POTOMAC ST NW" ~ 38.907829,
                                LOCATION == "INTERSECTION WASHINGTON CIR NW EA*" ~ 38.902401,
                                LOCATION == "INTERSECTION WASHINGTON CIR NW SO*" ~ 38.902401,
                                T ~ LATITUDE),
           LONGITUDE = case_when(LOCATION == "I395 SW 250 FEET AFTER EXIT 4 E/B" ~ -77.025571,
                                 LOCATION == "DC295 SE 0.4 MILES S/O E/2 S/B (WZ)" ~ -77.007777,
                                 LOCATION == "DC295 SW .05 MILE S/O EXIT 1 N/B" ~ -77.016632,
                                 LOCATION == "S DAKOTA AVE SE/B @ BLADENSBURG RD" ~ -76.965241,
                                 LOCATION == "INTERSECTION  WASHINGTON CIR NW  @*" ~ -77.050043,
                                 LOCATION == "INTERSECTION  10TH ST NW  @FLORIDA*" ~ -77.025983,
                                 LOCATION == "MONROE ST E/B 16TH ST NE" ~ -76.982720,
                                 LOCATION == "TAFT BRIDGE CONNECTICUT AVE NW SO*" ~ -77.050116,
                                 LOCATION == "INTERSECTION 14TH ST NW @U ST NW" ~ -77.031947,
                                 LOCATION == "TAFT BRIDGE CONNECTICUT AVE NW NO*" ~ -77.050116,
                                 LOCATION == "INTERSECTION 17TH ST NW @CORCORA*" ~ -77.038461,
                                 LOCATION == "INTERSECTION 9TH ST NW @K ST NW" ~ -77.023968,
                                 LOCATION == "INTERSECTION WASHINGTON CIR NW @*" ~ -77.050043,
                                 LOCATION == "INTERSECTION BRANCH AVE SE @FRAN*" ~ -76.958968,
                                 LOCATION == "INTERSECTION M ST NW @WISCONSIN *" ~ -77.062794,
                                 LOCATION == "INTERSECTION BRANCH AVE SE @ERIE*" ~ -76.958964,
                                 LOCATION == "INTERSECTION 18TH ST NE @EVARTS *" ~ -76.978977,
                                 LOCATION == "INTERSECTION 20TH ST NW @L ST NW" ~ -77.044891,
                                 LOCATION == "INT O & POTOMAC ST NW" ~ -77.065274,
                                 LOCATION == "INTERSECTION WASHINGTON CIR NW EA*" ~ -77.050043,
                                 LOCATION == "INTERSECTION WASHINGTON CIR NW SO*" ~ -77.050043,
                                 T ~ LONGITUDE))
}
# Replacing the hand geo-coded coordinates in each dataframe in the list.
dcMovingViolationsDFs <- map(dcMovingViolationsDFs, cleanCoordinates)

# Making each data frame in the list an sf point object with the coordinates column using the DC wards shapefile crs.
Sys.sleep(5)
dcMovingViolationsDFs <- map(dcMovingViolationsDFs, ~ st_as_sf(x = .x, coords = c("LONGITUDE", "LATITUDE"), crs = as.integer(st_crs(dcWards)[["input"]]), remove = T, na.fail = F))
Sys.sleep(5)
# This join to st_within will take each point in the list of moving violations data frames and determine which ward it is within
# using the ward polygon in the dcWards shapefile. It will assume planar points.
dcMovingViolationsDFsWithWards <- map(dcMovingViolationsDFs, ~ st_join(.x, dcWards, join = st_within))
Sys.sleep(5)

# Creating a dataframe of all violations for each month across years
aprilMovingViolations <- rbind(dcMovingViolationsDFsWithWards[[1]], dcMovingViolationsDFsWithWards[[4]])
Sys.sleep(10)
marchMovingViolations <- rbind(dcMovingViolationsDFsWithWards[[2]], dcMovingViolationsDFsWithWards[[5]])
Sys.sleep(10)
februaryMovingViolations <- rbind(dcMovingViolationsDFsWithWards[[3]], dcMovingViolationsDFsWithWards[[6]])

# This funciton will get the percentage change between the two different years for each monthly dataframes
get_pct_change_df <- function(dataframe) {
  pct_chg_df <- dataframe %>% 
    as_tibble() %>% 
    select(-geometry) %>% 
    filter(!is.na(WARD)) %>% 
    mutate(ISSUE_DATE = ymd_hms(ISSUE_DATE), ISSUE_YEAR = year(ISSUE_DATE)) %>% 
    group_by(WARD, ISSUE_YEAR) %>%
    arrange(ISSUE_DATE) %>% 
    summarize(stops = n()) %>% 
    mutate(pct_change = ((stops - lag(stops)) / lag(stops)) * 100) %>% 
    ungroup() %>% 
    filter(ISSUE_YEAR == 2020) %>% 
    select(-c(ISSUE_YEAR, stops)) %>% 
    right_join(dcWards, by = c("WARD" = "WARD")) %>% 
    st_as_sf()
  return(pct_chg_df)
}

# These grouped data frames are for the year-over-year 2019-2020 percent changes in moving violations 
# broken out by Ward. They will provide the statistics throughout the second paragraph.
Sys.sleep(5)
aprilMovingViolationsPctChg <- get_pct_change_df(aprilMovingViolations)
# April saw most Wards reflect a moderate decline in moving violations, with Wards 5 and 8 bucking that trend
# and experiencing a 34.6% and 117% increase, respectively. The rise of violations in these Wards were the main
# force behind the overall year-over-year rise in April from 2019 to 2020.
Sys.sleep(5)
marchMovingViolationsPctChg <- get_pct_change_df(marchMovingViolations)
# Wards 5, 8, and especially Ward 6 saw massive increases over the prior year of 55.7%, 97.1%, and 222%, respectively
Sys.sleep(5)
februaryMovingViolationsPctChg <- get_pct_change_df(februaryMovingViolations)
## February was relatively stable with a 25.9% increase in Ward 5 and a 
## 27.1% and 33.1% decrease year-over-year in Wards 6 and 8, respectively.


# Adjusting the dcWards bbox for the chrolorpleth maps
regularDCBbox <- st_bbox(dcWards)
xrange <- unname(regularDCBbox["xmax"] - regularDCBbox["xmin"])
adjustedDCBbox <- regularDCBbox
adjustedDCBbox["xmax"] <- adjustedDCBbox["xmax"] + (xrange * .18)
adjustedDCBbox["xmin"] <- adjustedDCBbox["xmin"] - (xrange * .11)

movingViolationCaption <- "Data from Open Data DC"

# Making the by ward chloropleth maps for percentage change in moving violations each month
tm_shape(februaryMovingViolationsPctChg, bbox = adjustedDCBbox) + 
  tm_polygons("pct_change", palette = "RdYlGn", title = "") +
  tm_layout(title = "February",
            title.size = 1.5, 
            title.position = c("left", "top"),
            bg.color = "gray85", 
            inner.margins = 0.1,
            frame.lwd = 2,
            legend.width = .65,
            legend.position = c("right", "bottom"),
            legend.text.size = .7,
            legend.text.fontface = "bold",
            legend.format = list(fun=function(x) paste0(formatC(x, digits=0, format="f"), "%"))) + 
  tm_text("NAME", size = 0.4) +
  tm_credits(text = movingViolationCaption, position = c("left", "bottom", size = 4, fontface = "bold")) -> febYoY
  
tmap_save(febYoY, filename = "febYoY.png", units = "px", height = 996, width = 2000)

tm_shape(marchMovingViolationsPctChg, bbox = adjustedDCBbox) + 
  tm_polygons("pct_change", palette = "RdYlGn", title = "") +
  tm_layout(title = "March",
            title.position = c("left", "top"),
            title.size = 1.5,
            bg.color = "gray85", 
            inner.margins = 0.1,
            frame.lwd = 2,
            legend.width = .65,
            legend.position = c("right", "bottom"),
            legend.text.size = .7,
            legend.text.fontface = "bold",
            legend.format = list(fun=function(x) paste0(formatC(x, digits=0, format="f"), "%"))) +
  tm_text("NAME", size = 0.4) +
  tm_credits(text = movingViolationCaption, position = c("left", "bottom", size = 4, fontface = "bold")) -> marYoY

tmap_save(marYoY, filename = "marYoY.png", units = "px", height = 996, width = 2000)

tm_shape(aprilMovingViolationsPctChg, bbox = adjustedDCBbox) +
  tm_polygons("pct_change", palette = "RdYlGn", title = "") +
  tm_layout(title = "April",
            title.position = c("left", "top"),
            title.size = 1.5,
            bg.color = "gray85", 
            inner.margins = 0.1,
            frame.lwd = 2,
            legend.width = .65,
            legend.position = c("right", "bottom"),
            legend.text.size = .65,
            legend.text.fontface = "bold",
            legend.format = list(fun=function(x) paste0(formatC(x, digits=0, format="f"), "%"))) +
  tm_text("NAME", size = 0.4) +
  tm_credits(text = movingViolationCaption, position = c("left", "bottom", size = 4, fontface = "bold")) -> aprYoY

tmap_save(aprYoY, filename = "aprYoY.png", units = "px", height = 996, width = 2000)

# Creating a full set data frame of all 6 month's moving violations and re converting it out from an sf object
Sys.sleep(10)
fullMovingViolationsSet <- rbind(aprilMovingViolations, marchMovingViolations, februaryMovingViolations)
Sys.sleep(5)

fullMovingViolationsSet <- fullMovingViolationsSet %>% 
  as_tibble() %>% 
  select(-geometry) %>% 
  mutate(Month = month(ISSUE_DATE), Year = year(ISSUE_DATE))

# Making the ggplot theme
dcMovingViolationsTheme <- function() {
  theme_classic() + 
    theme(text = element_text(family = "Georgia", color = "gray25"),
          plot.title = element_text(size = 36, face = "bold"), 
          plot.subtitle = element_text(size = 20),
          plot.caption = element_text(color = "gray30", size = 18),
          legend.position = "right",
          legend.text = element_text(color = "gray30", size = 12, face = "bold"),
          legend.title = element_text(color = "gray30", size = 14, face = "bold"),
          axis.text.x =  element_text(color = "gray25", size = 20, face = "bold"),
          axis.text.y = element_text(color = "gray25", size = 20, face = "bold"),
          axis.title.y = element_text(color = "gray25", size = 18, face = "bold"),
    )
}


# Making the grouped bar graphs of the 5 most frequent moving violations and citation agency for each month and year
Moving_Violations_by_Type <- fullMovingViolationsSet %>% 
  group_by(Month, Year, VIOLATION_PROCESS_DESC) %>% 
  summarize(Citations = n()) %>% 
  arrange(desc(Citations), .by_group = T) %>% 
  top_n(5) %>%
  mutate(MonthName = case_when(Month == 2 ~ "Feb",
                           Month == 3 ~ "Mar",
                           Month == 4 ~ "Apr")) %>%
  mutate(MonthName = factor(MonthName, levels = month.abb[2:4])) %>% 
  ggplot(aes(x = MonthName, y = Citations, fill = reorder(VIOLATION_PROCESS_DESC, -Citations))) +
  geom_col(position = "dodge") +
  scale_fill_brewer(palette = "Set2", type = "qual") +
  xlab("") +
  dcMovingViolationsTheme() +
  labs(title = "Moving Violation Citations", subtitle = "February - April 2019 & February - April 2020", caption = movingViolationCaption, fill = "Violation") +
  facet_wrap(~Year)

ggsave(filename = "/home/adrian/Documents/Stories/Moving_Violations_DC_Covid-19/Moving_Violations_by_Type.png", plot = Moving_Violations_by_Type, width = 400, height = 325, units = "mm")

Moving_Violations_by_Issuing_Agency <- fullMovingViolationsSet %>% 
  group_by(Month, Year, ISSUING_AGENCY_NAME) %>% 
  summarize(Citations = n()) %>% 
  arrange(desc(Citations), .by_group = T) %>% 
  top_n(5) %>%
  mutate(MonthName = case_when(Month == 2 ~ "Feb",
                               Month == 3 ~ "Mar",
                               Month == 4 ~ "Apr")) %>%
  mutate(MonthName = factor(MonthName, levels = month.abb[2:4])) %>% 
  ggplot(aes(x = MonthName, y = Citations, fill = reorder(ISSUING_AGENCY_NAME, -Citations))) +
  geom_col(position = "dodge") +
  scale_fill_brewer(palette = "Set2", type = "qual") +
  scale_y_continuous(trans = 'log2', breaks = scales::trans_breaks("log2", function(x) 2^x)) +
  xlab("") +
  dcMovingViolationsTheme() +
  labs(title = "Moving Violation Issuing Agencies", subtitle = "February - April 2019 & February - April 2020", caption = movingViolationCaption, fill = "Violation") +
  facet_wrap(~Year)

ggsave(filename = "/home/adrian/Documents/Stories/Moving_Violations_DC_Covid-19/Moving_Violations_by_Issuing_Agency.png", plot = Moving_Violations_by_Issuing_Agency, width = 400, height = 325, units = "mm")

## This will give a data frame of the most frequent location for moving violations for each of the six month
# period being analyzed (Feburary - April of 2019 - 2020). For the three months in 2019 and Feburary 2020
# the most frequent location is 600 Kenilworth Ave NW going southbound. However in March 2020 that changed to
# the I-395 going eastbound just after exit 4 (Maine Ave) with 27,691 violations. In April 2020 it was the I-295 going southbound
# just after exit 2 (South Capital Street) with 12,937 violations. This was in the third paragraph
mostTicketedLocations <- map_df(dcMovingViolationsDFs, function(x) {
    x %>% 
      mutate(X = unname(st_coordinates(geometry)[,"X"]), Y = unname(st_coordinates(geometry)[,"Y"])) %>% 
      as_tibble() %>% 
      select(-geometry) %>%
      mutate(Date = paste(month(ISSUE_DATE, label = T, abbr = T), year(ISSUE_DATE), sep = "-")) %>% 
      group_by(LOCATION, X, Y, Date) %>% 
      tally() %>% 
      arrange(desc(n), .by_group = T) %>% 
      ungroup() %>% 
      top_n(1) %>%
      rename(Violations = n) %>% 
      bind_rows()
})

# This is to answer what percentage of total March 2020 moving violations were given out at the most
# frequent location on I-395. What percentage of total April 2020 moving violations were given out at 
# the most frequent location in that month of I-295 southbound. This was in the third paragraph.
percentOfI395Mar2020 <- as.integer(unname(mostTicketedLocations[2,"Violations"])) / nrow(dcMovingViolationsDFs[["Moving_Violations_Issued_in_March_2020"]])
percentOfI295Apr2020 <- as.integer(unname(mostTicketedLocations[1,"Violations"])) / nrow(dcMovingViolationsDFs[["Moving_Violations_Issued_in_April_2020"]])


## This is to answer the March 2019 to March 2020 changes in all speeding moving violations and just 11-25 mph.
# As well as the April 2019 to 2020 changes for those same speed range violations. This is in the fourth paragraph

speedViolation11_25March19 <- fullMovingViolationsSet %>% 
  group_by(Month, Year, VIOLATION_PROCESS_DESC) %>% 
  filter(Month == 3, Year == 2019, VIOLATION_PROCESS_DESC == "SPEED 11-15 MPH OVER THE SPEED LIMIT") %>% 
  summarize(Citations = n()) %>%
  ungroup() %>% 
  select(Citations) %>% 
  as.integer()

speedViolation11_25March20 <- fullMovingViolationsSet %>% 
  group_by(Month, Year, VIOLATION_PROCESS_DESC) %>% 
  filter(Month == 3, Year == 2020, VIOLATION_PROCESS_DESC == "SPEED 11-15 MPH OVER THE SPEED LIMIT") %>% 
  summarize(Citations = n()) %>%
  ungroup() %>% 
  select(Citations) %>% 
  as.integer()

## Percent change 11-25mph over speed limit March 2019 to March 2020

pctChgSpeed11_25Mar1920 <- ((speedViolation11_25March20 - speedViolation11_25March19) / speedViolation11_25March19) * 100


speedViolation11_25April19 <- fullMovingViolationsSet %>% 
  group_by(Month, Year, VIOLATION_PROCESS_DESC) %>% 
  filter(Month == 4, Year == 2019, VIOLATION_PROCESS_DESC == "SPEED 11-15 MPH OVER THE SPEED LIMIT") %>% 
  summarize(Citations = n()) %>%
  ungroup() %>% 
  select(Citations) %>% 
  as.integer()

speedViolation11_25April20 <- fullMovingViolationsSet %>% 
  group_by(Month, Year, VIOLATION_PROCESS_DESC) %>% 
  filter(Month == 4, Year == 2020, VIOLATION_PROCESS_DESC == "SPEED 11-15 MPH OVER THE SPEED LIMIT") %>% 
  summarize(Citations = n()) %>%
  ungroup() %>% 
  select(Citations) %>% 
  as.integer()


## Percent change 11-25mph over speed limit April 2019 to April 2020

pctChgSpeed11_25Apr1920 <- ((speedViolation11_25April20 - speedViolation11_25April19) / speedViolation11_25April19) * 100

## This is to get the percent change of all speeding violations from March 2019-2020 to April 2019-2020

allSpeedViolationMarch19 <- fullMovingViolationsSet %>% 
  group_by(Month, Year, VIOLATION_PROCESS_DESC) %>% 
  filter(Month == 3, Year == 2019) %>% 
  filter(VIOLATION_PROCESS_DESC == "SPEED 11-15 MPH OVER THE SPEED LIMIT" | VIOLATION_PROCESS_DESC == "SPEED 16-20 MPH OVER THE SPEED LIMIT" | VIOLATION_PROCESS_DESC == "SPEED 21-25 MPH OVER THE SPEED LIMIT") %>% 
  summarize(Citations = n()) %>% 
  ungroup() %>%
  select(Citations) %>% 
  sum()

allSpeedViolationMarch20 <- fullMovingViolationsSet %>% 
  group_by(Month, Year, VIOLATION_PROCESS_DESC) %>% 
  filter(Month == 3, Year == 2020) %>% 
  filter(VIOLATION_PROCESS_DESC == "SPEED 11-15 MPH OVER THE SPEED LIMIT" | VIOLATION_PROCESS_DESC == "SPEED 16-20 MPH OVER THE SPEED LIMIT" | VIOLATION_PROCESS_DESC == "SPEED 21-25 MPH OVER THE SPEED LIMIT") %>% 
  summarize(Citations = n()) %>% 
  ungroup() %>%
  select(Citations) %>% 
  sum()
  
pctChgAllSpeedMar1920 <- ((allSpeedViolationMarch20 - allSpeedViolationMarch19) / allSpeedViolationMarch19) * 100  

## Now for all speeding violations percent change between April 2019 and 2020

allSpeedViolationApril19 <- fullMovingViolationsSet %>% 
  group_by(Month, Year, VIOLATION_PROCESS_DESC) %>% 
  filter(Month == 4, Year == 2019) %>% 
  filter(VIOLATION_PROCESS_DESC == "SPEED 11-15 MPH OVER THE SPEED LIMIT" | VIOLATION_PROCESS_DESC == "SPEED 16-20 MPH OVER THE SPEED LIMIT" | VIOLATION_PROCESS_DESC == "SPEED 21-25 MPH OVER THE SPEED LIMIT") %>% 
  summarize(Citations = n()) %>% 
  ungroup() %>%
  select(Citations) %>% 
  sum()

allSpeedViolationApril20 <- fullMovingViolationsSet %>% 
  group_by(Month, Year, VIOLATION_PROCESS_DESC) %>% 
  filter(Month == 4, Year == 2020) %>% 
  filter(VIOLATION_PROCESS_DESC == "SPEED 11-15 MPH OVER THE SPEED LIMIT" | VIOLATION_PROCESS_DESC == "SPEED 16-20 MPH OVER THE SPEED LIMIT" | VIOLATION_PROCESS_DESC == "SPEED 21-25 MPH OVER THE SPEED LIMIT") %>% 
  summarize(Citations = n()) %>% 
  ungroup() %>%
  select(Citations) %>% 
  sum()

pctChgAllSpeedApr1920 <- ((allSpeedViolationApril20 - allSpeedViolationApril19) / allSpeedViolationApril19) * 100  

# Making a map of the most frequent violation locations for all six months analyzed
mostTicketedLocations <- mostTicketedLocations %>% 
  mutate(Date = factor(Date, levels = c("Apr-2020", "Mar-2020", "Feb-2020", "Apr-2019", "Mar-2019", "Feb-2019"))) %>% 
  rename(Location = LOCATION) %>% 
  mutate(Location = case_when(Location == "DC295 SE 0.4 MILES S/O E/2 S/B (WZ)" ~ "DC295 SE 0.4 MILES\nS/O E/2 S/B (WZ)",
                              Location == "I395 SW 250 FEET AFTER EXIT 4 E/B" ~ "I395 SW 250 FEET\nAFTER EXIT 4 E/B",
                              Location == "600 KENILWORTH AVE NE S/B" ~ "600 KENILWORTH\nAVE NE S/B"))

register_google("Your_API_here")

DC_Map <- ggmap(get_map(location = c(lon = -77.019, lat = 38.869), zoom = 12, maptype = "toner-lite", scale = 2, color = 'color', source = "stamen"))

Map_of_most_frequent_moving_Violations <- DC_Map +
  geom_point(data = mostTicketedLocations, aes(x = X, y = Y, color = Date, size = Violations, stroke = 4), position = position_jitter(width = 0.003, height = 0.003)) +
  geom_label(data = mostTicketedLocations, aes(x = X, y = Y, label = Location), hjust = 1, vjust = 1, nudge_x = -0.007) +
  xlab('') + 
  ylab('') +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        legend.text = element_text(color = "gray30", size = 14, face = "bold"),
        legend.title = element_text(color = "gray30", size = 16, face = "bold"),
        plot.title = element_text("Most Frequent Moving Violation Location"), plot.subtitle = element_text("February - April 2019 - 2020"), plot.caption = element_text(movingViolationCaption)) +
  guides(color = guide_legend(override.aes = list(size = 10)))

ggsave(filename = "/home/adrian/Documents/Stories/Moving_Violations_DC_Covid-19/Map_of_most_frequent_moving_Violations.png", plot = Map_of_most_frequent_moving_Violations, width = 400, height = 325, units = "mm")


# Saving a copy of the list of moving violations dataframes from the API
walk2(dcMovingViolationsDFs, paste0(dcMovingViolationsDFNames, ".csv"), write_csv)

