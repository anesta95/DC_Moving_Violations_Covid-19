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

dcMovingViolationsURLs <- c("https://opendata.arcgis.com/datasets/186e3b6cf45f44b1ac0fe750518e3cab_3.geojson", 
                            "https://opendata.arcgis.com/datasets/6ceb38b8e24a464a94434c7d39934ebd_2.geojson",
                            "https://opendata.arcgis.com/datasets/c3e91eed970149e6a41853ddadf36394_1.geojson",
                            "https://opendata.arcgis.com/datasets/878e5e25b4fe47bbbbd3a37c77285a63_3.geojson",
                            "https://opendata.arcgis.com/datasets/0e38e123d4414d37905d0bd64af456ad_2.geojson",
                            "https://opendata.arcgis.com/datasets/a03b8a80a06e4451951497dee78959ab_1.geojson")

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

dcMovingViolationsDFs <- vector(mode = "list", length = length(dcMovingViolationsURLs))
dcMovingViolationsDFNames <- vector(mode = "character", length = length(dcMovingViolationsURLs))
Sys.sleep(15)
for (i in seq_along(dcMovingViolationsURLs)) {
  movingViolationDF <- getMovingViolations(dcMovingViolationsURLs[i])
  dcMovingViolationsDFs[[i]] <- as_tibble(movingViolationDF$features$properties)
  dcMovingViolationsDFNames[i] <- movingViolationDF$name
  
}
Sys.sleep(15)
rm(movingViolationDF)
dcMovingViolationsDFs <- set_names(dcMovingViolationsDFs, dcMovingViolationsDFNames)

dcMovingViolationsDFs <- dcMovingViolationsDFs %>% map(~ {
  mutate(., LOCATION = str_squish(str_remove_all(str_to_upper(LOCATION), "BLK|BLOCK|OF"))) %>% 
  mutate(., cleanedAddress = paste(str_to_title(str_extract(LOCATION, "\\d{3,}\\s\\w+\\s\\w+\\s\\w+")), "Washington DC")) %>% 
  mutate(., cleanedAddress = str_replace_all(cleanedAddress, "NA Washington DC", NA_character_)) %>% 
  mutate(., cleanedAddress = str_replace_all(cleanedAddress, " Se ", " SE ")) %>%
  mutate(., cleanedAddress = str_replace_all(cleanedAddress, " Sw ", " SW ")) %>%
  mutate(., cleanedAddress = str_replace_all(cleanedAddress, " Ne ", " NE ")) %>%
  mutate(., cleanedAddress = str_replace_all(cleanedAddress, " Nw ", " NW "))  
})

Sys.sleep(10)
addressesToGeocode <- function(df) {
  return(df %>% 
    filter((is.na(LATITUDE) & is.na(LONGITUDE) & !is.na(cleanedAddress))) %>% 
    select(cleanedAddress) %>%
    unlist() %>% 
    unname() %>% 
    toJSON())
  }

walk2(dcMovingViolationsDFs, names(dcMovingViolationsDFs), ~{
  write(addressesToGeocode(.x), paste0(.y, ".json"))
})

Sys.sleep(5)

## Using Bash script to hide the Geocodio API
## This will geocode the missing lat long rows that had addresses that were able to be parsed
# system("bash ../../Bash_Scripts/DC_Moving_Violations_Geocode.sh")

geocodedMovingViolationsJSONFiles <- list.files(path = ".", pattern = "*_Results.json", full.names = T)
geocodedMovingViolationsJSONFilesOrdered <- as.character(sort(factor(geocodedMovingViolationsJSONFiles, 
                                                                     levels = c(geocodedMovingViolationsJSONFiles[2], 
                                                                                geocodedMovingViolationsJSONFiles[6], 
                                                                                geocodedMovingViolationsJSONFiles[4], 
                                                                                geocodedMovingViolationsJSONFiles[1], 
                                                                                geocodedMovingViolationsJSONFiles[5], 
                                                                                geocodedMovingViolationsJSONFiles[3]))))
Sys.sleep(15)
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
copy <- dcMovingViolationsDFs

for (i in seq_along(dcMovingViolationsDFs)) {
  dcMovingViolationsDFs[[names(copy)[i]]][(is.na(copy[[i]]$LATITUDE) & is.na(copy[[i]]$LONGITUDE) & !is.na(copy[[i]]$cleanedAddress)),][["LATITUDE"]] <- listOfCoords[[i]][["lat"]]
  dcMovingViolationsDFs[[names(copy)[i]]][(is.na(copy[[i]]$LATITUDE) & is.na(copy[[i]]$LONGITUDE) & !is.na(copy[[i]]$cleanedAddress)),][["LONGITUDE"]] <- listOfCoords[[i]][["lng"]]
}

rm(copy)
Sys.sleep(10)

dcWards <- st_read("/home/adrian/Documents/US_County_Shapfile_Population/9fd1d3b6-e8c5-4158-a4db-03cad2c1beca2020328-1-ammg2.x7wow5.shp")
dcWards <- dcWards %>% arrange(WARD)

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

dcMovingViolationsDFs <- map(dcMovingViolationsDFs, cleanCoordinates)

Sys.sleep(5)
dcMovingViolationsDFs <- map(dcMovingViolationsDFs, ~ st_as_sf(x = .x, coords = c("LONGITUDE", "LATITUDE"), crs = as.integer(st_crs(dcWards)[["input"]]), remove = T, na.fail = F))
Sys.sleep(5)
dcMovingViolationsDFsWithWards <- map(dcMovingViolationsDFs, ~ st_join(.x, dcWards, join = st_within))
Sys.sleep(5)

aprilMovingViolations <- rbind(dcMovingViolationsDFsWithWards[[1]], dcMovingViolationsDFsWithWards[[4]])
Sys.sleep(10)
marchMovingViolations <- rbind(dcMovingViolationsDFsWithWards[[2]], dcMovingViolationsDFsWithWards[[5]])
Sys.sleep(10)
februaryMovingViolations <- rbind(dcMovingViolationsDFsWithWards[[3]], dcMovingViolationsDFsWithWards[[6]])

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
Sys.sleep(5)
aprilMovingViolationsPctChg <- get_pct_change_df(aprilMovingViolations)
Sys.sleep(5)
marchMovingViolationsPctChg <- get_pct_change_df(marchMovingViolations)
Sys.sleep(5)
februaryMovingViolationsPctChg <- get_pct_change_df(februaryMovingViolations)


regularDCBbox <- st_bbox(dcWards)
xrange <- unname(regularDCBbox["xmax"] - regularDCBbox["xmin"])
adjustedDCBbox <- regularDCBbox
adjustedDCBbox["xmax"] <- adjustedDCBbox["xmax"] + (xrange * .18)
adjustedDCBbox["xmin"] <- adjustedDCBbox["xmin"] - (xrange * .11)

movingViolationCaption <- "Data from Open Data DC"

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
  
tmap_save(febYoY, filename = "febYoY.png", units = "px", height = 935, width = 1875)

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

tmap_save(marYoY, filename = "marYoY.png", units = "px", height = 935, width = 1875)

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

tmap_save(aprYoY, filename = "aprYoY.png", units = "px", height = 935, width = 1875)

Sys.sleep(10)
fullMovingViolationsSet <- rbind(aprilMovingViolations, marchMovingViolations, februaryMovingViolations)
Sys.sleep(5)

fullMovingViolationsSet <- fullMovingViolationsSet %>% 
  as_tibble() %>% 
  select(-geometry) %>% 
  mutate(Month = month(ISSUE_DATE), Year = year(ISSUE_DATE))

dcMovingViolationsTheme <- function() {
  theme_classic() + 
    theme(text = element_text(family = "Georgia", color = "gray25"),
          plot.title = element_text(size = 24, face = "bold"), 
          plot.subtitle = element_text(size = 16),
          plot.caption = element_text(color = "gray30", size = 12),
          legend.position = "right",
          legend.text = element_text(color = "gray30", size = 8, face = "bold"),
          legend.title = element_text(color = "gray30", size = 10, face = "bold"),
          axis.text = element_text(color = "gray25", size = 14)
    )
}



fullMovingViolationsSet %>% 
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
  labs(title = "Moving Violation Citations", subtitle = "February - April 2019 - 2020", caption = movingViolationCaption, fill = "Violation") +
  facet_wrap(~Year)

fullMovingViolationsSet %>% 
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
  labs(title = "Moving Violation Issuing Agencies", subtitle = "February - April 2019 - 2020", caption = movingViolationCaption, fill = "Violation") +
  facet_wrap(~Year)


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

mostTicketedLocations <- mostTicketedLocations %>% 
  mutate(Date = factor(Date, levels = c("Apr-2020", "Mar-2020", "Feb-2020", "Apr-2019", "Mar-2019", "Feb-2019"))) %>% 
  rename(Location = LOCATION) %>% 
  mutate(Location = case_when(Location == "DC295 SE 0.4 MILES S/O E/2 S/B (WZ)" ~ "DC295 SE 0.4 MILES\nS/O E/2 S/B (WZ)",
                              Location == "I395 SW 250 FEET AFTER EXIT 4 E/B" ~ "I395 SW 250 FEET\nAFTER EXIT 4 E/B",
                              Location == "600 KENILWORTH AVE NE S/B" ~ "600 KENILWORTH\nAVE NE S/B"))

register_google("Your-API-here")

DC_Map <- ggmap(get_map(location = c(lon = -77.019, lat = 38.869), zoom = 12, maptype = "toner-lite", scale = 2, color = 'color'))

DC_Map +
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


