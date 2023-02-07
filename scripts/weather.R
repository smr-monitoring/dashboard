library(tidyverse)
library(lubridate)
library(googlesheets4)
library(httr)
library(jsonlite)
library(timetk)

start <- ymd("2022-05-25")
end <- Sys.Date()

cols <- c("epoch", "wind_lull", "wind_avg", "wind_gust", "wind_dir", "wind_sam_int", "pressure", "airtemp", "rel_hum", "illum", "uv_index", "solar_rad", "rain_accum", "precip_type", "avg_strike_dist", "strike_ct", "battery", "report_int", "day_rain_accum", "nc_rain_accum", "local_nc_rain_accum", "precip_analy_type")

baseurl <- "https://swd.weatherflow.com/swd/rest/observations/device/186698?day_offset="
token <- "&token=b111ba3f-2091-4ab0-b328-7c1659986fff"

weather_list <- list()

offset1 <- as.numeric(ceiling(Sys.Date() - start))
offset2 <- as.numeric(ceiling(Sys.Date() - end))

offsets <- offset1:offset2

for(i in 1:length(offsets)){
  dayoffset <- offsets[i]
  fullurl <- paste0(baseurl, dayoffset, token)
  raw <- GET(fullurl)
  raw_text <- content(raw, as = "text", encoding = "UTF-8")
  data <- fromJSON(raw_text)
  obs <- as_tibble(data$obs)
  weather_list[[i]] = obs
}

weather <- do.call(rbind, weather_list)

names(weather) <- cols

#wind categories from https://www7.ncdc.noaa.gov/climvis/help_wind.html
#beaufort scale from https://www.weather.gov/mfl/beaufort
weather <- weather %>%
  mutate(datetime = as_datetime(epoch, tz = "EST"),
         wind_cat = case_when(wind_dir %in% c(349:360, 0:11) ~ "N",
                              wind_dir %in% 12:33 ~ "NNE",
                              wind_dir %in% 34:56 ~ "NE", 
                              wind_dir %in% 57:78 ~ "ENE",
                              wind_dir %in% 79:101 ~ "E",
                              wind_dir %in% 102:123 ~ "ESE",
                              wind_dir %in% 124:146 ~ "SE",
                              wind_dir %in% 147:168 ~ "SSE",
                              wind_dir %in% 169:191 ~ "S",
                              wind_dir %in% 192:213 ~ "SSW",
                              wind_dir %in% 214:236 ~ "SW",
                              wind_dir %in% 237:258 ~ "WSW",
                              wind_dir %in% 259:281 ~ "W",
                              wind_dir %in% 282:303 ~ "WNW",
                              wind_dir %in% 304:326 ~ "NW",
                              wind_dir %in% 327:348 ~ "NNW"),
         wind_mph = wind_avg * 2.23694,
         beaufort = case_when(wind_mph >= 0 & wind_mph < 1 ~ "Calm",
                              wind_mph >= 1 & wind_mph < 4 ~ "Light air",
                              wind_mph >= 4 & wind_mph < 8 ~ "Light breeze",
                              wind_mph >= 8 & wind_mph < 13 ~ "Gentle breeze",
                              wind_mph >= 13 & wind_mph < 19 ~ "Moderate breeze",
                              wind_mph >= 19 & wind_mph < 25 ~ "Fresh breeze",
                              wind_mph >= 25 & wind_mph < 32 ~ "Strong breeze",
                              wind_mph >= 32 & wind_mph < 39 ~ "Near gale",
                              wind_mph >= 39 & wind_mph < 47 ~ "Gale",
                              wind_mph >= 47 & wind_mph < 55 ~ "Severe gale",
                              wind_mph >= 55 & wind_mph < 64 ~ "Storm",
                              wind_mph >= 64 & wind_mph < 72 ~ "Violent storm",
                              wind_mph >= 72 ~ "Hurricane")) %>%
  select(-epoch)

weather <- weather %>%
  mutate(date = as.Date(datetime, format="%Y-%m-%d"))


## Save data
saveRDS(weather, "weather-smcmdock.rds")
