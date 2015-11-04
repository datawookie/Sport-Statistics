library(RCurl)
library(XML)
library(stringr)
library(dplyr)
library(jsonlite)

url <- "https://en.wikipedia.org/wiki/Marathon_world_record_progression"
#
if (!exists("html")) {
  html <- getURL(url)
}

# Extract tables from HTML.
#
men <- readHTMLTable(html, which = 1, stringsAsFactors = FALSE)
women <- readHTMLTable(html, which = 2, stringsAsFactors = FALSE)

# PREPROCESS ==========================================================================================================

prepare.table <- function(df) {
  df$Notes <- NULL
  #
  names(df)[5] <- "Location"
  #
  df$Location <- sub("\\[.*\\]", " ", df$Location)
  #
  df$Nationality <- str_trim(gsub("\\[[[:digit:]]*\\]", "", df$Nationality))
  #
  df$Date <- sub("\\[.*\\]", "", df$Date)
  df$Date <- as.Date(strptime(df$Date, "%B %d, %Y", tz = "UTC"))
  #
  df$Source <- gsub("\\[[[:digit:]]*\\]", "", df$Source)
  df$Source <- strsplit(df$Source, ",? ")
  #
  df$Time <- sub(":xx", "", df$Time)
  #
  df
}

men <- prepare.table(men)
women <- prepare.table(women)

men$gender <- "Male"
women$gender <- "Female"

marathon.records <- rbind(men, women)
#
names(marathon.records) <- tolower(names(marathon.records))
#
marathon.records <- select(marathon.records, gender, date, name, nationality, time, location, source) %>%
  arrange(date)

# WRITE ===============================================================================================================

cat(toJSON(marathon.records, pretty = TRUE), file = "marathon-records.json")

marathon.records <- transform(marathon.records,
                              time_decimal = decimal.hours(time),
                              text = paste0(name, " [", nationality, "] ", time, "<br>", date, " (", location, ")")
)

write.csv(marathon.records[, -7], file = "marathon-records.csv")
