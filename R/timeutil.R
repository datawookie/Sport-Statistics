library(lubridate)

decimal.hours <- function(time) {
  time <- strsplit(time, ":")
  #
  sapply(time, function(t) {
    t <- as.numeric(t)
    #
    # Add in missing minutes and/or seconds
    #
    while (length(t) < 3) {
      t <- c(t, 0)
    }
    t[1] + (t[2] + t[3] / 60) / 60
  })
}

# TODO: Put these into separate tests file.
#
print(decimal.hours("05:30:25.3"))
print(decimal.hours("05:30"))
print(decimal.hours(c("05:30:25.3", "04:15")))