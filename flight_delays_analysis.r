# Flight Delays Analysis
# R Project - Simplilearn

# Install and load required package
install.packages("readxl")
library(readxl)

# Check working directory
dir()

# Load the flight delays dataset
# Update the path below to the location of the Excel dataset on your computer.
flight_delays <- read_excel("1657873325_flightdelays 2.xlsx")

# View the dataset
View(flight_delays)

# Check missing values
colSums(is.na(flight_delays))

# Summary statistics
summary(flight_delays)

# Structure of the dataset
str(flight_delays)


# ---------------------------------------------------------
# HISTOGRAMS
# Understand relationships between scheduled time, carrier,
# destination, origin, weather, and day of the week
# ---------------------------------------------------------

library(plotly)

for (var in c("schedtime", "carrier", "dest", "origin", "weather", "dayweek")) {
  plot_ly(
    flight_delays,
    x = ~get(var),
    type = "histogram",
    nbinsx = 30
  ) %>%
    layout(title = paste("Histogram of", var)) %>%
    print()
}


# ---------------------------------------------------------
# SCATTER PLOT
# Flights on time and delayed
# ---------------------------------------------------------

# Scheduled time
p <- plot_ly(
  data = flight_delays,
  x = ~schedtime,
  y = ~deptime,
  type = "scatter",
  mode = "markers",
  marker = list(color = "red"),
  name = "Scheduled Time"
)

# Departure time
p <- p %>%
  add_trace(
    x = ~schedtime,
    y = ~deptime,
    type = "scatter",
    mode = "markers",
    marker = list(color = "blue"),
    name = "Departure Time"
  )

p


# ---------------------------------------------------------
# BOX PLOT
# Understand flight delays by day of the month
# ---------------------------------------------------------

plot_ly(
  flight_delays,
  x = ~as.factor(daymonth),
  y = ~deptime,
  color = ~delay,
  type = "box"
) %>%
  layout(title = "Box plot for flight delays by day of the month")


# ---------------------------------------------------------
# DEPARTURE HOUR ANALYSIS
# ---------------------------------------------------------

# Define the hours of departure
flight_delays$deptime_char <- as.character(flight_delays$deptime)

# Extract the hour portion
flight_delays$hour_of_departure <- as.numeric(
  substr(
    flight_delays$deptime_char,
    start = 1,
    stop = nchar(flight_delays$deptime_char) - 2
  )
)

# View the data
head(flight_delays[, c("deptime", "hour_of_departure")])


# ---------------------------------------------------------
# CATEGORICAL REPRESENTATION
# ---------------------------------------------------------

contingency_table <- table(
  flight_delays$carrier,
  flight_delays$delay
)

contingency_table


# ---------------------------------------------------------
# REDEFINE DELAY VARIABLE
# ---------------------------------------------------------

flight_delays$delay <- ifelse(
  flight_delays$delay == "ontime",
  0,
  1
)


# ---------------------------------------------------------
# SUMMARY OF MAJOR VARIABLES
# ---------------------------------------------------------

major_variables <- c(
  "schedtime",
  "deptime",
  "distance",
  "weather",
  "dayweek",
  "daymonth",
  "delay"
)

summary(flight_delays[, major_variables])


# ---------------------------------------------------------
# PIE CHART
# Distribution of flight delays
# ---------------------------------------------------------

delay_counts <- table(flight_delays$delay)

plot_ly(
  labels = c("On Time", "Delayed"),
  values = delay_counts,
  type = "pie"
) %>%
  layout(title = "Distribution of Flight Delays")
