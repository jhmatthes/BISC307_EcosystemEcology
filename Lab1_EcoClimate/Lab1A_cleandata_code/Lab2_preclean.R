# Answer Key for Lab 2.1

library(tidyverse)
library(lubridate)

# Load .RData file with Boston & Phoenix climate data 
load("data/BOSPHO_climate.RData")

# Clip out 1981-2010 in BOS from the climate data
PHO.climrecent <- filter(BOS.PHO.climate, year(date) > 1980 & year(date) < 2011,
                         station == "PHOENIX AIRPORT AZ US")

# Plot Phoenix and Boston maximum temperature, 1981-2010
ggplot(data = PHO.climrecent) + 
  geom_line(mapping = aes(x = date, y = tmax))

# Add a new tibble column with just month & day from date
PHO.climrecent.v2 <- mutate(PHO.climrecent, 
                            month.day = as.Date(format(date, format = "%m-%d"), format = "%m-%d"))

# Remove leap year dates by removing days that are NA: small detail, simplifies climatology
PHO.climrecent.v2 <- PHO.climrecent.v2[!is.na(PHO.climrecent.v2$month.day),]

# Group tibble by month.day for climatology
PHO.clim.grouped <- group_by(PHO.climrecent.v2, month.day)

# Calculate 30-year average tmax for each day of the year
PHO.tmax30yr <- summarize(PHO.clim.grouped, tmax.mean = mean(tmax, na.rm=TRUE))

# Plot Boston climatological maximum temperature, 1981-2010
ggplot(data = PHO.tmax30yr) + 
  geom_line(mapping = aes(x = month.day, y = tmax.mean)) + 
  labs(x = "Date", y = "Maximum Temperature [C]")

# Add 30-year mean daily max temp to new tibble
PHO.climatology <- mutate(PHO.climrecent.v2, 
                          tmax.mean = rep(PHO.tmax30yr$tmax.mean, 30))

# Add column for difference between daily maximum temperature and the climatological mean
PHO.climatology <- mutate(PHO.climatology, 
                          tmax.diff = tmax - tmax.mean)

# Plot Boston difference from climatological maximum temperature, 1981-2010
ggplot(data = PHO.climatology) + 
  geom_line(mapping = aes(x = date, y = tmax.diff)) + 
  labs(x = "Date", y = "Temperature difference from 30-year average [C]")

# Plot Boston difference from climatological maximum temperature, 1981-2010
ggplot(data = PHO.climatology) + 
  geom_density(mapping = aes(x = tmax.diff)) +
  labs(x = "Daily max temp difference from 30-year normal [C]", y = "Probability") 


