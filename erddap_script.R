###

library(rerddap)

### Find temp data

sstInfo <- info('erdHadISST')
murSST <- griddap(sstInfo, latitude = c(-43.6, -43.6), longitude = c(172.7, 172.7), 
                  time = c("2013-05-29", "2019-10-31"))

lyttelton_weather <- murSST$data

### linking to Shiny script: clean weather data, save data as a Rdata/csv; load into Shiny script

save(lyttelton_weather, file = "lyttelton_weather.RData")
