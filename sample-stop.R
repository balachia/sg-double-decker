#!/usr/bin/env Rscript
suppressMessages({
library(tidyverse)
library(data.table)
library(httr)
library(jsonlite)
library(lubridate)
})

# load api key
apikey <- fromJSON('~/.credentials/lta-datamall')

# load sampling stops
sample.stops <- fread('./data/sampled-stops.csv')
sample.stops <- sample.stops[, stop.id]

# endpoints
#url.arrival <- 'http://datamall2.mytransport.sg/ltaodataservice/BusArrivalv2?$skip=%d'
url.arrival <- 'http://datamall2.mytransport.sg/ltaodataservice/BusArrivalv2?BusStopCode=%s'

download.stop <- function(url, stop.id) {
    req.time <- Sys.time()
    res <- GET(sprintf(url.arrival, stop.id), do.call(add_headers, apikey))
    data <- fromJSON(rawToChar(res$content))$Services
    # augment with time
    data <- data %>%
        mutate(RequestTime = strftime(req.time, tz = 'Asia/Singapore', usetz = TRUE))
    data
}

clean.data <- function(raw.df) {
    clean.df <- raw.df %>%
        as.data.table() %>%
        pivot_longer(starts_with('NextBus'), names_to = c('Order', '.value'), names_sep = '\\.') %>%
        mutate(Order = recode(Order, NextBus = 1, NextBus2 = 2, NextBus3 = 3))
    clean.df <- clean.df %>%
        mutate(service.id = paste(ServiceNo, OriginCode, DestinationCode, sep = '-')) %>%
        mutate(ArrTime = strptime(EstimatedArrival, '%Y-%m-%dT%T') - strptime(RequestTime, '%Y-%m-%d %T'))
    as.data.table(clean.df)
}

scan.all <- function(stops, timestamp, interval = 0.1) {
    nstops <- length(stops)
    for(i in seq_len(nstops)) {
        stopid <- stops[i]
        cat(sprintf('Sampling %s (%d / %d) ...', stopid, i, nstops))
        raw.df <- download.stop(url.arrival, stopid)
        clean.df <- clean.data(raw.df)
        fwrite(clean.df, sprintf('./samples/%s-%s.csv', stopid, timestamp))
        cat(sprintf('done\n'))
        Sys.sleep(interval)
    }
}

timestamp <- Sys.time() %>%
    strftime('%Y%m%d-%H-%M')

scan.all(sample.stops, timestamp)

#test <- download.stop(url.arrival, sample.stops[1])
#t2 <- clean.data(test)
#t2 <- t2[!is.na(ArrTime)]
#t2[, service.id := paste(ServiceNo, OriginCode, DestinationCode, sep = '-')]
#t2[, .(max.arr = max(ArrTime, na.rm = TRUE)), by = service.id][, min(max.arr)]
#t2[, .(RequestTime, EstimatedArrival, ArrTime)]

#t2 <- test %>%
#    as.data.table() %>%
#    pivot_longer(starts_with('NextBus'), names_to = c('Order', '.value'), names_sep = '\\.') %>%
#    mutate(Order = recode(Order, NextBus = 1, NextBus2 = 2, NextBus3 = 3))
