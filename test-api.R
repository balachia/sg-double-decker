library(tidyverse)
library(data.table)
library(httr)
library(jsonlite)

# load api key
apikey <- fromJSON('~/.credentials/lta-datamall')

# endpoints
url.services <- 'http://datamall2.mytransport.sg/ltaodataservice/BusServices?$skip=%d'
url.routes <- 'http://datamall2.mytransport.sg/ltaodataservice/BusRoutes?$skip=%d'
url.stops <- 'http://datamall2.mytransport.sg/ltaodataservice/BusStops?$skip=%d'

download.all <- function(ep.name, ep.url, sleep = 0.1) {
    continue.download <- TRUE
    skip <- 0
    datas <- list()
    while(continue.download) {
        cat('downloading', ep.name, 'at skip', skip, '\n')
        res <- GET(sprintf(ep.url, skip), do.call(add_headers, apikey))
        data <- fromJSON(rawToChar(res$content))$value
        datas <- c(datas, list(data))
        # check for loop
        continue.download <- nrow(data) >= 500
        if(continue.download) {
            skip <- skip + 500
            Sys.sleep(sleep)
        }
    }
    rbindlist(datas)
}

# download all services
data.services <- download.all('services', url.services)
data.routes <- download.all('routes', url.routes)
data.stops <- download.all('stops', url.stops)

# save all services
fwrite(data.services, './data/services.csv')
fwrite(data.routes, './data/routes.csv')
fwrite(data.stops, './data/stops.csv')

saveRDS(data.services, './data/services.Rds')
saveRDS(data.routes, './data/routes.Rds')
saveRDS(data.stops, './data/stops.Rds')

# regenerate unique route id (incorporating direction)
data.routes[, service.id := paste0(ServiceNo, '.', Direction)]
route2stop <- data.routes[, .(weight = .N), by = .(service.id, stop.id = BusStopCode)]
setkey(route2stop, stop.id)

# rank stops by number of visiting routes
weighted.stops <- route2stop[, .(wgt = sum(weight), uwgt = .N), by = stop.id]
weighted.stops <- weighted.stops[order(-uwgt)]

all.services <- route2stop[, unique(service.id)]

# service to stop matrix
m.s2s <- sapply(weighted.stops[,stop.id], function(sid) { all.services %in% route2stop[J(sid), service.id] } ) %>%
    t()

stop.names.dt <- data.stops[, .(stop.id = BusStopCode, name = paste0(RoadName, ' -- ', Description))]
setkey(stop.names.dt, stop.id)

# identify inferior stations (station x inferior to y if all routes visiting x also visit y)
# is there a shorter way to do this than compare everything to everything else?
# start from most popular and mark all least popular
stop.ids <- weighted.stops[, stop.id]
stop.names <- stop.names.dt[J(stop.ids), name]
nstops <- nrow(weighted.stops)
ninferior <- 0
inferior <- rep(FALSE, nstops)
inferiorto <- rep(as.character(NA), nstops)
for(i in seq_len(nstops)) {
    if(inferior[i]) { next }
    idi <- stop.ids[i]
    namei <- stop.names[i]
    cat(sprintf('%d / %d (%s, %s)\n', i, nstops, idi, namei))
    #cat(i, '/', nstops, '(', namei, ')\n')
    services.i <- m.s2s[i,]
    for(j in (i+1):(nstops)) {
        if(inferior[j]) { next }
        #idj <- weighted.stops[j, stop.id]
        services.j <- m.s2s[j,]
        # if inferior: if the intersection of i and j doesn't differ from j
        if( !any(xor(services.j, services.j & services.i)) ) {
            idj <- stop.ids[j]
            namej <- stop.names[j]
            ninferior <- ninferior + 1
            inferior[j] <- TRUE
            inferiorto[j] <- idj
            #cat(sprintf('  sup to %s, %s (%d)\n', idj, namej, ninferior))
            #cat('\t', 'sup to', idj, '(', ninferior, ')\n')
        }
    }
    cat(sprintf('    %d\n', ninferior))
}

# non-inferior stops:
remaining.ids <- stop.ids[!inferior]
remaining.s2s <- route2stop[J(remaining.ids)]
setkey(remaining.s2s, stop.id)

# calculate remaining services
get.remaining.services <- function(s2s) {
    s2s[, unique(service.id)]
}

## dumb coverage sample
#top.stops <- remaining.s2s[, .N, by = stop.id][order(-N), stop.id]
#sampled.stops <- character(0)
#stopi <- 1
#rem.services <- get.remaining.services(remaining.s2s)
#rem.s2s <- remaining.s2s
#while(length(rem.services) > 0) {
#    top.stop <- top.stops[stopi]
#    sampled.stops <- c(sampled.stops, top.stop)
#    covered.services <- rem.s2s[stop.id == top.stop, service.id]
#    rem.s2s <- rem.s2s[!(service.id %in% covered.services)]
#    rem.services <- get.remaining.services(rem.s2s)
#    cat(sprintf('remove %s, covering %d routes\n', top.stop, length(covered.services)))
#    stopi <- stopi + 1
#}

# greedy coverage sample
sampled.stops <- character(0)
rem.services <- get.remaining.services(remaining.s2s)
rem.s2s <- remaining.s2s
while(length(rem.services) > 0) {
    top.stop <- rem.s2s[, .N, by = stop.id][order(-N), stop.id][1]
    #top.stop <- top.stops[stopi]
    sampled.stops <- c(sampled.stops, top.stop)
    # remove covered services
    covered.services <- rem.s2s[stop.id == top.stop, service.id]
    rem.s2s <- rem.s2s[!(service.id %in% covered.services)]
    # update services left to cover
    rem.services <- get.remaining.services(rem.s2s)
    cat(sprintf('remove %s, covering %d routes\n', top.stop, length(covered.services)))
}
fwrite(list(stop.id = sampled.stops), './data/sampled-stops.csv')

# genetic style, fitness function
coverage.f <- function(x) sum(xor(x, rep(c(T,F), 5)))
n.services <- remaining.s2s[, length(unique(service.id))]
n.stops <- remaining.s2s[, length(unique(stop.id))]
coverage.f <- function(x, uncov.weight = 1.0, .n.services = n.services, .n.stops = n.stops, debug = FALSE) {
    covered <- remaining.ids[x == 1]
    n.uncovered.services <- .n.services - remaining.s2s[J(covered), length(unique(service.id))]
    if(debug) {
        print(c(length(covered), n.uncovered.services))
    }
    - uncov.weight * n.uncovered.services * .n.stops - length(covered)
}

res <- ga('binary', coverage.f,
          lower = rep(FALSE, n.stops),
          upper = rep(TRUE, n.stops),
          nBits = n.stops,
          uncov.weight = 0.1,
          optim = TRUE, popSize = 100, maxiter = 500)
summary(res)
opt <- (res@solution == 1)[1,]
coverage.f(opt, debug = TRUE)
