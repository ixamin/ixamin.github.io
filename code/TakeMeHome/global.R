## load necessary packages
if (!require(shinyjs)) install.packages("shinyjs"); library(shinyjs)
if (!require(shiny)) install.packages("shiny"); library(shiny)
if (!require(htmltools)) install.packages("htmltools"); library(htmltools)
if (!require(data.table)) install.packages("data.table"); library(data.table)
if (!require(plyr)) install.packages("plyr"); library(plyr)
if (!require(httr)) install.packages("httr"); library(httr)
if (!require(countrycode)) install.packages("countrycode"); library(countrycode)
if (!require(lubridate)) install.packages("lubridate"); library(lubridate)
if (library('V8',logical.return = TRUE)) require('V8'); library(V8)


## API keys
source(".keys.R")

## User timezone
getTimeZone <- '
$(document).ready(function() {
    var tz = jstz.determine();
    var tzone = document.getElementById("tzone");
    tzone.value = tz.name()
    //alert(tzone.value);
})
'

## Move focus to Go Button after date selected
goFocus <- "shinyjs.goFocus = function() {document.getElementById('goButton').focus();}"
## Move focus to 'HERE' box after page load
loadFocus <- "shinyjs.loadFocus = function() {document.getElementById('here.country').focus();}"
## selectize.js option list
selectize.options <- list(placeholder = 'select a state name',highlight = TRUE,closeAfterSelect = TRUE,selectOnTab = TRUE)

## return responses from SITA Airport FIDS API
sita.response <- GET(url = "https://airport.api.aero/airport", query = list(user_key = sita.key))
sita.content <- content(sita.response, as = "parsed", encoding = "Latin-1")
IATA.airports <- data.table(t(sapply(sita.content$airports, function(x) c(code = ifelse(is.null(x[[1]]),NA,x[[1]]), name = ifelse(is.null(x[[2]]),NA,x[[2]]), city = ifelse(is.null(x[[3]]),NA,x[[3]]), country = ifelse(is.null(x[[4]]),NA,x[[4]]), timezone = ifelse(is.null(x[[5]]),NA,x[[5]]), latitude = ifelse(is.null(x[[6]]),NA,x[[6]]), longitude = ifelse(is.null(x[[7]]),NA,x[[7]]), terminal = ifelse(is.null(x[[8]]),NA,x[[8]]), gate = ifelse(is.null(x[[9]]),NA,x[[9]])))))
IATA.airports <- IATA.airports[!is.na(name)]
setkey(IATA.airports,country,city,code)

## for collecting timings
qpx.response <- NULL

## take User Input and return responses from Google's QPX Express Airfare API
OptionsTable <- function(here,home,date,sale,timezone) {
  tmp <- list(
    request = list(
      passengers = list(
        adultCount = 1L
      ),
      slice = data.frame(
        list(
          origin = here,
          # origin = "YYC",
          destination = home,
          # destination = "LHR",
          date = date,
          # date = format(Sys.Date(), format = "%Y-%m-%d"),
          maxStops = 2L
        )
      ),
      solutions = 3L,
      saleCountry = sale,
      ticketingCountry = sale
    )
  )
  body <- jsonlite::toJSON(tmp, auto_unbox = TRUE, pretty = TRUE)
  url <- "https://www.googleapis.com/qpxExpress/v1/trips/search"
  qpxExpress.response <- POST(url = url, query = list(fields = "trips",key = qpx.key), body = body, encode = "json", content_type_json())
  qpx.response <<- qpxExpress.response
  qpx.content <<- content(qpxExpress.response, as = "parsed", encoding = "Latin-1")
  ifelse(length(qpx.content$trip$data) == 1L, options <- data.table(NULL), {#capture null return from Google
    ## rework response into lookup data.tables
    lookup.data <- qpx.content$trips$data
    airport <- data.table(t(sapply(lookup.data$airport, function(x) c(code = x[[2]], city = x[[3]], name = x[[4]]))))
    airport[, id := seq_len(.N)]
    setkey(airport,id)
    city <- data.table(t(sapply(lookup.data$city, function(x) c(code = x[[2]], name = x[[3]]))))
    city[, id := seq_len(.N)]
    setkey(city,id)
    aircraft <- data.table(t(sapply(lookup.data$aircraft, function(x) c(code = x[[2]], name = x[[3]]))))
    aircraft[, id := seq_len(.N)]
    setkey(aircraft,id)
    carrier <- data.table(t(sapply(lookup.data$carrier, function(x) c(code = x[[2]], name = x[[3]]))))
    carrier[, id := seq_len(.N)]
    setkey(carrier,id)
    tax <- data.table(t(sapply(lookup.data$tax, function(x) c(code = x[[2]], name = x[[3]]))))
    tax[, id := seq_len(.N)]
    setkey(tax,id)

    ## rework response into options data.table
    trip.options <- qpx.content$trips$tripOption
    options.dt <- data.table(sapply(trip.options, unlist, recursive = FALSE, use.names = TRUE), keep.rownames = FALSE)

    options <- data.table(ldply(options.dt[, .SD], function(x) data.table(parameter = names(unlist(x)), value = unlist(x, use.names = FALSE))))
    options[parameter == "", parameter := as.character(seq_len(.N)), by = .id][, id := seq_len(.N)][, pseq := seq_len(.N), by = .(.id,parameter)][,variable := paste(parameter,pseq,sep = ".")][is.na(variable), variable := parameter]
    setnames(options,".id", "option")
    setkey(options,id,parameter,pseq)
    setcolorder(options,c("id","option","parameter","pseq","variable","value"))

    ## add lookup values to options DT
    setkey(options,value)
    setkey(airport,code)
    # options[airport, lookup  := i.name, nomatch = 0]
    options[airport, lookup  := i.name]
    setkey(airport,id)
    setkey(city,code)
    # options[city, lookup  := i.name, nomatch = 0]
    options[city, lookup  := i.name]
    setkey(city,id)
    setkey(aircraft,code)
    # options[aircraft, lookup  := i.name, nomatch = 0]
    options[aircraft, lookup  := i.name]
    setkey(aircraft,id)
    setkey(carrier,code)
    # options[carrier, lookup  := i.name, nomatch = 0]
    options[carrier, lookup  := i.name]
    setkey(carrier,id)
    setkey(tax,code)
    # options[tax, lookup  := i.name, nomatch = 0]
    options[tax, lookup  := i.name]
    setkey(tax,id)
    options[is.na(lookup), lookup := value]
    setkey(options,id,parameter,pseq)

    ## reduce dataset to selected parameters and recast wide
    selected <- c("fare.origin","fare.destination","saleTotal","duration","segment.leg.origin","segment.leg.departureTime","segment.leg.destination","segment.leg.arrivalTime","segment.flight.carrier","segment.flight.number", "segment.leg.aircraft","segment.connectionDuration" )
    options <- options[parameter %in% selected, ]
    options[, c("parameter","pseq","value") := NULL]
    setnames(options, "lookup", "value")
    setcolorder(options,c("id","option","variable","value"))
    setkey(options,id,option,variable)
    })
  return(options)
}

## Take options and return routing table
RoutingTable <- function(options) {
  ifelse(length(options) == 0, routes <- data.table(NULL), {
  routes <- data.table(sapply(options[, unique(option)], function(x) {c(
    option.number = switch(x, V1 = "first", V2 = "second", V3 = "third"),
    cost = options[option == x & variable == "saleTotal.1", value],
    duration = round(options[option == x & variable == "duration.1", as.numeric(value)/60],1),
    origin = options[option == x & variable == "segment.leg.origin.1",value],
    departure.time = options[option == x & variable == "segment.leg.departureTime.1", value],
    route = ifelse(last(options[option == x & variable %like% "segment.leg.origin", variable]) == "segment.leg.origin.1", paste("direct to ",last(options[option == x & variable %like% "segment.leg.destination", value])), paste0("via ",options[option == x & variable == "segment.leg.origin.2", value],ifelse(is.na(options[option == "x" & variable > "segment.leg.origin.2", variable]),"The North Pole",paste0(" and ",options[option == x & variable == "segment.leg.origin.3", value],collapse = "")))),
    destination = last(options[option == x & variable %like% "segment.leg.destination", value]),
    arrival.time = last(options[option == x & variable %like% "segment.leg.arrivalTime", value]),
    first.carrier = head(options[option == x & variable == "segment.flight.carrier.1", value],n = 1),
    first.flightno = head(options[option == x & variable == "segment.flight.number.1", value],n = 1),
    connection.duration = round(options[option == x & variable %like% "segment.connectionDuration", (sum(as.numeric(value)))/60],1),
    carriers = paste(unique(options[option == x & variable %like% "segment.flight.carrier", value]), sep = " and ", collapse = " and ")
  )
  }),
  keep.rownames = TRUE)
  })
  return(routes)
}

## take routes and desccribe available flights
FlightOptions <- function(routes) {
  ifelse(length(routes) == 0, flights <- HTML("<div class = 'row' style = 'text-align:center;padding-top: 0px;'>
  <div class = 'span' style = 'color:red;margin: 1px;'>There are no results for one or both of the Airports chosen.  Please try a different selection.
                                            </div>
                                            </div>"), {
  flights <- lapply(routes[, -"rn", with = FALSE], function(x) {
    HTML(paste0(
      "Your ", x[1],
      " option costs ", paste0(gsub("\\d+(\\.\\d+)?","",x[2]),"", formatC(round(as.numeric(gsub("[A-Z]{3}","",x[2])),0),format="f",digits=0,big.mark=",")),
      " with a travel time of ", x[3]," hours. Airport layover times total ", x[11], " hours.<br><br>",
      "The flight leaves ", x[4]," at ", format(strptime(x[5],"%Y-%m-%dT%H:%M"), format = "%a. %b %e/%y %I:%M %p", usetz = FALSE),
      " and flies ", x[6], ", ",
      "arriving in ", x[7],
      " on ", format(strptime(x[8],"%Y-%m-%dT%H:%M"), format = "%a. %b %e/%y %I:%M %p", usetz = FALSE), ". <br><br>",
      "Origin departure is on ",x[9],", flight # ",x[10],".","<br><hr>"
    ))
  })
  })
  return(flights)
}
