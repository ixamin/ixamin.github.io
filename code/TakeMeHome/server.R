## file server.R
shinyServer(function(input,output,session) {
  options(shiny.trace=TRUE,shiny.testmode=TRUE)
  updateSelectizeInput(session, 'here.country', choices = c(unique(IATA.airports[, country])), server = TRUE)
  updateSelectizeInput(session, 'here.city', choices = list(), server = TRUE)
  updateSelectizeInput(session, 'here.airport', choices = list(), server = TRUE)
  updateSelectizeInput(session, 'home.country', choices = c(unique(IATA.airports[, country])), server = TRUE)
  updateSelectizeInput(session, 'home.city', choices = list(), server = TRUE)
  updateSelectizeInput(session, 'home.airport', choices = list(), server = TRUE)

  observe({
    here.country <<- input$here.country
    here.city.choices <- unique(IATA.airports[country == here.country, city])
    updateSelectInput(session, "here.city", choices = c(Choose = '',here.city.choices))
  })
  observe({
    here.city <- input$here.city
    here.airport.choices <- unique(IATA.airports[city == here.city, code])
    updateSelectInput(session, "here.airport", choices = c(Choose = '',here.airport.choices))
  })
  observe({
    home.country <- input$home.country
    home.city.choices <- unique(IATA.airports[country == home.country, city])
    updateSelectInput(session, "home.city", choices = c(Choose = '',home.city.choices))
  })
  observe({
    home.city <- input$home.city
    home.airport.choices <- unique(IATA.airports[city == home.city, code])
    updateSelectInput(session, "home.airport", choices = c(Choose = '',home.airport.choices))
  })

  newLoad <- 0
  observeEvent(input$leaveDate, {
    toggle(selector = "div.datepicker",anim = TRUE,animType = "fade")
    # js$goFocus()
    if(newLoad == 0) {js$loadFocus(); newLoad <<- newLoad + 1} else {js$goFocus()}
    })

  options.data <- eventReactive(input$goButton, {
    validate(
      need(input$here.airport, "Please complete all 'HERE:' inputs"),
      need(input$home.airport, "Please complete all 'HOME:' inputs")
    )
    here.airport <- input$here.airport
    home.airport <- input$home.airport
    client.tzone <<- input$tzone
    depart.date <- input$leaveDate
    sale.country <- countrycode(here.country, "country.name", "iso2c")
    options <- OptionsTable(
      here = here.airport,home = home.airport,date = depart.date,sale = sale.country)
    routes <- RoutingTable(options)
    FlightOptions(routes)
  })
  output$flights <- renderUI({HTML(unlist(options.data()))})

  client.timezone <- reactive({input$tzone})
  output$timezone <- renderText(client.timezone())
  })
