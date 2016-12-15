## file ui.R

shinyUI(fluidPage(
  shinyjs::useShinyjs(),
  extendShinyjs(text = goFocus),
  extendShinyjs(text = loadFocus),
  # src script goes in www directory
  tags$head(tags$script(type="text/javascript", src = "jstz.min.js")),
  # HTML function is used to so special characters don't get escaped
  tags$head(tags$script(type="text/javascript",HTML(getTimeZone))),
  tags$head(tags$link(rel="stylesheet", type="text/css", href="app.css")),
  tags$head(tags$script(type="text/javascript",src = ".google-analytics.js")),
  tags$div(HTML(' <input type="text" id="tzone" name="tzone" style="display: none;"> ')),
  titlePanel(title="Take Me Home for the Holidays!",windowTitle="Take Me Home!"),
  fluidRow(
    column(12, "",
           fluidRow(id = "inputs",
                column(1, offset = 0),
                    column(3, offset = 0, id="here_box",
                    wellPanel(
                      "I'm leaving HERE:",
                      # selectInput("here.country", "Country", choices = c(Choose = '',unique(IATA.airports[, country])), selectize = TRUE),
                      # selectInput("here.city", "City", choices = list(), selectize = TRUE),
                      # selectInput("here.airport", "Airport", choices = list(), selectize = TRUE)
                      selectizeInput("here.country", "", choices = NULL, options = list(placeholder = "Country",highlight = TRUE,closeAfterSelect = TRUE,selectOnTab = TRUE)),
                      selectizeInput("here.city", "", choices = NULL, options = list(placeholder = "City",highlight = TRUE,closeAfterSelect = TRUE,selectOnTab = TRUE)),
                      selectizeInput("here.airport", "", choices = NULL, options = list(placeholder = "Airport",highlight = TRUE,closeAfterSelect = TRUE,selectOnTab = TRUE))
                      )
                    ),
                column(3, offset = 0, id="home_box",
                    wellPanel(
                      "for HOME:",
                      # selectInput("home.country", "Country", choices = c(Choose = '',unique(IATA.airports[, country])), selectize = TRUE),
                      # selectInput("home.city", "City", choices = list(), selectize = TRUE),
                      # selectInput("home.airport", "Airport", choices = list(), selectize = TRUE)
                      selectizeInput("home.country", "", choices = c(unique(IATA.airports[, country])), options = list(placeholder = "Country",highlight = TRUE,closeAfterSelect = TRUE,selectOnTab = TRUE)),
                      selectizeInput("home.city", "", choices = NULL, options = list(placeholder = "City",highlight = TRUE,closeAfterSelect = TRUE,selectOnTab = TRUE)),
                      selectizeInput("home.airport", "", choices = NULL, options = list(placeholder = "Airport",highlight = TRUE,closeAfterSelect = TRUE,selectOnTab = TRUE))
                      )
                    ),
                column(4, offset = 0, id="date_box",
                    wellPanel(
                      "on:",
                      dateInput("leaveDate", "", format = "D. M d, yyyy",min = Sys.Date(),value = Sys.Date() + 1, width = "100%"),
                      "Your current timezone: ",h5(textOutput("timezone")),
                      br(),
                      tags$div(class = "row",
                               tags$div(class = "span", actionButton("goButton", "Go!"),
                                        style = "margin: 1px;"),
                               style = "text-align:center;padding-top: 0px;")
                      )
                    ),
                column(1, offset = 0)
             ),
           fluidRow(
             column(12, "",
                    br(),
                    h4("Flight Options"),
                    tags$div(
                      tags$div(
                        uiOutput(outputId = "flights"),
                        style = "margin: 1px;"),
                      tags$div(
                        p("The flights quoted are based on one passenger flying coach, and having no more than two stops along the way.  There may be more options, but this app restricts the number to three.",
                          br(),
                          "All times are relative to the airports selected. Currency used is that of the departure country.",
                          br(),br(),
                          strong("PRIVACY:")," This app is not coded to store user session information.  However, it is possible that ShinyApps.io does collect and store your data.  Check their ",a(href = "https://www.rstudio.com/about/privacy-policy/","Privacy Policy")," to find out more.",
                          br(),br(),
                          "Although the data supplied by Google is believed to be accurate, do not rely on this application's representation of it when making real travel arrangements.  The author shall not be responsible for any losses or damages whatsoever however arising from your use of this application."),
                        style = "font-style:italic;padding:13px;margin-left:20px;margin-right:20px;border:1px solid blue;"),
                      tags$div(
                        h5("Help"),
                        helpText("A selection must be made in all of the dropdown boxes in order to get a result. Selecting a Country reduces the set of Cities to be displayed in the next dropdown, and similarly, the City chosen limits the Airport codes available. Typing direcly in the box is a fast way to search through the list.",
                                 br(),br(),
                                 "Once the six dropdowns are filled, select a departure date, press the GO button, and wait patiently for your results - it shouldn't be long!",
                                 br(),br()),
                        style = "text-align:left;margin-left:20px;margin-right:20px;padding-left:13px;padding-right:13px;")
                      )
                    )
           ),
           fluidRow(
             column(12, "",
                    hr(),
                    tags$div(
                      tags$div(p("Copyright",HTML("&copy;"),year(today())," Jonathan Hodge.  All rights reserved."),
                               style = "text-align:center")
                      )
                    )
             )
           ) #fluidRow
    ) #fluidPage
)
)
