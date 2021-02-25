#THIS SI THE CODE FROM THE DATACAMP COURSE - USE THIS AS BASISI FOR DESIGNING UI ETC
#WILL NEED TO DOWNLOAD AND EDIT DATA BEFORE APP WRITTEN

ui <- fluidPage(
    # CODE BELOW: Add a title
    titlePanel("UFO Sightings"),  
    sidebarLayout(
        sidebarPanel(
            # CODE BELOW: One input to select a U.S. state
            # And one input to select a range of dates
            selectInput("state",
                        "label",
                        choices = unique(usa_ufo_sightings$state)),
            dateRangeInput("date", "Choose date",
                           start = min(usa_ufo_sightings$date_sighted),
                           end = max(usa_ufo_sightings$date_sighted))
        ),
        mainPanel()
    )
)

server <- function(input, output) {
    
}

shinyApp(ui, server)