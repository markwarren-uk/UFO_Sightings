#THIS IS THE CODE FROM THE DATACAMP COURSE - USE THIS AS BASIS FOR DESIGNING UI ETC
#WILL NEED TO DOWNLOAD AND EDIT DATA BEFORE APP WRITTEN

# devtools::install_github("datadotworld/data.world-r", build_vignettes = TRUE)
# 
# saved_cfg <- data.world::save_config("YOUR<>API<>TOKEN")
# data.world::set_config(saved_cfg)

# vignette("quickstart", package = "data.world")
# ?data.world

library(data.world)
library(tidyverse)
# Aliens <- query(
#     qry_sql("select * from `national-ufo-reporting-center-reports`"),
#     dataset = "drwaz/nuforc_events")

Aliens <- read_csv("https://query.data.world/s/kupja3avi2x7xdswrxssepmy7pdl7m")
glimpse(Aliens)
length(unique(Aliens$State))
#71 states - must be USA and Canada
with(Aliens, table(State))

sort(unique(Aliens$Year))
table(Aliens$Year)

#US state codes
USA <- c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", "FL", "GA", "HI", "ID", "IL", 
  "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", 
  "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", 
  "TN", "TX", "UT", "VA", "VT", "WA", "WV", "WI", "WY")

Aliens <- Aliens %>% 
    filter(Year >= 1960) %>% 
    filter(State %in% USA)


# #CHOOSING STATE AND DATE RANGE
# ui <- fluidPage(
#     # CODE BELOW: Add a title
#     titlePanel("Alien Sightings!"),  
#     sidebarLayout(
#         sidebarPanel(
#             # CODE BELOW: One input to select a U.S. state
#             # And one input to select a range of dates
#             selectInput("state",
#                         "label",
#                         choices = unique(Aliens$State)),
#             dateRangeInput("date", "Choose date",
#                            start = min(Aliens$Event_Date),
#                            end = max(Aliens$Event_Date))
#         ),
#         mainPanel()
#     )
# )
# 
# server <- function(input, output) {
#     
# }
# 
# shinyApp(ui, server)


#CHOOSING SHAPE AND ADDING TABLE
ui <- fluidPage(
    titlePanel("Aliens! Aliens!"),
    sidebarLayout(
        sidebarPanel(
            selectInput("state", "Choose a U.S. state:", choices = sort(unique(Aliens$State))),
            dateRangeInput("date", "Choose date",
                           start = min(Aliens$Event_Date),
                           end = max(Aliens$Event_Date))
        ),
        
        #Create a tab layout for the dashboard
        mainPanel(tabsetPanel(
        tabPanel('UFO sightings plot',plotOutput("shapes")),
        tabPanel('UFO sightings table', tableOutput("duration_table"))
        )
    )
    )
)
    


server <- function(input, output) {
    # CODE BELOW: Create a plot output name 'shapes', of sightings by shape,
    # For the selected inputs
    output$shapes <- renderPlot({
        Aliens %>%
            filter(State == input$state,
                   Event_Date >= input$date[1],
                   Event_Date <= input$date[2]) %>%
            ggplot(aes(Shape)) +
            geom_bar() +
            labs(x = "Shape", y = "No. Sighted") +
            theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
    })
    # CODE BELOW: Create a table output named 'duration_table', by shape,
    # of # sighted, plus mean, median, max, and min time of day of events
    # for the selected inputs
    output$duration_table <- renderTable({
        Aliens %>%
            filter(
                State == input$state,
                Event_Date >= input$date[1],
                Event_Date <= input$date[2]
            ) %>%
            group_by(Shape) %>%
            summarize(
                No_sighted = n(),
                Avg_ToD = mean(Hour, na.rm=T),
                Median_ToD = median(Hour, na.rm=T),
                Min_ToD = min(Hour, na.rm=T),
                Max_ToD = max(Hour, na.rm=T)
            )
    })
}


shinyApp(ui, server)

# #TAB LAYOUT
# ui <- fluidPage(
#     titlePanel("UFO Sightings"),
#     sidebarPanel(
#         selectInput("state", "Choose a U.S. state:", choices = unique(usa_ufo_sightings$state)),
#         dateRangeInput("dates", "Choose a date range:",
#                        start = "1920-01-01",
#                        end = "1950-01-01"
#         )
#     ),
#     # MODIFY CODE BELOW: Create a tab layout for the dashboard
#     mainPanel(tabsetPanel(
#         tabPanel('UFO sightings plot',plotOutput("shapes")),
#         tabPanel('UFO sightings table', tableOutput("duration_table"))
#     )    
#     )
# )
# 
# server <- function(input, output) {
#     output$shapes <- renderPlot({
#         usa_ufo_sightings %>%
#             filter(
#                 state == input$state,
#                 date_sighted >= input$dates[1],
#                 date_sighted <= input$dates[2]
#             ) %>%
#             ggplot(aes(shape)) +
#             geom_bar() +
#             labs(
#                 x = "Shape",
#                 y = "# Sighted"
#             )
#     })
#     
#     output$duration_table <- renderTable({
#         usa_ufo_sightings %>%
#             filter(
#                 state == input$state,
#                 date_sighted >= input$dates[1],
#                 date_sighted <= input$dates[2]
#             ) %>%
#             group_by(shape) %>%
#             summarize(
#                 nb_sighted = n(),
#                 avg_duration_min = mean(duration_sec) / 60,
#                 median_duration_min = median(duration_sec) / 60,
#                 min_duration_min = min(duration_sec) / 60,
#                 max_duration_min = max(duration_sec) / 60
#             )
#     })
# }
# 
# shinyApp(ui, server)


