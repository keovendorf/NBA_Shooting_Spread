#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(plotly)
library(tidyverse)
library(rsconnect)
library(readr)
library(ggplot2)
library(plyr)
library(dplyr)
library(jpeg)
library(grid)
library(RCurl)
library(shinyWidgets)

# import all NBA teams csv files into one dataframe
mydir = "NBA Teams 2017-2018"
myfiles = list.files(path = mydir, pattern = "*.csv", full.names = TRUE)
myfiles

# declare csv file
data_csv = ldply(myfiles, read_csv)

# upload image of court for ggplot
courtImg <- "http://robslink.com/SAS/democd54/nba_court_dimensions.jpg"
court <- rasterGrob(readJPEG(getURLContent(courtImg)),
                    width = unit(1, "npc"), height = unit(1, "npc"))

# ui for shiny dashboard
ui = fluidPage(
  
  # change the theme to be easier on the eyes
  theme = shinytheme('darkly'),
  
    # title of shiny app
    headerPanel("NBA 2017-2018 Season: Shooting Analysis"),
        
        sidebarPanel(
            
          # create a subset of the data that changes based on user input and narrows down the choices 
            selectizeGroupUI(
                id = "my-filters",
                inline = FALSE,
                params = list(
                    team_name = list(inputId = "team_name", title = "NBA Team", placeholder = 'Select NBA Team'),
                    name = list(inputId = "name", title = "Player", placeholder = 'Select a Player'),
                    shot_type = list(inputId = "shot_type", title = "2 PT or 3 PT", placeholder = 'Select Value'),
                    shot_made_flag = list(inputId = "shot_made_flag", title = "FGA / FG", placeholder = 'Select Between All Shot Attempts or Only Shots Made'),
                    action_type = list(inputId = "action_type", title = "Shot Type", placeholder = 'Select Shot Type'))
                ),
            status = "primary"
        ),
    
    mainPanel(
    
    # plot the court next to the user input
    plotOutput(outputId = "court_plot", width = '800px')
    )
)

# server side of the app that allows the user interactions to affect the dataframe
server <- function(input, output, session) {
    
  # allow the custom subset dataframe to be called
    res_mod <- callModule(
        module = selectizeGroupServer,
        id = "my-filters",
        data = data_csv,
        vars = c("team_name", "name", "shot_type", "shot_made_flag", "action_type")
        )
    

    # the code side of the
    output$court_plot <- renderPlot({
        data_i = res_mod()
        data_i %>% ggplot(aes(x, y)) +
            annotation_custom(court, -250, 250, -50, 420) +
            geom_point(color = data_i$shot_type) +
            xlim(-250, 250) +
            ylim(-50, 420)
        })

}

shinyApp(ui = ui, server = server)