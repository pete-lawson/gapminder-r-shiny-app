#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyWidgets)
library(ggplot2)
library(gapminder)
library(dplyr)
library(plotly)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("GapMinder Visualization"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            
            shinyWidgets::sliderTextInput(inputId = "year", 
                                          label = "Year:", 
                                          choices = unique(gapminder$year)),
            
            checkboxGroupInput(inputId = "continent", 
                               label = "Continent:", 
                               choices = unique(gapminder$continent), 
                               selected = unique(gapminder$continent)[1])
        ),

        # Show a plot of the generated distribution
        mainPanel(
            HTML("<p><b>R Shiny web application for replicating Hans Rosling's bubble-plot software presented at a <a href='https://www.gapminder.org/videos/ted-us-state-department/'>TED Talk given to the US State Department in 2009</a></b></p>"),
            p("Gapminder identifies systematic misconceptions about important global trends and proportions and uses reliable data to develop easy to understand teaching materials to rid people of their misconceptions. Gapminder is an independent Swedish foundation with no political, religious, or economic affiliations."),
           plotlyOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$distPlot <- renderPlotly({
        
        # Filter to year of interest
        gapminder_single_year <- filter(gapminder, 
                                        (year==input$year & continent %in% input$continent))
        
        # Ensure colors are consistent
         
        cont_colors <- rainbow(length(unique(gapminder$continent)))
        names(cont_colors) <- unique(gapminder$continent)
        
        # generate bins based on input$bins from ui.R
        ggplot(data = gapminder_single_year, aes(x = gdpPercap, 
                                                 y = lifeExp,
                                                 text = paste("Country:", country),
                                                 size = pop, 
                                                 fill = continent)) + 
            geom_point(alpha=0.5, shape=21, color="black") +
            xlim(0, max(gapminder$gdpPercap)) +
            ylim(min(gapminder$lifeExp), max(gapminder$lifeExp)) +
            scale_size(range = c(.1, 20), name="Population (M)", guide = "none") +
            scale_fill_manual("Legend", values = cont_colors);
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
