#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#



# LOOK AT THIS: https://shiny.rstudio.com/gallery/navbar-example.html
# https://shiny.rstudio.com/gallery/plot-interaction-basic.html
# https://shiny.rstudio.com/gallery/plot-interaction-advanced.html

library(shiny)
library(tidyverse)
library(ggplot2)
library(scales)

college_data <- read.csv("football_academic_data.csv")

conf_names <- c("All", "ACC", "Big 10", "Big 12", "Pac 12", "SEC")

pub_status <- c("All", "Private", "Public")



# Define UI for application 
ui <- fluidPage(
  
  navbarPage("Football & Enrollment Correlations",
             tabPanel(
               "About",
               titlePanel("About This Project"),
               br(),
               div(
                 "This project began after an informal observation of enrollment changes at Kansas State University (K-State), the local university in my hometown. Between 2010 and 2012 K-State's football team had phenomenal success - the team was ranked #1 in the nation for a time. During this same time period the university experienced record student enrollment rates. Meanwhile, K-State's in-state rival, the University of Kansas (KU), experienced some of the worst football seasons in university history during this timeframe. The university also suffered declining enrollment. Only a few years later, though, KSU began experiencing declining enrollment as its football team regressed. This made me wonder: do most large universities experience surges in enrollment when their football teams perform well and declines in enrollment when their teams struggle?"
                 ),
               br(),
               div(
                 "To answer my question I decided to analyze university application data and football team records (wins vs losses) for a period of approximately 10 years. I looked only at Power 5 universities - that is, universities whose football teams are members of either the Big 10, Big 12, SEC, ACC, or Pac 12 athletic conferences. I reason that these schools recruit students nationally rather than regionally, so their student bodies are more likely to change based on the performance of their football teams than are the student bodies of smaller, regional colleges. I chose not to look at data for universities that changed athletic conferences during the time period of interest since that change could effect enrollment changes. I gathered enrollment data from the National Center for Education Statistics and football data from the NCAA."
                 ),
               br(),
               div(img(src="KU_medium.jpg")),
               br()
             ), 
    tabPanel(
      "Interactive Plot",
      titlePanel("Plotted Correlations"),
      

      sidebarPanel(
        selectInput("conference", "Conference", conf_names),
        radioButtons("pubstat", "University Status", pub_status),
      ),
      
      mainPanel(plotOutput("plot", hover = hoverOpts(id = "plot_hover")), uiOutput("hover_info"))
    )
    )
  
    
  
)

# Define server logic required to draw output
server <- function(input, output) {
  
    dataInput <- reactive({ 
      switch(input$pubstat,
             "Public" = filter_data <- college_data %>%
               filter(pub_status == "Public"),
             "Private" = filter_data <- college_data %>%
               filter(pub_status == "Private"),
             "All" = filter_data <- college_data
               )
      
      switch(input$conference,
             "All" = filter_data <- filter_data, 
             "ACC" = filter_data <- filter_data %>%
               filter(conference == "ACC"),
             "Big 10" = filter_data <- filter_data %>%
               filter(conference == "Big 10"),
             "Big 12" = filter_data <- filter_data %>%
               filter(conference == "Big 12"),
             "Pac 12" = filter_data <- filter_data %>%
               filter(conference == "Pac 12"),
             "SEC" = filter_data <- filter_data %>%
               filter(conference == "SEC"))
      
    })   
      output$plot <- renderPlot(
        
      ggplot(dataInput(), aes(x = win_pct_diff, y = applcn_pct_chng)) +
                                  geom_point() + 
                                  geom_jitter(aes(color = conference)) +
                                  geom_smooth(method='lm') +
                                  scale_y_continuous(labels = percent_format(scale = 1)) +
                                  scale_x_continuous(labels = percent_format(scale = 1)) +
                                  labs(
                                    title = "University Applications vs. Football Team Success",
                                    subtitle = "Change in Applications per Change in Football Team Ranking Over the Previous Year",
                                    x = "Percent Change in University Football Games Won",
                                    y = "Percent Change in Applications to University",
                                    caption = "Showing data for Power 5 universities that did not change conferences between 2000 and 2012",
                                    color = "Conference"
                                  )
      
    )
            
      output$hover_info <- renderUI({
        hover <- input$plot_hover
        point <- nearPoints(dataInput(), hover, threshold = 5, maxpoints = 1, addDist = TRUE)
        if(nrow(point) == 0){return(NULL)} 
        
        # calculate point position INSIDE the image as percent of total dimensions
        # from left (horizontal) and from top (vertical)
        
        left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
        top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
        
        # calculate distance from left and bottom side of the picture in pixels
        left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
        top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
        
        # create style property fot tooltip
        # background color is set so tooltip is a bit transparent
        # z-index is set so we are sure are tooltip will be on top
        
        style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                        "left:", left_px + 2, "px; top:", top_px + 2, "px;")
        
        # actual tooltip created as wellPanel
        wellPanel(
          style = style,
          p(HTML(paste0("<b> College: </b>", point$instnm, "<br/>",
                        "<b> Application Year: </b>", point$year2, "<br/>",
                        "<b> Change in Wins: </b>", point$win_pct_diff, "%", "<br/>", 
                        "<b> Change in Applications: </b>", point$applcn_pct_chng, "%")))
        )
        
        
        
      })                       
   
    
}

# Run the application 
shinyApp(ui = ui, server = server)
