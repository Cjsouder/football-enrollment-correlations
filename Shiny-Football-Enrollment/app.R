library(shiny)
library(tidyverse)
library(ggplot2)
library(shinythemes)
library(scales)

# Read in csv files with data

college_data <- read.csv("football_academic_data.csv")
fit_data <- read.csv("fit_data.csv")
reps_data <- read.csv("reps_data.csv")

#

conf_names <- c("All", "ACC", "Big 10", "Big 12", "Pac 12", "SEC")

#

pub_status <- c("All", "Private", "Public")

# Create the app's user interface

ui <- fluidPage(
  
  
  navbarPage(
    
    title = "Football & Enrollment Correlations",
    
    theme = shinytheme("united"),
    
    tabPanel(
      title = "Conference Plot",
      titlePanel("Plotted Correlations"),
      sidebarPanel(
        selectInput("conference", "Conference", conf_names),
        radioButtons("pubstat", "University Status", pub_status),
      ),
      mainPanel(plotOutput("plot", hover = hoverOpts(id = "plot_hover")), uiOutput("hover_info"))
    ),
    
    tabPanel(
      title = "Statistics",
      titlePanel("R Squared Test"),
      sidebarPanel(
        selectInput("conf", "Conference", sort(unique(college_data$conference))),
        uiOutput("second_selection"),
      ),
      mainPanel(plotOutput("stat_plot"),
                plotOutput("scatter_plot", hover = hoverOpts(id = "plot_scat")), uiOutput("hover_scatter"))
    ),
    
    tabPanel(
      title = "Conclusions",
      fluidRow(
        column(12,
               wellPanel(
                 htmlOutput("summary")
               ))
      )
    ),
    
    tabPanel(
      title = "About",
      fluidRow(
        column(12,
               wellPanel(
                 htmlOutput("about")
               ))
      )
    ) 
    )
  
    
  
)

# Code reactive elements of app

server <- function(input, output) {
  
    data_input <- reactive({ 
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
        
      ggplot(data_input(), aes(x = win_pct_diff, y = applcn_pct_chng)) +
                                  geom_point() + 
                                  geom_jitter(aes(color = conference)) +
                                  geom_smooth(method='lm') +
                                  labs(
                                    title = paste("University Applications vs. Football Team Success:", input$conference),
                                    subtitle = "Change in Applications per Change in Football Team Ranking Over the Previous Year",
                                    x = "Percent Change in University Football Games Won",
                                    y = "Percent Change in Applications to University",
                                    caption = "Showing data for Power 5 universities that did not change conferences between 2000 and 2012.\nFootball data at each datapoint corresponds to the season before the application year of that datapoint.",
                                    color = "Conference"
                                  )
      
    )
            
      output$hover_info <- renderUI({
        hover <- input$plot_hover
        point <- nearPoints(data_input(), hover, threshold = 5, maxpoints = 1, addDist = TRUE)
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
                        "left:", left_px - 200, "px; top:", top_px + 2, "px;")
        
        # actual tooltip created as wellPanel
        wellPanel(
          style = style,
          p(HTML(paste0("<b> College: </b>", point$instnm, "<br/>",
                        "<b> Application Year: </b>", point$year2, "<br/>",
                        "<b> Change in Wins: </b>", point$win_pct_diff, "%", "<br/>", 
                        "<b> Change in Applications: </b>", point$applcn_pct_chng, "%")))
        )
        
      })    
      
      
    output$second_selection <- renderUI({
      selectInput("college", "College", sort(unique(college_data[college_data$conference == input$conf,"instnm"])))
            
    })  
      
    rep_input <- reactive({
      
      # This tells Shiny that user input will be referenced within this reactive
      # element (Shiny throws a warning otherwise)
      
      req(input$college)
      
      reps2_data <- reps_data %>%
        filter(instnm == input$college)
      
    })
    
    quantile_input <- reactive({
      
      # This tells Shiny that user input will be referenced within this reactive
      # element (Shiny throws a warning otherwise)
      
      req(input$college)
      
      quant_data <- reps_data %>%
        filter(instnm == input$college) 
      
      quant_data <- quant_data %>%
        summarise(quants = list(enframe(quantile(quant_data$r_squared, probs=c(0.025,0.5,0.975))))) %>%
        unnest() %>%
        spread(key = name, value = value)
      
    })
    
    fitInput <- reactive({
      
      # This tells Shiny that user input will be referenced within this reactive
      # element (Shiny throws a warning otherwise)
      
      req(input$college)
      
      fit2_data <- fit_data %>%
        filter(instnm == input$college)
      
    })
    
    scatter_input <- reactive({
      
      # This tells Shiny that user input will be referenced within this reactive
      # element (Shiny throws a warning otherwise)
      
      req(input$college)
      
      scatter_data <- college_data %>%
        filter(instnm == input$college)

      
    })
    

    output$stat_plot <- renderPlot(
      
      ggplot(rep_input(), aes(x = r_squared)) +
        geom_density() + 
        geom_vline(data = quantile_input(), aes(xintercept= `50%`)) +
        geom_vline(data = quantile_input(), aes(xintercept= `2.5%`), linetype = "dashed") +
        geom_vline(data = quantile_input(), aes(xintercept= `97.5%`), linetype = "dashed") +
        geom_vline(data = fitInput(), aes(xintercept = r_squared, color = "red")) +
        guides(colour = FALSE) +
        labs(
          title = expression(paste("R"^2, " estimate of Percent Change in Applications per Percent Change in Football Wins")),
          subtitle = paste("Density plot of 1000 bootstrapped samples:", input$college),
          x = bquote("R"^2),
          y = "Density",
          caption = "Dashed lines show boundary of 95% confidence interval of bootstrapped samples. Solid black line shows median. \nRed line shows observed R-squared value of non-bootstrapped data."
        )
      
    )
    
    output$scatter_plot <- renderPlot(
      ggplot(scatter_input(), aes(x = win_pct_diff, y = applcn_pct_chng)) +
        geom_point() + 
        geom_jitter(aes(color = conference)) +
        geom_smooth(method='lm') +
        guides(colour = FALSE) +
        labs(
          title = paste("University Applications vs. Football Team Success:", input$college),
          subtitle = "Change in Applications per Change in Football Team Ranking Over the Previous Year",
          x = "Percent Change in University Football Games Won",
          y = "Percent Change in Applications to University",
          caption = "Football data at each datapoint corresponds to the season before the application year of that datapoint",
          color = "Conference"
        )
    )
    
    output$hover_scatter <- renderUI({
      hover <- input$plot_scat
      point <- nearPoints(data_input(), hover, threshold = 5, maxpoints = 1, addDist = TRUE)
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
                      "left:", left_px - 200, "px; top:", top_px + 280, "px;")
      
      # actual tooltip created as wellPanel
      wellPanel(
        style = style,
        p(HTML(paste0("<b> Application Year: </b>", point$year2, "<br/>",
                      "<b> Change in Wins: </b>", point$win_pct_diff, "%", "<br/>", 
                      "<b> Change in Applications: </b>", point$applcn_pct_chng, "%")))
      )
      
    }) 
    
    # Create the About page. This code chunk adds all the text that will
    # appear on the page.
    
     output$about <- renderUI({
       HTML(paste(
         h2("About This Project"),
         br(),
         div(
           "This project began after an informal observation of enrollment changes at Kansas State University (K-State), the local university in my hometown. Between 2010 and 2012 K-State's football team had phenomenal success - the team was ranked #1 in the nation for a time. During this same time period the university experienced record student enrollment rates. Meanwhile, K-State's in-state rival, the University of Kansas (KU), experienced some of the worst football seasons in university history during this timeframe. The university also suffered declining enrollment. Only a few years later, though, KSU began experiencing declining enrollment as its football team regressed. This made me wonder: do most large universities experience surges in enrollment when their football teams perform well and declines in enrollment when their teams struggle?"
         ),
         br(),
         div(
           "To answer my question I decided to analyze university application data and football team records (wins vs losses) for a period of approximately 10 years. I looked only at Power 5 universities - that is, universities whose football teams are members of either the Big 10, Big 12, SEC, ACC, or Pac 12 athletic conferences. I reason that these schools recruit students nationally rather than regionally, so their student bodies are more likely to change based on the performance of their football teams than are the student bodies of smaller, regional colleges. I chose not to look at data for universities that changed athletic conferences during the time period of interest since that change could effect enrollment changes."
         ),
         br(),
         h4("Reasoning Behind Choice of Models"),
         div("Coming Soon!"),
         br(),
         h4("External Links"),
         div(
           "I gathered enrollment data from the", a("National Center for Education Statistics", href = "https://nces.ed.gov/ipeds/datacenter/InstitutionByName.aspx"), "and football data from the", a("NCAA." , href = "http://web1.ncaa.org/stats/StatsSrv/rankings?doWhat=archive&sportCode=MFB")
         ),
         br(),
         div(
           "The Github Repo for this project can be found", a("here.", href = "https://github.com/rbrown146/football-enrollment-correlations")
         ),
         br(),
         div(img(src = "KU_medium.jpg")),
         br(),
         h4("About the Author"),
         p(
           "Rick Brown is a sophomore studying economics at Harvard College. He is interested in the intersection of economics, sociology, and government as well as all things related to Kansas."
         ),
         p("Rick can be contacted at rbrown@college.harvard.edu")
       ))
     })
    
     # Create the Conclusion page. This code chunk adds all the text that will
     # appear on the page.
     
     output$summary <- renderUI({
       HTML(paste(
         h2("Takeaways from This Project"),
         br(),
         h4("Correlations Seem Weak"),
         div(
           "The slope of the regression line relating change in football wins to change in college enrollment is nearly flat for the data as a whole. When results are narrowed by conference the slope of the resulting regression line is marginally steeper for some conferences (Big 10, Big 12, Pac 12) but has a slight negative slope for others (ACC)."
         ),
         br(),
         div(
           "The data becomes more interesting when analyzed by college. Some colleges seem to be affected by outliers - for example, Georgia Tech's r-squared estimate of percent change in enrollment per percent change in football wins appears to be roughly .125, a rather low value. However, a 95% confidence interval created from bootstrapping 1000 samples from the dataset contains r-squared values up to approximately r-squared = .625. Plotting a scatterplot of datapoints makes it evident that there is one point on the far left that is skewing results. Some universities have r-squared values that are relatively high. Purdue, for instance, has an r-squared value approximately equal to .45, while Washington State has a r-squared value of .625. Most universities, though, have low r-squared values and scatterplots with points that are fairly spread out. Some colleges, like Iowa State, have regression lines with negative slopes as a result."
         ),
         br(),
         h4("Possible Problems with Setup"),
         div(
           "Many assumptions were made when setting up this model that may have influenced outcomes. When plotting datapoints for each application year the change in applications for that year was matched with the change in football wins that occured in the previous year. For example, a datapoint representing the change in applications between 2010 and 2011 would be matched with football data representing the change in wins from 2009 to 2010. The reasoning for modeling the data in this manner was that applicants would apply before seeing results of the current season. However, both application and football seasons begin at roughly the end of August, so it is possible that applicants were influenced by more recent data when making application decisions. Also, perhaps the change in football wins is not a valid measure of the quality of a football team. Maybe there is a higher correlation between college applications and football teams entering or moving up in the AP list of top 25 programs."
         )
       ))
    })
}

# Run the application 

shinyApp(ui = ui, server = server)
