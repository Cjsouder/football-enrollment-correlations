library(shiny)
library(tidyverse)
library(ggplot2)
library(shinythemes)
library(scales)
library(vembedr)
library(htmltools)

# Read in csv files with data

college_data <- read.csv("football_academic_data.csv")
fit_data <- read.csv("fit_data.csv")
reps_data <- read.csv("reps_data.csv")

# Create a list with conference names. The items in this list will be displayed
# as options in a dropdown list.

conf_names <- c("All", "ACC", "Big 10", "Big 12", "Pac 12", "SEC")

# Create a list with options relating to a university's public/private status.
# The items in this list will be used as radio button options.

pub_status <- c("All", "Private", "Public")

# Create the app's user interface, placing a navbar at the top of the app

ui <- navbarPage(
  
  # Title the navbar
  
  title = "Football Wins and College Applications",
  
  # Apply a premade Shiny theme to the app
  
  theme = shinytheme("united"),
  
  # Add first element to the navbar and code the page to which it links. This
  # page, "Conference Plot", is coded to contain a dropdown selector and radio
  # buttons beside a main plot. The plot reacts when hovered over.
  
  tabPanel(
    title = "Conference Plot",
    titlePanel("Plotted Correlations"),
    br(),
    sidebarPanel(
      selectInput("conference", "Conference", conf_names),
      radioButtons("pubstat", "University Status", pub_status),
    ),
    mainPanel(plotOutput("plot", hover = hoverOpts(id = "plot_hover")), uiOutput("hover_info"))
  ),
  
  # Add second element to the navbar and code the page to which it links. This
  # page, "Statistics", contains two dropdown selectors and two plots, the
  # second of which reacts when hovered over.
  
  tabPanel(
    title = "Statistics",
    titlePanel("R Squared Test"),
    br(),
    sidebarPanel(
      selectInput("conf", "Conference", sort(unique(
        college_data$conference
      ))),
      uiOutput("second_selection"),
    ),
    mainPanel(
      plotOutput("stat_plot"),
      plotOutput("scatter_plot", hover = hoverOpts(id = "plot_scat")),
      uiOutput("hover_scatter")
    )
  ),
  
  # Add third element to the navbar and code the page to which it links. This
  # page, "Conclusions, contains only text.
  
  tabPanel(title = "Conclusions",
           fluidRow(column(
             12, wellPanel(htmlOutput("summary"))
           ))),
  
  # Add final element to the navbar and code the page to which it links. This
  # page, "About", contains only text.
  
  tabPanel(title = "About",
           fluidRow(column(
             12, wellPanel(htmlOutput("about"))
           )))
)


# Code reactive elements of app

server <- function(input, output) {
  
    # This code processess the inputs selected by the user on the Conference
    # Plot page
  
    data_input <- reactive({
      
      # Filter the data displayed in the scatterplot depending on which radio
      # button is selected by the user. These options relate to the
      # public/private status of each university.
      
      switch(input$pubstat,
             "Public" = filter_data <- college_data %>%
               filter(pub_status == "Public"),
             "Private" = filter_data <- college_data %>%
               filter(pub_status == "Private"),
             "All" = filter_data <- college_data
               )
      
      # Filter the data displayed in the scatterplot depending on which option
      # is selected from the dropdown list. These options dictate which
      # conference to plot.
      
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
               filter(conference == "SEC")
             )
    })
    
    # Create vectors assigning colors to each college's scatterpoint. I think
    # this makes the scatterplot more aesthetically pleasing. (Before I did this
    # R assigned Oklahoma a sky blue point. That didn't seem right!) I assign
    # colors most similar to each univerity's actual colors except in instances
    # in which multiple universities have similar colors, in which case I assign
    # random colors to some colleges. I use light colors so that they display
    # well.
    
    conf_colors <-
      c(
        "ACC" = "lightskyblue",
        "Big 10" = "royalblue",
        "Big 12" = "red",
        "Pac 12" = "gray",
        "SEC" = "gold"
      )
    acc_colors <-
      c(
        "Clemson" = "orange",
        "Duke" = "blue",
        "Florida State" = "firebrick",
        "Georgia Tech" = "gold",
        "Maryland" = "black",
        "North Carolina" = "deepskyblue",
        "North Carolina State" = "red",
        "Virginia" = "gray",
        "Wake Forest" = "lightgoldenrodyellow"
      )
    big_10_colors <-
      c(
        "Illinois" = "darkorange",
        "Indiana" = "gray",
        "Iowa" = "gold",
        "Michigan" = "blue",
        "Michigan State" = "limegreen",
        "Minnesota" = "black",
        "Northwestern" = "purple",
        "Ohio State" = "firebrick",
        "Penn State" = "lightskyblue",
        "Purdue" = "lightgoldenrodyellow",
        "Wisconsin" = "red"
      )
    big_12_colors <-
      c(
        "Baylor" = "limegreen",
        "Iowa State" = "gold",
        "Kansas" = "blue",
        "Kansas State" = "purple",
        "Oklahoma" = "firebrick",
        "Oklahoma State" = "black",
        "Texas" = "darkorange",
        "Texas Tech" = "red"
      )
    pac_12_colors <-
      c(
        "Arizona" = "gray",
        "Arizona State" = "gold",
        "California" = "blue",
        "Oregon" = "limegreen",
        "Oregon State" = "darkorange",
        "Southern California" = "red",
        "Stanford" = "firebrick",
        "UCLA" = "lightskyblue",
        "Washington" = "purple",
        "Washington State" = "black"
      )
    sec_colors <-
      c(
        "Alabama" = "firebrick",
        "Arkansas" = "pink",
        "Auburn" = "tan",
        "Florida" = "blue",
        "Georgia" = "red",
        "Kentucky" = "deepskyblue",
        "LSU" = "purple",
        "Mississippi" = "limegreen",
        "Mississippi State" = "Gray",
        "South Carolina" = "black",
        "Tennessee" = "orange",
        "Vanderbilt" = "lightgoldenrodyellow"
      )
    
    # Code a scatterplot on the Conference Plot page according to the inputs
    # selected by the user (processed and stored under the name "data_input").
    # Percentage change in football games won is plotted on the x axis and
    # percentage change in applications is plotted on the y axis. I use
    # geom_jitter to show points that otherwise would be hidden beneath others,
    # and I color points by conference (if the user selects "All") or college
    # name if the user chooses a particular conference. I assign college colors
    # based on values specified in the vectors above. I set a dark background
    # theme so that the datapoints stand out. Finally, I plot a regression line
    # to show the general relationship of the data.
    
    output$plot <- renderPlot(
      ggplot(data_input(), aes(x = win_pct_diff, y = applcn_pct_chng)) +
        geom_jitter(aes_string(color = ifelse(input$conference == "All", "conference", "instnm"))) +
        geom_smooth(method = 'lm', se = FALSE) +
        labs(
          title = paste("University Applications vs. Football Team Success:", input$conference),
          subtitle = "Change in Applications per Change in Football Team Ranking Over the Previous Year",
          x = "Percent Change in University Football Games Won",
          y = "Percent Change in Applications to University",
          caption = "Showing data for Power 5 universities that did not change conferences between 2000 and 2012.\nFootball data at each datapoint corresponds to the season before the application year of that datapoint.",
          color = ifelse(input$conference == "All", "Conference", "College")
        ) +
        theme_dark() +
        theme(plot.title = element_text(size = 16)) +
        scale_color_manual(values = if (input$conference == "All") {
          conf_colors
        } else if (input$conference == "ACC") {
          acc_colors
        } else if (input$conference == "Big 10") {
          big_10_colors
        } else if (input$conference == "Big 12") {
          big_12_colors
        } else if (input$conference == "Pac 12") {
          pac_12_colors
        } else {
          sec_colors
        })
    )
       
    # Display a window with summary information whenever a user hovers over the
    # scatterplot on the Conference Plot page
           
    output$hover_info <- renderUI({
        
      # Create new variables "hover" and "point" that tell R whether the cursor
      # is hovering over the data and how far the cursor is from the data points.
        
      hover <- input$plot_hover
      point <- nearPoints(data_input(), hover, threshold = 5, maxpoints = 1, addDist = TRUE)
        
      # This tells R to only display a window if the cursor is near a datapoint
        
      if(nrow(point) == 0){return(NULL)} 
        
      # Calculate the percent location of the cursor from the left and top sides
      # of the window
        
      left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
      top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
        
      # Calculate the pixel distance of the cursor from the left and bottom
      # sides of the window
        
      left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
      top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
        
      # Specify the styling and position of the panel that will appear when a
      # user hovers over a datapoint
        
      style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                      "left:", left_px - 200, "px; top:", top_px + 2, "px;")
        
      # Code what information to show on the the panel that will appear when a
      # user hovers over a datapoint
        
      wellPanel(
        style = style,
        p(HTML(paste0("<b> College: </b>", point$instnm, "<br/>",
                      "<b> Application Year: </b>", point$year2, "<br/>",
                      "<b> Change in Wins: </b>", point$win_pct_diff, "%", "<br/>", 
                      "<b> Change in Applications: </b>", point$applcn_pct_chng, "%")))
      )
    })    
      
    # Display a second drop down list containing only the names of the colleges
    # that are part of the conference selected by the user on the Statistics
    # page of the app. Only results for the college selected from this dropdown
    # list will be used to generate the graphs on the Statistics page of the app
    
    output$second_selection <- renderUI({
      selectInput("college", "College", sort(unique(college_data[college_data$conference == input$conf, "instnm"])))
    })  
    
    # This function processes the data used to generate the density plot on the
    # Statistics page of the app such that only datapoints corresponding to the
    # college chosen by the user will be included in the plot
    
    rep_input <- reactive({
      
      # This tells Shiny that user input will be referenced within this reactive
      # element (Shiny throws a warning otherwise)
      
      req(input$college)
      
      # Filter the data such that only results for the college selected by the user are shown
      
      reps2_data <- reps_data %>%
        filter(instnm == input$college)
      
    })
    
    # Limit the data of bootstrapped samples to only results corresponding to
    # the college selected by the user on the Statistics page of the app.
    # Calculate the median and boundaries of the 95% confidence interval of
    # r-squared values for this data, and store these values. They will later be
    # used to plot lines on the density plot.
    
    quantile_input <- reactive({
      
      # This tells Shiny that user input will be referenced within this reactive
      # element (Shiny throws a warning otherwise)
      
      req(input$college)
      
      # Filter the data such that only results for the college selected by the user remain
      
      quant_data <- reps_data %>%
        filter(instnm == input$college) 
      
      # Determine the median and 95% confidence interval of the r-squared data
      # generated from bootstrapping samples from the data of the college
      # selected by the user. The result is reformatted so that data is easy to
      # pull. This data will be used to plot lines denoting the median and
      # boundaries of the confidence interval on the density graph
      
      quant_data <- quant_data %>%
        summarise(quants = list(enframe(quantile(quant_data$r_squared, probs=c(0.025,0.5,0.975))))) %>%
        unnest(cols = c(quants)) %>%
        spread(key = name, value = value)
      
    })
    
    # From the dataset of r-squared values calculated without bootstrapping
    # limit results to only the college chosen by the user. This data will later
    # be used to plot a line at that r-squared value on the density plot.
    
    fit_input <- reactive({
      
      # This tells Shiny that user input will be referenced within this reactive
      # element (Shiny throws a warning otherwise)
      
      req(input$college)
      
      # Filter the data such that only the result for the college selected by the user is shown
      
      fit2_data <- fit_data %>%
        filter(instnm == input$college)
      
    })
    
    # This function processes the data used to generate the scatter plot on the
    # Statistics page of the app such that only datapoints corresponding to the
    # college chosen by the user will be plotted
    
    scatter_input <- reactive({
      
      # This tells Shiny that user input will be referenced within this reactive
      # element (Shiny throws a warning otherwise)
      
      req(input$college)
      
      # Filter the data such that only results for the college selected by the user are shown
      
      scatter_data <- college_data %>%
        filter(instnm == input$college)
      
    })
    
    # Create a density plot showing the distribution of the r-squared values of
    # the percent change in applications per percent change in football wins for
    # 1000 bootstraped samples selected according to the the college chosen by
    # the user. I use data from the quantile_input and fit_input dataframes to
    # plot the observed r-squared value of the data as well as the median and
    # boundaries of a 95% confidence interval generatted from bootstrapping. I
    # color the observed r-squared value red so that it stands out.
    
    output$stat_plot <- renderPlot(
      ggplot(rep_input(), aes(x = r_squared)) +
        geom_density() +
        geom_vline(data = quantile_input(), aes(xintercept= `50%`)) +
        geom_vline(data = quantile_input(), aes(xintercept= `2.5%`, color = "conf_color"), linetype = "dashed") +
        geom_vline(data = quantile_input(), aes(xintercept= `97.5%`, color = "conf_color"), linetype = "dashed") +
        geom_vline(data = fit_input(), aes(xintercept = r_squared, color = "r_color")) +
        guides(colour = FALSE) +
        theme_dark() +
        theme(plot.title = element_text(size = 14)) +
        scale_color_manual(values = c("conf_color" = "lightblue",
                                      "r_color" = "red")) +
        labs(
          title = expression(paste("R"^2, " estimate, Percent Change in Applications per Percent Change in Football Wins")),
          subtitle = paste("Density plot of 1000 bootstrapped samples:", input$college),
          x = bquote("R"^2),
          y = "Density",
          caption = "Dashed lines show boundary of 95% confidence interval of bootstrapped samples. Solid black line shows median. \nRed line shows observed R-squared value of non-bootstrapped data."
        )
    )
    
    # Code a scatterplot on the Statistics page according to the inputs selected
    # by the user (processed and stored under the name "scatter_input").
    # Percentage change in football games won is plotted on the x axis and
    # percentage change in applications is plotted on the y axis. I use
    # geom_jitter to show points that otherwise would be hidden beneath others,
    # and I plot a regression line to show the general relationship of the data.
    # I color each scatterpoint based on the colors used in the scatterplot on
    # the "Conference Plot" page.
    
    output$scatter_plot <- renderPlot(
      ggplot(scatter_input(), aes(x = win_pct_diff, y = applcn_pct_chng)) +
        geom_jitter(aes(color = input$college), na.rm = TRUE) +
        geom_smooth(method='lm', se = FALSE) +
        guides(colour = FALSE) +
        theme_dark() +
        theme(plot.title = element_text(size = 16)) +
        labs(
          title = paste("University Applications vs. Football Team Success:", input$college),
          subtitle = "Change in Applications per Change in Football Team Ranking Over the Previous Year",
          x = "Percent Change in University Football Games Won",
          y = "Percent Change in Applications to University",
          caption = "Football data at each datapoint corresponds to the season before the application year of that datapoint",
          color = "Conference"
        ) +
        scale_color_manual(values = if (input$conf == "ACC") {
          acc_colors
        } else if (input$conf == "Big 10") {
          big_10_colors
        } else if (input$conf == "Big 12") {
          big_12_colors
        } else if (input$conf == "Pac 12") {
          pac_12_colors
        } else {
          sec_colors
        })
    )
    
    # Display a window with summary information whenever a user hovers over the
    # scatterplot on the Statistics page
    
    output$hover_scatter <- renderUI({
      
      # Create new variables "hover_input" and "point_input" that tell R whether
      # the cursor is hovering over the data and how far the cursor is from the
      # data points.
      
      hover_input <- input$plot_scat
      point_input <- nearPoints(scatter_input(), hover_input, threshold = 5, maxpoints = 1, addDist = TRUE)
      
      # This tells R to only display a window if the cursor is near a datapoint
      
      if(nrow(point_input) == 0){return(NULL)} 
      
      # Calculate the percent location of the cursor from the left and top sides
      # of the window
      
      left_percent <- (hover_input$x - hover_input$domain$left) / (hover_input$domain$right - hover_input$domain$left)
      top_percent <- (hover_input$domain$top - hover_input$y) / (hover_input$domain$top - hover_input$domain$bottom)
      
      # Calculate the pixel distance of the cursor from the left and bottom
      # sides of the window
      
      left_pixel <- hover_input$range$left + left_percent * (hover_input$range$right - hover_input$range$left)
      top_pixel <- hover_input$range$top + top_percent * (hover_input$range$bottom - hover_input$range$top)
      
      # Specify the styling and position of the panel that will appear when a
      # user hovers over a datapoint
      
      style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                      "left:", left_pixel - 200, "px; top:", top_pixel + 280, "px;")
      
      # Code what information to show on the the panel that will appear when a
      # user hovers over a datapoint
      
      wellPanel(
        style = style,
        p(HTML(paste0("<b> Application Year: </b>", point_input$year2, "<br/>",
                      "<b> Change in Wins: </b>", point_input$win_pct_diff, "%", "<br/>", 
                      "<b> Change in Applications: </b>", point_input$applcn_pct_chng, "%")))
      )
      
    }) 
    
    # Create the About page. This code chunk adds all the text that will
    # appear on the page.
    
    output$about <- renderUI({
      HTML(
        paste(
          h2("About This Project"),
          br(),
          div(
            "This project began after an informal observation of enrollment changes at Kansas State University (K-State), the local university in my hometown. Between 2010 and 2012 K-State's football team had phenomenal success - the team was ranked #1 in the nation for a time. During this same time period the university experienced record student enrollment rates. Meanwhile, K-State's in-state rival, the University of Kansas (KU), experienced some of the worst football seasons in university history during this timeframe. The university also suffered declining enrollment. Only a few years later, though, KSU began experiencing declining enrollment as its football team regressed. This made me wonder: do most large universities experience surges in applications when their football teams perform well and declines in applications when their teams struggle?"
          ),
          br(),
          div(
            "To answer my question I decided to analyze university application data and football team records (wins vs losses) for a period of approximately 10 years. I looked only at Power 5 universities - that is, universities whose football teams are members of either the Big 10, Big 12, SEC, ACC, or Pac 12 athletic conferences. I reason that these schools recruit students nationally rather than regionally, so their student bodies are more likely to change based on the performance of their football teams than are the student bodies of smaller, regional colleges. I chose not to look at data for universities that changed athletic conferences during the time period of interest since that change could effect application changes."
          ),
          br(),
          h4("Modeling Decisions"),
          div(
            "There are two variables of interest in this model: percent change in football wins and percent change in college applications. Looking at percent change is more insightful than looking at raw changes in wins and applications. For example, it is a greater change to go from winning two games to four in a twelve-game season than it is to go from winning seven games to nine, and fans probably prefer large improvements to small ones. It is possible that the number of games won is more important than the change in the percentage won - perhaps when a college suddenly wins enough games to enter the AP list of top 25 programs or become eligible to play in a bowl game it causes a spike in applications - but that is an assumption for a different model."
          ),
          br(),
          div(
            "Results are shown in a scatterplot since this type of graph allows the values of both variables to be displayed for many different data points. To plot each data point the change in applications for each year is matched with the change in football wins that occurred in the previous year. For example, a datapoint representing the change in applications between 2010 and 2011 will be matched with football data representing the change in wins from 2009 to 2010. The reasoning for modeling the data in this manner is that applicants apply before seeing the end of the current season and therefore rely on data from the previous season. Because both application and football seasons begin at roughly the end of August it is possible that applicants are influenced by more recent data when they make application decisions, but it is assumed that this is not the case. "
          ),
          br(),
          div(
            "A density plot of R-squared values is shown to provide context for how unusual each college's observed R-squared value may be. The plot uses 1000 bootstrapped samples taken from eleven years of observed data for each college. A necessary assumption is that eleven data points are not too few to bootstrap."
          ),
          br(), 
          h4("External Links"),
          div(
            "I gathered enrollment data from the",
            a("National Center for Education Statistics", href = "https://nces.ed.gov/ipeds/datacenter/InstitutionByName.aspx"),
            "and football data from the",
            a("NCAA." , href = "http://web1.ncaa.org/stats/StatsSrv/rankings?doWhat=archive&sportCode=MFB")
          ),
          br(),
          div(
            "The Github Repo for this project can be found",
            a("here.", href = "https://github.com/rbrown146/football-enrollment-correlations")
          ),
          br(),
          div(img(src = "KU_medium.jpg")),
          br(),
          h4("About the Author"),
          p(
            "Rick Brown is a sophomore studying economics at Harvard College. He is interested in the intersection of economics, sociology, and government as well as all things related to Kansas."
          ),
          p("Rick can be contacted at rbrown@college.harvard.edu")
        )
      )
    })
    
    # Create the Conclusion page. This code chunk adds all the text that will
    # appear on the page.
     
    output$summary <- renderUI({
      HTML(paste(
        h2("Takeaways from This Project"),
        br(),
        embed_youtube("BXHvPfoBiuI"),
        br(),
        br(),
        h4("Correlations Seem Weak"),
        div(
          "The slope of the regression line relating change in football wins to change in college enrollment is nearly flat for the data as a whole. When results are narrowed by conference the slope of the resulting regression line is marginally steeper for some conferences (Big 10, Big 12, Pac 12) but has a slight negative slope for others (ACC)."
        ),
        br(),
        div(
          "The data becomes more interesting when analyzed by college. Some colleges seem to be affected by outliers - for example, Georgia Tech's r-squared estimate of percent change in enrollment per percent change in football wins appears to be roughly 0.125, a rather low value. However, a 95% confidence interval created from bootstrapping 1000 samples from the dataset contains r-squared values up to approximately r-squared = 0.625. Plotting a scatterplot of datapoints makes it evident that there is one point on the far left that is skewing results. Some universities have r-squared values that are relatively high. Purdue, for instance, has an r-squared value approximately equal to 0.45, while Washington State has a r-squared value of 0.625. An overwhelming majority of universities, though, have low r-squared values and scatterplots with points that are fairly spread out. Some colleges, like Iowa State, have regression lines with negative slopes as a result."
        ),
        br(),
        div(
          "Perhaps change in wins is not the primary football statistic applicants use when deciding where to send their applications. Scholars have identified a trend called the ",
          a("Flutie Effect,", href = "https://hbswk.hbs.edu/item/diagnosing-the-flutie-effect-on-college-marketing"),
          " named after Boston College quarterback Doug Flutie's dramatic game-winning touchdown pass and the resulting surge in applications it brought, in which memorable moments in sports games lead to a spike in applications to colleges. It could be that beating rival schools or winning prestigious bowl games are more statistically significant factors in determining the change in applications to colleges."
        )
      ))
    })
}

# Run the application 

shinyApp(ui = ui, server = server)
