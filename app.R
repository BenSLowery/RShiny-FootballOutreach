#
# This is a Shiny web application. 
#

library(shiny)
library(shinydashboard)
library(DT)
library(tidyverse)
library(forcats)

# General Functions
highlight <- function(x, value, col.value, col=NA, ...){
  hst <- hist(x, breaks=20)
  idx <- findInterval(value, hst$breaks)
  cols <- rep(col, length(hst$counts))
  cols[idx] <- col.value
  hist(x, col=cols, breaks=20)
}

# Function for plotting player overall and potential over the games
plot_player_rating <- function(player_name) {
  player_overalls <- c()
  player_potentials <- c()
  
  for (i in 1:length(fifa_data_all_years)) {
    player_data <- fifa_data_all_years[[i]][fifa_data_all_years[[i]]$Name == player_name, ]
    if (length(player_data) > 0) {
      player_overalls <- c(player_overalls, player_data$Overall[1])
      player_potentials <- c(player_potentials, player_data$Potential[1])
    } else {
      player_overalls <- c(player_overalls, NA)
      player_potentials <- c(player_potentials, NA)
    }
  }
  
  data <- data.frame(Year = years, Overall = player_overalls, Potential = player_potentials)
  
  ggplot(data, aes(x = Year, y = Overall)) +
    geom_line(aes(y = Overall), color = "blue", size = 1.5) +
    geom_point(aes(y = Overall), color = "red", size = 3) +
    geom_text(aes(y = Overall, label = Overall), hjust = 0.5, vjust = -1.5, size = 3) +
    geom_line(aes(y = Potential), color = "blue", size = 1.5) +
    geom_point(aes(y = Potential), color = "red", size = 3) +
    geom_text(aes(y = Potential, label = Potential), hjust = 0.5, vjust = -1.5, size = 3) +
    geom_ribbon(aes(ymin=Overall,ymax=Potential), fill="blue", alpha=0.2) +
    labs(x = "Year", y = "Rating", title = paste("Overall and Potential Ratings of", player_name, "over the Years")) +
    xlim(17, 22) +
    ylim(30,99) +
    theme_minimal()
}

# Map a number to a colour (to highlight how good/bad a stat is)
map_num_to_col <- function(num) {
  if (num < 50) {
    colour <- 'red'
  } else if (51 < num & num < 79) {
    colour <- 'orange'
  } else {
    colour <- 'green'
  }
  
  return(colour)
}

# Get certain columns of the fifa data
# data <- readRDS(file="fifa_data.Rda")
data <- read.csv('data/FIFA22_official_data.csv')
data <- data[c('Name', 'Age', 'Overall', 'Club', 'Acceleration', 'Finishing', 'ShortPassing', 'Dribbling', 'StandingTackle', 'Strength')]

years <- c(17, 18, 19, 20, 21, 22)
fifa_data_all_years <- lapply(years, function(year) {
  read_csv(paste0("data/FIFA", year, "_official_data.csv"), skip_empty_rows = TRUE)[c('Name', 'Age', 'Overall', 'Club', 'Acceleration', 'Finishing', 'ShortPassing', 'Dribbling', 'StandingTackle', 'Strength', 'Potential')]
})

# UI of the app (using Shiny Dashboard)
ui <- dashboardPage(
  dashboardHeader(title = "Scout App"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Comparsion", tabName = "comparsion", icon = icon("people-group")),
      menuItem("Individual", tabName = "individual", icon = icon("person")),
      menuItem("Who to sign?", tabName = "w2s", icon = icon("magnifying-glass")),
      menuItem("Progression", tabName = "PlayerProgression", icon = icon("chart-area")),
      menuItem("Position Evaluator", tabName = "PositionEvaluator", icon = icon("map"))
      menuItem("Card Stats", tabName = "CardStats", icon = icon("chart-bar")),
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
    tabItem(tabName = "individual",
    # Boxes need to be put in a row (or column)
    box(width = 12,
    inputPanel(
      selectInput("year", "Select a year:", choices = c(2017,2018,2019,2020,2021,2022))
    ),
    ),
    fluidRow(
      box(width=12,
          DT::dataTableOutput("mytable"),
          tags$br(),
          tags$br(),
          #actionButton("clear", "Clear Selection"), 
      ),
         
      box(width=6,
         selectInput("histogram_col", "Attribute compared to others:",
                     c("Age" = "Age",
                       "Overall" = "Overall",
                       "Acceleration" = "Acceleration",
                       "Finishing" = "Finishing",
                       "Short Passing" = "ShortPassing",
                       "Dribbling" = "Dribbling",
                       "Standing Tackle" = "StandingTackle",
                       "Strength" = "Strength"
                       )),
    
         plotOutput('histogram')
      ),

     #TODO - Lollipop chart
     box(width = 6,
         plotOutput('lollipop')
        ),
      
    )
  ),
  tabItem(tabName = "comparsion",
          h2("Compare Players from Man City and Benfica"),
          box(width=12,
            inputPanel(
              selectInput(
                "SelectPlayerManCity",
                label = "Man. City",
                choices = subset(data, Club=="Manchester City")$Name
              ),
                selectInput(
                  "SelectPlayerBenfica",
                  label = "Benfica",
                  choices = subset(data, Club=="SL Benfica")$Name
                ),

            ),
            DT::dataTableOutput('PlayerComparison')
          ),
  ),
  tabItem(tabName = "w2s",
          h2("Given a position, we then give a list of players, and see who's best"),
          fluidRow(
            box(width=4,
            selectInput("ws2_player_select", "Select a Player:",
              c("Erling Haaland" = 249,
                              "Nicolas Otmamendi" = 2519,
                              "Jean Michael Seri" = 637,
                              "Robbie Mckenzie" = 10042,
                              "Kylian Hazard" =6353
              )),
              h3(textOutput("w2s_player_name")),
              p(textOutput("w2s_player_club")),
              p(textOutput("w2s_player_age")),
            ),
            box(width=8,
            # A static valueBox
            valueBoxOutput("PlayerPaceBox"),
            valueBoxOutput("PlayerShootingBox"),
            valueBoxOutput("PlayerStrengthBox"),
            valueBoxOutput("PlayerPassingBox"),
            valueBoxOutput("PlayerDribblingBox"),
            valueBoxOutput("PlayerTacklingBox"),
            )
          ),
          fluidRow(
            box(width=12,
              h2("What we're looking for: A Striker"),
              valueBox("Finishing", "Good at Shooting", icon = icon("meteor"),color='green'),
              valueBox("Pace", "Quick and agile", icon = icon("person-running"),color='green'),
              valueBox("Strength", "Tall and Strong", icon = icon("dumbbell"),color='green'),
              
            )
          )

  ),
  tabItem(tabName = "PlayerProgression",
          h2("Show player progression over generations"),
          uiOutput("name"),
          box(width=12,
              plotOutput("ribbonplot")
          )
  )
  )
 
  )
)

# Server side logic
server <- function(input, output) {
  
    #####
    # Main Player Database logic
    #####
    output$mytable = DT::renderDataTable({
      DT::datatable(
      fifa_data_all_years[[as.numeric(input$year) - 2016]],
      options = list(
        autoWidth = FALSE, scrollX = TRUE),
      selection = list(mode = 'single', selected = 1)
      )
    })
    
    output$histogram = renderPlot({
      s = input$mytable_rows_selected
     
      hist(fifa_data_all_years[[as.numeric(input$year) - 2016]][[input$histogram_col]], breaks=20)
      if (length(s)) highlight(fifa_data_all_years[[as.numeric(input$year) - 2016]][[input$histogram_col]], fifa_data_all_years[[as.numeric(input$year) - 2016]][s,][[input$histogram_col]], "red")
    })
    # 
    output$lollipop = renderPlot({
      s = input$mytable_rows_selected

      # Get data
      player_selected <- fifa_data_all_years[[as.numeric(input$year) - 2016]][s,c('Acceleration', 'Finishing', 'ShortPassing', 'Dribbling', 'StandingTackle', 'Strength')]

      # transpose all but the first column (name)
      tp <- as.data.frame(t(player_selected))
      tp <- cbind(attribute = rownames(tp), tp)
      rownames(tp) <- 1:nrow(tp)
      colnames(tp) <- c('attribute', 'val')

      tp %>%
        arrange(val) %>%    # First sort by val. This sort the dataframe but NOT the factor levels
        mutate(attribute=factor(attribute, levels=attribute)) %>%   # This trick update the factor levels
        ggplot( aes(x=attribute, y=val)) +
        geom_segment(aes(xend=attribute, yend=0)) +
        geom_point( size=4, color="blue") +
        ylim(0,100) +
        coord_flip() +
        xlab("Attribute") +
        ylab("Score out of 100") +
        theme_bw()

    })
    
    
    #####
    # Player scouting logic
    #####
    output$PlayerComparison <- DT::renderDataTable({ 
      
      # Transpose the data frame, make column names the new row names and display
      player_comps_temp <- subset(data, (Name==input$SelectPlayerManCity & Club=='Manchester City') | (Name==input$SelectPlayerBenfica & Club=='SL Benfica')) 
      player_comps_temp <- player_comps_temp[order(player_comps_temp$Club),]
      player_comps <- t(player_comps_temp[ , !(names(player_comps_temp) %in% c("Name", "Club"))]) # Transpose so we can look by column
      colnames(player_comps) <- player_comps_temp$Name # Make the name the head of the column
      # Display comparison as a data table and make the highest stat bold for each row
      DT::datatable(player_comps,options=list(dom = 't',ordering=F, rowCallback = JS(
        'function(row, data) {
    var num_data = data.slice(1,data.length)
    var row_max = Math.max.apply(Math,num_data);
    var row_min = Math.min.apply(Math,num_data);
    for(i=1;i < data.length; i++) {
      if(data[i]==row_max) {
        $("td:eq("+i+")", row).css("font-weight", "bold")
      }
    }
  }'))) 
    })
    
   #####
   # Who to scout logic
   #####
   output$w2s_player_name <- renderText({paste("Name: ",data[input$ws2_player_select, 'Name'])})
   output$w2s_player_club <- renderText({paste("Club: ",data[input$ws2_player_select, 'Club'])})
   output$w2s_player_age <- renderText({paste("Age: ",data[input$ws2_player_select, 'Age'])})
  
   output$PlayerPaceBox <- renderValueBox({
     valueBox(
       paste0(data[input$ws2_player_select, 'Acceleration']), "Speed", icon = icon("person-running"),
       color = map_num_to_col(data[input$ws2_player_select, 'Acceleration'])
     )
   })
   
   output$PlayerShootingBox <- renderValueBox({
     valueBox(
       paste0(data[input$ws2_player_select, 'Finishing']), "Shooting", icon = icon("meteor"),
       color = map_num_to_col(data[input$ws2_player_select, 'Finishing'])
     )
   })
   
   output$PlayerStrengthBox <- renderValueBox({
     valueBox(
       paste0(data[input$ws2_player_select, 'Strength']), "Strength", icon = icon("dumbbell"),
       color = map_num_to_col(data[input$ws2_player_select, 'Strength'])
     )
   })
   output$PlayerTacklingBox <- renderValueBox({
     valueBox(
       paste0(data[input$ws2_player_select, 'StandingTackle']), "Tackling", icon = icon("shield-halved"),
       color = map_num_to_col(data[input$ws2_player_select, 'StandingTackle'])
     )
   })
   
   output$PlayerPassingBox <- renderValueBox({
     valueBox(
       paste0(data[input$ws2_player_select, 'ShortPassing']), "Passing", icon = icon("futbol"),
       color = map_num_to_col(data[input$ws2_player_select, 'ShortPassing'])
     )
   })
   
   output$PlayerDribblingBox <- renderValueBox({
     valueBox(
       paste0(data[input$ws2_player_select, 'Dribbling']), "Dribbling", icon = icon("wand-magic-sparkles"),
       color = map_num_to_col(data[input$ws2_player_select, 'Dribbling'])
     )
   })

  ###
  # Player progression over generations
  ###

  my_list <- reactive({
    data <- fifa_data_all_years[[1]]$Name
    my_list <- as.character(data)

  })

  output$name <- renderUI({

    selectInput(inputId = "name", label = "Name", choices = my_list())
  })
  
  output$ribbonplot <- renderPlot({
    plot_player_rating(input$name)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
