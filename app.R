#
# This is a Shiny web application. 
#

library(shiny)
library(shinydashboard)
library(DT)
library(tidyverse)
library(forcats)
library(magick)

# Download image of football field

url <- 'https://upload.wikimedia.org/wikipedia/commons/8/82/Soccer_Field_Transparant.svg'
download.file(url, destfile = 'soccer_field.svg')
img <- image_read_svg('soccer_field.svg')

# General Functions
highlight <- function(x, value, col.value, col=NA, ...){
  hst <- hist(x, breaks=30)
  idx <- findInterval(value, hst$breaks)
  cols <- rep(col, length(hst$counts))
  cols[idx] <- col.value
  hist(x, col=cols, breaks=30, main=NULL, xlab=NULL, ylab='Frequency')
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

get_ut_stats <- function(player_ID, year) {
  player_data <- fifa_data_all_years[[year - 2016]][fifa_data_all_years[[year - 2016]]$ID == player_ID,]
  
  ut_stats <- list(
    Pace = round(player_data$SprintSpeed * 0.55 + player_data$Acceleration * 0.45),
    Shooting = round(player_data$Finishing * 0.45 + player_data$ShotPower * 0.2 + player_data$ShotPower * 0.2 + player_data$Positioning * 0.05 + player_data$Penalties * 0.05 + player_data$Volleys * 0.05),
    Passing = round(player_data$ShortPassing * 0.35 + player_data$Vision * 0.2 + player_data$Crossing * 0.2 + player_data$LongPassing * 0.15 + player_data$Curve * 0.05 + player_data$FKAccuracy * 0.05),
    Dribbling = round(player_data$Dribbling * 0.5 + player_data$BallControl * 0.35 + player_data$Agility * 0.1 + player_data$Balance * 0.05),
    Defending = round(player_data$Marking * 0.3 + player_data$Interceptions * 0.2 + player_data$StandingTackle * 0.3 + player_data$HeadingAccuracy * 0.1 + player_data$SlidingTackle * 0.1),
    Physicality = round(player_data$Strength * 0.5 + player_data$Stamina * 0.25 + player_data$Aggression * 0.2 + player_data$Jumping * 0.05)
  )
  
  return(ut_stats)
}

get_player_position_ratings <- function(player_ID, year) {
    rating_dict <- {}
    positions <- colnames(value_table)
    for (position in positions) {
        player_data <- fifa_data_all_years[[year-2016]][fifa_data_all_years[[year-2016]]$ID == player_ID,]
        position_rating <- sum(player_data[rownames(value_table)] * value_table[[position]])
        rating_dict[position] <- round(position_rating)
    }
    return(rating_dict)
}

# Map a number to a colour (to highlight how good/bad a stat is)
map_num_to_col <- function(num) {
  if (num <= 50) {
    colour <- 'red'
  } else if (51 <= num & num <= 79) {
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
  read_csv(paste0("data/FIFA", year, "_official_data.csv"), skip_empty_rows = TRUE, show_col_types = FALSE, locale = readr::locale(encoding = "UTF-8"))
})

# in the fifa 20, 21, 22 data remove the marking column and rename the defensive awareness column to marking
for (i in 4:6) {
  fifa_data_all_years[[i]] <- fifa_data_all_years[[i]] %>%
    select(-Marking) %>%
    rename(Marking = `DefensiveAwareness`)
}

value_table <- read_csv('data/value_table.csv', show_col_types = FALSE) %>% 
  tibble::column_to_rownames('Stat')

# Get common columns
common_columns <- Reduce(intersect, lapply(fifa_data_all_years, colnames))

# Remove columns that are not common to every dataframe
fifa_data_all_years <- lapply(fifa_data_all_years, function(df) select(df, all_of(common_columns)))

for (i in seq_along(fifa_data_all_years)) {
  fifa_data_all_years[[i]] <- fifa_data_all_years[[i]] %>%
    select(-c(Photo, Flag, `Club Logo`, `Real Face`, `Jersey Number`, `Loaned From`, `Contract Valid Until`, Joined, Position)) %>%
    mutate(Club = replace_na(Club, 'No Club'))
}

# Go through data and remove any players with a number at the beginning of their name
for (i in seq_along(fifa_data_all_years)) {
  fifa_data_all_years[[i]] <- fifa_data_all_years[[i]] %>%
    filter(!grepl("^\\d", Name))
}

# Change the data type of the value and wage colums to numeric substituting the K and M with the appropriate number
for (i in seq_along(fifa_data_all_years)) {
  fifa_data_all_years[[i]] <- fifa_data_all_years[[i]] %>%
    mutate(Value = as.numeric(gsub("€", "", gsub("M", "e6", gsub("K", "e3", Value)))), 
           Wage = as.numeric(gsub("€", "", gsub("M", "e6", gsub("K", "e3", Wage)))))
}

# Rename the best position column to Preferred Position
for (i in seq_along(fifa_data_all_years)) {
  fifa_data_all_years[[i]] <- fifa_data_all_years[[i]] %>%
    rename(`Preferred Position` = `Best Position`)
}


# UI of the app (using Shiny Dashboard)
ui <- dashboardPage(
  dashboardHeader(title = "Scout App"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Comparsion", tabName = "comparsion", icon = icon("people-group")),
      menuItem("Individual", tabName = "individual", icon = icon("person")),
      menuItem("Who to sign?", tabName = "w2s", icon = icon("magnifying-glass")),
      menuItem("Progression", tabName = "PlayerProgression", icon = icon("chart-area")),
      menuItem("Ultimate Scouter", tabName = "scouter", icon = icon("map"))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
    tabItem(tabName = "individual",
    # Boxes need to be put in a row (or column)
    box(width = 12,
    # inputPanel(
    #   selectInput("year", "Select a year:", choices = c(2017,2018,2019,2020,2021,2022))
    # ),
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
                       "Strength" = "Strength",
                       'Potential' = 'Potential'
                       )),
    
         plotOutput('histogram')
      ),

     #TODO - Lollipop chart
     box(width = 6,
         plotOutput('lollipop')
     )
      
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
          )
  ),
  tabItem(tabName = "w2s",
          h2("Given a position, we then give a list of players, and see who's best"),
          fluidRow(
            box(width=4,
            selectInput("ws2_player_select", "Select a Player:",
              c("Erling Haaland" = 252,
                              "Nicolas Otmamendi" = 2574,
                              "Jean Michael Seri" = 650,
                              "Robbie Mckenzie" = 10215,
                              "Thorgan Hazard" = 306
              )),
              h3(textOutput("w2s_player_name")),
              p(textOutput("w2s_player_club")),
              p(textOutput("w2s_player_age"))
            ),
            box(width=8,
            # A static valueBox
            valueBoxOutput("PlayerPaceBox"),
            valueBoxOutput("PlayerShootingBox"),
            valueBoxOutput("PlayerStrengthBox"),
            valueBoxOutput("PlayerPassingBox"),
            valueBoxOutput("PlayerDribblingBox"),
            valueBoxOutput("PlayerTacklingBox")
            )
          ),
          fluidRow(
            box(width=12,
              h2("What we're looking for: A Striker"),
              valueBox("Finishing", "Good at Shooting", icon = icon("meteor"),color='green'),
              valueBox("Pace", "Quick and agile", icon = icon("person-running"),color='green'),
              valueBox("Strength", "Tall and Strong", icon = icon("dumbbell"),color='green')
            )
          )

  ),
  tabItem(tabName = "PlayerProgression",
          h2("Show player progression over generations"),
          uiOutput("name"),
          box(width=12,
              plotOutput("ribbonplot")
          )
  ),
  tabItem(tabName = "scouter",
        fluidRow(
        h2("Ultimate Scouter"),
        tags$br(),
        tags$br(),
        h3("This is the ultimate scouting tool. Search for a player in the table to see their stats and even see how they've progressed over the years"),
        tags$br(),
        tags$br(),
        # These are sliders which narrow down the search for a player, more can easily be added in the future but these will do for now. QoL in the future would be to move search from top right of table to here
        box(width=4,
          sliderInput("Age", "Age:", min = 16, max = 54, step = 1, value = c(16, 54)),
          sliderInput("Value", "Value:", min = 0, max = 200000000, step = 1000000, value = c(0, 200000000), pre = "€"),
          sliderInput("Wage", "Wage:", min = 0, max = 1000000, step = 10000, value = c(0, 1000000), pre = "€"),
          sliderInput("Overall", "Overall:", min = 0, max = 100, step = 1, value = c(0, 100)),
          sliderInput("Potential", "Potential:", min = 0, max = 100, step = 1, value = c(0, 100)),
          selectizeInput("Nation", "Nation:", choices = c('All', unique(fifa_data_all_years[[6]]$Nationality)), multiple = FALSE, selected = 'All'),
          selectizeInput("PreferredPosition", "Preferred Position:", choices = c('All', unique(fifa_data_all_years[[6]]$`Preferred Position`)), multiple = FALSE, selected = 'All')
          ),
        box(width = 8, height = 672,
        DT::dataTableOutput("scoutingtable")
        ),
        tags$br(),
        tags$br(),
        box(width= 6,
        # Copied from Ben
        selectInput("histogram_col", "Attribute compared to others:",
                     c("Age" = "Age",
                       "Overall" = "Overall",
                       "Acceleration" = "Acceleration",
                       "Finishing" = "Finishing",
                       "Short Passing" = "ShortPassing",
                       "Dribbling" = "Dribbling",
                       "Standing Tackle" = "StandingTackle",
                       "Strength" = "Strength",
                       'Potential' = 'Potential'
                       )),
        plotOutput('scoutinghistogram')
        # Add Table in at the top of the page so that if two players have the same name, we can select the correct one
        ),
        # Copied from Ben but used UT card stats
        box(width=6,
          plotOutput('scoutinglollipop')
          ),
        # Football pitch showing players ratings in different positions
        box(width = 8, height = 800,
          plotOutput('scoutingField')
        ),
        # Legend for the formation on the left
        box(width = 4, height = 800,
          h3("Positions"),
          h4("GK - Goalkeeper"),
          h4("CB - Centre Back"),
          h4("LB/RB - Left/Right Back"),
          h4("LWB/RWB - Left/Right Wing Back"),
          h4("CDM - Central Defensive Midfielder"),
          h4("CM - Central Midfielder"),
          h4("CAM - Central Attacking Midfielder"),
          h4("LM/RM - Left/Right Midfielder"),
          h4("LW/RW - Left/Right Winger"),
          h4("ST - Striker"),
          h4("CF - Centre Forward")
        )
  )
  )
  )
))

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
      if (length(s)) {
        hist(fifa_data_all_years[[as.numeric(input$year) - 2016]][[input$histogram_col]], breaks=20 ,main=NULL, xlab=NULL, ylab='Frequency')
        highlight(fifa_data_all_years[[as.numeric(input$year) - 2016]][[input$histogram_col]], fifa_data_all_years[[as.numeric(input$year) - 2016]][s,][[input$histogram_col]], "red")
      } else {
        hist(fifa_data_all_years[[as.numeric(input$year) - 2016]][[input$histogram_col]], breaks=20, main=NULL, xlab=NULL, ylab='Frequency')
      }
    })
    # 
    output$lollipop = renderPlot({
      s = input$mytable_rows_selected
      if (length(s)){
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

      }
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
  # Player progression over generations logic
  ###

  # Just remove this and associated UI if I can't figure out how to make it faster in time

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

  # Ulitimate Scouter logic

  scouting_dt <- fifa_data_all_years[[6]][c('ID','Name', 'Age', 'Nationality', 'Club', 'Overall', 'Potential', 'Value', 'Wage', 'Preferred Position')]

  # This is messy af but I don't want to spend any more time on it :) Gets the correct data based on the filters input by the user
  filtered_dt <- reactive(
    if (input$Nation == 'All') {
      if (input$PreferredPosition == 'All') {
        scouting_dt[scouting_dt$Age >= input$Age[1] & scouting_dt$Age <= input$Age[2] & scouting_dt$Value >= input$Value[1] & scouting_dt$Value <= input$Value & scouting_dt$Wage >= input$Wage[1] & scouting_dt$Wage <= input$Wage[2] & scouting_dt$Overall >= input$Overall[1] & scouting_dt$Overall <= input$Overall[2] & scouting_dt$Potential >= input$Potential[1] & scouting_dt$Potential <= input$Potential[2],]
      } else {
        scouting_dt[scouting_dt$Age >= input$Age[1] & scouting_dt$Age <= input$Age[2] & scouting_dt$Value >= input$Value[1] & scouting_dt$Value <= input$Value & scouting_dt$Wage >= input$Wage[1] & scouting_dt$Wage <= input$Wage[2] & scouting_dt$Overall >= input$Overall[1] & scouting_dt$Overall <= input$Overall[2] & scouting_dt$Potential >= input$Potential[1] & scouting_dt$Potential <= input$Potential[2] & scouting_dt$`Preferred Position` == input$PreferredPosition ,]
      }
    } else {
      if (input$PreferredPosition == 'All') {
        scouting_dt[scouting_dt$Age >= input$Age[1] & scouting_dt$Age <= input$Age[2] & scouting_dt$Value >= input$Value[1] & scouting_dt$Value <= input$Value & scouting_dt$Wage >= input$Wage[1] & scouting_dt$Wage <= input$Wage[2] & scouting_dt$Overall >= input$Overall[1] & scouting_dt$Overall <= input$Overall[2] & scouting_dt$Potential >= input$Potential[1] & scouting_dt$Potential <= input$Potential[2] & scouting_dt$Nationality == input$Nation ,]
      } else {
        scouting_dt[scouting_dt$Age >= input$Age[1] & scouting_dt$Age <= input$Age[2] & scouting_dt$Value >= input$Value[1] & scouting_dt$Value <= input$Value & scouting_dt$Wage >= input$Wage[1] & scouting_dt$Wage <= input$Wage[2] & scouting_dt$Overall >= input$Overall[1] & scouting_dt$Overall <= input$Overall[2] & scouting_dt$Potential >= input$Potential[1] & scouting_dt$Potential <= input$Potential[2] & scouting_dt$Nationality == input$Nation & scouting_dt$`Preferred Position` == input$PreferredPosition ,]
      
      }
    }
  )

  # Outputs the data table, borrowed from Ben with some QoL changes
  output$scoutingtable = DT::renderDataTable({
      DT::datatable(
      filtered_dt()[c('Name', 'Age', 'Nationality', 'Club', 'Overall', 'Potential', 'Value', 'Wage', 'Preferred Position')],
      options = list(
      autoWidth = FALSE, scrollX = TRUE, dom='fpt', pageLength = 15),
      selection = list(mode = 'single', selected = 1)
      ) %>% formatCurrency(c('Value', 'Wage'), currency = '€', interval = 3, mark = ',', digits = 0)
    })

  # Borrowed from Ben
  output$scoutinghistogram = renderPlot({
      s = input$scoutingtable_rows_selected
      if (length(s)) {
        hist(fifa_data_all_years[[6]][[input$histogram_col]], breaks=30 ,main=NULL, xlab='Rating', ylab='Frequency')
        highlight(fifa_data_all_years[[6]][[input$histogram_col]], fifa_data_all_years[[6]][fifa_data_all_years[[6]]$ID == filtered_dt()[s,]$ID,][[input$histogram_col]], "red")
      } else {
        hist(fifa_data_all_years[[6]][[input$histogram_col]], breaks=30, main=NULL, xlab='Rating', ylab='Frequency')
      }
    })

  # Borrowed from Ben few QoL changes
  output$scoutinglollipop = renderPlot({
      s = input$scoutingtable_rows_selected
      if (length(s)){
        # Get data
        selected_player <- filtered_dt()[s,]

        stats <- get_ut_stats(selected_player$ID, 2022)

        # plot stats on a lollipop chart

        tp <- data.frame(x=names(stats), y=unname(unlist(stats)))

  ggplot(tp, aes(x=x, y=y)) +
    geom_segment( aes(x=x, xend=x, y=0, yend=y), color="skyblue") +
    geom_point( color="blue", size=4, alpha=0.6) +
    geom_text(aes(label=y), hjust=0.5, vjust=-1.5, size=6) +
    xlab("") +
    ylab("") +
    ggtitle("Ultimate Team Stats", ) +
    theme_light() +
    coord_flip() +
    theme(
      panel.grid.major.y = element_blank(),
      panel.border = element_blank(),
      axis.ticks.y = element_blank(),
      text = element_text(size=20), # Make font size of all elements larger
      plot.title = element_text(hjust=0.35) # Center the title
    )





      }
    })

    # Renders players position ratings on the football field
    output$scoutingField = renderPlot({
      s = input$scoutingtable_rows_selected
      position_ratings <- get_player_position_ratings(filtered_dt()[s, ]$ID, 2022)
      image_ggplot(img) +
      annotate("text", x = 225, y = 60, size = 10, label=paste("GK\n", as.character(position_ratings['GK']))) +
      annotate("text", x = 225, y = 145, size = 10, label=paste("CB\n", as.character(position_ratings['CB']))) +
      annotate("text", x = 375, y = 180, size = 10, label=paste("RB\n", as.character(position_ratings['LB/RB']))) +
      annotate("text", x = 75, y = 180, size = 10, label=paste("LB\n", as.character(position_ratings['LB/RB']))) +
      annotate("text", x = 75, y = 280, size = 10, label=paste("LWB\n", as.character(position_ratings['LWB/RWB']))) +
      annotate("text", x = 375, y = 280, size = 10, label=paste("RWB\n", as.character(position_ratings['LWB/RWB']))) +
      annotate("text", x = 225, y = 250, size = 10, label=paste("CDM\n", as.character(position_ratings['CDM']))) +
      annotate("text", x = 225, y = 340, size = 10, label=paste("CM\n", as.character(position_ratings['CM']))) +
      annotate("text", x = 225, y = 420, size = 10, label=paste("CAM\n", as.character(position_ratings['CAM']))) +
      annotate("text", x = 375, y = 380, size = 10, label=paste("RM\n", as.character(position_ratings['LM/RM']))) +
      annotate("text", x = 75, y = 380, size = 10, label=paste("LM\n", as.character(position_ratings['LM/RM']))) +
      annotate("text", x = 375, y = 500, size = 10, label=paste("RW\n", as.character(position_ratings['LW/RW']))) + 
      annotate("text", x = 75, y = 500, size = 10, label=paste("LW\n", as.character(position_ratings['LW/RW']))) +
      annotate("text", x = 225, y = 620, size = 10, label=paste("ST\n", as.character(position_ratings['ST']))) +
      annotate("text", x = 225, y = 520, size = 10, label=paste("CF\n", as.character(position_ratings['CF']))) 
    },
    height = 780,
    )

  
}

# Run the application 
shinyApp(ui = ui, server = server)
