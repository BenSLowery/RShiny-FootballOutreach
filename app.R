#
# This is a Shiny web application. 
# Authors: 
# Ben Lowery
# Jack Trainer
#

library(shiny)
library(shinydashboard)
library(DT)
library(tidyverse)
library(forcats)
library(magick)
library(rsvg)


# Download image of football field

url <- 'https://upload.wikimedia.org/wikipedia/commons/8/82/Soccer_Field_Transparant.svg'

if (!file.exists("assets/soccer_field.svg")) {
  download.file(url, destfile = 'assets/soccer_field.svg')
}

img <- image_read_svg('assets/soccer_field.svg')

# General Functions
highlight <- function(x, value, col.value, col=NA, ...){
  hst <- hist(x, breaks=30)
  idx <- findInterval(value, hst$breaks)
  cols <- rep(col, length(hst$counts))
  cols[idx] <- col.value
  hist(x, col=cols, breaks=30, main=NULL, xlab=NULL, ylab='Frequency')
}

# Function for plotting pl ayer overall and potential over the games
plot_player_rating <- function(player_name) {
  player_overalls <- c()
  player_potentials <- c()
  
  for (i in 1:length(fifa_data_all_years)) {
    player_data <- fifa_data_all_years[[i]][fifa_data_all_years[[i]]$Name == player_name, ]
    if (length(player_data) > 0) {
      player_overalls <- c(player_overalls, player_data$Overall[1])
      player_potentials <- c(player_potentials, player_data$Potential[1])
    } 
    
  }

  data <- data.frame(Year = years, Overall = player_overalls, Potential = player_potentials) %>% na.omit

  ggplot(data, aes(x = Year, y = Overall)) +
    geom_line(aes(y = Overall), color = "blue", linewidth = 1.5) +
    geom_point(aes(y = Overall), color = "red", size = 3) +
    geom_text(aes(y = Overall, label = Overall), hjust = 0.5, vjust = 2.5, size = 3) +
    geom_line(aes(y = Potential), color = "blue", linewidth = 1.5) +
    geom_point(aes(y = Potential), color = "red", size = 3) +
    geom_text(aes(y = Potential, label = Potential), hjust = 0.5, vjust = -1.5, size = 3) +
    geom_ribbon(aes(ymin=Overall,ymax=Potential), fill="blue", alpha=0.2) +
    labs(x = "Year", y = "Rating", title = paste("Overall and Potential Ratings of", player_name, "over the Years")) +
    xlim(17, 22) +
    ylim(30,99) +
    theme_minimal()
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

# Get fifa data
years <- c(17,18,19,20,21,22)
fifa_data_all_years <- lapply(years, function(year) {
  read_csv(paste0("data/FIFA_", 2000+year, "_data.csv"), skip_empty_rows = TRUE, show_col_types = FALSE, locale = readr::locale(encoding = "UTF-8"))
})

school_datas <- paste0('./custom_data/', list.files(path = "./custom_data/", pattern = "*.csv")) %>% map_df(~read_csv(.))


# Add index and ID to front of CV. ID=0 and the index is the last row of the length of the DF
# TODO broken
school_meta_data <- data.frame(Index = double(), ID=double())
for (i in 1:nrow(school_datas)) {
  school_meta_data[i,] <- c(nrow(fifa_data_all_years[[6]])+i,max(fifa_data_all_years[[1]]$ID)+i)
}
school_data <- bind_cols(school_meta_data, school_datas)
fifa_data_all_years[[6]]<-bind_rows(fifa_data_all_years[[6]],school_data)


value_table <- read_csv('data/value_table.csv', show_col_types = FALSE) %>% 
tibble::column_to_rownames('Stat')




# UI of the app (using Shiny Dashboard)
ui <- dashboardPage(
  dashboardHeader(title = "Scout App"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName='home', icon=icon('house')),
      
      menuItem("Who to sign?" , tabname = "", icon = icon("magnifying-glass"),
               startExpanded = FALSE,
               menuSubItem("Man. City",
                           tabName = "w2s_man_city", icon=icon('ship')),
                menuSubItem("Morecambe",
                           tabName = "w2s_morecambe", icon=icon('shrimp')),
                menuSubItem("Benfica",
                            tabName = "w2s_benfica", icon=icon('crow'))),
      menuItem('Ultimate Scouter', tabname = "", icon = icon("map"), 
               startExpanded = TRUE, 
               menuSubItem("Player Analysis", tabName = "scouter", icon=icon("user-secret")),
               menuSubItem("Player Progression", tabName = "PlayerProgression", icon=icon("chart-area")),
               menuSubItem("Comparsion", tabName = "comparsion", icon = icon("people-group"))
              )
    )
  ),
  dashboardBody(
    
  tabItems(
  
  tabItem(tabName = "home", 
          fluidRow(
            tabBox(width=12,
              id = "HomeTabs",
              tabPanel("Welcome", icon=icon('house'),
                       h3("Who to Scout Questions?"),
                       tags$p("Go to who to sign and complete the scenarios!"),
                       h3("Ultimate Scouter App Questions"),
                       tags$ol(
                         tags$li("Who's the highest rated player who's valued at €80m or less?"),
                         tags$li("Find Jude Bellingham, what is his potential? Is this still the case now or do you think it should be higher?"),
                         tags$li("Find Mason Mount. What was his rating in 2017?"),
                         tags$li("Find Bukayo Saka, compare him to Phil Foden in the Comparison tab. Who would you say is better?"),
                         tags$li("Who's the oldest player with a rating between 80 and 90?"),
                         tags$li("Find the School Player. Compare their skills to the Man City, Morecambe and Benfica players.")
                       ))
            ),
          ),
          ),
  
  tabItem(tabName = "comparsion_old",
          h2("Compare Players from Man City and Benfica"),
          box(width=12,
            inputPanel(
              selectInput(
                "SelectPlayerManCity",
                label = "Man. City",
                choices = subset(fifa_data_all_years[[6]], Club=="Manchester City")$Name
              ),
                selectInput(
                  "SelectPlayerBenfica",
                  label = "Benfica",
                  choices = subset(fifa_data_all_years[[6]], Club=="SL Benfica")$Name
                ),

            ),
            DT::dataTableOutput('PlayerComparison')
          )
  ),
  tabItem(tabName = "comparsion",
          h2("Compare Player to either Manchester City, Benfica or Morecambe players."),
          box(width=12,
              inputPanel(
                selectInput(
                  "SelectClub",
                  label = "Club",
                  choices = c('Manchester City', "SL Benfica", "Morecambe")
                ),
                selectInput(
                  "SelectPlayer",
                  label = "Player",
                  choices = c()
                ),
                
              ),
              DT::dataTableOutput('PlayerComparison')
          )
  ),
  tabItem(tabName = "w2s_man_city",
          h2("Brief:"),
          p("You're a scout for Manchester City, who need to sign a star striker for the upcoming season. The club want a player with the following stats"),
          fluidRow(
            box(width=12,
                h2("What we're looking for: A Striker"),
                valueBox("Shooting", "Excellent at Finishing", icon = icon("meteor"),color='green'),
                valueBox("Pace", "Quick and agile", icon = icon("person-running"),color='green'),
                valueBox("Strength", "Tall and Strong", icon = icon("dumbbell"),color='green')
            )
          ),
          p("Who, out of the players below, is the best fit?"),
          fluidRow(
            box(width=4,
            selectInput("ws2_player_select_man_city", "Select a Player:",
              c(
                              "Nicolas Otmamendi" = 2150,
                              "Jean Michael Seri" = 560,
                              "Erling Haaland" = 224,
                              "Tom Lowery" = 4937,
                              "Thorgan Hazard" = 276
              )),
              h3(textOutput("w2s_player_name_man_city")),
              p(textOutput("w2s_player_club_man_city")),
              p(textOutput("w2s_player_age_man_city"))
            ),
            box(width=8,
            # A static valueBox
            valueBoxOutput("PlayerPaceBox_man_city"),
            valueBoxOutput("PlayerShootingBox_man_city"),
            valueBoxOutput("PlayerStrengthBox_man_city"),
            valueBoxOutput("PlayerPassingBox_man_city"),
            valueBoxOutput("PlayerDribblingBox_man_city"),
            valueBoxOutput("PlayerTacklingBox_man_city")
            )
          )

  ),
  tabItem(tabName = "w2s_morecambe",
          h2("Brief:"),
          p("You're a scout for Morecambe, who need to sign an experienced defender to help them push for promotion."),
          
          fluidRow(
            box(width=12,
                h2("What we're looking for: A Defender"),
                valueBox("Age", "Older than 28 with an experinced head", icon = icon("brain"),color='green'),
                valueBox("Defending", "Crunching tackles", icon = icon("shield-halved"),color='yellow'),
                valueBox("Strength", "A rock at the back", icon = icon("dumbbell"),color='green')
            )
          ),
          p("Which player fits the best? Remember we want an older player!"),
          
          fluidRow(
            box(width=4,
                selectInput("ws2_player_select_morecambe", "Select a Player:",
                            c( "Mark Marshall" = 4965,
                              "Joe Taylor" = 6857,
                              "Jair Amador" =  9720,
                              "Joe Bell" = 3282,
                              "Aynsley Pears" = 13458
                            )),
                h3(textOutput("w2s_player_name_morecambe")),
                p(textOutput("w2s_player_club_morecambe")),
                p(textOutput("w2s_player_age_morecambe"))
            ),
            box(width=8,
                # A static valueBox
                valueBoxOutput("PlayerPaceBox_morecambe"),
                valueBoxOutput("PlayerShootingBox_morecambe"),
                valueBoxOutput("PlayerStrengthBox_morecambe"),
                valueBoxOutput("PlayerPassingBox_morecambe"),
                valueBoxOutput("PlayerDribblingBox_morecambe"),
                valueBoxOutput("PlayerTacklingBox_morecambe")
            )
          )
          
  ),
  tabItem(tabName = "w2s_benfica",
          h2("Brief:"),
          p("You're a scout for Benfica, who need to sign a tricky winger from the region to tear down opposition defences."),
          fluidRow(
            box(width=12,
                h2("What we're looking for: A Winger"),
                valueBox("Dribbling", "Tricky with his feet", icon = icon("wand-magic-sparkles"),color='green'),
                valueBox("Pace", "Quick and agile", icon = icon("person-running"),color='green'),
                valueBox("Local Hero", "From Spain or Portugal", icon = icon("earth-europe"),color='green')
            )
          ),
          p("Which player fits the best? Keep an eye out for which country they are from!"),
          fluidRow(
            box(width=4,
                selectInput("ws2_player_select_benfica", "Select a Player:",
                            c("A. Wildig" = 4376  ,
                              "H. Mkhitaryan" = 116 ,
                              "Z. Bailey" = 10232,
                              "João Mário" = 2652,
                              "Samu Castillejo" = 1860
                            )),
                h3(textOutput("w2s_player_name_benfica")),
                p(textOutput("w2s_player_club_benfica")),
                p(textOutput("w2s_player_age_benfica")),
                p(textOutput("w2s_player_nation_benfica"))
            ),
            box(width=8,
                # A static valueBox
                valueBoxOutput("PlayerPaceBox_benfica"),
                valueBoxOutput("PlayerShootingBox_benfica"),
                valueBoxOutput("PlayerStrengthBox_benfica"),
                valueBoxOutput("PlayerPassingBox_benfica"),
                valueBoxOutput("PlayerDribblingBox_benfica"),
                valueBoxOutput("PlayerTacklingBox_benfica")
            )
          )

          
  ),
  tabItem(tabName = "PlayerProgression",
          h2("Show player progression over generations"),
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
          sliderInput("Age", "Age:", min = 0, max = 54, step = 1, value = c(0, 54)),
          sliderInput("Value", "Value:", min = 0, max = max(fifa_data_all_years[[6]]$Value), step = 1000000, value = c(0, max(fifa_data_all_years[[6]]$Value)), pre = "€"),
          sliderInput("Wage", "Wage:", min = 0, max = max(fifa_data_all_years[[6]]$Wage), step = 10000, value = c(0, max(fifa_data_all_years[[6]]$Wage)), pre = "€"),
          sliderInput("Overall", "Overall:", min = 0, max = max(fifa_data_all_years[[6]]$Overall), step = 1, value = c(0, max(fifa_data_all_years[[6]]$Overall))),
          sliderInput("Potential", "Potential:", min = 0, max = max(fifa_data_all_years[[6]]$Potential), step = 1, value = c(0, max(fifa_data_all_years[[6]]$Potential))),
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
                       'Potential' = 'Potential',
                       "Pace" = "Pace",
                       "Shooting" = "Shooting",
                       "Passing" = "Passing",
                       "Dribbling" = "Dribbling",
                       "Defending" = "Defending",
                       "Physicality" = "Physicality"
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
server <- function(input, output, session) {
  
  
    #### 
    # Welcome page logic
    ####
    
  
    #####
    # Player comparison logic
    #####
    observeEvent(input$SelectClub, {
      
      #Filter countries based on current continent selection
      playersToShow = subset(fifa_data_all_years[[6]], Club==input$SelectClub)$Name
      
      #Update the actual input
      updateSelectInput(session, "SelectPlayer", choices = playersToShow, 
                        selected = playersToShow[1])
      
    })
    output$PlayerComparison <- DT::renderDataTable({ 
      
      # Transpose the data frame, make column names the new row names and display
      if (length(input$scoutingtable_rows_selected))  {
      player_comps_temp <- rbind(subset(fifa_data_all_years[[6]], (ID==filtered_dt()[input$scoutingtable_rows_selected,]$ID)), subset(fifa_data_all_years[[6]], (Name==input$SelectPlayer & Club==input$SelectClub)))
      player_comps_temp <- player_comps_temp[order(player_comps_temp$Club),]
      player_comps_temp <- player_comps_temp[c('Name', 'Club', 'Pace', 'Shooting', 'Passing', 'Dribbling', 'Defending', 'Physicality')]
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
    }
      })
    
   #####
   # Who to scout logic
   #####
   output$w2s_player_name_man_city <- renderText({paste("Name: ",fifa_data_all_years[[6]][input$ws2_player_select_man_city, 'Name'])})
   output$w2s_player_club_man_city <- renderText({paste("Club: ",fifa_data_all_years[[6]][input$ws2_player_select_man_city, 'Club'])})
   output$w2s_player_age_man_city <- renderText({paste("Age: ",fifa_data_all_years[[6]][input$ws2_player_select_man_city, 'Age'])})
  
   output$PlayerPaceBox_man_city <- renderValueBox({
     valueBox(
       paste0(fifa_data_all_years[[6]][input$ws2_player_select_man_city, 'Pace']), "Speed", icon = icon("person-running"),
       color = map_num_to_col(fifa_data_all_years[[6]][input$ws2_player_select_man_city, 'Pace'])
     )
   })
   
   output$PlayerShootingBox_man_city <- renderValueBox({
     valueBox(
       paste0(fifa_data_all_years[[6]][input$ws2_player_select_man_city, 'Shooting']), "Shooting", icon = icon("meteor"),
       color = map_num_to_col(fifa_data_all_years[[6]][input$ws2_player_select_man_city, 'Shooting'])
     )
   })
   
   output$PlayerStrengthBox_man_city <- renderValueBox({
     valueBox(
       paste0(fifa_data_all_years[[6]][input$ws2_player_select_man_city, 'Physicality']), "Strength", icon = icon("dumbbell"),
       color = map_num_to_col(fifa_data_all_years[[6]][input$ws2_player_select_man_city, 'Physicality'])
     )
   })
   output$PlayerTacklingBox_man_city <- renderValueBox({
     valueBox(
       paste0(fifa_data_all_years[[6]][input$ws2_player_select_man_city, 'Defending']), "Defending", icon = icon("shield-halved"),
       color = map_num_to_col(fifa_data_all_years[[6]][input$ws2_player_select_man_city, 'Defending'])
     )
   })
   
   output$PlayerPassingBox_man_city <- renderValueBox({
     valueBox(
       paste0(fifa_data_all_years[[6]][input$ws2_player_select_man_city, 'Passing']), "Passing", icon = icon("futbol"),
       color = map_num_to_col(fifa_data_all_years[[6]][input$ws2_player_select_man_city, 'Passing'])
     )
   })
   
   output$PlayerDribblingBox_man_city <- renderValueBox({
     valueBox(
       paste0(fifa_data_all_years[[6]][input$ws2_player_select_man_city, 'Dribbling']), "Dribbling", icon = icon("wand-magic-sparkles"),
       color = map_num_to_col(fifa_data_all_years[[6]][input$ws2_player_select_man_city, 'Dribbling'])
     )
   })
   
   output$w2s_player_name_morecambe <- renderText({paste("Name: ",fifa_data_all_years[[6]][input$ws2_player_select_morecambe, 'Name'])})
   output$w2s_player_club_morecambe  <- renderText({paste("Club: ",fifa_data_all_years[[6]][input$ws2_player_select_morecambe, 'Club'])})
   output$w2s_player_age_morecambe  <- renderText({paste("Age: ",fifa_data_all_years[[6]][input$ws2_player_select_morecambe, 'Age'])})
   
   output$PlayerPaceBox_morecambe  <- renderValueBox({
     valueBox(
       paste0(fifa_data_all_years[[6]][input$ws2_player_select_morecambe, 'Pace']), "Speed", icon = icon("person-running"),
       color = map_num_to_col(fifa_data_all_years[[6]][input$ws2_player_select_morecambe, 'Pace'])
     )
   })
   
   output$PlayerShootingBox_morecambe <- renderValueBox({
     valueBox(
       paste0(fifa_data_all_years[[6]][input$ws2_player_select_morecambe, 'Shooting']), "Shooting", icon = icon("meteor"),
       color = map_num_to_col(fifa_data_all_years[[6]][input$ws2_player_select_morecambe, 'Shooting'])
     )
   })
   
   output$PlayerStrengthBox_morecambe  <- renderValueBox({
     valueBox(
       paste0(fifa_data_all_years[[6]][input$ws2_player_select_morecambe, 'Physicality']), "Strength", icon = icon("dumbbell"),
       color = map_num_to_col(fifa_data_all_years[[6]][input$ws2_player_select_morecambe, 'Physicality'])
     )
   })
   output$PlayerTacklingBox_morecambe  <- renderValueBox({
     valueBox(
       paste0(fifa_data_all_years[[6]][input$ws2_player_select_morecambe, 'Defending']), "Defending", icon = icon("shield-halved"),
       color = map_num_to_col(fifa_data_all_years[[6]][input$ws2_player_select_morecambe, 'Defending'])
     )
   })
   
   output$PlayerPassingBox_morecambe <- renderValueBox({
     valueBox(
       paste0(fifa_data_all_years[[6]][input$ws2_player_select_morecambe, 'Passing']), "Passing", icon = icon("futbol"),
       color = map_num_to_col(fifa_data_all_years[[6]][input$ws2_player_select_morecambe, 'Passing'])
     )
   })
   
   output$PlayerDribblingBox_morecambe <- renderValueBox({
     valueBox(
       paste0(fifa_data_all_years[[6]][input$ws2_player_select_morecambe , 'Dribbling']), "Dribbling", icon = icon("wand-magic-sparkles"),
       color = map_num_to_col(fifa_data_all_years[[6]][input$ws2_player_select_morecambe, 'Dribbling'])
     )
   })
   
   output$w2s_player_name_benfica <- renderText({paste("Name: ",fifa_data_all_years[[6]][input$ws2_player_select_benfica, 'Name'])})
   output$w2s_player_club_benfica <- renderText({paste("Club: ",fifa_data_all_years[[6]][input$ws2_player_select_benfica, 'Club'])})
   output$w2s_player_age_benfica <- renderText({paste("Age: ",fifa_data_all_years[[6]][input$ws2_player_select_benfica, 'Age'])})
   output$w2s_player_nation_benfica <- renderText({paste("Nation :",fifa_data_all_years[[6]][input$ws2_player_select_benfica, 'Nationality'])})
   
   
   output$PlayerPaceBox_benfica <- renderValueBox({
     valueBox(
       paste0(fifa_data_all_years[[6]][input$ws2_player_select_benfica, 'Pace']), "Speed", icon = icon("person-running"),
       color = map_num_to_col(fifa_data_all_years[[6]][input$ws2_player_select_benfica, 'Pace'])
     )
   })
   
   output$PlayerShootingBox_benfica <- renderValueBox({
     valueBox(
       paste0(fifa_data_all_years[[6]][input$ws2_player_select_benfica, 'Shooting']), "Shooting", icon = icon("meteor"),
       color = map_num_to_col(fifa_data_all_years[[6]][input$ws2_player_select_benfica, 'Shooting'])
     )
   })
   
   output$PlayerStrengthBox_benfica <- renderValueBox({
     valueBox(
       paste0(fifa_data_all_years[[6]][input$ws2_player_select_benfica, 'Physicality']), "Strength", icon = icon("dumbbell"),
       color = map_num_to_col(fifa_data_all_years[[6]][input$ws2_player_select_benfica, 'Physicality'])
     )
   })
   output$PlayerTacklingBox_benfica <- renderValueBox({
     valueBox(
       paste0(fifa_data_all_years[[6]][input$ws2_player_select_benfica, 'Defending']), "Defending", icon = icon("shield-halved"),
       color = map_num_to_col(fifa_data_all_years[[6]][input$ws2_player_select_benfica, 'Defending'])
     )
   })
   
   output$PlayerPassingBox_benfica <- renderValueBox({
     valueBox(
       paste0(fifa_data_all_years[[6]][input$ws2_player_select_benfica, 'Passing']), "Passing", icon = icon("futbol"),
       color = map_num_to_col(fifa_data_all_years[[6]][input$ws2_player_select_benfica, 'Passing'])
     )
   })
   
   output$PlayerDribblingBox_benfica <- renderValueBox({
     valueBox(
       paste0(fifa_data_all_years[[6]][input$ws2_player_select_benfica, 'Dribbling']), "Dribbling", icon = icon("wand-magic-sparkles"),
       color = map_num_to_col(fifa_data_all_years[[6]][input$ws2_player_select_benfica, 'Dribbling'])
     )
   })

  ###
  # Player progression over generations logic
  ###

  # Just remove this and associated UI if I can't figure out how to make it faster in time


  #updateSelectizeInput(session, 'player_progression_name', choices = fifa_data_all_years[[1]]$Name, server = TRUE)
  # output$name <- renderUI({
  #   selectInput(inputId = "name", label = "Name", choices = my_list())
  # })
  
   # Change the player progression player plot to have the player selected in the previous table
  output$ribbonplot <- renderPlot({
    if (length(input$scoutingtable_rows_selected)) {
      plot_player_rating(filtered_dt()[input$scoutingtable_rows_selected,]$Name)
    }
  })
  

    
  # Ulitimate Scouter logic

  scouting_dt <- fifa_data_all_years[[6]][c('ID','Name', 'Age', 'Nationality', 'Club', 'Overall', 'Potential', 'Value', 'Wage', 'Preferred Position')]

  # This is messy af but I don't want to spend any more time on it :) Gets the correct data based on the filters input by the user
  
  filtered_dt <- reactive(
    if (input$Nation == 'All') {
      if (input$PreferredPosition == 'All') {
        scouting_dt[scouting_dt$Age >= input$Age[1] & scouting_dt$Age <= input$Age[2] & scouting_dt$Value >= input$Value[1] & scouting_dt$Value <= input$Value[2] & scouting_dt$Wage >= input$Wage[1] & scouting_dt$Wage <= input$Wage[2] & scouting_dt$Overall >= input$Overall[1] & scouting_dt$Overall <= input$Overall[2] & scouting_dt$Potential >= input$Potential[1] & scouting_dt$Potential <= input$Potential[2],]
      } else {
        scouting_dt[scouting_dt$Age >= input$Age[1] & scouting_dt$Age <= input$Age[2] & scouting_dt$Value >= input$Value[1] & scouting_dt$Value <= input$Value[2] & scouting_dt$Wage >= input$Wage[1] & scouting_dt$Wage <= input$Wage[2] & scouting_dt$Overall >= input$Overall[1] & scouting_dt$Overall <= input$Overall[2] & scouting_dt$Potential >= input$Potential[1] & scouting_dt$Potential <= input$Potential[2] & scouting_dt$`Preferred Position` == input$PreferredPosition ,]
      }
    } else {
      if (input$PreferredPosition == 'All') {
        scouting_dt[scouting_dt$Age >= input$Age[1] & scouting_dt$Age <= input$Age[2] & scouting_dt$Value >= input$Value[1] & scouting_dt$Value <= input$Value[2] & scouting_dt$Wage >= input$Wage[1] & scouting_dt$Wage <= input$Wage[2] & scouting_dt$Overall >= input$Overall[1] & scouting_dt$Overall <= input$Overall[2] & scouting_dt$Potential >= input$Potential[1] & scouting_dt$Potential <= input$Potential[2] & scouting_dt$Nationality == input$Nation ,]
      } else {
        scouting_dt[scouting_dt$Age >= input$Age[1] & scouting_dt$Age <= input$Age[2] & scouting_dt$Value >= input$Value[1] & scouting_dt$Value <= input$Value[2] & scouting_dt$Wage >= input$Wage[1] & scouting_dt$Wage <= input$Wage[2] & scouting_dt$Overall >= input$Overall[1] & scouting_dt$Overall <= input$Overall[2] & scouting_dt$Potential >= input$Potential[1] & scouting_dt$Potential <= input$Potential[2] & scouting_dt$Nationality == input$Nation & scouting_dt$`Preferred Position` == input$PreferredPosition ,]
      
      }
    }
  )

  # Outputs the data table, borrowed from Ben with some QoL changes
  output$scoutingtable = DT::renderDataTable({
      DT::datatable(
      filtered_dt()[c('Name', 'Age', 'Nationality', 'Club', 'Overall', 'Potential', 'Value', 'Wage', 'Preferred Position')],
      options = list(
      autoWidth = FALSE, scrollX = TRUE, dom='fpt', pageLength = 10, scrollY = '500'),
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
        stats <- fifa_data_all_years[[6]][fifa_data_all_years[[6]]$ID == selected_player$ID,c('Pace', 'Shooting', 'Passing', 'Dribbling', 'Defending', 'Physicality')]
        stats_order <- c(6,5,4,3,2,1) # Ensure the order is preserved from the columns we select
        # plot stats on a lollipop chart

        tp <- data.frame(x=names(stats), y=unname(unlist(stats)))
        tp %>% arrange(stats_order) %>%
        mutate(x = factor(x, unique(x))) %>%
        ggplot(aes(x=x, y=y)) +
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
      if (length(s)) {
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
    } else {
      # If no input is selected just plot the image of the pitch (just makes it look cleaner)
      image_ggplot(img)
    }},
    height = 780,
    )
    
    session$onSessionEnded(function() {
      stopApp()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
