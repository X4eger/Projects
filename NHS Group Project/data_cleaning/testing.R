library(shiny)
library(tidyverse)
library(bslib)
library(CodeClanData)

jamie_theme <- theme(panel.background = element_rect(fill = "white"),
                     panel.grid = element_line(colour = "#ededed"),
                     strip.background = element_rect(fill = "white")
) 


nhs_specialty <- read_csv(here::here("../clean_data/nhs_speciality.csv"))
nhs_deprivation <- read_csv(here::here("../clean_data/nhs_deprivation.csv"))
nhs_age_sex <- read_csv(here::here("../clean_data/nhs_age_sex.csv"))
nhs_bed_occupancy <- read_csv(here::here("../clean_data/nhs_bed_occupancy.csv"))


## my theme 

ui <- fluidPage(
  titlePanel("Game Sales"),
  theme = bs_theme(bootswatch = "minty", version = 5),
  tabsetPanel(    
    tabPanel(# first tab contains a graph that lets the user see the total 
      # sales of games for the subcategories in 5 different category types 
      "Total Game Sales",
      selectInput(
        inputId = "specialty",
        label = "Specialty:",
        choices = nhs_bed_occupancy %>% distinct(specialty_name) %>% pull()
      ),
      plotOutput("plot")
    ),
    tabPanel(# second tab shows user a graph of average game rating vs
      # year of release. The user can use the layered inputs to 
      # customize the graph in a variety of ways
      "Scores Over Time",
      selectInput(
        inputId = "location",
        label = "Location:",
        choices = nhs_specialty %>% distinct(location_name) %>% pull()
      ),
      selectInput(
        inputId = "admission_type",
        label = "Admission Type:",
        choices = nhs_specialty %>% distinct(admission_type) %>% pull()
      ),
      selectInput(
        inputId = "selection1",
        label = "Selection 1:",
        choices = nhs_specialty %>% distinct(specialty_name) %>% pull()
      ),
      plotOutput("plot2")
    ),
    tabPanel(# third tab pulls a random game from the data and shows its
      # row in the table
      "Random Game Selector",
      sidebarLayout(
        sidebarPanel(
          width = 3,
          actionButton(
            inputId = "randomizer",
            label = "Randomizer")
        ),
        mainPanel(
          width = 9, 
          DT::DTOutput(
            outputId = "random_game"
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
 
  output$plot <- renderPlot(
    nhs_bed_occupancy %>% 
      filter(location_name == "Scotland",
             str_detect(quarter, "201[6-9]"),
             specialty_name == input$specialty) %>% 
      ggplot() +
      geom_line(aes(x = quarter, y = percentage_occupancy, group = 1)) +
      geom_line(aes(x = quarter, y = 81.74, group = 1), colour = "red", size = 2) +
      scale_y_continuous(limits = c(NA, NA))
  )
  
  
  observe({ #updates select input based on previous select input
    updateSelectInput(session, "genre", choices = as.character(
      CodeClanData::game_sales %>% 
        rename("x" = "developer") %>% 
        distinct(x) %>% 
        pull(x)
    ))
  })
  
  output$plot2 <- renderPlot(
    nhs_specialty %>%
      filter(location_name == input$location,
             admission_type == input$admission_type,
             specialty_name == input$selection1,
             str_detect(quarter, "201[6-9]")) %>% 
      ggplot() +
      geom_line(aes(x = quarter, y = spells, group = 1)) +
      scale_y_continuous(limits = c(0, NA))
    )
  
  randomizer <- eventReactive(
    eventExpr = input$randomizer,
    CodeClanData::game_sales %>% 
      sample_n(1)
  )
  
  output$random_game <- DT::renderDT(
    randomizer()
  ) 
  
  
  
}

shinyApp(ui, server)



