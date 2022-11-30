#-- Libraries -----------------------------
library(shiny)
library(tidyverse)
library(bslib)
library(here)
library(gghighlight)
library(sf)
library(lubridate)
library(tsibble)
library(fable)
library(scales)

# -- home data and extra libraries -----

nhs_bed_occupancy <- read_csv(here::here("../../clean_data/nhs_bed_occupancy.csv"))
health_board_names <- read_csv(here::here("../../raw_data/health_board_codes.csv"))
nhs_stays_hb <- read_csv(here::here("../../clean_data/nhs_speciality.csv"))
scotland_shape <- st_read(here::here("../../shape_files/SG_NHS_HealthBoards_2019/SG_NHS_HealthBoards_2019.shp"))
hb_pop_formatted <- read_csv(here::here("../../clean_data/hb_pop_formatted.csv"))

covid_healthboards <- read_csv(here::here("../../clean_data/daily_covid_health_board.csv"))

age_sex <- read_csv("../../clean_data/nhs_age_sex.csv")
deprivation <- read_csv("../../clean_data/nhs_deprivation.csv")

options(scipen = 10)


health_boards <- c(health_board_names %>% distinct(HBName) %>% pull)
regions <- c(health_board_names %>% distinct(HBName) %>% pull, "Scotland")
specialties <- nhs_stays_hb %>% distinct(specialty_name) %>% pull()
admission_type <- nhs_stays_hb %>% distinct(admission_type) %>% pull()
age_brackets <- age_sex %>% distinct(age) %>% pull()
deprivation_cats <- deprivation %>% drop_na(simd) %>% distinct(simd) %>% pull()

  # Shape File
healthboards <- st_read("../../shape_files/SG_NHS_HealthBoards_2019/SG_NHS_HealthBoards_2019.shp")
healthboards$HBName <- as.character(healthboards$HBName)
healthboards$HBName <-  str_c("NHS ", healthboards$HBName)
#--------- Theme ---------------------------

nhs_theme <- function(){
  theme(
    plot.background = element_rect(fill = "#F0F4F5", colour = "#F0F4F5"),
    panel.background = element_rect("white"),
    panel.grid.major = element_line("#E8EDEE"),
    panel.grid.minor = element_line("#E8EDEE"),
    panel.border = element_rect("#768692", fill = NA),
    legend.background = element_rect("#EDF1F1"),
    plot.title = element_text(size = 18),
    plot.subtitle = element_text(size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 14, vjust = -1),
    axis.title.y = element_text(size = 14)
  )
}
#-- Headline -------------------------
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
                    body{
                    background-color: #F0F4F5;
                    color: #212B32;"))
  ),
  tags$br(),
  titlePanel(tags$b("NHS Data Project - PHS Winter and Covid Impact")),
  tags$br(),
  tabsetPanel(
# -- Home page/ Tab 1 UI Below -----------------------------------------
tabPanel(
  "Home",
  tags$br(),
  tags$br(),
  fluidRow(
    column(
      width = 3,
      p("PHS WACIA (Winter and Covid Impact App) – brings together a 
            range of data \nto visualise the impact of winter on the NHS 
            between 2017 and 2021. Pre covid, posts covid and demographic 
            investigation is possible using the tabs."),
        
        p("The App is interactive 
            and allows user exploration to select several filters for 
            independent or team review. This can then be used as a tool for 
            service planning.")
    ),
      column(
        width = 2,
      selectInput(
        inputId = "specialty",
        label = "Select Specialty for all Visuals",
        choices = "All",
        selected = "All"
      )
    ),
    column(
      offset = 2,
      width = 3,
      selectInput(
        inputId = "region",
        label = "Select Region for Graphs",
        choices = regions,
        multiple = TRUE,
        selected = "Scotland"
      )
    ),
  ),
  tags$br(),
  tags$br(),
  fluidRow(
    column(
      offset = 1,
      width = 1,
      radioButtons(
        inputId = "year",
        label = "Select Year for Map",
        choices = c("2017", "2018", "2019")
      ),
      tags$br(),
      tags$br(),
      radioButtons(
        inputId = "quarter",
        label = "Select Quarter for Map",
        choices = c("Q1", "Q2", "Q3", "Q4")
      )
    ),
    column(
      width = 3,
      plotOutput("interactive_map", width = "65%")
    ),
    column(
      width = 1
    ),
    column(
      width = 6,
      plotOutput("bed_occupancy_home")
    )
  ),
  fluidRow(
    column(
      width = 6,
      plotOutput("hospital_stays_home")
    ),
    column(
      width = 6,
      plotOutput("staffed_bed_hours_home")
    )
  ),
  tags$br(),
  tags$br(),
  tags$br()
),
# ---   Impact of Covid - Tab 2 UI Below -------------------------------
tabPanel(
  "Impacts of Covid 19 Pandemic",
  tags$br(),
  tags$br(),
  fluidRow(
    column(
      width = 6,
      plotOutput("scotland_positives", width = "100%", height = "200px")
    ),
    column(
      width = 6,
      plotOutput("scotland_deaths", width = "100%", height = "200px")
    )
    ## Top of the page, covid plot for covid cases over the year
  ),
  tags$br(),
  tags$br(),
  fluidRow(
    sidebarLayout(
      sidebarPanel(
        width = 2,
      selectInput(
        inputId = "tab2region",
        label = "Select Region",
        choices = regions,
        selected = "Scotland"
      ),
      selectInput(
        inputId = "tab2spec",
        label = "Select Specialty",
        choices = "All",
        selected = "All"
      ),
      selectInput(
        inputId = "forecast",
        label = "Show Non-Covid Hypothetical",
        choices = c("Yes" = "yes", "No" = "no"),
        selected = "no"
      ),
        plotOutput("tab2_map")
    ),

  mainPanel(
    column(
      width = 2,
      tags$br(),
      p("Covid data is included in this tab along with the positive cases and
        deaths to provide further context to the timeline of the pandemic in 
        Scotland. The exploration filters are similar other than the 
        introduction of a forecast model to consider the impacts with/without 
        Covid.")
    ),
    column(
      offset = 0.5,
    width = 10,
    fluidRow(
      plotOutput("hospital_stays_tab2", width = "100%", height = "300px")
    ),
    tags$br(),
    tags$br(),
    fluidRow(
      plotOutput("bed_occupancy_tab2", width = "100%", height = "300px")
    ),
    tags$br(),
    tags$br(),
    fluidRow(
      plotOutput("staffed_bed_hours_tab2", width = "100%", height = "300px")
    ),
    tags$br(),
    tags$br(),
    tags$br()
  )
    )
  )
  )
),


# --- Demographic Breakdown - Tab 3 UI Below ---------------------------
    tabPanel(
      "Demographic Breakdown",
      tags$br(),
      tags$br(),
      fluidRow(
        column(
          width = 3,
        p("Demographic information is included here. This provides insights 
          into the characteristics of the Scottish population 
          (such as age and gender) SIMD (Scottish Index on Multiple Deprivation)
          is also included to consider if covid had a disproportionate impact 
          on people in areas of prosperity or deprivation.")
        ),
        column(
          offset = 1,
          width = 3,
          selectInput(
            inputId = "regiondem",
            label = "Select Region",
            choices = regions,
            selected = "Scotland"
          )
         ),
        column(
          width = 3,
        radioButtons(
          inputId = "covid",
          label = "Select Time Frame", 
          choices = c("All" = "20[1-2][01789]", "pre-COVID" = "201[7-9]", "COVID" = "202[0-1]"),
          inline = TRUE
        )
        )
        ),
      fluidRow(
        column(
          width = 2,
          tags$br(),
          tags$br(),
          tags$br(),
          tags$br(),
          tags$br(),
          tags$br(),
          selectInput(
            inputId = "sex",
            label = "Select Sex",
            choices = c("Male", "Female"),
            multiple = TRUE,
            selected = c("Male", "Female")
          ),
          tags$br(),
          radioButtons(
            inputId = "rate_sex",
            label = "Raw Numbers or Rate per 1000people?",
            choices = c("Raw Numbers", "Rate")
          )
        ),
        tags$br(),
        column(
          width = 9,
          plotOutput("bed_occupancy_dem")
          )
        ),
      tags$br(),
      fluidRow(
        column(
          width = 2,
          tags$br(),
          tags$br(),
          tags$br(),
          tags$br(),
          tags$br(),
          tags$br(),
          selectInput(
            inputId = "age",
            label = "Age Ranges",
            choices = age_brackets,
            multiple = TRUE,
            selected = age_brackets
          ),
          tags$br(),
          radioButtons(
            inputId = "rate_age",
            label = "Raw Numbers or Rate per 1000 people?",
            choices = c("Raw Numbers", "Rate")
          )
        ),
        tags$br(),
        column(
          width = 9,
          plotOutput("hospital_stays_dem")
        )
      ),
      tags$br(),
      fluidRow(
        column(
          width = 2,
          tags$br(),
          tags$br(),
          tags$br(),
          tags$br(),
          tags$br(),
          tags$br(),
          selectInput(
            inputId = "deprivation",
            label = "Deprivation category",
            choices = deprivation_cats,
            multiple = TRUE,
            selected = deprivation_cats
          )
        ),
        tags$br(),
        column(
          width = 9,
          plotOutput("staffed_bed_hours_dem")
        )
      ),
      tags$br(),
      tags$br(),
      tags$br()
      )
    )
#--------------

)





server <- function(input, output, session) {

#-- Home Page Server Below --------------------------------------------

  observe({ #updates select input based on previous select input
    updateSelectInput(session, "specialty", choices = as.character(
      c("All",
        nhs_stays_hb %>%
        filter(location_name %in% c(input$region, "A")) %>%
        distinct(specialty_name) %>%
        pull(specialty_name))
    ))
  })


  ## plot 1 bed occupancy

    output$bed_occupancy_home <- renderPlot(
      if(input$specialty == "All"){
        nhs_bed_occupancy %>% 
          filter(location_name %in% input$region,
                 specialty_name %in% specialties,
                 str_detect(quarter, "201[7-9]")) %>%
          group_by(location_name, quarter) %>% 
          summarise(percentage_occupancy = sum(total_occupied_beddays) / sum(all_staffed_beddays) * 100) %>% 
        ggplot() +
        geom_line(aes(x = quarter, y = percentage_occupancy, group = location_name, colour = location_name)) +
        labs(
            x = "Timeline",
            y = "Occupied Percentage",
            title = "Percentage of beds occupied",
            subtitle = "2017 to 2019",
          )+
          guides(
            colour = 
            guide_legend("Region")
          )+
        scale_y_continuous(limits = c(0, 100),
                           labels = scales::label_comma()) +
          nhs_theme()
    } else {  
    nhs_bed_occupancy %>% 
      filter(location_name %in% input$region,
             str_detect(quarter, "201[7-9]"),
             specialty_name == input$specialty) %>%
      ggplot() +
      geom_line(aes(x = quarter, y = percentage_occupancy, group = location_name, colour = location_name)) +
      scale_y_continuous(limits = c(0, NA)) +
      labs(
          x = "Timeline",
          y = "Occupied Percentage",
          title = "Percentage of beds occupied",
          subtitle = "2017 to 2019"
        )+
        guides(
          colour = 
            guide_legend("Region")
        )+
      scale_y_continuous(limits = c(0, NA),
                         labels = scales::label_comma()) +
        nhs_theme()
    }
    )

    ## plot 2 spell numbers

  output$hospital_stays_home <- renderPlot(
    if(input$specialty == "All"){
    nhs_stays_hb %>% 
      filter(location_name %in% input$region,
             str_detect(quarter, "201[7-9]"),
             admission_type == "All Inpatients and Day cases") %>%
        group_by(location_name, quarter) %>% 
        summarise(spells = sum(spells)) %>% 
      ggplot() +
      geom_line(aes(x = quarter, y = spells, group = location_name, colour = location_name)) +
        labs(
          x = "Timeline",
          y = "Stays",
          title = "Hospital stays",
          subtitle = "2017 to 2019"
        )+
        guides(
          colour = 
            guide_legend("Region")
        )+
      scale_y_continuous(limits = c(0, NA),
                         labels = scales::label_comma())+
        nhs_theme()
    } else {
      nhs_stays_hb %>% 
        filter(location_name %in% input$region,
               str_detect(quarter, "201[7-9]"),
               admission_type == "All Inpatients and Day cases",
               specialty_name == input$specialty) %>%
        ggplot() +
        geom_line(aes(x = quarter, y = spells, group = location_name, colour = location_name)) +
        labs(
          x = "Timeline",
          y = "Stays",
          title = "Hospital stays",
          subtitle = "2017 to 2019"
        )+
        guides(
          colour = 
            guide_legend("Region")
        )+
        scale_y_continuous(limits = c(0, NA),
                           labels = scales::label_comma())+
        nhs_theme()

  }
  )

  # plot 3 staffed bed hours

  output$staffed_bed_hours_home <- renderPlot(
    if(input$specialty %in% "All"){
      nhs_bed_occupancy %>% 
        filter(location_name %in% input$region,
               specialty_name %in% specialties,
               str_detect(quarter, "201[7-9]")) %>%
        group_by(location_name, quarter) %>% 
        summarise(all_staffed_beddays = sum(all_staffed_beddays) / 1000) %>% 
        ggplot() +
        geom_line(aes(x = quarter, y = all_staffed_beddays, group = location_name, colour = location_name)) +
        labs(
          x = "Timeline",
          y = "Staffed bed hours",
          title = "Total staffed bed hours",
          subtitle = "2017 to 2019"
        )+
        guides(
          colour = 
            guide_legend("Region")
        )+
        scale_y_continuous(limits = c(0, NA),
                           labels = scales::label_comma())+
        nhs_theme()
    } else {  
      nhs_bed_occupancy %>% 
        filter(location_name %in% input$region,
               str_detect(quarter, "201[7-9]"),
               specialty_name == input$specialty) %>%
        mutate(all_staffed_beddays = all_staffed_beddays/1000) %>% 
        group_by(location_name) %>% 
        ggplot() +
        geom_line(aes(x = quarter, y = all_staffed_beddays, group = location_name, colour = location_name)) +
        labs(
          x = "Timeline",
          y = "Staffed bed hours",
          title = "Total Staffed bed hours",
          subtitle = "2017 to 2019"
        )+
        guides(
          colour = 
            guide_legend("Region")
        )+
        scale_y_continuous(limits = c(0, NA),
                           labels = scales::label_comma()) +
    nhs_theme()    
    }
  )

  # map plot

  output$interactive_map <- renderPlot(
    if(input$specialty == "All"){
      nhs_bed_occupancy %>%
       filter(location_name %in% health_boards,
               specialty_name %in% specialties,
               str_detect(quarter, "201[7-9]"),
               quarter == paste(input$year, input$quarter)) %>%
        group_by(location_name, hb, quarter) %>%
        summarise(percentage_occupancy = sum(total_occupied_beddays) / sum(all_staffed_beddays) * 100, .groups = 'drop') %>%
        ungroup() %>%
        right_join(scotland_shape, by = c("hb" = "HBCode")) %>%
        ggplot(aes(fill = percentage_occupancy, geometry = geometry)) +
        geom_sf() +
        scale_fill_gradient2(
          name = "Percentage",
          low = "#FDEFEC",
          mid = "#DA9485",
          high = "#541102",
          midpoint = 80,
          space = "Lab",
          na.value = "grey50",
          guide = "colourbar",
          aesthetics = "fill"
        ) +
        theme_void() +
        theme(plot.background = element_rect(fill = "#E5F1FA")) +
        labs(title = "Bed Occupancy by Region")
      

      
    } else {
      nhs_bed_occupancy %>%
        filter(location_name %in% health_boards,
               str_detect(quarter, "201[7-9]"),
               specialty_name == input$specialty,
               quarter == paste(input$year, input$quarter)) %>%
        select(hb, percentage_occupancy) %>%
        right_join(scotland_shape, by = c("hb" = "HBCode")) %>%
        ggplot() +
        geom_sf(aes(fill = percentage_occupancy, geometry = geometry)) +
        scale_fill_gradient2(
          name = "Percentage",
          low = "#FDEFEC",
          mid = "#DA9485",
          high = "#541102",
          midpoint = 80,
          space = "Lab",
          na.value = "grey50",
          guide = "colourbar",
          aesthetics = "fill"
        )+
        theme_void() +
        theme(plot.background = element_rect(fill = "#E5F1FA")) +
        labs(title = "Bed Occupancy by Region")
    }
  )



#-- Covid Impact Server Below ----------------------------------------

  observe({ #updates select input based on previous select input
    updateSelectInput(session, "tab2spec", choices = as.character(
      c("All",
        nhs_stays_hb %>%
          filter(location_name == input$tab2region) %>%
          distinct(specialty_name) %>%
          pull(specialty_name))
    ))
  })
  

  

  # Scotland Covid Cases Plot
  
 output$scotland_positives <- renderPlot(
   covid_healthboards %>%
     filter(hb_name == "Scotland") %>%
     group_by(date) %>%
     summarise(monthly_cases = sum(daily_positive)) %>%
     ggplot(aes(x = date, y = monthly_cases, group = 1))+
     geom_line()+
     labs(
       x= "Timeline",
       y = "Cases",
       title = "Positive Covid cases per day"
     )+
     nhs_theme()+
     theme(
       axis.text.x = element_text(hjust = 1)
     )+
     scale_y_continuous(labels = scales::label_comma())
 )
  # Scotland Covid Deaths Plot
 
  output$scotland_deaths <- renderPlot(
    covid_healthboards %>%
      filter(hb_name == "Scotland") %>%
      group_by(date) %>%
      summarise(monthly_deaths = sum(daily_deaths)) %>%
      ggplot(aes(x = date, y = monthly_deaths, group = 1))+
      geom_line()+
      labs(
        x= "Timeline",
        y = "Deaths",
        title = "Covid related deaths per day"
      )+
      nhs_theme()+
      theme(
        axis.text.x = element_text(hjust = 1)
      )
  )
    
  # Bed Occupancy Plot
  
  output$bed_occupancy_tab2 <- renderPlot(
    
    if(input$tab2spec == "All"){
      if(input$forecast == "yes"){
        nhs_forecast <- nhs_bed_occupancy %>% 
          filter(location_name == input$tab2region,
                 specialty_name %in% specialties) %>% 
          group_by(quarter) %>%
          summarise(percentage_occupancy = sum(total_occupied_beddays) / sum(all_staffed_beddays) * 100)
        
        
        my_ts <- ts(data = nhs_forecast %>% pull(percentage_occupancy),
                    start = c(2016, 4), frequency = 4) %>%
          as_tsibble() %>% 
          filter(str_detect(index, "201"))
        
        my_ts2 <- ts(data = nhs_forecast %>% pull(percentage_occupancy),
                     start = c(2016, 4), frequency = 4) %>%
          as_tsibble() 
        
        fit <- my_ts %>% 
          model(ETS(value ~ error("A") + trend("A") + season("A")))
        
        forecast_2years <- fit %>%
          fabletools::forecast(h = 8) %>% 
          mutate(con = hilo(value, 95)) %>%
          unpack_hilo(con)
        
        forecast <- as_tibble(forecast_2years) %>% 
          select(index, value_forecast = '.mean', con_lower, con_upper) %>% 
          mutate(value = NA)
        
        my_ts2 %>% 
          left_join(forecast, by = "index") %>% 
          ggplot() +
          geom_line(aes(x = index, y = value.x), colour = "red") +
          geom_line(aes(x = index, y = value_forecast), colour = "blue") +
          geom_ribbon(aes(x = index, ymin=con_lower,ymax= con_upper),alpha=0.3, fill = "blue")+
          labs(
            x = "Timeline",
            y = "Occupied Percentage",
            title = "Percentage of beds occupied"
          )+
          nhs_theme()+
          scale_y_continuous(limits = c(0, 100))
      } else{ 
        nhs_bed_occupancy %>%
          filter(location_name == input$tab2region,
                 specialty_name %in% specialties,
                 quarter != "2016 Q4") %>%
          group_by(quarter) %>%
          summarise(percentage_occupancy = sum(total_occupied_beddays) / sum(all_staffed_beddays) * 100) %>%
          ggplot() +
          geom_line(aes(x = quarter, y = percentage_occupancy, group = 1)) +
          labs(
            x = "Timeline",
            y = "Occupied Percentage",
            title = "Percentage of beds occupied"
          )+
          geom_vline(xintercept = "2020 Q1",
                     col = "firebrick",
                     lwd = 1)+
          annotate("text",
                   x = "2020 Q2",
                   y = 0,
                   label = paste("COVID"),
                   col = "firebrick",
                   size = 3)+
          scale_y_continuous(limits = c(0, 100))+
          nhs_theme()+
          theme(
            axis.text.x = element_text(angle = -45, hjust = 0, vjust = 2)
          )
      }
    } else {
      if(input$forecast == "yes"){
        
        nhs_forecast <- nhs_bed_occupancy %>% 
          filter(location_name == input$tab2region,
                 specialty_name == input$tab2spec) 
        
        
        my_ts <- ts(data = nhs_forecast %>% pull(percentage_occupancy),
                    start = c(2016, 4), frequency = 4) %>%
          as_tsibble() %>% 
          filter(str_detect(index, "201"))
        
        my_ts2 <- ts(data = nhs_forecast %>% pull(percentage_occupancy),
                     start = c(2016, 4), frequency = 4) %>%
          as_tsibble() 
        
        fit <- my_ts %>% 
          model(ETS(value ~ error("A") + trend("A") + season("A")))
        
        forecast_2years <- fit %>%
          fabletools::forecast(h = 8) %>% 
          mutate(con = hilo(value, 95)) %>%
          unpack_hilo(con)
        
        forecast <- as_tibble(forecast_2years) %>% 
          select(index, value_forecast = '.mean', con_lower, con_upper) %>% 
          mutate(value = NA)
        
        my_ts2 %>% 
          left_join(forecast, by = "index") %>% 
          ggplot() +
          geom_line(aes(x = index, y = value.x), colour = "red") +
          geom_line(aes(x = index, y = value_forecast), colour = "blue") +
          geom_ribbon(aes(x = index, ymin=con_lower,ymax= con_upper),alpha=0.3, fill = "blue")+
          labs(
            x = "Timeline",
            y = "Occupied Percentage",
            title = "Percentage of beds occupied"
          )+
          nhs_theme()+
          scale_y_continuous(limits = c(0, NA))
        
      }else {
        nhs_bed_occupancy %>%
          filter(location_name == input$tab2region,
                 specialty_name == input$tab2spec,
                 quarter != "2016 Q4") %>%
          ggplot() +
          geom_line(aes(x = quarter, y = percentage_occupancy, group = 1)) +
          labs(
            x = "Timeline",
            y = "Occupied Percentage",
            title = "Percentage of beds occupied"
          )+
          geom_vline(xintercept = "2020 Q1",
                     col = "firebrick",
                     lwd = 1)+
          annotate("text",
                   x = "2020 Q2",
                   y =  0,
                   label = paste("COVID"),
                   col = "firebrick",
                   size = 3)+
          scale_y_continuous(limits = c(0, NA))+
          nhs_theme()+
          theme(
            axis.text.x = element_text(angle = -45,  hjust = 0, vjust = 2)
          )
      }
    }
    
  )
  
  # Hospital Stays Plot
  
  output$hospital_stays_tab2 <- renderPlot(
    if(input$tab2spec == "All"){
      if(input$forecast == "yes"){
        nhs_forecast <- nhs_stays_hb %>% 
          filter(location_name == input$region,
                 admission_type == "All Inpatients and Day cases") %>% 
          group_by(quarter) %>% 
          summarise(spells = sum(spells)) 
        
        
        my_ts <- ts(data = nhs_forecast %>% pull(spells),
                    start = c(2016, 4), frequency = 4) %>%
          as_tsibble() %>% 
          filter(str_detect(index, "201"))
        
        my_ts2 <- ts(data = nhs_forecast %>% pull(spells),
                     start = c(2016, 4), frequency = 4) %>%
          as_tsibble() 
        
        fit <- my_ts %>% 
          model(ETS(value ~ error("A") + trend("A") + season("A")))
        
        forecast_2years <- fit %>%
          fabletools::forecast(h = 8) %>% 
          mutate(con = hilo(value, 95)) %>%
          unpack_hilo(con)
        
        forecast <- as_tibble(forecast_2years) %>% 
          select(index, value_forecast = '.mean', con_lower, con_upper) %>% 
          mutate(value = NA)
        
        my_ts2 %>% 
          left_join(forecast, by = "index") %>% 
          ggplot() +
          geom_line(aes(x = index, y = value.x), colour = "red") +
          geom_line(aes(x = index, y = value_forecast), colour = "blue") +
          geom_ribbon(aes(x = index, ymin=con_lower,ymax= con_upper),alpha=0.3, fill = "blue")+
          labs(
            x = "Timeline",
            y = "Stays",
            title = "Hospital stays",
            subtitle = "2017 to 2022"
          )+
          scale_y_continuous(labels = scales::label_comma(),
                             limits = c(0, NA))+
          nhs_theme()
      } else{
      nhs_stays_hb %>%
        filter(location_name == input$tab2region,
               admission_type == "All Inpatients and Day cases",
               quarter != "2016 Q4") %>%
        group_by(quarter) %>%
        summarise(spells = sum(spells)) %>%
        ggplot() +
        geom_line(aes(x = quarter, y = spells, group = 1)) +
        labs(
          x = "Timeline",
          y = "Stays",
          title = "Hospital stays",
          subtitle = "2017 to 2022"
        )+
        geom_vline(xintercept = "2020 Q1",
                   col = "firebrick",
                   lwd = 1)+
        annotate("text",
                 x = "2020 Q2",
                 y = 0,
                 label = paste("COVID"),
                 col = "firebrick",
                 size = 3)+
        scale_y_continuous(labels = scales::label_comma(), 
                           limits = c(0, NA))+
        nhs_theme()+
          theme(
            axis.text.x = element_text(angle = -45,  hjust = 0, vjust = 2)
          )
      }
    } else {
      if(input$forecast == "yes"){
        nhs_forecast <- nhs_stays_hb %>% 
          filter(location_name == input$region,
                 admission_type == "All Inpatients and Day cases",
                 specialty_name == input$tab2spec) %>% 
          group_by(quarter) %>% 
          summarise(spells = sum(spells)) 
        
        
        my_ts <- ts(data = nhs_forecast %>% pull(spells),
                    start = c(2016, 4), frequency = 4) %>%
          as_tsibble() %>% 
          filter(str_detect(index, "201"))
        
        my_ts2 <- ts(data = nhs_forecast %>% pull(spells),
                     start = c(2016, 4), frequency = 4) %>%
          as_tsibble() 
        
        fit <- my_ts %>% 
          model(ETS(value ~ error("A") + trend("A") + season("A")))
        
        forecast_2years <- fit %>%
          fabletools::forecast(h = 8) %>% 
          mutate(con = hilo(value, 95)) %>%
          unpack_hilo(con)
        
        forecast <- as_tibble(forecast_2years) %>% 
          select(index, value_forecast = '.mean', con_lower, con_upper) %>% 
          mutate(value = NA)
        
        my_ts2 %>% 
          left_join(forecast, by = "index") %>% 
          ggplot() +
          geom_line(aes(x = index, y = value.x), colour = "red") +
          geom_line(aes(x = index, y = value_forecast), colour = "blue") +
          geom_ribbon(aes(x = index, ymin=con_lower,ymax= con_upper),alpha=0.3, fill = "blue")+
          labs(
            x = "Timeline",
            y = "Stays",
            title = "Hospital stays",
            subtitle = "2017 to 2022"
          )+
          nhs_theme()+
          scale_y_continuous(limits = c(0,NA),
                             labels = scales::label_comma())
      } else{
      nhs_stays_hb %>%
        filter(location_name == input$tab2region,
               admission_type == "All Inpatients and Day cases",
               specialty_name == input$tab2spec,
               quarter != "2016 Q4") %>%
        ggplot() +
        geom_line(aes(x = quarter, y = spells, group = 1)) +
        labs(
          x = "Timeline",
          y = "Stays",
          title = "Hospital stays",
          subtitle = "2017 to 2022"
        )+
        geom_vline(xintercept = "2020 Q1",
                   col = "firebrick",
                   lwd = 1)+
        annotate("text",
                 x = "2020 Q2",
                 y = 0,
                 label = paste("COVID"),
                 col = "firebrick",
                 size = 3)+
        scale_y_continuous(labels = scales::label_comma()
                           , limits = c(0, NA))+
        nhs_theme()+
          theme(
            axis.text.x = element_text(angle = -45,  hjust = 0, vjust = 2)
          )
      }
    }
  )
  # Staffed Bed Hours Plot 
  
  output$staffed_bed_hours_tab2 <- renderPlot(
    if(input$tab2spec == "All"){
      if(input$forecast == "yes"){
        nhs_forecast <- nhs_bed_occupancy %>% 
          filter(location_name == input$tab2region,
                 specialty_name %in% specialties) %>% 
          group_by(quarter) %>%
          summarise(all_staffed_beddays = sum(all_staffed_beddays)/100) 
        
        
        my_ts <- ts(data = nhs_forecast %>% pull(all_staffed_beddays),
                    start = c(2016, 4), frequency = 4) %>%
          as_tsibble() %>% 
          filter(str_detect(index, "201"))
        
        my_ts2 <- ts(data = nhs_forecast %>% pull(all_staffed_beddays),
                     start = c(2016, 4), frequency = 4) %>%
          as_tsibble() 
        
        fit <- my_ts %>% 
          model(ETS(value ~ error("A") + trend("A") + season("A")))
        
        forecast_2years <- fit %>%
          fabletools::forecast(h = 8) %>% 
          mutate(con = hilo(value, 95)) %>%
          unpack_hilo(con)
        
        forecast <- as_tibble(forecast_2years) %>% 
          select(index, value_forecast = '.mean', con_lower, con_upper) %>% 
          mutate(value = NA)
        
        my_ts2 %>% 
          left_join(forecast, by = "index") %>% 
          ggplot() +
          geom_line(aes(x = index, y = value.x), colour = "red") +
          geom_line(aes(x = index, y = value_forecast), colour = "blue") +
          geom_ribbon(aes(x = index, ymin=con_lower,ymax= con_upper),alpha=0.3, fill = "blue")+
          labs(
            x = "Timeline",
            y = "Staffed bed hours (per 100)",
            title = "Total Staffed bed hours",
            subtitle = "2017 to 2022"
          )+
          nhs_theme()+
          scale_y_continuous(limits = c(0, NA),
                             labels = scales::label_comma())
      }else{
      nhs_bed_occupancy %>%
        filter(location_name == input$tab2region,
               specialty_name %in% specialties,
               quarter != "2016 Q4") %>%
        group_by(quarter) %>%
        summarise(all_staffed_beddays = sum(all_staffed_beddays)) %>%
        ggplot() +
        geom_line(aes(x = quarter, y = all_staffed_beddays/100, group = 1)) +
        labs(
          x = "Timeline",
          y = "Staffed bed hours (per 100)",
          title = "Total Staffed bed hours",
          subtitle = "2017 to 2022"
        )+
        geom_vline(xintercept = "2020 Q1",
                   col = "firebrick",
                   lwd = 1)+
        annotate("text",
                 x = "2020 Q2",
                 y = 0,
                 label = paste("COVID"),
                 col = "firebrick",
                 size = 3)+
        scale_y_continuous(limits = c(0, NA),
                           labels = scales::label_comma())+
        nhs_theme()+
          theme(
            axis.text.x = element_text(angle = -45, hjust = 0, vjust = 2)
          )
      }
    } else {
      if(input$forecast == "yes"){
        nhs_forecast <- nhs_bed_occupancy %>% 
          filter(location_name == input$tab2region,
                 specialty_name == input$tab2spec) %>%
          mutate(all_staffed_beddays = all_staffed_beddays/100)
        
        
        my_ts <- ts(data = nhs_forecast %>% pull(all_staffed_beddays),
                    start = c(2016, 4), frequency = 4) %>%
          as_tsibble() %>% 
          filter(str_detect(index, "201"))
        
        my_ts2 <- ts(data = nhs_forecast %>% pull(all_staffed_beddays),
                     start = c(2016, 4), frequency = 4) %>%
          as_tsibble() 
        
        fit <- my_ts %>% 
          model(ETS(value ~ error("A") + trend("A") + season("A")))
        
        forecast_2years <- fit %>%
          fabletools::forecast(h = 8) %>% 
          mutate(con = hilo(value, 95)) %>%
          unpack_hilo(con)
        
        forecast <- as_tibble(forecast_2years) %>% 
          select(index, value_forecast = '.mean', con_lower, con_upper) %>% 
          mutate(value = NA)
        
        my_ts2 %>% 
          left_join(forecast, by = "index") %>% 
          ggplot() +
          geom_line(aes(x = index, y = value.x), colour = "red") +
          geom_line(aes(x = index, y = value_forecast), colour = "blue") +
          geom_ribbon(aes(x = index, ymin=con_lower,ymax= con_upper),alpha=0.3, fill = "blue")+
          labs(
            x = "Timeline",
            y = "Staffed bed hours (per 100)",
            title = "Total Staffed bed hours",
            subtitle = "2017 to 2022"
          )+
          nhs_theme()+
        scale_y_continuous(limits = c(0, NA),
                           labels = scales::label_comma())
      }else{
      nhs_bed_occupancy %>%
        filter(location_name == input$tab2region,
               specialty_name == input$tab2spec,
               quarter != "2016 Q4") %>%
        ggplot() +
        geom_line(aes(x = quarter, y = all_staffed_beddays/100, group = 1)) +
        labs(
          x = "Timeline",
          y = "Staffed bed hours (per 100)",
          title = "Total Staffed bed hours",
          subtitle = "2017 to 2022"
        )+
        geom_vline(xintercept = "2020 Q1",
                   col = "firebrick",
                   lwd = 1)+
        annotate("text",
                 x = "2020 Q2",
                 y = 0,
                 label = paste("COVID"),
                 col = "firebrick",
                 size = 3)+
        scale_y_continuous(limits = c(0, NA),
                           labels = scales::label_comma())+
        nhs_theme()+
          theme(
            axis.text.x = element_text(angle = -45, hjust = 0, vjust = 2)
          )
      }
    }
  )
  
  # Map Plot
  
  output$tab2_map <- renderPlot(
        if(input$tab2region == "Scotland"){
          healthboards %>%
            ggplot(aes(fill = HBName))+
            geom_sf()+
            guides(fill = "none")+
            theme(
              plot.background = element_rect("#F0F4F5"),
              panel.background = element_rect("#E5F1FA"),
              panel.grid.major = element_line("#E8EDEE"),
              panel.grid.minor = element_line("#E8EDEE"),
              panel.border = element_rect("#768692", fill = NA)
            )
          }else{
            healthboards %>%
              ggplot(aes(fill = HBName))+
              geom_sf()+
              gghighlight::gghighlight(
              HBName %in% input$tab2region,
              keep_scales = TRUE,
              max_highlight = 14,
              unhighlighted_params = list(fill = "white"))+
              guides(fill = "none")+
              theme(
                plot.background = element_rect("#F0F4F5"),
                panel.background = element_rect("#E5F1FA"),
                panel.grid.major = element_line("#E8EDEE"),
                panel.grid.minor = element_line("#E8EDEE"),
                panel.border = element_rect("#768692", fill = NA)
              )
        }
      )


#-- Demographic Breakdown Server Below -------------------------------

  
  ## plot 1 sex
  
  output$bed_occupancy_dem <- renderPlot(
    if(input$rate_sex == "Raw Numbers"){
      age_sex %>% 
        filter(location_name == input$regiondem,
               str_detect(quarter, input$covid),
               admission_type == "All Inpatients and Day cases",
               sex %in% input$sex) %>%
        group_by(quarter, sex) %>% 
        summarise(stays = sum(stays)) %>% 
        ggplot() +
        geom_line(aes(x = quarter, y = stays, colour = sex, group = sex)) +
        labs(
          x = "Timeline",
          y = "Admissions",
          title = "Demographic Impact of Covid",
          subtitle = "By Gender"
        )+
        guides(
          colour = guide_legend(title = "Sex")
        )+
        scale_y_continuous(limits = c(0, NA),
                           labels = scales::label_comma()) +
        nhs_theme()+
        theme(legend.position = "top",
              legend.direction = "horizontal",
              legend.text = element_text(size = 12),
              legend.title = element_text(size = 14))
    } else{
      age_sex %>% 
        mutate(year = as.numeric(str_extract(quarter, "[0-9]{4}"))) %>% 
        filter(location_name %in% regions,
               year != 2016) %>% 
        left_join(hb_pop_formatted, by = c("year", "age", "sex", "location_code" = "hb")) %>% 
        mutate(population = population / 1000) %>% 
        filter(location_name == input$regiondem,
               str_detect(quarter, input$covid),
               admission_type == "All Inpatients and Day cases",
               sex %in% input$sex) %>% 
        group_by(quarter, location_name, sex) %>% 
        summarise(stays = sum(stays),
                  population = sum(population),
                  stays_per_1000 = stays / population) %>% 
        ggplot() +
        geom_line(aes(x = quarter, y = stays_per_1000, group = sex, colour = sex))+
        labs(
          x = "Timeline",
          y = "Admissions",
          title = "Demographic Impact of Covid",
          subtitle = "By Gender"
        )+
        guides(
          colour = guide_legend(title = "Sex")
        )+
        scale_y_continuous(labels = scales::label_comma())+
        nhs_theme()+
        theme(legend.position = "top",
              legend.direction = "horizontal",
              legend.text = element_text(size = 12),
              legend.title = element_text(size = 14))
    }
  )
  
  ## plot 2 age 
  
  output$hospital_stays_dem <- renderPlot(
    if(input$rate_age == "Raw Numbers"){
      age_sex %>% 
        filter(location_name == input$regiondem,
               str_detect(quarter, input$covid),
               admission_type == "All Inpatients and Day cases",
               age %in% input$age) %>%
        group_by(quarter, age) %>% 
        summarise(stays = sum(stays)) %>% 
        ggplot() +
        geom_line(aes(x = quarter, y = stays, colour = age, group = age)) +
        labs(
          x = "Timeline",
          y = "Admissions",
          title = "Demographic Impact of Covid",
          subtitle = "By Age"
        )+
        guides(
          colour = guide_legend(title = "Age")
        )+
        scale_y_continuous(limits = c(0, NA),
                           labels = scales::label_comma())+
        nhs_theme()+
        theme(legend.position = "top",
              legend.direction = "horizontal",
              legend.text = element_text(size = 12),
              legend.title = element_text(size = 14))+
        scale_color_brewer(palette = "Paired")
    } else{
      age_sex %>% 
        mutate(year = as.numeric(str_extract(quarter, "[0-9]{4}"))) %>% 
        filter(location_name %in% regions,
               year != 2016) %>% 
        left_join(hb_pop_formatted, by = c("year", "age", "sex", "location_code" = "hb")) %>% 
        mutate(population = population / 1000) %>% 
        filter(location_name == input$regiondem,
               str_detect(quarter, input$covid),
               admission_type == "All Inpatients and Day cases",
               age %in% input$age) %>% 
        group_by(quarter, location_name, age) %>% 
        summarise(stays = sum(stays),
                  population = sum(population),
                  stays_per_1000 = stays / population) %>% 
        ggplot() +
        geom_line(aes(x = quarter, y = stays_per_1000, group = age, colour = age))+
        labs(
          x = "Timeline",
          y = "Admissions",
          title = "Demographic Impact of Covid",
          subtitle = "By Age"
        )+
        guides(
          colour = guide_legend(title = "Age")
        )+
        scale_y_continuous(labels = scales::label_comma())+
        nhs_theme()+
        theme(legend.position = "top",
              legend.direction = "horizontal",
              legend.text = element_text(size = 12),
              legend.title = element_text(size = 14)) +
        scale_color_brewer(palette = "Paired")
    }
  )
    
  
  # plot 3 deprivation 
  
  output$staffed_bed_hours_dem <- renderPlot(
    
      deprivation %>% 
        drop_na(simd) %>% 
        filter(location_name == input$regiondem,
               str_detect(quarter, input$covid),
               admission_type == "All Inpatients and Day cases",
               simd %in% input$deprivation) %>%
        group_by(quarter, simd) %>% 
        summarise(stays = sum(stays)) %>% 
        ggplot() +
        geom_line(aes(x = quarter, y = stays, colour = as.character(simd), group = as.character(simd))) +
        labs(
          x = "Timeline",
          y = "Admissions",
          title = "Demographic Impact of Covid",
          subtitle = "By Deprivation"
        )+
        guides(
          colour = guide_legend(title = "Deprivation Level")
        )+
        scale_y_continuous(limits = c(0, NA),
                           labels = scales::label_comma())+
        nhs_theme()+
        theme(legend.position = "top",
              legend.direction = "horizontal",
              legend.text = element_text(size = 12),
              legend.title = element_text(size = 14)) +
        scale_color_brewer(palette = "Paired")
    
  )
  
  

}

shinyApp(ui, server)
