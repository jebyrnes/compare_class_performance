#' Make an R shiny app with a sidebar and a main panel with four tabs. 
#' In the sidebar, have an dropdown input that will dynamically populate a 
#' second input with a set of radio buttons. Make sure the app loads the dplyr, 
#' ggplot, tidyr, and ggalluvial libraries. Give me the code for the app. 
#' Do not use shinydashboard. Use the spacelab theme. Just give me the code for the 
#' whole app together with no commentary.

# This code assumes you have installed and loaded the necessary packages

# some libraries
library(readxl)
library(ggplot2)
library(ggalluvial)
library(patchwork)
library(dplyr)
library(tidyr)
library(shiny)
library(glue)

theme_set(theme_bw(base_size = 14))
setwd(here::here())

#### Load and prep data ####
grades <- read_excel("tmpBioStsMathBioCrses_combined.xlsx") |>
  #for ease of filtering and use on course number
  filter(!grepl("[a-zA-Z]", CATALOG_NBR)) |>
  mutate(CATALOG_NBR = as.numeric(CATALOG_NBR)) |> 
  filter(!is.na(CATALOG_NBR)) |>
  
  #not useful grades
  filter(!(grade_cd %in% c("P", "-", "AUD", "NA", "SAT", "Y"))) |> 
  filter(!is.na(grade_cd)) |> 
  
  #make grades factors for plotting
  mutate(grade_cd = factor(grade_cd, levels = rev(sort(unique(grade_cd))))) |>
  
  # ethnicities with too few to be useful
  filter(!(race_federal %in% c("0MULTI", "5HAWPAC", "6INDALK", "7NSPEC", "9NONRES"))) |>
  
  # classes we are potentually interested in
  filter(CLASS_TITLE %in% c(
    "Calculus I", "Calculus II", "Cell Biology",
    "College Algebra","Ecology", "General Biology I", "General Biology II", "Genetics", 
    "Introductory Statistics",  "Linear Algebra","Population Biology", "Pre-Cal Mgt&Soc Sci", "Precalculus", "Probability and Statistics", 
    "Probability and Statistics I","Survey Of Calculus")
  )

classes <- grades |>
  select(SBJCT_CD, CATALOG_NBR, CLASS_TITLE) |>
  group_by(SBJCT_CD, CATALOG_NBR, CLASS_TITLE) |>
  slice(1L) |>
  ungroup() |>
  filter(CATALOG_NBR < 400)

get_class <- function(sbjct){
  classes |>
    filter(SBJCT_CD==sbjct) |>
    arrange(CATALOG_NBR) |>
    pull(CLASS_TITLE)
}

#### Define UI for app ####
ui <- fluidPage(
  
  # Set the application title
  titlePanel("Exporing Relationships Between Performance in Classes"),
  
  # Generate a row with a sidebar
  sidebarLayout(
    
    # Define the sidebar with the dropdown input
    sidebarPanel(
      selectInput("dropdown_x", "Choose a class discipline of classes for the X axis:",
                  grades |> pull(SBJCT_CD) |> unique() |> sort(),
                  selected = "BIOL"),
      
      # Generate radio buttons based on the dropdown input
      uiOutput("radio_buttons_x"),
      
      selectInput("dropdown_y", "Choose a class discipline of classes for the Y axis:",
                  grades |> pull(SBJCT_CD) |> unique() |> sort(),
                  selected = "MATH"),
      
      # Generate radio buttons based on the dropdown input
      uiOutput("radio_buttons_y")
    ),
    
    # Create a main panel with tabs
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Summary Information About Class Combination", value = 1,
                           htmlOutput("class_info")
                           ),
                  
                  
                  
                  tabPanel("Point Matrix Plots", value = 2,
                           plotOutput("point_matrices", height = "2000px")),
                  
                  tabPanel("Alluvial Plots", value = 3,
                           plotOutput("alluvial", height = "2000px")),
                  
                  tabPanel("Line Transfer Plots", value = 4,
                           plotOutput("lines", height = "2000px")),
                  
                  tabPanel("Grade Distributions", value = 5,
                           plotOutput("dists", height = "2000px")),
                  
                  tabPanel("You Tell Me What You Want Here", value = 6,
                           HTML("I dunno. Tell me other plot families you want."))
      )
    )
  )
)

#### Define server logic ####
server <- function(input, output) {
  
  selected_classes <- reactive({
    grades |>
      filter(CLASS_TITLE %in% c(input$radio_x, input$radio_y)) |>
      group_by(EMPLID) |>
      mutate(has_both = length(unique(CLASS_TITLE)) == 2) |>
      ungroup() |>
      filter(has_both) |>
      group_by(EMPLID, CLASS_TITLE) |> 
      slice(n()) |>
      ungroup() |>
      filter(!is.na(grade_cd))
      
  })
  
  selected_classes_wide <- reactive({
    
    selected_classes() |>
      group_by(EMPLID, CLASS_TITLE) |> 
      select(EMPLID, gender, race_federal, residency_tuition, college_short, 
             dept_short, CLASS_TITLE, grade_cd) |>
      pivot_wider(names_from = "CLASS_TITLE", 
                  values_from = "grade_cd")
  })

  
  selected_classes_wide_all <- reactive({
    
    selected_classes_wide()|>
      group_by(across(.cols = c(input$radio_x, input$radio_y))) |>
      summarize(count = n()) |>
      ungroup()
  })
  
  
  selected_classes_wide_race <- reactive({
    
    selected_classes_wide()|>
      group_by(across(.cols = c(input$radio_x, input$radio_y, race_federal))) |>
      summarize(count = n()) |>
      ungroup()
  })
  
  selected_classes_wide_gender <- reactive({
    
    selected_classes_wide()|>
      group_by(across(.cols = c(input$radio_x, input$radio_y, gender))) |>
      summarize(count = n()) |>
      ungroup()
  })
  
  selected_classes_wide_both <- reactive({
    
    selected_classes_wide()|>
      group_by(across(.cols = c(input$radio_x, input$radio_y, race_federal, gender))) |>
      summarize(count = n()) |>
      ungroup()
  })
  
    
  # Create radio buttons based on the dropdown input
  output$radio_buttons_x <- renderUI({
    selectInput("radio_x",
                label = "Choose a class:",
                choices = get_class(input$dropdown_x))
    
  })
  
  # Create radio buttons based on the dropdown input
  output$radio_buttons_y <- renderUI({
    selectInput("radio_y",
                label = "Choose a class:",
                choices = get_class(input$dropdown_y))
  })
  
  #make 'em sym so they can be evaluated
  x <- reactive({sym(input$radio_x)})
  y <- reactive({sym(input$radio_y)})
  
  # Insert code here to generate plots based on the dropdown and radio button inputs
  
  #### Summary Info About Data Selected ####
  output$class_info <- renderText({
    

    num_students <- selected_classes() |>
      pull(EMPLID) |>
      unique() |> length()
    
    num_students_race <- selected_classes() |>
      group_by(race_federal) |>
      summarize(count = n_distinct(EMPLID)) |>
      knitr::kable("html") |>
      kableExtra::kable_styling()
    
    num_students_gender <- selected_classes() |>
      group_by(gender) |>
      summarize(count = n_distinct(EMPLID)) |>
      knitr::kable("html") |>
      kableExtra::kable_styling()
    
    num_students_both <- selected_classes() |>
      group_by(race_federal, gender ) |>
      summarize(count = n_distinct(EMPLID)) |>
      knitr::kable("html") |>
      kableExtra::kable_styling()

     glue("Number of students in data who took both: {num_students} <br><br>
          Breakdown by gender: <br> {num_students_gender}<br><br>
          Breakdown by federal race category: <br>  {num_students_race}<br><br>
          Breakdown by both: <br> {num_students_both}")
  })
  
  #### Point plots ####
  output$point_matrices <- renderPlot({
    
    
   all <-  ggplot(selected_classes_wide_all(), 
           aes(x = .data[[input$radio_x]], 
               y = .data[[input$radio_y]], 
               size = count)) +
      geom_point()
   
   gender <-  ggplot(selected_classes_wide_gender(), 
                     aes(x = .data[[input$radio_x]], 
                         y = .data[[input$radio_y]], 
                         size = count)) +
     geom_point() +
     facet_wrap(vars(gender))
   
   race <-  ggplot(selected_classes_wide_race(), 
                     aes(x = .data[[input$radio_x]], 
                         y = .data[[input$radio_y]], 
                         size = count)) +
     geom_point() +
     facet_wrap(vars(race_federal))
   
   both <-  ggplot(selected_classes_wide_both(), 
                     aes(x = .data[[input$radio_x]], 
                         y = .data[[input$radio_y]], 
                         size = count)) +
     geom_point() +
     facet_grid(vars(gender), vars(race_federal))
   
   all + gender + race + both + 
     plot_layout(ncol = 1, heights = c(1,1,2,2))
  })
  
  #### Alluvial plots ####
  output$alluvial <- renderPlot({
    
    
    all <-  ggplot(selected_classes_wide_all(), 
                   aes(y = count, 
                       axis1 = .data[[input$radio_x]], 
                       axis2 = .data[[input$radio_y]])) +
      geom_alluvium( aes(fill = count), width = 1/12) +
      geom_stratum(width = 1/12, fill = "black", color = "grey") +
      geom_label(stat = "stratum", aes(label = after_stat(stratum))) +
      scale_x_discrete(limits = c(input$radio_x, input$radio_y), expand = c(.05, .05)) +
      scale_fill_viridis_c()
    
    gender <-  ggplot(selected_classes_wide_gender(), 
                      aes(y = count, 
                          axis1 = .data[[input$radio_x]], 
                          axis2 = .data[[input$radio_y]])) +
      geom_alluvium( aes(fill = count), width = 1/12) +
      geom_stratum(width = 1/12, fill = "black", color = "grey") +
      geom_label(stat = "stratum", aes(label = after_stat(stratum))) +
      scale_x_discrete(limits = c(input$radio_x, input$radio_y), expand = c(.05, .05)) +
      scale_fill_viridis_c() +
      facet_wrap(vars(gender), scale = "free_y")
    
    race <-  ggplot(selected_classes_wide_race(), 
                    aes(y = count, 
                        axis1 = .data[[input$radio_x]], 
                        axis2 = .data[[input$radio_y]])) +
      geom_alluvium( aes(fill = count), width = 1/12) +
      geom_stratum(width = 1/12, fill = "black", color = "grey") +
      geom_label(stat = "stratum", aes(label = after_stat(stratum))) +
      scale_x_discrete(limits = c(input$radio_x, input$radio_y), expand = c(.05, .05)) +
      scale_fill_viridis_c() +
      facet_wrap(vars(race_federal), scale = "free_y")
    
    both <-  ggplot(selected_classes_wide_both(), 
                    aes(y = count, 
                        axis1 = .data[[input$radio_x]], 
                        axis2 = .data[[input$radio_y]])) +
      geom_alluvium( aes(fill = count), width = 1/12) +
      geom_stratum(width = 1/12, fill = "black", color = "grey") +
      geom_label(stat = "stratum", aes(label = after_stat(stratum))) +
      scale_x_discrete(limits = c(input$radio_x, input$radio_y), expand = c(.05, .05)) +
      scale_fill_viridis_c()+
      facet_grid(vars(gender), vars(race_federal), scale = "free_y")
    
    all + gender + race + both + 
      plot_layout(ncol = 1, heights = c(1,1,2,2))
  })

  #### Line plots ####
  output$lines <- renderPlot({
    print(selected_classes_wide_all() |>
            mutate(id = paste(!!x(), !!y())) |>
            pivot_longer(cols = c(input$radio_x, input$radio_y),
                         names_to = "CLASS_TITLE", values_to = "grade_cd") )
    
    all <- selected_classes_wide_all() |>
      mutate(id = paste(!!x(),!!y())) |>
      pivot_longer(cols = c(input$radio_x, input$radio_y),
                        names_to = "CLASS_TITLE", values_to = "grade_cd") |>
      filter(grade_cd != "NA") |>
      ggplot(aes(x = CLASS_TITLE, y = grade_cd, group = id)) +
      geom_line(aes(linewidth = count, alpha = count)) +
      scale_linewidth_continuous(range = c(0.5,3))
    
    gender <- selected_classes_wide_gender() |>
      mutate(id = paste(!!x(),!!y(), gender)) |>
      pivot_longer(cols = c(input$radio_x, input$radio_y),
                   names_to = "CLASS_TITLE", values_to = "grade_cd") |>
      filter(grade_cd != "NA") |>
      ggplot(aes(x = CLASS_TITLE, y = grade_cd, group = id)) +
      geom_line(aes(linewidth = count, alpha = count)) +
      scale_linewidth_continuous(range = c(0.5,3)) +
      facet_wrap(vars(gender))
    
    race <- selected_classes_wide_race() |>
      mutate(id = paste(!!x(),!!y())) |>
      pivot_longer(cols = c(input$radio_x, input$radio_y),
                   names_to = "CLASS_TITLE", values_to = "grade_cd") |>
      filter(grade_cd != "NA") |>
      ggplot(aes(x = CLASS_TITLE, y = grade_cd, group = id)) +
      geom_line(aes(linewidth = count, alpha = count)) +
      scale_linewidth_continuous(range = c(0.5,3)) +
      facet_wrap(vars(race_federal))
    
    both <- selected_classes_wide_both() |>
      mutate(id = paste(!!x(),!!y())) |>
      pivot_longer(cols = c(input$radio_x, input$radio_y),
                   names_to = "CLASS_TITLE", values_to = "grade_cd") |>
      filter(grade_cd != "NA") |>
      ggplot(aes(x = CLASS_TITLE, y = grade_cd, group = id)) +
      geom_line(aes(linewidth = count, alpha = count)) +
      scale_linewidth_continuous(range = c(0.5,3)) +
      facet_grid(vars(gender), vars(race_federal))
    
    all + gender + race + both +
      plot_layout(ncol = 1)
  })
  
  #### Distribution plots ####
  output$dists <- renderPlot({

    #debug
    print(selected_classes_wide_all() |>
            pivot_longer(c(input$radio_x, input$radio_y)) |>
            group_by(name, value) |>
            summarize(count = sum(count)))
    
    all <- selected_classes_wide_all() |>
      pivot_longer(c(input$radio_x, input$radio_y)) |>
      group_by(name, value) |>
      summarize(count = sum(count)) |>
      ggplot(aes(x = value, y = count)) +
      geom_col() +
      facet_wrap(vars(name), ncol = 1, scale = "free_y")
    
    gender <- selected_classes_wide_gender() |>
      pivot_longer(c(input$radio_x, input$radio_y)) |>
      group_by(name, gender, value) |>
      summarize(count = sum(count)) |>      
      group_by(name, gender) |>
      mutate(count = count/sum(count)) |>
      ggplot(aes(x = value, y = count, fill = gender)) +
      geom_col(position = "dodge") +
      facet_wrap(vars(name), ncol = 1, scale = "free_y")+
      labs(y = "fraction")
    
    
    race <- selected_classes_wide_race() |>
      pivot_longer(c(input$radio_x, input$radio_y)) |>
      group_by(name, race_federal, value) |>
      summarize(count = sum(count)) |>
      group_by(name, race_federal) |>
      mutate(count = count/sum(count)) |>
      ggplot(aes(x = value, y = count, fill = race_federal)) +
      geom_col(position = "dodge") +
      facet_wrap(vars(name), ncol = 1, scale = "free_y") +
      scale_color_brewer(type = "qual") +
      labs(y = "fraction")
    
    all+ gender + race +
      plot_layout(ncol = 1)
    
   
  })
  
} 

# Create the Shiny app
shinyApp(ui = ui, server = server)
