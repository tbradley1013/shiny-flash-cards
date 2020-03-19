library(tidyverse)
library(shiny)
library(shinyjs)
library(readxl)
library(janitor)
library(shinyalert)
library(waiter)

ui <- tagList(
  useShinyjs(),
  useShinyalert(),
  tags$head(
    tags$link(href='http://fonts.googleapis.com/css?family=Merienda+One', rel='stylesheet', type='text/css'),
    tags$link(href='http://fonts.googleapis.com/css?family=Lobster+Two', rel='stylesheet', type='text/css'),
    tags$link(href="https://fonts.googleapis.com/css?family=Open+Sans&display=swap", rel="stylesheet")
  ),
  fluidPage(
    includeCSS("www/styles.css"),
    includeScript("www/button_click.js"),
    shinyjs::hidden(
      div(
        id = "main-content",
        div(
          id = "links-div",
          actionButton(
            inputId = "change_dataset",
            label = "",
            icon = icon("database"),
            class = "btn-primary btn-sm"
          ),
          tags$a(
            icon("github"),
            href = "https://github.com/tbradley1013/shiny-flash-cards"
          )
        ),
        div(
          h2("Shiny Flash Cards"),
          h4("An application designed to make memorization easier!"),
          style = "text-align:center;"
        ),
        br(),
        uiOutput("card"),
        br(),
        fluidRow(
          actionButton(
            inputId = "know_it",
            label = "I know it!",
            class = "btn-success btn-lg",
            width = "33%" 
          ),
          actionButton(
            inputId = "show_answer",
            label = "Show Answer",
            class = "btn-primary btn-lg",
            width = "33%"
          ),
          actionButton(
            inputId = "next_question",
            label = "Next Question",
            class = "btn-danger btn-lg",
            width = "33%"
          ),
          inline = TRUE,
          style = "width:50%;margin: 0 auto;"
        ),
        div(
          tags$p(tags$kbd("a"), ": Toggle Question/Answer"),
          tags$p(tags$kbd("d"), ": Next Question"),
          tags$p(tags$kbd("w"), ": I Know it!")
        )
      )
    )
  )
)


server <- function(input, output, session){
  session$onSessionEnded(stopApp)
  
  dialog <- modalDialog(
    h1("Select Dataset!"),
    selectInput(
      inputId = "dataset_choice",
      label = "Datasets",
      choices = c(
        "Advanced R" = "adv_r",
        "Delaware River Watershed" = "drbc",
        "Custom" = "custom"
      ),
      selected = "adv_r"
    ),
    uiOutput("custom_data"),
    footer = tagList(
      actionButton(
        inputId = "load_dataset",
        label = "Load Flash Cards!",
        class = "btn-primary"
      )
    )
  )
  
  observe({
    showModal(dialog)
  })
  
  output$custom_data <- renderUI({
    req(input$dataset_choice)
    
    if (input$dataset_choice == "custom"){
      out <- tagList(
        fileInput(
          inputId = "custom_file",
          label = "Custom File",
          accept = c("xlsx", "csv", "rds")
        ),
        helpText("File must be a `.csv`, `.xlsx`, or `.rds` file with columns named `question` and `answer`.\nThese columns can contain HTML for custom formatting!")
      )
    } else out <- NULL
    
    return(out)
  })
  
  w <- waiter::Waiter$new()
  
  dat <- eventReactive(input$load_dataset, {
    dat_choice <- input$dataset_choice
    if (dat_choice == "custom"){
      dat_file <- input$custom_file
      dat_ext <- tools::file_ext(dat_file$datapath)
    } 
    removeModal()
    w$show
    
    if (dat_choice == "adv_r"){
      dat <- readr::read_rds("data/adv-r-deck.rds")
    } else if (dat_choice == "drbc"){
      dat <- readxl::read_excel("data/drbc-deck-1.xlsx") %>%
        janitor::clean_names() %>%
        dplyr::mutate(answer = as.character(answer))
    } else {
      if (dat_ext == "xlsx"){
        dat <- readxl::read_excel(dat_file$datapath) %>% 
          janitor::clean_names()
      } else if (dat_ext == "csv"){
        dat <- readr::read_csv(dat_file$datapath) %>% 
          janitor::clean_names()
      } else if (dat_ext == "rds"){
        dat <- readr::read_rds(dat_file$datapath) %>% 
          janitor::clean_names()
      } else {
        showModal(dialog)
        return(NULL)
      }
      
      if (!all(c("question", "answer") %in% colnames(dat))){
        showModal(dialog)
        return(NULL)
      }
    }
    
    shinyjs::show("main-content")
    w$hide
    
    return(dat)
  })
  
  observeEvent(input$change_dataset, {
    shinyjs::hide("main-content")
    showModal(dialog)
  })
  
  
  rv <- reactiveValues(
    answer_visible = FALSE,
    question_visible = TRUE,
    card_keep = numeric(0),
    card_know = numeric(0)
  )
  
  observe({
    rv$n_cards <- length(unique(dat()$question))
    rv$card_idx <- sample(1:rv$n_cards, rv$n_cards)
    rv$n <- 1
  })
  
  card_html <- reactive({
    dat() %>% 
      select(question, answer) %>% 
      group_nest(question, .key = "answer") %>% 
      mutate(
        question = purrr::map(question, ~{
          tagList(
            tags$div(
              class = "question-card",
              id = "question-div",
              tags$div(
                class = "question",
                HTML(.x)
              )
            )
          )
        }),
        answer = purrr::map(answer, ~{
          tagList(
            tags$div(
              class = "answers-card",
              tags$div(
                class = "answers",
                tags$ul(HTML(paste0("<li>", .x$answer, "</li>")))
              )
            )
          )
        })
      )  
  })
  
  output$card <- renderUI({
    req(rv$n)
    selected_card <- card_html()[rv$card_idx[rv$n],]
    if (rv$question_visible){
      return(tagList(selected_card$question[[1]]))
    } else if (rv$answer_visible) {
      return(tagList(selected_card$answer[[1]]))
    }
  })
  
  observeEvent(input$show_answer, {
    if (rv$question_visible){
      rv$answer_visible <- TRUE
      rv$question_visible <- FALSE
      
      updateActionButton(session, "show_answer", label = "Show Question")
    } else if (rv$answer_visible){
      rv$answer_visible <- FALSE
      rv$question_visible <- TRUE
      
      updateActionButton(session, "show_answer", label = "Show Answer")
    }
    
  })
  
  observeEvent(input$next_question, {
    if (rv$answer_visible){
      rv$answer_visible <- FALSE
      rv$question_visible <- TRUE
      updateActionButton(session, "show_answer", label = "Show Answer")
    }
    
    
    if (length(rv$card_idx) > rv$n){
      rv$n <- rv$n + 1
    } else {
      rv$n <- 1
    }
    
  })
  
  observeEvent(input$know_it, {
    if (rv$answer_visible){
      rv$answer_visible <- FALSE
      rv$question_visible <- TRUE
    }
    
    rv$card_know <- c(rv$card_know, rv$card_idx[rv$n])
    rv$card_idx <- rv$card_idx[-rv$n]
    updateActionButton(session, "show_answer", label = "Show Answer")
    
    if (length(rv$card_idx) > rv$n){
      rv$n <- rv$n + 1
    } else {
      if (length(rv$card_idx) > 0){
        rv$n <- 1
      } else {
        shinyalert::shinyalert(title = "Congrats!", text = "You have indicated that you know all of the cards! The deck will now be reset!", type = "success")
        rv$card_idx <- sample(rv$card_know, length(rv$card_know))
        rv$n <- 1
      }
    }
    
  })
  
}


shinyApp(ui = ui, server = server)