library(tidyverse)
library(shiny)
library(shinyjs)
library(shinythemes)
library(readxl)
library(janitor)
library(shinyalert)

dat <- readxl::read_excel("data/drbc-deck-1.xlsx") %>% 
  janitor::clean_names() %>% 
  dplyr::mutate(answer = as.character(answer))

ui <- tagList(
  useShinyjs(),
  useShinyalert(),
  fluidPage(
    theme = shinytheme("journal"),
    includeCSS("www/styles.css"),
    includeScript("www/button_click.js"),
    div(
      actionButton(
        inputId = "show_answer",
        label = "Show Answer",
        class = "btn-primary",
        width = "100%"
      ),
      # shinyjs::hidden(
      #   actionButton(
      #     inputId = "back_to_question",
      #     label = "Back to Question",
      #     class = "btn-primary",
      #     width = "100%"
      #   )
      # ),
      # actionButton(
      #   inputId = "next_question",
      #   label = "Next Question",
      #   class = "btn-error",
      #   width = "45%"
      # ),
      style = "width:200px;margin:0 auto;"
    ),
    br(),
    uiOutput("card"),
    br(),
    div(
      actionButton(
        inputId = "know_it",
        label = "I know it!",
        class = "btn-success",
        width = "45%" 
      ),
      actionButton(
        inputId = "next_question",
        label = "Next Question",
        class = "btn-danger",
        width = "45%"
      ),
      style = "width:400px;margin: 0 auto;"
    )
  )
)


server <- function(input, output, session){
  session$onSessionEnded(stopApp)
  
  rv <- reactiveValues(
    dat = dat,
    card_idx = 1:length(unique(dat$question)),
    answer_visible = FALSE,
    question_visible = TRUE,
    card_keep = numeric(0),
    card_know = numeric(0)
    # answer_click = 0,
    # question_click = 1
  )
  
  observe({
    rv$n <- sample(rv$card_idx, 1)
  })
  
  card_html <- reactive({
    rv$dat %>% 
      group_nest(question, .key = "answer") %>% 
      mutate(
        question = purrr::map(question, ~{
          tagList(
            tags$div(
              class = "question-card",
              id = "question-div",
              tags$div(
                class = "question",
                .x
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
    # browser()
    selected_card <- card_html()[rv$n,]
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
      
      updateActionButton(session, "show_answer", label = "Back to Question")
      # shinyjs::hide("show_answer")
      # shinyjs::show("back_to_question")
    } else if (rv$answer_visible){
      rv$answer_visible <- FALSE
      rv$question_visible <- TRUE
      
      updateActionButton(session, "show_answer", label = "Show Answer")
    }
    
    # rv$answer_click <- rv$answer_click + 1
    
  })
  
  # observeEvent(input$back_to_question, {
  #   # rv$question_click <- rv$question_click + 1
  #   
  #   shinyjs::hide("back_to_question")
  #   shinyjs::show("show_answer")
  # })
  
  observeEvent(input$next_question, {
    if (rv$answer_visible){
      rv$answer_visible <- FALSE
      rv$question_visible <- TRUE
    }
    
    rv$card_keep <- c(rv$card_keep, rv$n)
    rv$card_idx <- rv$card_idx[rv$card_idx != rv$n]
    
    if (length(rv$card_idx) > 0){
      rv$n <- sample(rv$card_idx, 1)
    } else {
      rv$card_idx <- rv$card_keep
      rv$card_keep <- numeric(0)
      rv$n <- sample(rv$card_idx, 1)
    }
    
    shinyjs::show("show_answer")
    shinyjs::hide("back_to_question")
  })
  
  observeEvent(input$know_it, {
    if (rv$answer_visible){
      rv$answer_visible <- FALSE
      rv$question_visible <- TRUE
    }
    
    rv$card_know <- c(rv$card_know, rv$n)
    rv$card_idx <- rv$card_idx[rv$card_idx != rv$n]
    
    if (length(rv$card_idx) > 0){
      rv$n <- sample(rv$card_idx, 1)
    } else {
      if (length(rv$card_keep) > 0){
        rv$card_idx <- rv$card_keep
        rv$card_keep <- numeric(0)
        rv$n <- sample(rv$card_idx, 1)
      } else {
        shinyalert::shinyalert(title = "Congrats!", text = "You have indicated that you know all of the cards! The deck will now be reset!", type = "success")
        rv$card_idx <- rv$card_know
        rv$n <- sample(rv$card_idx, 1)
      }
    }
    
    shinyjs::show("show_answer")
    shinyjs::hide("back_to_question")
  })
  
}


shinyApp(ui = ui, server = server)