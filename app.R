library(tidyverse)
library(shiny)
library(shinyjs)
# library(shinythemes)
library(readxl)
library(janitor)
library(shinyalert)
# library(bsplus)

dat <- readxl::read_excel("data/drbc-deck-1.xlsx") %>% 
  janitor::clean_names() %>% 
  dplyr::mutate(answer = as.character(answer))

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
    div(
      id = "links-div",
      tags$a(
        icon("github"),
        href = "https://github.com/tbradley1013/shiny-flash-cards"
      )
    ),
    div(
      h3("Shiny Flash Cards"),
      h5("An application designed to make memorization easier!"),
      style = "text-align:center;"
    ),
    div(
      actionButton(
        inputId = "show_answer",
        label = "Show Answer",
        class = "btn-primary",
        width = "100%"
      ),
      style = "width:200px;margin:10px auto 0;"
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
    ),
    div(
      tags$p(tags$kbd("a"), ": Toggle Question/Answer"),
      tags$p(tags$kbd("d"), ": Next Question"),
      tags$p(tags$kbd("w"), ": I Know it!")
    )
  )
)


server <- function(input, output, session){
  session$onSessionEnded(stopApp)
  
  rv <- reactiveValues(
    dat = dat,
    n_cards = length(unique(dat$question)),
    card_idx = 1:length(unique(dat$question)),
    answer_visible = FALSE,
    question_visible = TRUE,
    card_keep = numeric(0),
    card_know = numeric(0)
  )
  
  observe({
    rv$card_idx <- sample(1:rv$n_cards, rv$n_cards)
    rv$n <- 1
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
    
    # rv$card_keep <- c(rv$card_keep, rv$n)
    # rv$card_idx <- rv$card_idx[rv$card_idx != rv$n]
    
    if (length(rv$card_idx) > rv$n){
      rv$n <- rv$n + 1
    } else {
      # rv$card_idx <- rv$card_keep
      # rv$card_keep <- numeric(0)
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
    
    if (length(rv$card_idx) > rv$n){
      rv$n <- rv$n + 1
    } else {
      if (length(rv$card_idx) > 0){
        # rv$card_idx <- rv$card_keep
        # rv$card_keep <- numeric(0)
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