library(tidyverse)
library(shiny)
library(shinyjs)

dat <- tribble(
  ~question, ~answer,
  "Why Plan?", "A logical and orderly way to think about the future",
  "Why Plan?", "Today's decisions have future consequences so we need to plan before we manage and implement",
  "Net Present Value of DRB?", "$683 Billion"
)

ui <- tagList(
  useShinyjs(),
  
  fluidPage(
    includeCSS("www/styles.css"),
    actionButton(
      inputId = "show_answer",
      label = "Show Answer",
      class = "btn-primary"
    ),
    shinyjs::hidden(
      actionButton(
        inputId = "back_to_question",
        label = "Back to Question",
        class = "btn-primary"
      )
    ),
    actionButton(
      inputId = "next_question",
      label = "Next Question",
      class = "btn-error"
    ),
    br(),
    uiOutput("card")
  )
)


server <- function(input, output, session){
  session$onSessionEnded(stopApp)
  
  rv <- reactiveValues(
    dat = dat,
    n_cards = length(unique(dat$question)),
    n = 1,
    answer_click = 0,
    question_click = 1
  )
  
  card_html <- reactive({
    rv$dat %>% 
      group_nest(question, .key = "answer") %>% 
      mutate(
        question = purrr::map(question, ~{
          tagList(
            tags$div(
              class = "question-card",
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
    rv$n
    # browser()
    selected_card <- card_html()[rv$n,]
    if (rv$question_click > rv$answer_click){
      return(tagList(selected_card$question[[1]]))
    } else {
      return(tagList(selected_card$answer[[1]]))
    }
  })
  
  observeEvent(input$show_answer, {
    rv$answer_click <- rv$answer_click + 1
    shinyjs::hide("show_answer")
    shinyjs::show("back_to_question")
  })
  
  observeEvent(input$back_to_question, {
    rv$question_click <- rv$question_click + 1
    shinyjs::hide("back_to_question")
    shinyjs::show("show_answer")
  })
  
  observeEvent(input$next_question, {
    if (rv$question_click == rv$answer_click){
      rv$question_click <- rv$question_click + 1
    }
    
    if (rv$n < rv$n_cards){
      rv$n <- rv$n + 1
    } else {
      rv$n <- 1
    }
    
    shinyjs::show("show_answer")
    shinyjs::hide("back_to_question")
  })
  
}


shinyApp(ui = ui, server = server)