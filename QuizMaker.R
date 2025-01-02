library(shiny)

ui <- fluidPage(
  titlePanel("Quiz Maker with GK Questions"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Add a New Question"),
      textInput("question", "Question:", ""),
      textInput("option1", "Option 1:", ""),
      textInput("option2", "Option 2:", ""),
      textInput("option3", "Option 3:", ""),
      textInput("option4", "Option 4:", ""),
      selectInput("correct_answer", "Correct Answer:", 
                  choices = c("Option 1", "Option 2", "Option 3", "Option 4")),
      actionButton("add_question", "Add Question"),
      hr(),
      h4("Quiz Management"),
      actionButton("start_quiz", "Start Quiz"),
      actionButton("reset_quiz", "Reset Quiz")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Quiz Questions",
          tableOutput("questions_table")
        ),
        tabPanel(
          "Take Quiz",
          uiOutput("quiz_condition_ui")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  initial_questions <- list(
    list(
      question = "What is the capital of France?",
      options = c("Berlin", "Madrid", "Paris", "Rome"),
      correct_answer = "Option 3"
    ),
    list(
      question = "Who wrote 'Romeo and Juliet'?",
      options = c("Charles Dickens", "William Shakespeare", "Mark Twain", "Jane Austen"),
      correct_answer = "Option 2"
    ),
    list(
      question = "Which planet is known as the Red Planet?",
      options = c("Earth", "Mars", "Jupiter", "Venus"),
      correct_answer = "Option 2"
    ),
    list(
      question = "What is the largest mammal in the world?",
      options = c("Elephant", "Blue Whale", "Giraffe", "Hippopotamus"),
      correct_answer = "Option 2"
    ),
    list(
      question = "What is the chemical symbol for water?",
      options = c("O2", "H2O", "CO2", "HO2"),
      correct_answer = "Option 2"
    )
  )
  
  quiz_data <- reactiveVal(initial_questions)
  current_question_index <- reactiveVal(1)
  quiz_started <- reactiveVal(FALSE)
  score <- reactiveVal(0)
  observeEvent(input$add_question, {
    new_question <- list(
      question = input$question,
      options = c(input$option1, input$option2, input$option3, input$option4),
      correct_answer = input$correct_answer
    )
    quiz_data(c(quiz_data(), list(new_question)))
    updateTextInput(session, "question", value = "")
    updateTextInput(session, "option1", value = "")
    updateTextInput(session, "option2", value = "")
    updateTextInput(session, "option3", value = "")
    updateTextInput(session, "option4", value = "")
    updateSelectInput(session, "correct_answer", selected = "Option 1")
  })
  output$questions_table <- renderTable({
    data <- quiz_data()
    data.frame(
      Question = sapply(data, function(q) q$question),
      "Option 1" = sapply(data, function(q) q$options[1]),
      "Option 2" = sapply(data, function(q) q$options[2]),
      "Option 3" = sapply(data, function(q) q$options[3]),
      "Option 4" = sapply(data, function(q) q$options[4]),
      "Correct Answer" = sapply(data, function(q) q$correct_answer)
    )
  })
  observeEvent(input$start_quiz, {
    if (length(quiz_data()) > 0) {
      quiz_started(TRUE)
      current_question_index(1)
      score(0)
    }
  })
  observeEvent(input$reset_quiz, {
    quiz_started(FALSE)
    quiz_data(initial_questions)
    current_question_index(1)
    score(0)
  })
  output$quiz_condition_ui <- renderUI({
    if (quiz_started()) {
      question <- quiz_data()[[current_question_index()]]
      list(
        h4(question$question),
        radioButtons(
          "quiz_answer", 
          "Choose an answer:", 
          choices = question$options
        ),
        actionButton("submit_answer", "Submit Answer"),
        textOutput("quiz_feedback")
      )
    } else {
      h4("No Quiz in Progress")
    }
  })
  observeEvent(input$submit_answer, {
    question <- quiz_data()[[current_question_index()]]
    selected <- input$quiz_answer
    
    if (selected == question$options[match(question$correct_answer, c("Option 1", "Option 2", "Option 3", "Option 4"))]) {
      score(score() + 1)
      output$quiz_feedback <- renderText("Correct!")
    } else {
      output$quiz_feedback <- renderText(
        paste("Wrong! The correct answer was:", 
              question$options[match(question$correct_answer, c("Option 1", "Option 2", "Option 3", "Option 4"))])
      )
    }
    
    if (current_question_index() < length(quiz_data())) {
      current_question_index(current_question_index() + 1)
    } else {
      quiz_started(FALSE)
      output$quiz_feedback <- renderText(paste("Quiz finished! Your final score is:", score()))
    }
  })
}

shinyApp(ui, server)
