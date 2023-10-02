library(shiny)
library(shinyjs)
library(readxl)
library(dplyr)

ui <- fluidPage(
  useShinyjs(),
  
  # Center the main content
  tags$style(HTML("
    .container-fluid {
      display: flex;
      flex-direction: column;
      justify-content: center;
      align-items: center;
      height: 100vh;
    }
  ")),
  
  # Login panel
  wellPanel(
    textInput("user", "User ID:"),
    passwordInput("password", "Password:"),
    br(),
    actionButton("login", "Login")
  ),
  
  # Display panel (hidden initially)
  hidden(
    div(id = "display",
        h3("Suite à votre participation"),
        htmlOutput("text2"),
        br(),
        uiOutput("download_ui"),
        br(),
        h3("Vos commentaires", id = "header2"),
        htmlOutput("text1"),
        br(),
        htmlOutput("text3"),
        htmlOutput("random_comments")
    )
  )
)

server <- function(input, output, session) {
  observeEvent(input$login, {
    user_id <- input$user
    password <- input$password
    
    # Read the Excel file with user data and conditions
    user_data <- read_excel("input.xlsx")
    
    # Check if user ID and password match in the Excel file
    matched_user <- user_data[user_data$userid == user_id & user_data$password == password, ]
    
    if (nrow(matched_user) > 0) {
      # Check for missing values in 'decision_don' and 'winning_draw'
      if (!is.na(matched_user$decision_don) && !is.na(matched_user$winning_draw)) {
        # If there is a match, check the conditions for displaying text and PDF download
        if (matched_user$decision_don == 1 && matched_user$winning_draw == 1) {
          if (matched_user$game %in% c(1,2)) {
            output$text1 <- renderText({
              HTML(paste("L’évaluation moyenne de cette décision (1 étant considéré comme très immoral et 5 comme très moral) réalisée par 3 autres participants est égale à :", round(matched_user$mean_evaluation, 2)))
            })
            
            output$text3 <- renderText({
              HTML("Ces évaluations étaient assorties des commentaires suivants :")
            })
            
            
            # Display comments from 'comment1', 'comment2', and 'comment3' as bullet points
            output$random_comments <- renderText({
              comments <- c(
                matched_user$comment1,
                matched_user$comment2,
                matched_user$comment3
              )
              
              # Filter out NA (missing) comments
              comments <- comments[!is.na(comments)]
              
              if (length(comments) > 0) {
                comments <- lapply(comments, function(comment) {
                  paste(comment)  # Display comments as bullet points
                })
                
                paste("<ul>", 
                      sapply(comments, function(comment) {
                        paste("<li>", comment, "</li>")
                      }),
                      "</ul>", sep = "")
              } else {
                "No comments available."
              }
            })
            
          } else {
            output$text1 <- NULL
            output$random_comments <- NULL
            output$text3 <- NULL
          }
          
          output$text2 <- renderText({
            HTML("Vous avez participé récemment à une étude au cours de laquelle vous aviez obtenu la somme de 25 euros et vous aviez décidé de renoncer à cette somme et de réaliser un don de 100 euros à Médecins Sans Frontières en cas d’obtention. 
                 <br> <br> 
                 Vous pouvez télécharger ici la confirmation officielle de réception de votre don émanant de Médecins Sans Frontières.
                 <br>")
          })
          
          output$download_ui <- renderUI({
            downloadButton("download_pdf", "Télécharger PDF")
          })
          
          output$download_pdf <- downloadHandler(
            filename = function() {
              paste("ConfirmationMSF.pdf", sep = "")
            },
            content = function(file) {
              # Generate and save the PDF file here
              pdf_path <- "ConfirmationMSF.pdf"
              file.copy(pdf_path, file)
            }
          )
        } else if (matched_user$decision_don == 1 && matched_user$winning_draw == 0) {
            if (matched_user$game %in% c(1,2)) {
              output$text1 <- renderText({
                HTML(paste("L’évaluation moyenne de cette décision (1 étant considéré comme très immoral et 5 comme très moral) réalisée par 3 autres participants est égale à :", matched_user$mean_evaluation))
              })
              
              output$text3 <- renderText({
                HTML("Ces évaluations étaient assorties des commentaires suivants :")
              })
              
              
              # Display comments from 'comment1', 'comment2', and 'comment3' as bullet points
              output$random_comments <- renderText({
                comments <- c(
                  matched_user$comment1,
                  matched_user$comment2,
                  matched_user$comment3
                )
                
                # Filter out NA (missing) comments
                comments <- comments[!is.na(comments)]
                
                if (length(comments) > 0) {
                  comments <- lapply(comments, function(comment) {
                    paste(comment)  # Display comments as bullet points
                  })
                  
                  paste("<ul>", 
                        sapply(comments, function(comment) {
                          paste("<li>", comment, "</li>")
                        }),
                        "</ul>", sep = "")
                } else {
                  "No comments available."
                }
              })
              
            } else {
              output$text1 <- NULL
              output$random_comments <- NULL
              output$text3 <- NULL
          }
          
          output$text2 <- renderText({
            HTML("Vous avez participé récemment à une étude au cours de laquelle vous n’aviez pas obtenu la somme de 25 euros et vous aviez décidé de renoncer à cette somme et de réaliser un don de 100 euros à Médecins Sans Frontières en cas d’obtention.")
          })
          
          output$download_ui <- NULL
          output$download_pdf <- NULL
          
        } else if (matched_user$decision_don == 0 && matched_user$winning_draw == 1) {
            if (matched_user$game %in% c(1,2)) {
              output$text1 <- renderText({
                HTML(paste("L’évaluation moyenne de cette décision (1 étant considéré comme très immoral et 5 comme très moral) réalisée par 3 autres participants est égale à :", round(matched_user$mean_evaluation, 2)))
              })
              
              output$text3 <- renderText({
                HTML("Ces évaluations étaient assorties des commentaires suivants :")
              })
              
              # Display comments from 'comment1', 'comment2', and 'comment3' as bullet points
              output$random_comments <- renderText({
                comments <- c(
                  matched_user$comment1,
                  matched_user$comment2,
                  matched_user$comment3
                )
                
                # Filter out NA (missing) comments
                comments <- comments[!is.na(comments)]
                
                if (length(comments) > 0) {
                  comments <- lapply(comments, function(comment) {
                    paste(comment)  # Display comments as bullet points
                  })
                  
                  paste("<ul>", 
                        sapply(comments, function(comment) {
                          paste("<li>", comment, "</li>")
                        }),
                        "</ul>", sep = "")
                } else {
                  "No comments available."
                }
              })
              
            } else {
              output$text1 <- NULL
              output$random_comments <- NULL
              output$text3 <- NULL
            }
          
          output$text2 <- renderText({
            HTML("Vous avez participé récemment à une étude au cours de laquelle vous aviez obtenu la somme de 25 euros et vous aviez décidé de conserver la somme de 25 euros en cas d’obtention.
                 <br>")
          })
          output$download_ui <- NULL
          output$download_pdf <- NULL
        } else if (matched_user$decision_don == 0 && matched_user$winning_draw == 0) {
            if (matched_user$game %in% c(1,2)) {
              output$text1 <- renderText({
                HTML(paste("L’évaluation moyenne de cette décision (1 étant considéré comme très immoral et 5 comme très moral) réalisée par 3 autres participants est égale à :", matched_user$mean_evaluation))
              })
              
              output$text3 <- renderText({
                HTML("Ces évaluations étaient assorties des commentaires suivants :")
              })
              
              
              # Display comments from 'comment1', 'comment2', and 'comment3' as bullet points
              output$random_comments <- renderText({
                comments <- c(
                  matched_user$comment1,
                  matched_user$comment2,
                  matched_user$comment3
                )
                
                # Filter out NA (missing) comments
                comments <- comments[!is.na(comments)]
                
                if (length(comments) > 0) {
                  comments <- lapply(comments, function(comment) {
                    paste(comment)  # Display comments as bullet points
                  })
                  
                  paste("<ul>", 
                        sapply(comments, function(comment) {
                          paste("<li>", comment, "</li>")
                        }),
                        "</ul>", sep = "")
                } else {
                  "No comments available."
                }
              })
              
            } else {
              output$text1 <- NULL
              output$random_comments <- NULL
              output$text3 <- NULL
            }
          
          output$text2 <- renderText({
            HTML("Vous avez participé récemment à une étude au cours de laquelle vous n’aviez pas obtenu la somme de 25 euros et vous aviez décidé de conserver la somme de 25 euros en cas d’obtention.
                 <br>")
          })
          output$download_ui <- NULL
          output$download_pdf <- NULL
        } else {
          output$text1 <- renderText({
            "No text to display for the current conditions."
          })
          output$download_ui <- NULL
          output$download_pdf <- NULL
        }
        
        # Show the display panel
        shinyjs::show("display")
        # Hide the header if game is not 1 or 2
        shinyjs::toggle(id = "header2", condition = matched_user$game %in% c(1,2))
        
      } else {
        # If no match, show an error message
        showModal(modalDialog(
          title = "Login Failed",
          "You are not eligible to see the results for this survey.",
          easyClose = TRUE
        ))
        output$download_ui <- NULL
        output$download_pdf <- NULL
      }
    } else {
      # Handle missing values in 'decision_don' or 'winning_draw'
      showModal(modalDialog(
        title = "Missing Values",
        "You are not eligible to see the results for this survey.",
        easyClose = TRUE
      ))
    }
  })
}

shinyApp(ui, server)
