library(shiny)
library(shinyauthr)
library(shiny)
library(sortable)
library(dplyr)
library(googlesheets4)
library(shinyjs)

gs4_auth(cache = ".secrets", email = "x@gmail.com")


# dataframe that holds usernames, passwords and other user data
user_base <- tibble::tibble(
  user = c("RDD", "user2"),
  password = sapply(c("2023", "pass2"), sodium::password_store),
  permissions = c("admin", "standard"),
  name = c("User One", "User Two")
)

ui <- fluidPage(
  # logout button
  #div(class = "pull-right", shinyauthr::logoutUI(id = "logout")),
  
  # login section
  shinyauthr::loginUI(id = "login"),
  
   # Plot to show user info after login
 # plotOutput("distPlot")
  uiOutput("mainbucket"),
  
  uiOutput("save"),
 
 #verbatimTextOutput('summary')
)

server <- function(input, output, session) {
  
  credentials <- shinyauthr::loginServer(
    id = "login",
    data = user_base,
    user_col = user,
    pwd_col = password,
    sodium_hashed = TRUE,
    log_out = reactive(logout_init())
  )
  
  # Logout to hide
  logout_init <- shinyauthr::logoutServer(
    id = "logout",
    active = reactive(credentials()$user_auth)
  )
  
  
  output$sidebarpanel <- renderUI({
    
    # Show only when authenticated
    req(credentials()$user_auth)
    
    tagList(
      # Sidebar with a slider input
      column(width = 4,
             sliderInput("obs",
                         "Number of observations:",
                         min = 0,
                         max = 1000,
                         value = 500)
      ),
      
      column(width = 4,
             p(paste("You have", credentials()$info[["permissions"]],"permission"))
      )
    )
    
  })
  
  output$mainbucket <- renderUI({
    req(credentials()$user_auth)
    
    bucket_list(
      header = "Drag Items into the Ranking Bucket.  Top = Rank 1",
      group_name = "bucket_list_group",
      orientation = "horizontal",
      add_rank_list(
        text = "Drag from here",
        labels = list(
          "Evaluation of MACEscrape vs NASS lists vs RDDscrape for hemp",
          "Non-FSA List Frame Coverage Project",
          "Off-farm grain stocks outreach program",
          "CDL-modernization (platform agnostic)",
          "FSA as a frame (CDL acreage estiamtion modernization)",
          "Create and disseminate Ag Census",
          "Propensity, Impact, and Estimation (PIE) research",
          "Translating climate indicators into decisions Winter Wheat Pilot",
          "Land Values Model",
          "Census Estimation Research",
          "Data driven edit limit research"
          
        ),
        input_id = "rank_list_1"
      ),
      add_rank_list(
        text = "Ranking Bucket",
        labels = NULL,
        input_id = "rank_list_2"
      )
    )
  })
 
  output$save <- renderUI({
    req(credentials()$user_auth)
    actionButton('saveit',"Submit")
  }) 
  
  observeEvent(input$saveit, {
    #abc <<- input$rank_list_1
    if(length(input$rank_list_1) > 0)
    {
      showModal(modalDialog(
        title="Try Again",
        "You haven't ranked all the items!"
      ))
      
    }
    else{
    sheet_id <- "https://docs.google.com/spreadsheets/d/x/"
    data <- as.data.frame(input$rank_list_2)
    data$rank <- seq.int(nrow(data))
    ip <- get('HTTP_X_FORWARDED_FOR', envir=session$request)
    #data <- data.frame(number = 4, letter = "D")
    data$ip <- ip
    sheet_append(sheet_id, data)
    shinyjs::hide('saveit')
    showModal(modalDialog(
      title="Thank You",
      "You may now close this page!"
    ))
    
    }
   
  })
  output$summary <- renderText({
    ls(env=session$request)
  })

}

shinyApp(ui = ui, server = server)