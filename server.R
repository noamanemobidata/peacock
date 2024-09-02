

server <- function(input, output, session) {
  observeEvent(input$submit_btn, {
    
    res <- openai_response(input$prompt, cons$name)
    session$sendCustomMessage("startTyping", content(res)$choices[[1]]$message$content)
  })


  observeEvent(input$claer_btn, {
    shinyAce::updateAceEditor(session = session, editorId = "ace_editor", value = "")

    res$status <- "IN"
    res$df <- NULL
  })
  
  cons <- reactiveValues(con= employees, name= "EmployeeDB")
  
  observeEvent(input$db,{
    
 
    res$status <- "IN"
    res$df <- NULL
    
      if( input$db=='sqlite'){
        generate_db_tree(employees, "EmployeeDB")
        cons$con <- employees
        cons$name <- "EmployeeDB" 

      }else{
        generate_db_tree(con,"NYCFlights13")
        cons$con <- con
        cons$name <- "NYCFlights13" 

      }
      
    
    
  })

  treeData <- reactive({
    
    if(is.null(input$db)){
      generate_db_tree(employees, "EmployeeDB")
    }else{
      if( input$db=='sqlite'){
        generate_db_tree(employees, "EmployeeDB")
      }else{
        generate_db_tree(con,"NYCFlights13")
      }
      
    }
  })


  output$tree4 <- renderTree({
    treeData()
  })



  observeEvent(input$dark_mode, {
    d <- ifelse(input$dark_mode == "dark", "terminal", "sqlserver")
    shinyAce::updateAceEditor(session = session, editorId = "ace_editor", theme = d)
  })

  res <- reactiveValues(df = NULL, status = "IN")

  observeEvent(input$start_btn, {
    req(input$ace_editor)


    tryCatch(
      {
        res$df <- dbGetQuery(cons$con, input$ace_editor)

        res$status <- "OK"
      },
      error = function(e) {
        showNotification(type = "warning", ui = "Query not executed")
        res$status <- "KO"
        res$df <- NULL
      }
    )
  })


  output$mygraph <- renderUI({
    req(res$df)

    d <- ifelse(input$dark_mode == "dark", "dark", "light")
    gwalkr(res$df, dark = d, visConfig = '[{"config":{"defaultAggregated":false}}]')
  })


  observeEvent(input$tree4, {
    if (length((get_selected(input$tree4, "names"))) > 0) {
      
      cond <- ((get_selected(input$tree4, "names"))[[1]] %in% dbListTables(conn = cons$con))


      if (cond) {
        qr <- glue("SELECT * FROM {(get_selected(input$tree4,'names'))[[1]]} LIMIT 1000;")
        res$df <- dbGetQuery(cons$con, qr)

        session$sendCustomMessage("startTyping", qr)

        res$status <- "OK"
      }
    }
  })


  output$status_query <- renderUI({
    nr <- ifelse(is.null(res$df), 0, nrow(res$df))

    switch(res$status,
      "OK" = HTML(glue('<i class="fas fa-check-circle" style="color:green;"></i>  {nr} ROWS')),
      "KO" = HTML(glue('<i class="fas fa-times-circle" style="color:red;"></i> {nr} ROWS')),
      "IN" = HTML('<i class="far fa-circle"></i> READY')
    )
  })


  output$main <- renderUI({
    d <- ifelse(input$dark_mode == "dark", "terminal", "sqlserver")

    fluidRow(
      fluidPage(
        fluidRow(
          column(
            12,
            card(
              fill = T, max_height = 300,
              card_header(
                div(
                  class = "input-group",
                  tags$input(
                    id = "prompt",
                    type = "text",
                    class = "form-control  form-control-sm",
                    placeholder = "find those departments where the average salary is less than the averages for all departments. Return department ID, average salary."
                  ),
                  tags$span(
                    class = "input-group-btn",
                    input_task_button(
                      id = "submit_btn",
                      label = "ASK",
                      icon = icon("paper-plane"),
                      class = "btn btn-sm btn-primary"
                    )
                  )
                )
              ),
              card_body(
                aceEditor(outputId = "ace_editor", mode = "sql", readOnly = F, autoComplete = "live", theme = d, value = "", placeholder = "-- START YOUR SQL QUERY HERE, OR ASK AI",minLines = 7)
              ),
              card_footer(
                tagList(
                  div(
                    style = "float:left;",
                    tags$span(
                      class = "input-group-btn",
                      input_task_button(
                        id = "start_btn",
                        label = "RUN QUERY",
                        icon = icon("play"),
                        class = "btn btn-sm btn-primary"
                      )
                    ),
                    tags$span(
                      class = "input-group-btn",
                      actionButton(
                        inputId = "claer_btn",
                        label = "CLEAR",
                        icon = icon("broom"),
                        class = glue("btn btn-sm btn-{input$dark_mode}")
                      )
                    )
                  ),
                  div(
                    style = "float:right;",
                    uiOutput("status_query")
                  )
                )
              )
            )
          )
        ),
        fluidRow(
          column(
            12,
            uiOutput("mygraph")
          )
        )
      )
    )
  })

  
  
  output$about <- renderUI({
    
    tags$div(class = "jumbotron",
             tags$h1(class = "display-4", "PEACOCK"),
             tags$p(class = "lead", "AI-Powered SQL Query Generation and Data Visualization"),
             tags$hr(class = "my-4"),
             tags$p("PEACOCK is an AI-driven app, designed to streamline database exploration and SQL query generation"),
            
             tags$p("Leveraging AI, users can simply input natural language prompts, and the app generates SQL queries automatically."),
             tags$p("The interface allows users to explore database structures, refine queries in a smart code editor, and visualize results interactively using the gwalkr package."), 
             tags$p("Contact: miskowski85@hotmail.fr")
            
    )
    
  })
  
  
  observe({
    updateTextInput(inputId = "tree4-search-input", placeholder = "🔍Type to search...")
  })
  
  
  output$settings <- renderUI({
    

    
    hidden(
      radioButtons("db", label = NULL, choices = c("sqlite", "duckdb"))
    )
    
    fluidPage(
      fluidRow(
        p("Select built-in database: "),
        column(
          12,
          div(
            id = "sqlite-option",
            class = "db-option selected",
            div(img(src = "www/sqlite.svg"), div("SQLite", class = "db-label")),
            onclick = "selectDatabase('sqlite')"
          ),
          div(
            id = "duckdb-option",
            class = "db-option",
            div(img(src = "www/duckdb.svg"), div("DuckDB", class = "db-label")),
            onclick = "selectDatabase('duckdb')"
          )
        )
      )
      , 
      br(), 
      fluidRow(
        p("Add your data: (wip)")
      )
      
    )
    
    
  })
  

  

    output$title_sidebar <- renderUI({
      
      logo <- ifelse(is.null(input$db), "sqlite", input$db)
      div(
        div(img(src = paste0("www/",logo, ".svg"), width='28px'), "Database Explorer", 
            
            popover( 
              icon("info-circle"),
              actionButton(inputId = "change_db", label = "CHANGE DB", icon = icon("exchange-alt"), 
                           class = "btn btn-sm btn-primary"),
              title = "Read Only DB"
            )
            
        #     tooltip(
        #   bsicons::bs_icon("info-circle"),
        #   "Read Only DB",
        #   id = "tooltip"
        # )
        )
        
        
      )

  })
    
    observeEvent(input$change_db, {
      
      nav_select(id = "tab_selector",selected = "Settings",session = session)
      
    })
  
  
}
