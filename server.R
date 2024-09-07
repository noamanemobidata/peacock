

server <- function(input, output, session) {
  observeEvent(input$submit_btn, {
    
    req(input$prompt)
    
    if(nchar(input$prompt)>528){
      
      showNotification(ui = "Too long message! ", type = "error")
      
    }else{
      res <- openai_response(input$prompt, cons$name)
      session$sendCustomMessage("startTyping", content(res)$choices[[1]]$message$content)
      
    }
    
    
  })
  
  
  observeEvent(input$clear_btn, {
    shinyAce::updateAceEditor(session = session, editorId = "ace_editor", value = "")
    
    res$status <- "IN"
    res$df <- NULL
  })
  
  cons <- reactiveValues(con= employees, name= "EmployeeDB")
  
  observeEvent(input$db,{
    
    
    res$status <- "IN"
    res$df <- NULL
    shinyAce::updateAceEditor(session = session, editorId = "ace_editor",value = NULL)
    if( input$db=='sqlite'){
      generate_db_tree(employees, "EmployeeDB")
      cons$con <- employees
      cons$name <- "EmployeeDB" 
      updateTextInput(session = session, inputId = "prompt", placeholder = "find those departments where the average salary is less than the averages for all departments. Return department ID, average salary.")
      
      
    }else{
      generate_db_tree(con,"NYCFlights13")
      cons$con <- con
      cons$name <- "NYCFlights13" 
      updateTextInput(session = session, inputId = "prompt", placeholder = "find the average distance by airline")
      
    }
    
    nav_select(id = "tab_selector",selected = "Home",session = session)
    
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
  
  
  editor_params <- reactiveValues(theme= "terminal", value="")
  
  observeEvent(input$dark_mode, {
    d <- ifelse(input$dark_mode == "dark", "terminal", "sqlserver")
    editor_params$theme <- d
    editor_params$value <- input$ace_editor
    shinyAce::updateAceEditor(session = session, editorId = "ace_editor", theme = editor_params$theme,value = editor_params$value)
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
  
  
  output$editor_ui <- renderUI({

    fluidRow(
      column(
        12,
        card(
          fill = TRUE,
          style = "height: 300px; resize: none; overflow: auto; position: relative;",  
          card_header(
            div(
              class = "input-group",
              tags$input(
                id = "prompt",
                type = "text",
                class = "form-control form-control-sm",
                placeholder = "Find those departments where the average salary is less than the averages for all departments. Return department ID, average salary."
              ),
              tags$span(
                class = "input-group-btn",
                input_task_button(
                  id = "submit_btn",
                  label = "✨ ASK",
                  label_busy = "thinking",
                  icon = icon("paper-plane"),
                  class = "btn btn-sm btn-primary"
                )
              )
            )
          ),
          
          card_body(
            style = "display: flex; flex-direction: column; justify-content: center; align-items: center; height: calc(100% - 20px); padding: 0;",  # Ajustement de la hauteur
            div(
              style = "width: 100%; height: 100%;",  # Prend toute la place dans le card_body
              aceEditor(
                height = "100%",  # L'éditeur s'adapte à la taille disponible
                outputId = "ace_editor", 
                mode = "sql", 
                readOnly = F, 
                autoComplete = "live",  
                theme = editor_params$theme,
                value = editor_params$value, 
                placeholder = "-- START YOUR SQL QUERY HERE, OR ASK AI",
                minLines = 7
              )
            )
          ),
      
          card_footer(
            div(
              id = "resize_handle",
              style = "display: flex; justify-content: space-between; align-items: center;width: 100%; cursor: ns-resize;",
              tagList(
                div(
                  tags$span(
                    class = "input-group-btn",
                    input_task_button(
                      id = "start_btn",
                      label = "RUN ",
                      icon = icon("play"),
                      label_busy = "thinking",
                      class = "btn btn-sm btn-primary"
                    )
                  ),
                  tags$span(
                    class = "input-group-btn",
                    actionButton(
                      inputId = "clear_btn",
                      label = "CLEAR",
                      icon = icon("broom"),
                      class = glue("btn btn-sm btn-{input$dark_mode}")
                    )
                  )
                ),
                div(
                  uiOutput("status_query")
                )
              )
            )
          )
        ), 
        
        # Ajout du JavaScript pour gérer le redimensionnement
        tags$script(HTML("
  (function() {
    var card = document.querySelector('.card');  // Sélectionne la carte
    var handle = document.getElementById('resize_handle');  // Sélectionne le handle de redimensionnement
    var startY, startHeight;

    handle.addEventListener('mousedown', function(e) {
      startY = e.clientY;
      startHeight = parseInt(document.defaultView.getComputedStyle(card).height, 10);
      
      document.documentElement.addEventListener('mousemove', resize, false);
      document.documentElement.addEventListener('mouseup', stopResize, false);
    });

    function resize(e) {
      var newHeight = startHeight + (e.clientY - startY);
      if (newHeight > 100) {  // Limite minimale pour ne pas rendre la carte trop petite
        card.style.height = newHeight + 'px';
      }
    }

    function stopResize() {
      document.documentElement.removeEventListener('mousemove', resize, false);
      document.documentElement.removeEventListener('mouseup', stopResize, false);
    }
  })();
"))
      )
    )
    
    
  })
  
  output$home <- renderUI({
   
    
      fluidPage(
        uiOutput("editor_ui"), 
        uiOutput("mygraph")
      
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
        p("Select a built-in database : "),
        column(
          12,
          div(
            id = "sqlite-option",
            class = "db-option selected",
            div(img(src = "www/sqlite.svg",width = "auto",height = "auto", alt=""), div("SQLite", class = "db-label")),
            onclick = "selectDatabase('sqlite')"
          ),
          div(
            id = "duckdb-option",
            class = "db-option",
            div(img(src = "www/duckdb.svg",width = "auto",height = "auto", alt=""), div("DuckDB", class = "db-label")),
            onclick = "selectDatabase('duckdb')"
          )
        )
      )
      , 
      br(), 
      fluidRow(
        p("Add yours : (wip)")
        
        
        
        
      )
      
    )
    
    
  })
  
  
  
  
  output$title_sidebar <- renderUI({
    
    logo <- ifelse(is.null(input$db), "sqlite", input$db)
    div(
      div(img(src = paste0("www/",logo, ".svg"), width='28px', width="auto", alt=""), "Database Explorer", 
          
          tooltip(
            
            actionButton(inputId = "switch_db", label = NULL, icon = icon("cog"),class="btn-secondary btn-sm", style="border-radius:50%;" ), 
            "Change DB - Read Only"
          )
          
      )
      
      
    )
    
  })
  
  observeEvent(input$switch_db, {
    
    nav_select(id = "tab_selector",selected = "Settings",session = session)
    shinyAce::updateAceEditor(session = session, editorId = "ace_editor", value = "")
    
  })
  
  
}