



server <- function(input, output, session) {
  # État réactif pour la connexion actuelle
  current_db <- reactiveVal("SQLite")
  
  # Fonction pour changer de base de données
  change_database <- function(db_type) {
    cons$con <- create_connection(db_type)
    cons$name <- db_configs[[db_type]]$name
    current_db(db_type)
    updateTextInput(session, "prompt", placeholder = db_configs[[db_type]]$placeholder)
    shinyAce::updateAceEditor(session, "ace_editor", value = "")
    res$status <- "IN"
    res$df <- NULL
  }
  
  # Initialisation
  cons <- reactiveValues(con = create_connection("SQLite"), name = db_configs$SQLite$name)
  
  observeEvent(input$db, {
    change_database(input$db)
    nav_select(id = "tab_selector", selected = "Home", session = session)
  })
  
  observeEvent(input$submit_btn, {
    req(input$prompt)
    if(nchar(input$prompt) > 528) {
      showNotification(ui = "Too long message!", type = "error")
    } else {
      res <- openai_response(input$prompt, cons$name)
      session$sendCustomMessage("startTyping", content(res)$choices[[1]]$message$content)
    }
  })
  
  observeEvent(input$clear_btn, {
    shinyAce::updateAceEditor(session = session, editorId = "ace_editor", value = "")
    res$status <- "IN"
    res$df <- NULL
  })
  
  treeData <- reactive({
    generate_db_tree(cons$con, cons$name)
  })
  
  output$tree4 <- renderTree({
    treeData()
  })
  
  editor_params <- reactiveValues(theme = "terminal", value = "")
  
  observeEvent(input$dark_mode, {
    d <- ifelse(input$dark_mode == "dark", "terminal", "sqlserver")
    editor_params$theme <- d
    editor_params$value <- input$ace_editor
    shinyAce::updateAceEditor(session = session, editorId = "ace_editor", theme = editor_params$theme, value = editor_params$value)
  })
  
  res <- reactiveValues(df = NULL, status = "IN")
  
  observeEvent(input$start_btn, {
    req(input$ace_editor)
    tryCatch({
      res$df <- dbGetQuery(cons$con, input$ace_editor)
      res$status <- "OK"
    }, error = function(e) {
      showNotification(type = "warning", ui = "Query not executed")
      res$status <- "KO"
      res$df <- NULL
    })
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
          style = "height: 250px; resize: none; overflow: auto; position: relative;",  
          card_header(
            div(
              class = "input-group",
              tags$input(
                id = "prompt",
                type = "text",
                class = "form-control form-control-sm",
                placeholder = db_configs[[current_db()]]$placeholder
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
            style = "display: flex; flex-direction: column; justify-content: center; align-items: center; height: calc(100% - 20px); padding: 0;",
            div(
              style = "width: 100%; height: 100%;",
              aceEditor(
                height = "100%",
                outputId = "ace_editor", 
                mode = "sql", 
                readOnly = FALSE, 
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
        tags$script(HTML("
          (function() {
            var card = document.querySelector('.card');
            var handle = document.getElementById('resize_handle');
            var startY, startHeight;
            handle.addEventListener('mousedown', function(e) {
              startY = e.clientY;
              startHeight = parseInt(document.defaultView.getComputedStyle(card).height, 10);
              document.documentElement.addEventListener('mousemove', resize, false);
              document.documentElement.addEventListener('mouseup', stopResize, false);
            });
            function resize(e) {
              var newHeight = startHeight + (e.clientY - startY);
              if (newHeight > 100) {
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
    fluidPage(
      fluidRow(
        p("Select a built-in database : "),
        column(
          12,
          lapply(names(db_configs), function(db) {
            div(
              id = paste0(db, "-option"),
              class = if (db == current_db()) "db-option selected" else "db-option",
              div(img(src = db_configs[[db]]$icon, width = "auto", height = "auto", alt = ""),
                  div(db_configs[[db]]$name, class = "db-label")),
              onclick = sprintf("selectDatabase('%s')", db)
            )
          })
        )
      )
    )
  })
  
  output$title_sidebar <- renderUI({
    div(
      div(img(src = db_configs[[current_db()]]$icon, width = '40px', width = "auto", alt = ""),
          "Database Explorer",
          actionButton(inputId = "switch_db", label = NULL, icon = icon("cog"),
                       class = "btn-secondary btn-sm", style = "border-radius:50%;")
      )
    )
  })
  
  observeEvent(input$switch_db, {
    nav_select(id = "tab_selector", selected = "Settings", session = session)
    shinyAce::updateAceEditor(session = session, editorId = "ace_editor", value = "")
  })
}