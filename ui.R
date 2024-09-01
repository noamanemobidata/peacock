



ui <- fluidPage(
  tags$head(
    tags$title("PEACOCK"),
    tags$meta(name = "title", content = "PEACOCK - ALK TO YOUR DB"),
    tags$meta(name = "description", content = "Author: miskowski85@hotmail.fr"),
    tags$link(rel = "icon", type = "image/x-icon", href = "logo.svg"),
    tags$script(src = "https://cdn.jsdelivr.net/npm/typed.js@2.0.12"),
    
    # Custom JavaScript for Typed.js integration
    tags$script(HTML(
      '
      Shiny.addCustomMessageHandler("startTyping", function(message) {
        var editor = ace.edit("ace_editor");
        editor.setValue("");  // Clear the editor before typing starts

        // Split the message into individual characters to simulate typing
        let characters = message.split("");
        let typedCode = "";

        function typeCharacter(index) {
          if (index < characters.length) {
            typedCode += characters[index];
            editor.setValue(typedCode, -1);  // Update editor with the typed content
            setTimeout(function() { typeCharacter(index + 1); }, 13);  // Adjust the speed by changing the timeout
          }
        }

        typeCharacter(0);  // Start typing
      });
      '
    ))
  ),
  includeCSS("www/style.css"),
  shinyjs::useShinyjs(),
  page_navbar(
    lang = "en",
    theme = bs_theme(
      preset = "shiny",
      "primary" = "#0675DD"
    ), 
    title = tags$span(
      tags$img(
        src = "logo.svg",
        width = "50px",
        height = "auto",
        class = "me-3",
        alt = "Shiny hex logo"
      ),
      "PEACOCK"
    ),
    sidebar = sidebar(
      title = div(
        "Database Explorer", tooltip(
          bsicons::bs_icon("info-circle"),
          "Read Only SQLite",
          id = "tooltip"
        )
      ),
      gap = "0.5rem", padding = "0.5rem",
      shinyTree("tree4",
                theme = "proton", themeIcons = T, themeDots = T, stripes = F, search = T, searchtime = 1000,
                types =
                  "{
          '#': {'valid_children' : ['database'] },
          'database' : { 'icon' : 'fa fa-database', 'valid_children' : ['table'] },
          'table' : { 'icon' : 'fa fa-table','valid_children' : ['column'] },
          'column' : { 'icon' : 'fa fa-caret-right', 'valid_children' : [] }
        }"
      )
    ),
    nav_spacer(),
    nav_panel(
      title = HTML('<i class="fas fa-home"></i> Main'),
      uiOutput("main")
    ),
    nav_panel(
      title = HTML('<i class="fas fa-cog"></i> settings'),
      uiOutput("settings")
    ),
    nav_panel(
      title = HTML('<i class="fas fa-info-circle"></i> About'),
      uiOutput("about")
    ),
    nav_item(
      input_dark_mode(id = "dark_mode", mode = "dark")
    )
  )
)
