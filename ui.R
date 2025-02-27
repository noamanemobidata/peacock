



ui <- fluidPage(
  lang = "en",
  tags$head(
    tags$title("PEACOCK"),
    tags$meta(name = "title", content = "PEACOCK - ALK TO YOUR DB"),
    tags$meta(name = "description", content = "Author: miskowski85@hotmail.fr"),
    tags$link(rel = "icon", type = "image/x-icon", href = "www/logo.svg"),
    tags$script(src = "https://cdn.jsdelivr.net/npm/typed.js@2.0.12"),
    tags$script(HTML('
      $(document).on("keydown", function (e) {
        if (e.ctrlKey && e.key === "Enter") {
          e.preventDefault();
          $("#start_btn").click();
        }
      });
    ')), 
    
    tags$script(HTML("
    function selectDatabase(db) {
      // Remove 'selected' class from all options
      $('.db-option').removeClass('selected');
      
      // Add 'selected' class to the clicked option
      $('#' + db + '-option').addClass('selected');
      
      // Update the hidden radio button value
      Shiny.setInputValue('db', db, {priority: 'event'});
    }
    
  ")), 
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
  fixedPanel(
    bottom = 10, left = 10,
    HTML('<script type="text/javascript" src="https://cdnjs.buymeacoffee.com/1.0.0/button.prod.min.js" data-name="bmc-button" data-slug="noamane" data-color="#FFDD00" data-emoji=""  data-font="Cookie" data-text="Buy me a coffee" data-outline-color="#000000" data-font-color="#000000" data-coffee-color="#ffffff" ></script>') 
  ),
  includeCSS("www/style.css"),
  shinyjs::useShinyjs(),
  page_navbar(
    id="tab_selector", 
    selected = "Home", 
    theme = bs_theme(
      preset = "shiny",
      "primary" = "#0675DD"
    ), 
    title = tags$span(
      tags$img(
        src = "www/logo.svg",
        width = "50px",
        height = "auto",
        class = "me-3",
        alt = "Shiny hex logo"
      ),
      "PEACOCK"
    ),
    sidebar = sidebar(
      title =uiOutput("title_sidebar"), 
      gap = "0.5rem", padding = "0.5rem",width = "300px", 
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
      icon = icon("home"), 
      title = 'Home',
      uiOutput("home")
    ),
    nav_panel(
      icon = icon("cog"), 
      title = "Settings",
      uiOutput("settings")
    ),
    nav_panel(
      icon=icon("info-circle"), 
      title ='About',
      uiOutput("about")
    ),
    nav_item(
      input_dark_mode(id = "dark_mode", mode = "dark")
    )
  )
)