source("global.R")
source("ui.R")
source("server.R")

addResourcePath("www", "www")


shiny::shinyApp(ui, server,options = list(port =3838 ,host = "0.0.0.0"))