#' Launch shiny app
#'
#' @param ... Not used yet.
#'
#' @return Launch a shiny app.
#' @export
#'
up <- function(...) {
  ui <- navbarPage("juicedown",
          tabPanel("Moodle Page Helper",
                   mdconvertUI("md2html"),
                   value = 1L),
          tabPanel("Code Samples",
                   fluidPage(
                     mainPanel(
                       includeHTML(system.file("www", "samples.html", package = .packageName))
                     )
                   ),
                   value = 2L)
          #, tabPanel("Component 3", value = 3L)
  )

  server <- function(input, output, session) {

    mdconvertServer("md2html")
    
    #stores setup
    appid = "juicedownApp001"
    shinyStorePlus::setupStorage(appId = appid,inputs = TRUE)
  }

  shinyApp(ui, server, options = list(launch.browser = TRUE))
}
