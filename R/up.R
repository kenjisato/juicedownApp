# Components
link_juicedown_app <- tags$a(
  shiny::icon("github"), "juicedownApp",
  href = "https://github.com/kenjisato/juicedownApp",
  target = "_blank"
)
link_juicedown <- tags$a(
  shiny::icon("github"), "juicedown",
  href = "https://github.com/kenjisato/juicedown",
  target = "_blank"
)

#' Launch shiny app
#'
#' @param allow_r logical. If set to FALSE, code chunks are converted into a 
#'  verbatim code block during the conversion process.
#' @param ... Not used yet.
#'
#' @return Launch a shiny app.
#' @export
#'
up <- function(allow_r = TRUE, ...) {
  
  if (allow_r) {
    sample_html <- includeHTML(pkg_file("www", "samples.html"))
  } else {
    sample_html <- includeHTML(pkg_file("www", "samples_web.html"))
  }
  
  ui <- page_navbar(
    title = "juicedown",
    theme = bs_theme(version = "5", preset = "bootstrap"),
    nav_panel("Converter", mdconvertUI("md2html"), value = 1L),
    nav_panel("Notes", page_fluid(sample_html), value = 2L),
    nav_spacer(),
    nav_item(input_dark_mode()),
    nav_menu(
      title = "Links",
      align = "right",
      nav_item(link_juicedown_app),
      nav_item(link_juicedown)
    )
  )

  server <- function(input, output, session) {
    #bs_themer()
    mdconvertServer("md2html", allow_r = allow_r)
    
    #stores setup
    appid = "juicedownApp001"
    shinyStorePlus::setupStorage(appId = appid,inputs = TRUE)
  }

  shinyApp(ui, server, options = list(launch.browser = TRUE))
}
