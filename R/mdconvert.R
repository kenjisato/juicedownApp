# Default documents ----
.sample_md <-
  paste(readLines(system.file("text", "sample.md", package = .packageName)),
        collapse = "\n")
.sample_page_css <-
  paste(readLines(system.file("css", "article.scss", package = "juicedown")),
        collapse = "\n")
.sample_label_css <-
  paste(readLines(system.file("css", "div.scss", package = "juicedown")),
        collapse = "\n")

# UI fragments ----

left_panel <- function(.ns) {
  tabsetPanel(
    type = "tabs",
    #
    # TextArea for Markdown
    #
    tabPanel(
      "Markdown",
      div(
        class = "ta",
        textAreaInput(.ns("md"), "", value = .sample_md)
      ),
      actionButton(.ns("default_md"), "Reset to default document",
                   icon = icon("broom")),
    )
    #
    # TextArea for CSS
    #
    , tabPanel(
      "CSS",
      div(
        class = "ta",
        textAreaInput(.ns("css"), "", value = .sample_page_css)
      ),
      fluidRow(
        column(6, actionButton(.ns("default_css"), "Reset to default CSS",
                              icon = icon("broom"))),
        column(6, radioButtons(.ns("type_css"), "Type: ",
                               c("Page" = "page", "Label" = "label"),
                               width = "100%", inline = TRUE))

      )
    )
    #
    # TextArea for CSS
    #
    , tabPanel(
      "Options",
      div(id = "options-container",
          fluidRow(
            checkboxInput(.ns("tweak_h2"), "Remove redundant heading (JS)", value = FALSE, width = NULL),
            checkboxInput(.ns("tweak_fn"), "Fix footnote styling (JS)", value = FALSE, width = NULL),
          )   
      )
    )
  )
}


right_panel <- function(.ns) {
  fluidRow(
    rclipboard::rclipboardSetup(),
    tags$head(
      tags$script(
        defer="",
        src="https://cdn.jsdelivr.net/combine/npm/katex/dist/katex.min.js,npm/katex/dist/contrib/auto-render.min.js,npm/@xiee/utils/js/render-katex.js",
        crossorigin="anonymous")
    ),
    div(
      id = "right-panel",
      fluidRow(
        column(9,
               actionButton(.ns("convert"),
                            "Convert", icon = icon("file-code")),
               uiOutput(.ns("clip"))
               ),
        column(3,
               downloadButton(.ns("downloadData"), "Download"))
      ),
      hr(),
      htmlOutput(.ns("preview"))
    )
  )
}


# Module UI ----

mdconvertUI <- function(id) {
  ns <- NS(id)
  tagList(
    shinyStorePlus::initStore(), ###############################################
    column(6, left_panel(ns)),
    column(6, right_panel(ns)),
    includeCSS(system.file("www", "css", "style.css", package = .packageName))
  )
}

# Server function ----

mdconvertServer <- function(id) {

  .tdir <- tempfile(pattern = "dir")
  dir.create(.tdir)
  .md_file <- file.path(.tdir, "page.md")
  .css_file <- file.path(.tdir, "style.scss")
  .html_file <- file.path(.tdir, "page.html")

  moduleServer(id, function(input, output, session) {

    r <- reactiveValues(html = "")

    # Convert button
    observeEvent(input$convert, {

      # Do the conversion.
      md <- if (stringr::str_length(input$md) == 0) {
        .sample_md
      } else {
        input$md
      }
      
      # Options
      option_lines <- character(0)
      if (input$tweak_h2) {
        option_lines <- c(option_lines, "juicedown::tweak_moodle_heading()")
      }
      if (input$tweak_fn) {
        option_lines <- c(option_lines, "juicedown::tweak_footnote_highlight()")
      }
      if (length(option_lines) > 0) {
        option_lines <- c("```{r}", option_lines, "```")
      }
      md <- c(md, option_lines)
      
      writeLines(md, .md_file)

      css <- input$css
      writeLines(css, .css_file)

      tag <- if (input$type_css == "page") "article" else "div"

      r$html <- juicedown::convert(
        .md_file, clip = FALSE, stylesheet = .css_file, tag = tag
      )
      output$preview <- renderUI(withMathJax(HTML(paste(r$html, collapse = "\n"))))
    })

    # Download Button
    output$downloadData <- downloadHandler(
      filename = function() {
        paste0("moodle-page-", format(Sys.time(), "%Y%m%d-%H%M%S"), ".zip")
      },
      content = function(file) {
        writeLines(r$html, .html_file)
        utils::zip(file, c(.md_file, .html_file, .css_file), extras = '-j')
      }
    )

    # Reset to default
    observeEvent(input$default_md, {
      updateTextAreaInput(inputId = "md", value = .sample_md)
    })

    observeEvent(input$default_css, {
      default_css <- if (input$type_css == "page") {
        .sample_page_css
      } else {
        .sample_label_css
      }
      updateTextAreaInput(inputId = "css", value = default_css)
    })

    # Copy to clipboard
    output$clip <- renderUI({
      output$clip <- renderUI({
        rclipboard::rclipButton(
          inputId = NS(id, "clipbtn"),
          label = "Copy HTML to Clipboard",
          clipText = r$html,
          icon = icon("clipboard")
        )
      })
    })

    observeEvent(input$clipbtn, {
      showNotification("Copied!", type = "message", duration = 2)
    })
  })
}

mdconvertApp <- function() {
  ui <- tags$body(
    fluidPage(
      title = "md2html",
      fluidRow(
        mdconvertUI("test")
      )
    )
  )
  server <- function(input, output, server) {
    mdconvertServer("test")
  }
  shinyApp(ui, server, options = list(launch.browser = TRUE))
}


