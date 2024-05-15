# Default documents ----
.sample_md <- read_pkg_file("text", "sample.md")
.sample_page_css <- read_pkg_file("css", "article.scss", pkg = "juicedown")
.sample_label_css <- read_pkg_file("css", "div.scss", pkg = "juicedown")

# UI fragments ----


left_panel <- function(.ns) {
  panel_md <- nav_panel("Markdown",
                        class = "ta",
                        textAreaInput(.ns("md"), NULL, value = .sample_md))
  panel_css <- nav_panel("CSS",
                         class = "ta",
                         textAreaInput(.ns("css"), NULL, value = .sample_page_css))
  
  panel_opts <- nav_panel(
    icon("gear"), 
    id = "options-container",
    card(
      fill = FALSE,
      card_header("Backup and Restore", class = "bg-dark"),
      downloadLink(.ns("downloadData"), "Download ZIP", icon = icon("download")),
      actionLink(
        .ns("default_md"),
        "Reset to default document",
        icon = icon("broom")
      ),
      layout_column_wrap(
        width = 1 / 2,
        actionLink(.ns("default_css"), "Reset to default CSS",
                   icon = icon("broom")),
        radioButtons(
          .ns("type_css"),
          "Type: ",
          c("Page" = "page", "Label" = "label"),
          width = "100%",
          inline = TRUE
        )
      )
    ),
    card(
      fill = FALSE,
      card_header("Tweaks", class = "bg-dark"),
      checkboxInput(
        .ns("tweak_h2"),
        "Remove redundant heading (JS)",
        value = FALSE,
        width = NULL
      ),
      checkboxInput(
        .ns("tweak_fn"),
        "Fix footnote styling (JS)",
        value = FALSE,
        width = NULL
      )
    )
  )
  
  page_fillable(id = "left-panel",
                navset_card_underline(panel_md,
                                      panel_css,
                                      nav_spacer(),
                                      panel_opts))
}


right_panel <- function(.ns) {
  btn_convert <- nav_item(
    actionButton(
      .ns("convert"),
      "Convert",
      icon = icon("file-code"),
      class = "btn-sm"
    )
  )
  
  tagList(
    rclipboard::rclipboardSetup(),
    tags$head(
      tags$script(
        defer = "",
        src = .katex_url,
        crossorigin = "anonymous"
      )
    ),
    page_fillable(
      id = "right-panel",
      navset_card_underline(
        id = .ns("right_nav"),
        nav_panel("Preview", htmlOutput(.ns("preview"))),
        nav_panel(shiny::icon("markdown"), includeHTML(pkg_file("www", "info.html"))),
        nav_spacer(),
        btn_convert,
        nav_item(uiOutput(.ns("clip")))
      )
    )
  )
}


# Module UI ----

mdconvertUI <- function(id) {
  ns <- NS(id)
  tagList(
    shinyStorePlus::initStore(),
    layout_column_wrap(width = 1 / 2, left_panel(ns), right_panel(ns)),
    includeCSS(pkg_file("www", "css", "style.css"))
  )
}

# Server function ----

mdconvertServer <- function(id, allow_r = TRUE) {
  .tdir <- tempfile(pattern = "dir")
  dir.create(.tdir)
  .md_file <- file.path(.tdir, "page.md")
  .css_file <- file.path(.tdir, "style.scss")
  .html_file <- file.path(.tdir, "page.html")
  .files <- c(.md_file, .css_file, .html_file)
  
  moduleServer(id, function(input, output, session) {
    r <- reactiveValues(html = "")
    
    # Initial Preview
    output$preview <-
      renderUI("Click on the convert button.")
    
    # Convert button
    observeEvent(input$convert, {
      
      md <- preprocess_md(input, allow_r)
      writeLines(md, .md_file)
      
      css <- input$css
      writeLines(css, .css_file)
      
      tag <- if (input$type_css == "page") "article" else "div"
      
      r$html <- juicedown::convert(.md_file,
                                   clip = FALSE,
                                   stylesheet = .css_file,
                                   tag = tag)
      output$preview <-
        renderUI(withMathJax(HTML(paste(
          r$html, collapse = "\n"
        ))))
      
      nav_select(id = "right_nav", selected = "Preview")
      unlink(.files)
    })
    
    # Download Button
    output$downloadData <- downloadHandler(
      filename = function() {
        paste0("juicedown-pack-",
               format(Sys.time(), "%Y%m%d-%H%M%S"),
               ".zip")
      },
      content = function(file) {
        
        md <- preprocess_md(input, allow_r)
        writeLines(md, .md_file)
        
        css <- input$css
        writeLines(css, .css_file)
        
        writeLines(r$html, .html_file)
        utils::zip(file, c(.md_file, .html_file, .css_file), extras = '-j')
        
        unlink(.files)
      }
    )
    
    # Reset to default
    observeEvent(input$default_md, {
      shinyalert::shinyalert(
        title = "Caution!",
        text = "Any changes you made to the markdown document will be discarded.",
        size = "s", 
        type = "warning",
        showConfirmButton = TRUE,
        showCancelButton = TRUE,
        confirmButtonText = "OK",
        confirmButtonCol = "#AEDEF4",
        cancelButtonText = "Cancel",
        callbackR = function(x) {
          if (x != FALSE) {
            updateTextAreaInput(inputId = "md", value = .sample_md)
          }
        }
      )
    })
    
    observeEvent(input$default_css, {
      default_css <- if (input$type_css == "page") {
        .sample_page_css
      } else {
        .sample_label_css
      }
      shinyalert::shinyalert(
        title = "Caution!",
        text = "Any changes you made to the custom CSS will be discarded.",
        size = "s", 
        type = "warning",
        showConfirmButton = TRUE,
        showCancelButton = TRUE,
        confirmButtonText = "OK",
        confirmButtonCol = "#AEDEF4",
        cancelButtonText = "Cancel",
        callbackR = function(x) {
          if (x != FALSE) {
            updateTextAreaInput(inputId = "css", value = default_css)
          }
        }
      )
    })
    
    # Copy to clipboard
    output$clip <- renderUI({
        rclipboard::rclipButton(
          inputId = NS(id, "clipbtn"),
          label = "Copy to Clipboard",
          clipText = r$html,
          icon = icon("clipboard"),
          class = "btn-sm"
        )
    })
    
    
    observeEvent(input$clipbtn, {
      showNotification("Copied!", type = "message", duration = 2)
    })
  })
}

mdconvertApp <- function() {
  ui <- tags$body(fluidPage(title = "md2html",
                            fluidRow(mdconvertUI("test"))))
  server <- function(input, output, server) {
    mdconvertServer("test")
  }
  shinyApp(ui, server, options = list(launch.browser = TRUE))
}
