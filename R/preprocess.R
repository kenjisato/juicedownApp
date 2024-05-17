process_command <- function(text) {
  # Convert app commands into juicedown function calls
  app_commands <- list(
    YouTube = "includeYT",
    Audio = "includeAudio"
  )
  
  for (i in seq_along(app_commands)) {
    name <- names(app_commands)[[i]]
    juicedown_name <- app_commands[[i]]
    from <-  paste0('\\{%\\s*', name, '\\(((\'.*\')|(\".*\"))\\)\\s*%\\}')
    to <- paste0('\n```{r}\n', juicedown_name, '(\\1)\n```\n')
    text <- stringr::str_replace(text, from, to)
  }
  text
}

process_chunk <- function(text, kill_chunk) {
  # 
  if (kill_chunk) {
    text <- text |>
      stringr::str_replace_all("```\\{r.*\\}", "```r") |>
      stringr::str_replace_all("`r(.*)`", "`\\1`")
  }
  text
}

process_options <- function(text, input, download) {
  if (download) return(text)
  
  option_lines <- c(
    if (input$tweak_h2) "juicedown::tweak_moodle_heading()" else NULL,
    if (input$tweak_fn) "juicedown::tweak_footnote_highlight()" else NULL
  )

  if (length(option_lines) > 0) {
    option_lines <- c("```{r}", option_lines, "```")
  }
  c(text, option_lines)
}


preprocess_md <- function(input, kill_chunk, download = FALSE) {
  md <- if (stringr::str_length(input$md) == 0) {
    installed_doc("text", "default")
  } else {
    input$md
  }
  
  # Process
  md <- md |>
    process_chunk(kill_chunk) |>
    process_command() |>
    process_options(input, download)
}

preprocess_css <- function(input) {
  if (input$css == "") {
    "/* empty */\n"
  } else {
    input$css
  }
}

