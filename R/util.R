### Utility functions

pkg_file <- function(..., pkg = .packageName) {
  system.file(..., package = pkg, mustWork = TRUE)
}

read_pkg_file <- function(..., pkg = .packageName) {
  paste(readLines(pkg_file(..., pkg = pkg)), collapse = "\n")
}

installed_doc <- function(type = c("text", "css"), name = NULL) {
  type <- match.arg(type)
  name <- if (is.null(name)) "default" else name
  
  file <- if (type == "text") {
    switch(name,
           "default" = read_pkg_file("text", "default.md"),
           read_pkg_file("text", "default.md")
    )
  } else if (type == "css") {
    switch(name, 
           "article" = read_pkg_file("css", "article.scss"),
           "div" = read_pkg_file("css", "div.scss"),
           "simple" = read_pkg_file("css", "simple.scss"),
           read_pkg_file("css", "article.scss")
    )
  } 
  file
}
