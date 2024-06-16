test_that("app command (Audio)", {
  expect_equal(
    process_command("{% Audio('foo') %}"),
    "\n```{r}\nincludeAudio('foo')\n```\n"
  )
  expect_equal(
    process_command('{% Audio("foo") %}'),
    '\n```{r}\nincludeAudio("foo")\n```\n'
  )
  expect_equal(
    process_command("{% Audio(foo) %}"),
    "{% Audio(foo) %}"
  )
})

test_that("app command works (YouTube)", {
  expect_equal(
    process_command("{% YouTube('foo') %}"),
    "\n```{r}\nincludeYT('foo')\n```\n"
  )
  expect_equal(
    process_command('{% YouTube("foo") %}'),
    '\n```{r}\nincludeYT("foo")\n```\n'
  )
  expect_equal(
    process_command("{% YouTube(foo) %}"),
    "{% YouTube(foo) %}"
  )
})

test_that("tweak options", {
  text <- c("hello")
  input <- list(
    tweak_h2 = TRUE,
    tweak_fn = TRUE
  )
  expect_equal(
    process_options(text, input),
    c(
      text,
      "```{r}",
      "juicedown::tweak_moodle_heading()",
      "juicedown::tweak_footnote_highlight()",
      "```"
    )
  )

  input <- list(
    tweak_h2 = TRUE,
    tweak_fn = FALSE
  )
  expect_equal(
    process_options(text, input),
    c(
      text,
      "```{r}",
      "juicedown::tweak_moodle_heading()",
      # "juicedown::tweak_footnote_highlight()",
      "```"
    )
  )

  input <- list(
    tweak_h2 = FALSE,
    tweak_fn = TRUE
  )
  expect_equal(
    process_options(text, input),
    c(
      text,
      "```{r}",
      # "juicedown::tweak_moodle_heading()",
      "juicedown::tweak_footnote_highlight()",
      "```"
    )
  )

  input <- list(
    tweak_h2 = FALSE,
    tweak_fn = FALSE
  )
  expect_equal(
    process_options(text, input),
    c(text)
  )
})
