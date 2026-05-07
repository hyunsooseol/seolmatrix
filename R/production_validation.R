gtheory_safe_formula <- function(formula_text) {
  
  if (is.null(formula_text) || !nzchar(trimws(formula_text)))
    stop("Formula is empty.", call. = FALSE)
  
  formula_text <- trimws(formula_text)
  
  # Block backtick-quoted function calls
  # Example: `base::system`('ls'), `system`('ls')
  if (grepl("`[^`]+`\\s*\\(", formula_text, perl = TRUE)) {
    stop(
      "Security violation: Backtick-quoted function calls are not permitted in formulas.",
      call. = FALSE
    )
  }
  
  # Strip backtick-quoted variable names before checking function calls.
  # This avoids false positives for variables like `Marker Level (ng/ml)`.
  formula_text_stripped <- gsub("`[^`]*`", "", formula_text, perl = TRUE)
  
  unsafe_patterns <- c(
    "\\bsystem\\s*\\(",
    "\\bsystem2\\s*\\(",
    "\\bshell\\s*\\(",
    "\\bsource\\s*\\(",
    "\\beval\\s*\\(",
    "\\bparse\\s*\\(",
    "\\bassign\\s*\\(",
    "\\bget\\s*\\(",
    "\\bmget\\s*\\(",
    "\\bdo\\.call\\s*\\(",
    "\\bload\\s*\\(",
    "\\breadRDS\\s*\\(",
    "\\bunserialize\\s*\\(",
    "\\bfile\\s*\\(",
    "\\bfile\\.remove\\s*\\(",
    "\\bfile\\.copy\\s*\\(",
    "\\bfile\\.create\\s*\\(",
    "\\bwrite\\s*\\(",
    "\\bwrite\\.csv\\s*\\(",
    "\\bwrite\\.table\\s*\\(",
    "\\bdownload\\.file\\s*\\(",
    "\\burl\\s*\\(",
    "\\bsocketConnection\\s*\\(",
    "::",
    ":::"
  )
  
  for (pattern in unsafe_patterns) {
    if (grepl(pattern, formula_text_stripped, perl = TRUE)) {
      stop("Unsafe formula was blocked.", call. = FALSE)
    }
  }
  
  stats::as.formula(formula_text)
}