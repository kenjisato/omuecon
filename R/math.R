correct_equations <- function(file) {
  mdvec <- readLines(file)

  # inline math
  mdvec <- gsub("\u3001$", "\u3001 $", mdvec, fixed = TRUE)
  mdvec <- gsub("\u3002$", "\u3002 $", mdvec, fixed = TRUE)

  # math begin
  math_lines <- which(head(mdvec, -1) == "$$" & tail(mdvec, -1) == "\\begin{aligned}")
  for (i in math_lines) {
    mdvec[[i]] <- "$$\\begin{aligned}"
    mdvec[[i+1]] <- NA
  }

  # math end
  math_lines <- which(head(mdvec, -1) == "\\end{aligned}" & tail(mdvec, -1) == "$$")
  for (i in math_lines) {
    mdvec[[i]] <- "\\end{aligned}$$"
    mdvec[[i+1]] <- NA
  }
  writeLines(na.omit(mdvec), file)
}
