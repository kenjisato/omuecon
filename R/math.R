correct_equations_ <- function(text) {
  # inline math
  text <- gsub("\u3001$", "\u3001 $", text, fixed = TRUE)
  text <- gsub("\u3002$", "\u3002 $", text, fixed = TRUE)

  # math begin
  math_lines <- which(head(text, -1) == "$$" & tail(text, -1) == "\\begin{aligned}")
  for (i in math_lines) {
    text[[i]] <- "$$\\begin{aligned}"
    text[[i+1]] <- NA
  }

  # math end
  math_lines <- which(head(text, -1) == "\\end{aligned}" & tail(text, -1) == "$$")
  for (i in math_lines) {
    text[[i]] <- "\\end{aligned}$$"
    text[[i+1]] <- NA
  }
  text
}

correct_equations <- function(file) {
  mdvec <- readLines(file)
  mdvec_ <- correct_equations(mdvec)
  writeLines(na.omit(mdvec_), file)
}
