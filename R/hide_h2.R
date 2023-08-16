
hide_h2 <- function() {
  js <- "<script>
    document.querySelector('#maincontent+h2').style.display = 'none';
  </script>"
  knitr::asis_output(js)
}
