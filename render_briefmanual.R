library(rmarkdown)

rmd_file <- "vignettes/briefmanual.Rmd"

# Render to HTML vignette
render(rmd_file,
       output_format = "rmarkdown::html_vignette",
       output_file = "briefmanual.html")

# Render to GitHub-flavored Markdown
render(rmd_file,
       output_format = "rmarkdown::github_document",
       output_file = "briefmanual.md")

# Move the markdown files to the root directory
file.rename("vignettes/briefmanual.md", "briefmanual.md")
file.copy(
  list.files("vignettes/briefmanual_files/", full.names = TRUE),
  "briefmanual_files/",
  recursive = TRUE)
unlink("vignettes/briefmanual_files/", recursive = TRUE)
