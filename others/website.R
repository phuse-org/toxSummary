library(usethis)
library(pkgdown)

pkgdown::build_site()
usethis::use_article("Introduction")
pkgdown::build_site()
pkgdown::preview_site()
devtools::build()
devtools::document()