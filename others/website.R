library(usethis)
library(pkgdown)

pkgdown::build_site()
usethis::use_article("Introduction")
pkgdown::build_site()
