library(usethis)
library(pkgdown)
usethis::use_vignette("Introduction")
pkgdown::build_site()
usethis::use_article("Introduction")
pkgdown::build_site()
pkgdown::preview_site()
devtools::build()
devtools::document()
# pkgdown::build_favicons( overwrite = T)
# when use logo, create index.md file and copy content from readme.md file.
# otherwise logo apprearce twice in website. 
# when create article keep the image in doc/articles/images directoryfile:///C:/Users/Md.Ali/OneDrive%20-%20FDA/yousuf/00_github_projects/toxSummary/docs/index.htmlP