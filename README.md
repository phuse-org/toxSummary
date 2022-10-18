

# toxSummary


## Overview
Visualize and Summarize Nonclinical Study Results


## How to run the app
Clone the repo and set repo as working directory. 
Open app.R file (or copy code given below) and run all the code.  

```
pkgload::load_all(".")

db_path <- "test_data/test_db.db"
study_list_path <- "test_data/IND_with_studies_2.csv"

toxSummary::toxSummary_app(
    database_path = db_path,
    studyid_file = study_list_path,
    save_file_path = NULL
)
```
 This will open toxSummary app.   
 The default/example database (in "test_data/test_db.db" directory) only contain few studies.
 To connect your database
 ```
 pkgload::load_all(".")
 toxSummary::toxSummary_app(
    database_path = "path/of/your/database.db"
    studyid_file = "path/for/IND_studyid_mapping.csv"
    save_file_path = NULL
)
 ```

## When published in public repo (GitHub)
This can be install like a R package
```
# install devtools if already not installed 
install.packages("devtools")
#install toxSummary package
devtools::install_github('phuse-org/toxSummary')
```
then run

```
toxSummary::toxSummary_app(
    database_path = "path/of/your/database.db"
    studyid_file = "path/for/IND_studyid_mapping.csv"
    save_file_path = NULL
)
```



```database_path = "path/of/your/database.db"```
```studyid_file = "path/for/IND_studyid_mapping.csv"``` 
Both argument are mandatory when  running the function from package.  
Package does not contain database. Database is too big for a package.

## Deploy the app (WIP)

### from cloned repo:
Deploying Shiny app as package is little different. 
There is not publish button when you open app.R file in RStudio.
__crate manifest.json file__
- shinyappsio 
  ``` rsconnect::deployApp()``` 
- rsconnect
  ```rsconnect::deployApp()``` 

### From the installed package
app.R file not included in the package.
first create app.R file in the working directory. Then copy the code from here and change the database and file paths.

```

 toxSummary::toxSummary_app(
    database_path = "path/of/your/database.db"
    studyid_file = "path/for/IND_studyid_mapping.csv"
    save_file_path = NULL
```


## Demo App
A demo app can be found here
 
[Demo Shiny App_updated](https://yousuf28.shinyapps.io/toxsummary)

older version of the app  
[Demo Shiny App](https://phuse-org.shinyapps.io/toxSummary/) 