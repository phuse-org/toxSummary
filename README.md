

# toxSummary


## Overview
Visualize and Summarize Nonclinical Study Results.  

## Installation  
Package can be installed from CRAN when published to CRAN.

```
#install toxSummary package
install.packages("toxSummary")
```
Development version can be installed from GitHub.

```
# install devtools if already not installed 
install.packages("devtools")
#install toxSummary package
devtools::install_github('phuse-org/toxSummary')
```
## Run the app
```
library(toxSummary)
toxSummary::toxSummary_app(
    database_path = "path/of/your/database.db",
    studyid_file = "path/for/IND_studyid_mapping.csv",
    save_file_path = NULL
)
```
an example database can be found in GitHub repository
[database link](https://github.com/phuse-org/toxSummary/tree/master/test_data)


```database_path = "path/of/your/database.db"```  
```studyid_file = "path/for/IND_studyid_mapping.csv"```   
Both argument are mandatory when  running the function from package.  
Package does not contain database. Database is too big for a package.

## How to run the app from cloned repo.
Clone the [GitHub](https://github.com/phuse-org/toxSummary) repo and set repo as
working directory. Open app.R file (or copy code given below) and run the code.  

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
 This will open toxSummary app.  The default/example database
(in "test_data/test_db.db" directory) only contain few studies. To connect your
database provide files directory.  

 ```
 pkgload::load_all(".")
 toxSummary::toxSummary_app(
    database_path = "path/of/your/database.db",
    studyid_file = "path/for/IND_studyid_mapping.csv",
    save_file_path = NULL
)
 ```

## How to deploy the app
#### From the installed package
First create app.R file in the working directory.  
Then copy the code from here and change the database and file paths.

```
 toxSummary::toxSummary_app(
    database_path = "path/of/your/database.db",
    studyid_file = "path/for/IND_studyid_mapping.csv",
    save_file_path = NULL,
    where_to_run = "rsconnect")

```

Deploying Shiny app as package is little different. 
There is no publish button when you open app.R file in RStudio.  Running
``` rsconnect::writeManifest() ``` command in R console will create
manifest.json file. app then can be deployed on rsconnect running  
``` rsconnect::deployApp()``` command in R console.  

Keep in mind that when app running on server, app need to have access to the
files given in database_path and studyid_file argument. 


## Demo App
A demo app can be found here
[update the link after deploy new version](https://phuse-org.shinyapps.io/toxSummary/) 