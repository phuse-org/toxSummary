
# toxSummary
toxSummary is an R Shiny app to visualize and summarize repeat-dose toxicology
study results. toxSummary app wrapped in R package (which is also called 
toxSummary) so app can be distributed easily. 

 <a href="https://github.com/phuse-org/toxSummary"><img
src="man/figures/logo.png" 
alt="toxSummary logo" align="right" width=30% height="auto"/></a>

## Overview

Prior to initiation of clinical trials, repeat-dose toxicology studies are
conducted in multiple species to support the safety of the active pharmaceutical
ingredient (API) in the proposed clinical dosing regimen, route of
administration, and duration of treatment.  The primary metric used to
extrapolate the safety of clinical dosage from repeat-dose toxicology study
results is the safety margin, i.e. the ratio of no observable adverse effect
level (NOAEL) from the toxicology study to the proposed clinical dose. This
ratio can be calculated by using allometric scaling to approximate the
equivalent human dose from that used in the toxicology study based on the body
surface area of the species employed or by comparing the empirically measured
maximum plasma concentration (Cmax) or total plasma exposure (AUC) between the
toxicokinetic animal data and the human pharmacokinetic data, if available.
Another important consideration in drug safety evaluation is the nature and
severity of the toxicities observed at doses above the NOAEL. As toxicity
studies of various durations are typically conducted in multiple species and
potentially via multiple routes of administration, it can be challenging to
effectively integrate all of this information. In collaboration with the
Pharmaceutical Users Software Exchange (PHUSE) Nonclinical Scripts Working Group
and with consultation from toxicologists at FDA, an open source R shiny
application was developed to allow users to interactively visualize safety
margins and the severity of user-defined significant toxicities across studies
throughout a drug development program in a single plot.  The application can
also present this information in tabular form that can be exported in various
formats, e.g. CSV, Excel or Word files. These functionalities are designed to
facilitate holistic evaluation of the drug safety by generating graphical and
tabular summaries of the full toxicological profile of an API. 

<a href="https://github.com/phuse-org/toxSummary"><img src="man/figures/toxsui.jpg" 
alt="UI" align="center" width=100% height="auto"/></a>

## Installation  

Package can be installed from CRAN.

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
toxSummary::toxSummary_app()

```
## Run the app with database connection

```
library(toxSummary)
toxSummary::toxSummary_app(
    database_path = "path/of/your/database.db",
    studyid_file = "path/for/program_studyid_mapping.csv",
    save_file_path = NULL
)
```

When save_file_path set to NULL, files will be saved in current working directory.
an example database can be found in GitHub repository
[database link](https://github.com/phuse-org/toxSummary/tree/master/test_data)

```database_path = "path/of/your/database.db"```  
```studyid_file = "path/for/program_studyid_mapping.csv"```   
Package does not contain database. Database is too big for a package.

## How to run the app from cloned repo.

Clone the [GitHub](https://github.com/phuse-org/toxSummary) repo and set repo as
working directory.
If you don't have any database to connect and want to run app, 
only run this following code in R console.
```
pkgload::load_all(".")
toxSummary::toxSummary_app()
```
To run the app with database connection follow this direction;
set repo as working directory and open app.R file (or copy code given below) 
and run the code.   if you connect to your database then change the path to 
your files.
otherwise this will connect to  example database available in test_data
directory.

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
(in "test_data/test_db.db" directory) only contain few studies.

## How to deploy the app
#### From the installed package
First create app.R file in the working directory.  
Then copy the code from here and change the database and file paths.

```
 toxSummary::toxSummary_app(
    database_path = "path/of/your/database.db",
    studyid_file = "path/for/program_studyid_mapping.csv",
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
[demo app link](https://phuse-org.shinyapps.io/toxSummary/) 

## toxSummary license  

MIT LICENSE 

Copyright (c) 2023 Food and Drug Administration (FDA) 

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE, ANY UPDATES TO THE SOFTWARE MADE BY CRAN, OR THE USE OR 
OTHER DEALINGS IN THE SOFTWARE OR ANY UPDATED VERSION.

This package/shiny app reflects the views of the author and should not be 
construed to represent FDA’s views or policies.

Any examples or sample analyses in this package are for illustrative purposes only.

Nothing in these scripts is intended to guide the analytic process and 
any interpretations of data found as a result of using these scripts are 
solely the responsibility of the user of the scripts and not the developers.
 All users are responsible for ensuring their own compliance with applicable laws, 
regulations, and agency guidance.
