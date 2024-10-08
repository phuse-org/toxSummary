% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tox_summary_new_UI.R
\name{toxSummary_app}
\alias{toxSummary_app}
\title{Run toxSummary app}
\usage{
toxSummary_app(
  database_path = NULL,
  studyid_file = NULL,
  save_file_path = NULL,
  where_to_run = "local"
)
}
\arguments{
\item{database_path}{Optional, character\cr
file path for database}

\item{studyid_file}{Optional, character\cr
file path for studyid}

\item{save_file_path}{optional, character\cr
directory where files will be saved when app create any files.
if NULL, all the files will be created in temp directory.}

\item{where_to_run}{Optional, character\cr
where app will be running. Default is "local". Should use "rsconnect"
when deploy to rsconnect.}
}
\value{
function run the app.
}
\description{
Run toxSummary app
}
\examples{
if(interactive()) {
toxSummary_app()
}
}
