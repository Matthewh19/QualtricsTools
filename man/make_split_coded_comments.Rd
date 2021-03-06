% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helper_functions.R
\name{make_split_coded_comments}
\alias{make_split_coded_comments}
\title{Split a Survey's Split Coded Comment Appendices}
\usage{
make_split_coded_comments(
  qsf_path,
  csv_path,
  sheets_dir,
  output_dir,
  split_by,
  n_threshold = 15,
  headerrows,
  code_type = "nvivo"
)
}
\arguments{
\item{qsf_path}{(optional) is the string path location of the .qsf file to be processed.}

\item{csv_path}{(optional) is the string path location of the .csv file to be processed.}

\item{sheets_dir}{is the string path location of the directory which contains Excel documents
with a "Coded" sheet formatted as specified on the wiki:
https://github.com/ctesta01/QualtricsTools/wiki/Comment-Coding}

\item{output_dir}{specifies the path of the directory to save the output file in.}

\item{split_by}{is a list which specifies which columns should be used to split the respondents.}

\item{n_threshold}{is the number of verbatim comments which must appear before an appendix of
coded comments will be included.}

\item{headerrows}{(optional) specifies the number of header rows in the CSV data.}

\item{code_type}{type of coded comment data export. The default is
to use NVivo crosstab export with the ResponseID in the first row and
second column labeled with the varnmae and containing response presence
1/0 indicator. For old filemaker pro format, use "fmp" specification.
(\code{"nvivo"} or \code{"fmp"})}
}
\description{
This question automates the entire process of splitting a
survey's text appendices by specific response columns. The QSF
and CSV file are passed as string arguments,
the sheets_dir specifies where the coded comments excel or csv
data is stored, and the output_dir specifies where the split
coded comment appendices should be saved. The n_threshold
specifies how many coded comments there must be before the coded
comment appendices are included, and headerrows is an argument
necessary to process the survey results correctly.
}
