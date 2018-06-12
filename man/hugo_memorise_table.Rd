\name{hugo_memorise_table}
\alias{hugo_memorise_table}
\title{Saves a table}
\usage{
  hugo_memorise_table(table = NULL, name = substitute(table))
}
\arguments{
  \item{table}{a table to be saved}
  
  \item{name}{a name of files}
}
\description{
  Saves a table in the directory chosen at the beginning of the current
  Hugo Investigation in the subfolder 'gallery' in .rda file and tables in markdown,
  .xlsx, and .docx format in .md file
}
\examples{
  \dontrun{a <- 1:3
  tab <- table(a, sample(a))
  hugo_memorise_table(tab, name = "table")
  Or save as "tab":
  hugo_memorise_table(tab)}
}
\author{
  Maria Piliszek
}
