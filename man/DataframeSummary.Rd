\name{DataframeSummary}
\alias{DataframeSummary}
\title{Datafram Summary}
\description{
  Summarize every numeric and factor column in a data frame.
}
\usage{
DataframeSummary(x, conf.intervals = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{dataframe object to be summarized}
  \item{conf.intervals}{a logical indicating whether you want confidence intervals in the output}
}
\value{
  Textual summary tables for numeric and factor columns in the dataframe.
}
\author{
Andrew Heiss \email{andrew@andrewheiss.com}
}
\examples{
DataframeSummary(USArrests)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{summary}
