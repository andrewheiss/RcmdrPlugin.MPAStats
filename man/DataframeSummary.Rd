\name{DataframeSummary}
\alias{DataframeSummary}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Dataframe Summary}
\description{
  Summarize every variable in a data frame.
}
\usage{
DataframeSummary(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{Dataframe object to be summarized.}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
  A dataframe object with a row for each of the original dataframe's columns
}
% \references{
% %% ~put references to the literature/web site here ~
% }
\author{
Andrew Heiss \email{andrew@andrewheiss.com}
}
% \note{
% %%  ~~further notes~~
% }

%% ~Make other sections like Warning with \section{Warning }{....} ~

% \seealso{
% %% ~~objects to See Also as \code{\link{help}}, ~~~
% }
\examples{
DataframeSummary(USArrests)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{summary}
