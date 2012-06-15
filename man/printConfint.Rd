\name{printConfint}
\alias{printConfint}
\title{Print confidence intervals}
\description{
  Extract and print the confidence intervals for an object of class \code{"htest"} 
  (i.e. results of a \code{"t.test"}, \code{"binom.test"}, \code{"poisson.test"}, etc.)
}
\usage{
  printConfint(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{an object of class \code{"htest"}}
}
% \details{
% %%  ~~ If necessary, more details than the description above ~~
% }
\value{Text printed to the console.}

\author{
  Andrew Heiss <\email{andrew@andrewheiss.com}>
}
% \note{
% %%  ~~further notes~~
% }

%% ~Make other sections like Warning with \section{Warning }{....} ~

% \seealso{
% %% ~~objects to See Also as \code{\link{help}}, ~~~
% }
\examples{
  x <- runif(100)
  .test <- t.test(x, conf.level=.99)
  printConfint(.test)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ print }
\keyword{ htest }% __ONLY ONE__ keyword per line
