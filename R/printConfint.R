printConfint <- function(x) {
  if (class(x) != "htest") 
    stop("must supply an object of class htest")
  
  cat(format(100 * attr(x$conf.int, "conf.level")),
      "percent confidence interval:\n",
      format(c(x$conf.int[1L], x$conf.int[2L])), "\n")
}