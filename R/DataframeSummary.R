DataframeSummary <- function(x, conf.intervals=TRUE) {
  if (class(x) != "data.frame") 
    stop("must supply a dataframe")
  
  #-------------------------------------
  # Continuous, numeric data summaries
  x.continuous <- x[ , sapply(x, is.numeric)]
  if (ncol(x.continuous) > 0) {
    x.min <- apply(x.continuous, 2, min, na.rm=TRUE)
    x.max <- apply(x.continuous, 2, max, na.rm=TRUE)
    x.sd <- apply(x.continuous, 2, sd, na.rm=TRUE)
    x.mean <- as.numeric(lapply(x.continuous, mean, na.rm=TRUE))
    x.n <- apply(!is.na(x.continuous), 2, sum)
    x.na <- apply(is.na(x.continuous), 2, sum)
    row.names <- colnames(x.continuous)
    
    # Build output dataframe
    results.continuous <- data.frame(x.mean, x.sd, x.min, x.max, x.n, x.na)
    colnames(results.continuous) <- c("Mean", "Std Dev", "Min", "Max", "N", "NAs")
    rownames(results.continuous) <- row.names
    
    # Optionall add confidence interval columns  TODO: Make sure this can handle missing values
    if (conf.intervals==TRUE) { 
      calculate.confidence.intervals <- function(x, ...) {  
        temp <- t.test(x)
        c(temp$conf.int[1L], temp$conf.int[2L])
      }
      
      conf.table <- t(apply(x.continuous, 2, calculate.confidence.intervals))
      colnames(conf.table) <- c("95% Conf. (lower)", "95% Conf. (upper)")
      
      results.continuous <- cbind(results.continuous, conf.table)
    }
    
    # Print the results
    writeLines("Summary of continuous variables")
    writeLines("-------------------------------")
    print(results.continuous)
  }
  
  #------------------------
  # Categorical summaries TODO: Add proportions to the factors (% of total non-NA responses) 
  # > .Table <- table(d$x4)
  # 
  # > .Table  # counts for x4
  # 
  #  1  2  3  4 
  # 20 23 28 29 
  # 
  # > round(100*.Table/sum(.Table), 2)  # percentages for x4
  # 
  #  1  2  3  4 
  # 20 23 28 29
  x.factors <- x[ , sapply(x, is.factor)]
  if (ncol(x.factors) > 0) {
    x.counts <- apply(x.factors, 2, summary.factor)
    
    lprint <- function(lst) {
      for (i in 1:length(lst)) {
        cat(names(lst[i]), "\n")
        print(lst[[i]])
        cat("\n")
      }
    }
    
    if (ncol(x.continuous) > 0)
      writeLines("\n")
    
    writeLines("Summary of categorical variables")
    writeLines("(first row = categories; second row = counts)")
    writeLines("---------------------------------------------")
    lprint(x.counts)
  }
}