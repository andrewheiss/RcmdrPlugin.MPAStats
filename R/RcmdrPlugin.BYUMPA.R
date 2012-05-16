#-------------------------------------
# Rcmdr menus for the BYUMPA package
# Author: Andrew Heiss
# Last modified: 19 April 2012
#-------------------------------------

# Note: the following function (with contributions from Richard Heiberger) 
# can be included in any Rcmdr plug-in package to cause the package to load
# the Rcmdr if it is not already loaded

# .First.lib <- function(libname, pkgname){
#   if (!interactive()) return()
#   Rcmdr <- options()$Rcmdr
#   plugins <- Rcmdr$plugins
#   if ((!pkgname %in% plugins) && !getRcmdr("autoRestart")) {
#     Rcmdr$plugins <- c(plugins, pkgname)
#     options(Rcmdr=Rcmdr)
#     closeCommander(ask=FALSE, ask.save=TRUE)
#     Commander()
#   }
# }

.onAttach <- function(libname, pkgname){
  if (!interactive()) return()
  Rcmdr <- options()$Rcmdr
  plugins <- Rcmdr$plugins
  if ((!pkgname %in% plugins) && !getRcmdr("autoRestart")) {
    Rcmdr$plugins <- c(plugins, pkgname)
    options(Rcmdr=Rcmdr)
    closeCommander(ask=FALSE, ask.save=TRUE)
    Commander()
  }
}

helloWorld <- function() {
  command <- paste('print("Hello world!")')
  doItAndPrint(command)
}

helloWorldDialog <- function() {
  initializeDialog(title=gettextRcmdr("Hello World Dialog"))
  nameVar <- tclVar("world")
  nameEntry <- tkentry(top, width="15", textvariable=nameVar)
  
  onOK <- function() {
    closeDialog()
    name <- as.character(tclvalue(nameVar))
    command <- paste("print('Hello ", name, "!')", sep="")
    doItAndPrint(command)
    tkfocus(CommanderWindow())
  }
  
  OKCancelHelp(helpSubject="lm")
  tkgrid(tklabel(top, text="Say hello to "), nameEntry, sticky="e")
  tkgrid.configure(nameEntry, sticky="w")
  tkgrid(buttonsFrame, sticky="w", columnspan=2)
  dialogSuffix(rows=4, columns=2, focus=nameEntry)
}