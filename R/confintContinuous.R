confintContinuous <- function () {
  defaults <- list (initial.x = NULL, initial.level = ".95")
  dialog.values <- getDialog ("confintContinuous", defaults)  
  initializeDialog(title = gettextRcmdr("Confidence intervals for continuous data"))
  xBox <- variableListBox(top, Numeric(), title = gettextRcmdr("Variable (pick one)"),
                          initialSelection = varPosn(dialog.values$initial.x, "numeric"))
  onOK <- function() {
    x <- getSelection(xBox)
    if (length(x) == 0) {
      errorCondition(recall = confintContinuous, 
        message = gettextRcmdr("You must select a variable."))
      return()
    }
    level <- tclvalue(confidenceLevel)
    putDialog ("confintContinuous", list (initial.x = x, initial.level = level))
    closeDialog()
    doItAndPrint(paste(".test <- t.test(", ActiveDataSet(), "$", x, 
                       ", conf.level=", level, ")", sep = ""))
    doItAndPrint("printConfint(.test)")
    tkdestroy(top)
    tkfocus(CommanderWindow())
  }
  OKCancelHelp(helpSubject = "t.test", reset = "confintContinuous")
  
  # Create main frames
  leftFrame <- getFrame(xBox)
  rightFrame <- tkframe(top)

  # Confidence frame
  confidenceFrame <- tkframe(rightFrame)
  confidenceLevel <- tclVar(dialog.values$initial.level)
  confidenceField <- ttkentry(confidenceFrame, width = "6", 
                              textvariable = confidenceLevel)

  # Labels
  tkgrid(labelRcmdr(rightFrame, text = ""), sticky = "w")
  tkgrid(labelRcmdr(confidenceFrame, text = gettextRcmdr("Confidence Level: ")), 
                    confidenceField, sticky = "w")

  # Place frames
  tkgrid(leftFrame, rightFrame)
  tkgrid(confidenceFrame, sticky = "w")

  # Set anchoring options
  tkgrid.configure(confidenceField, sticky = "e")
  tkgrid.configure(rightFrame, sticky="nw")
  tkgrid.configure(leftFrame, sticky="nw")

  # Final buttons
  tkgrid(buttonsFrame, columnspan = 2, sticky = "w")
  dialogSuffix(rows = 4, columns = 2)
}