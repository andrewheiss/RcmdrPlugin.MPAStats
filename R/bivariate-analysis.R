# Last modified: 2012-06-15 by Andrew Heiss
#--------------------------------------------

# Check for objects of class `sclm`
sclmP <- function() activeModelP() && inherits(get(ActiveModel()), 'sclm')


# Modify the built-in `ordinalRegressionModel()` to work with `clm()` in the ordinal library
ordinalRegressionModel.ordinal <- function(){
	defaults <- list(initial.type="logit")
	dialog.values <- getDialog("ordinalRegressionModel.ordinal", defaults)
	require("ordinal")
	initializeDialog(title=gettextRcmdr("Ordinal Logisitic Regression Model"))
	.activeModel <- ActiveModel()
	.activeDataSet <- ActiveDataSet()
  currentModel <- if (!is.null(.activeModel))
        class(get(.activeModel, envir=.GlobalEnv))[1] == "sclm"
      else FALSE
	if (currentModel) {
		currentFields <- formulaFields(get(.activeModel, envir=.GlobalEnv))
		if (currentFields$data != .activeDataSet) currentModel <- FALSE
	}
	if (isTRUE(getRcmdr("reset.model"))) {
		currentModel <- FALSE
		putRcmdr("reset.model", FALSE)
	}
	UpdateModelNumber()
	modelName <- tclVar(paste("OrdinalLogit.", getRcmdr("modelNumber"), sep=""))
	modelFrame <- tkframe(top)
	model <- ttkentry(modelFrame, width="20", textvariable=modelName)
	radioButtons(name="modelType",
			buttons=c("logit", "probit"), initialValue=dialog.values$initial.type,
			labels=gettextRcmdr(c("Proportional-odds logit", "Ordered probit")),
			title=gettextRcmdr("Type of Model"))
	onOK <- function(){
		modelValue <- trim.blanks(tclvalue(modelName))
		closeDialog()
		if (!is.valid.name(modelValue)){
			errorCondition(recall=proportionalOddsModel, message=sprintf(gettextRcmdr('"%s" is not a valid name.'), modelValue), model=TRUE)
			return()
		}
		subset <- tclvalue(subsetVariable)
		if (trim.blanks(subset) == gettextRcmdr("<all valid cases>") || trim.blanks(subset) == ""){
			subset <- ""
			putRcmdr("modelWithSubset", FALSE)
		}
		else{
			subset <- paste(", subset=", subset, sep="")
			putRcmdr("modelWithSubset", TRUE)
		}
		check.empty <- gsub(" ", "", tclvalue(lhsVariable))
		if ("" == check.empty) {
			errorCondition(recall=proportionalOddsModel, message=gettextRcmdr("Left-hand side of model empty."), model=TRUE)
			return()
		}
		check.empty <- gsub(" ", "", tclvalue(rhsVariable))
		if ("" == check.empty) {
			errorCondition(recall=proportionalOddsModel, message=gettextRcmdr("Right-hand side of model empty."), model=TRUE)
			return()
		}
		if (!is.factor(eval(parse(text=tclvalue(lhsVariable)), envir=get(.activeDataSet, envir=.GlobalEnv)))){
#        if (!is.factor(eval(parse(text=tclvalue(lhsVariable)), envir=eval(parse(text=.activeDataSet), envir=.GlobalEnv)))){
			errorCondition(recall=proportionalOddsModel, message=gettextRcmdr("Response variable must be a factor"))
			return()
		}
		if (is.element(modelValue, listProportionalOddsModels())) {
			if ("no" == tclvalue(checkReplace(modelValue, type=gettextRcmdr("Model")))){
				UpdateModelNumber(-1)
				proportionalOddsModel()
				return()
			}
		}
		putDialog("ordinalRegressionModel.ordinal", list(initial.type = tclvalue(modelTypeVariable)))
		formula <- paste(tclvalue(lhsVariable), tclvalue(rhsVariable), sep=" ~ ")
		command <- paste("clm(", formula, ', link="', tclvalue(modelTypeVariable),
				'", data=', .activeDataSet, subset, ")", sep="")
		logger(paste(modelValue, " <- ", command, sep=""))
		assign(modelValue, justDoIt(command), envir=.GlobalEnv)
		doItAndPrint(paste("summary(", modelValue, ")", sep=""))
		activeModel(modelValue)
		tkfocus(CommanderWindow())
	}
	OKCancelHelp(helpSubject="clm", model=TRUE, reset = "resetclm")
	tkgrid(labelRcmdr(modelFrame, text=gettextRcmdr("Enter name for model:")), model, sticky="w")
	tkgrid(modelFrame, sticky="w")
	modelFormula()
	subsetBox(model=TRUE)
	tkgrid(getFrame(xBox), sticky="w")
	tkgrid(outerOperatorsFrame, sticky="w")
	tkgrid(formulaFrame, sticky="w")
	tkgrid(subsetFrame, sticky="w")
	tkgrid(modelTypeFrame, sticky="w")
	tkgrid(buttonsFrame, sticky="w")
	dialogSuffix(rows=7, columns=1, focus=lhsEntry, preventDoubleClick=TRUE)
}


# Reset the clm model dialog
resetclm <- function() {
	putRcmdr("reset.model", TRUE)
	putDialog("ordinalRegressionModel.ordinal", NULL)
	ordinalRegressionModel.ordinal()
}