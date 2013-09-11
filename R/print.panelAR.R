print.panelAR <- function(obj, digits=max(3,getOption("digits")-3),...){
	if(obj$call$autoCorr=="none"){
		autoCorr.Method <- "no autocorrelation"
		} else{
			autoCorr.Method <- "AR(1) Prais-Winsten correction"
		}
	panelCorr.Method  <- switch(obj$call$panelCorrMethod,none="homoskedastic variance",phet="panel heteroskedasticity-robust standard errors",pwls="panel weighted least squares",pcse="panel-corrected standard errors",parks="Parks-Kmenta FGLS")

	cat(paste("\nPanel Regression with ",autoCorr.Method, " and ",panelCorr.Method,"\n", sep = "")) 
	cat("\nCall:\n", paste(deparse(obj$call), sep = "\n", collapse = "\n"), 
        "\n\n", sep = "")
        
    if(any(obj$aliased)){
    	coef <- rep(NA,length(obj$aliased))
    	names(coef) <- names(obj$aliased)
    	coef[!obj$aliased] <- coef(obj)
    } else{
    	coef <- coef(obj)
    }
    cat("Coefficients:\n")
    print.default(format(coef, digits = digits), print.gap = 2, 
    quote = FALSE)
    cat("\n")
    invisible(obj)
}
