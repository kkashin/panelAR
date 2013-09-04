print.panelAR <- function(obj, digits=max(3,getOption("digits")-3),...){
	if(obj$call$autoCorrType=="none"){
		autoCorr.Method <- "no autocorrelation"
		} else{
			autoCorr.Method <- "AR(1) Prais-Winsten correction"
		}
	if(obj$call$panelCorrType=="phet"){
		panelCorr.Method <- "heteroskedasticity across panels"
		} else if(obj$call$panelCorrType=="pcse"){
			panelCorr.Method <- "panel-corrected standard errors"
		} else{
			panelCorr.Method <- "homoskedastic variance"
			}
	cat(paste("\nPanel Regression with ",autoCorr.Method, " and ",panelCorr.Method,", estimated using ", obj$call$method, "\n", sep = "")) 
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
