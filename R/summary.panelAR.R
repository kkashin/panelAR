summary.panelAR <- function(obj,...){
	rdf <- obj$df.residual
	rank <- obj$rank
	N <- length(obj$residuals)
	k <- length(obj$aliased)
	df <- c(rank,rdf,k)
    
	# SE
	se <- sqrt(diag(obj$vcov))
	
	# test statistics
	coef <- obj$coefficients
	t.stat <- (coef)/se
	p.val <- 2*pt(abs(t.stat), rdf, lower.tail=FALSE)
	tab <- cbind(coef,se,t.stat,p.val)
	dimnames(tab) <- list(names(coef), c("Estimate", "Std. Error", "t value", "Pr(>|t|)"))
    
    # set up hypothesis for wald test
    hyp <- names(coef)
    hyp <- hyp[hyp!="(Intercept)"]
    
    # wald test
    lh <- linearHypothesis(obj, hyp, test=c("Chisq", "F"), vcov.=obj$vcov, singular.ok=FALSE)
    wald <- c(lh$Chisq[2],lh$Df[2],lh[["Pr(>Chisq)"]][2])
    names(wald) <- c("value","df","Pr(>Chisq)") # wald stat, model dof, p stat

    # check if balanced
    N.panel <- nrow(obj$panelStructure$obs.mat)
    N.time <- ncol(obj$panelStructure$obs.mat)
    balanced <- ifelse(N.panel*N.time==N,T,F)
    
	# calculate number of obs per panel, number of time observations, balanced vs. unbalanced 
	N.per.panel <- rowSums(obj$panelStructure$obs.mat)
    N.min <- min(N.per.panel)
    N.max <- max(N.per.panel)
    N.avg <- N/N.panel # average units per panel # 
    
    # create list with variables that describe panel structure
    panelStruct <- list(N=N,N.panel=N.panel,N.time=N.time,balanced=balanced,N.min=N.min,N.max=N.max,N.avg=N.avg,N.per.panel=N.per.panel)
    
    out <- list(call=obj$call,terms=obj$terms,coefficients=tab,residuals=obj$residuals, aliased=obj$aliased, df=df, rho = obj$panelStructure$rho, Sigma=obj$panelStructure$Sigma, r2=obj$r2, wald=wald, vcov=obj$vcov, na.action=obj$na.action,panelStructure=panelStruct)
    
	class(out) <- "summary.panelAR"
	out
	}