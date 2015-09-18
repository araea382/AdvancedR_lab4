




linreg <- function(x, ...){
    UseMethod("linreg")
}


linreg <- function(formula, data){
  library(ggplot2)
    X <- model.matrix(formula, data)
    y <- as.matrix(data[all.vars(formula)[!(all.vars(formula) %in% colnames(X))]])
    stopifnot(is.numeric(y)&is.numeric(X))
    Q <- qr.Q(qr(X))
    R <- qr.R(qr(X))
    
    ans <- solve(R) %*% t(Q) %*% y
    beta <- as.vector(ans)
    names(beta) <- colnames(X)
    
    #the fitted values 
    fit <- X %*% beta
    fit <- as.vector(fit)
    
    #the residuals
    res <- y - fit

    fit.res<-data.frame(fit,res)
    #not working if class is linreg
  # class(fit.res) <- "linreg"
    names(fit.res)<-c("fit","res")
    
    

    res <- as.vector(res)
    
    fit.res <- data.frame(fit, res)
    names(fit.res) <- c("fit", "res")
    

    #the degree of freedoms
    n <- nrow(X)
    p <- ncol(X)
    df <- n - p
    
    #the residual variance
    var_res <- as.vector((t(res) %*% res) / df)
    
    #the variance of the regression coefficients
    var_tmp <- var_res * solve(t(R) %*% R)
    var_coeff <- numeric()
    for(i in 1:nrow(var_tmp)){
        var_coeff <- c(var_coeff, var_tmp[i,i])
    }
    
    #the t-values for each coefficient
    tval <- numeric()
    for(i in 1:length(beta)){
        tval <- c(tval, beta[i] / sqrt(var_coeff[i]))
    }
    
    #p-value -> use pt()
    ##can't check the answer but i think the code is quite correct
    pval <- 2*pt(-abs(tval), df)
    
    a <- list(coefficients = beta, fitted = fit, residuals = res, varcoeff = var_coeff, tvalues = tval, pvalues = pval, df = df)
    a$call <- match.call()
    class(a) <- "linreg"
    return(a)    
}
    






print.linreg <- function(x, ...){
    cat("Call:\n")
    print(x$call)
    cat("\nCoefficients:\n")
    print(x$coefficients)
}




linreg <- structure(list(), class = "linreg")
##Test outside the function with faithful data
X <- model.matrix(eruptions~waiting, data = faithful)
y <- as.matrix(faithful[,1, drop = FALSE])


plot.linreg <- function(x, ...){
  ggplot(data=fit.res,aes(x=fit,y=res))+geom_point()
}

resid.linreg <- function(x, ...){
    return(res)
}

#???
pred.linreg <- function(x, ...){
    return(fit)
}

coef.linreg <- function(x, ...){
    return(x$coefficients)
}

#not finished
summary.linreg <- function(x, ...){
    cat("Call:\n")
    print(x$call)
    cat("Residuals:\n")
    print(summary(res))
    cat("Coefficients:\n")
    print(x$coefficients)
    print(x$varcoeff)
    cat("T-values:\n")
    print(x$tvalues)
    cat("P-values:\n")
    print(x$pvalues)
    cat("DF:\n")
    print(x$df)
}



