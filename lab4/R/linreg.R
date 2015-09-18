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
    
    a <- list(coefficients = beta, fitted = fit, residuals = res, varres = var_res, varcoeff = var_coeff, tvalues = tval, pvalues = pval, df = df)
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

#xlab is missing:fitted values 
# http://stackoverflow.com/questions/13223846/ggplot2-two-line-label-with-expression
#to make 2 lines
plot.linreg <- function(x, ...){
ggplot(data=fit.res,aes(x=fit,y=res))+geom_point()+
    geom_smooth(method = "loess", formula = y ~ x,se=FALSE, colour = "red") + 
    xlab(x$call) + ylab("residuals") + ggtitle("Residuals vs. Fitted")
 ggplot(data=fit.res,aes(x=fit,y=sqrt(abs(res))))+geom_point()+
   geom_smooth(method = "loess", formula = y ~ x,se=FALSE, colour = "red") + 
 xlab(x$call) + ylab(expression(paste(sqrt("Standardized residuals")))) + ggtitle("Scale−Location")

 }

resid.linreg <- function(x, ...){
    return(x$residuals)
}

#???
pred.linreg <- function(x, ...){
    return(x$fitted)
}

coef.linreg <- function(x, ...){
    return(x$coefficients)
}

<<<<<<< HEAD
summary.linreg <- function(x, ...){
    cat("Call:\n")
    print(x$call)
    
=======

   summary.linreg <- function(x, ...){
     cat("Call:\n")
    print(x$call) 
>>>>>>> 03b961cd663fc5be6dc41e5c1c8424fb7ce15f89
    cat("\nCoefficients:\n")
    sd_coeff <- sqrt(x$varcoeff)
    ls <- cbind(Estimate = x$coefficients ,Std.Error = sd_coeff, t.value = x$tval, p.value = x$pval)
    print(ls)
    cat("\nResidual standard error:")
    sd_res <- sqrt(x$varres)
    cat(sd_res, "on", x$df, "degree of freedom")
}





