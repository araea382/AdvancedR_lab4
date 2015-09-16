library(ggplot2)
<<<<<<< HEAD
#instead of stop if not function, add that it converts it into dummy variables
=======

linreg <- function(x, ...){
    UseMethod("linreg")
}

>>>>>>> e3e5116f9c5e18ec6e0d5d646712598f7eba586e
linreg <- function(formula, data){
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
<<<<<<< HEAD
    fit.res<-data.frame(fit,res)
    names(fit.res)<-c("fit","res")
    #not working, need to get ggplot to work with new class
    fit.res <- structure(data.frame(), class = "linreg")
    plot.linreg<-function(fit.res){
      ggplot(data=fit.res,aes(x=fit,y=res))+geom_point()
      }
    plot(fit.res)
=======
    res <- as.vector(res)
    
    fit.res <- data.frame(fit, res)
    names(fit.res) <- c("fit", "res")
    
>>>>>>> e3e5116f9c5e18ec6e0d5d646712598f7eba586e
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

<<<<<<< HEAD


linreg <- structure(list(), class = "linreg")
##Test outside the function with faithful data
X <- model.matrix(eruptions~waiting, data = faithful)
y <- as.matrix(faithful[,1, drop = FALSE])
=======
#not finished
plot.linreg <- function(x, ...){
    
}
>>>>>>> e3e5116f9c5e18ec6e0d5d646712598f7eba586e

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
    
}

###################        
    ans <- structure(ans, class = "linreg")
    
    #below other option which includes the formula, however as a list.
    print.linreg <- function(ans) {
        cat(colnames(X),"\n")
        cat(ans[1:2],"\n")
        #cat("linreg(",as.call(as.list(formula)),")","\n") #cannot get it to print the formula
    }
    
    print.linreg(ans)
}

p <- ggplot(data=fit.res,aes(x=fit,y=res))+geom_point()+ add_cat()


#or else

print.linreg <- function(ans) {
    a<-list(Coeff=c(ans[[1]][1:ncol(X)]),formul=print(formula))
    names(a$Coeff)=colnames(X)
    a}
print.linreg(ans)