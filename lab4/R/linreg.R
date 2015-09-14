linreg <- function(formula, data){
    X <- model.matrix(formula, data)

    ##dependent variable
    #don't knoww~
    #y <- all.vars(expression(formula))
    
    Q <- qr.Q(qr(X))
    R <- qr.R(qr(X))
    
    ans <- solve(R) %*% t(Q) %*% y
    return(ans)
}


##Test outside the function with faithful data
X <- model.matrix(eruptions~waiting, data = faithful)
y <- as.matrix(faithful[,1, drop = FALSE])

Q <- qr.Q(qr(X))
R <- qr.R(qr(X))

#regresions coefficients: intercept and slope 
beta <- solve(R) %*% t(Q) %*% y
#beta <- as.vector(beta)

####check with lm
##the coefficients is correct!
m <- lm(eruptions~waiting, faithful)
m

#the fitted values 
fit <- X %*% beta

#the residuals
res <- y - fit

#the degree of freedoms
n <- nrow(X)
p <- ncol(X)
df <- n - p

#the residual variance
var_res <- as.vector((t(res) %*% res) / df)
