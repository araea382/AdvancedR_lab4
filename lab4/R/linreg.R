library(ggplot2)

linreg <- function(formula, data){
    X <- model.matrix(formula, data)

    #like this maybe? takes all the arguments that are not and therefore in the X matrix
    y <- as.matrix(data[all.vars(formula)[!(all.vars(formula) %in% colnames(X))]])
    
    Q <- qr.Q(qr(X))
    R <- qr.R(qr(X))
    
    ans <- solve(R) %*% t(Q) %*% y
    beta<-ans
    #the fitted values 
    fit <- X %*% beta
    
    #the residuals
    res <- y - fit
    fit.res<-data.frame(fit,res)
    names(fit.res)<-c("fit","res")
    #the degree of freedoms
    n <- nrow(X)
    p <- ncol(X)
    df <- n - p
    
    #the residual variance
    var_res <- as.vector((t(res) %*% res) / df)
    
    ans<-structure(ans, class = "linreg")
    
    #below other option which includes the formula, however as a list.
    print.linreg <- function(ans) {
      cat(colnames(X),"\n")
      cat(ans[1:2],"\n")
#cat("linreg(",as.call(as.list(formula)),")","\n") #cannot get it to print the formula
    }
    
    print.linreg(ans)
}

p<-ggplot(data=fit.res,aes(x=fit,y=res))+geom_point()+ add_cat()


linreg <- structure(list(), class = "linreg")
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


#or else

print.linreg <- function(ans) {
  a<-list(Coeff=c(ans[[1]][1:ncol(X)]),formul=print(formula))
  names(a$Coeff)=colnames(X)
  a}
print.linreg(ans)
