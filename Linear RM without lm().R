data=read.csv("dataset.csv",sep=",")
x=data[ ,1]
y=data[ ,2]
m=length(y)

# Plot data

plot(x,y)

# Gradient Descent

x<- cbind(rep(1,m),x) 
x <- as.matrix(x)

theta <- c(8,3)

# Some gradient descent settings
i <- 1000
alpha <- 0.01


# Computing cost function
computeCost  <- function(x, y, theta)
{

m = length(y); 
J = 0;
dif <- x %*% theta - y
J <- (t(dif) %*% dif) / (2 * m)
J

}

gradientDescent <- function(X, y, theta, alpha, num_iters) {
  m <- length(y) 
  J_history = rep(0, num_iters + 1)
  theta_history = matrix(0, num_iters + 1, length(theta))
  theta_history[1,] = t(theta)
  J_history[1] = computeCost(X, y, theta)
  
  for (iter in 2:(num_iters + 1)) {
    theta_prev = theta
    p = dim(x)[2]
    for (j in 1:p) {
      # vectorized version
 
      deriv = (t(x %*% theta_prev - y) %*% x[, j]) / m
      
      # update theta_j
      theta[j] = theta_prev[j] - (alpha * deriv)
    }
    
    # Save the cost J in every iteration
    J_history[iter] = computeCost(x, y, theta)
    theta_history[iter,] = t(theta)
  }
  
  list(theta = theta, J_history = J_history, theta_history = theta_history)

}
  
gd <- gradientDescent(x, y, theta, alpha,i)
#Decompose list (gd) variables into global env variables
theta <- gd$theta
J_history <- gd$J_history
theta_history <- gd$theta_history
rm(gd)

# print theta to screen
cat('Theta found by gradient descent: ')
sprintf('%f %f \n', theta[1], theta[2])

# Plot the linear fit

lines(x[, 2], x  %*% theta, col="blue")
legend("topleft", c('Training data', 'Linear regression'), pch=c(4,NA),col=c("red","green"), lty=c(NA,1))



                  


