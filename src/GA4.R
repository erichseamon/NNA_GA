UI_GA <- function(fitness_fun) {



usePackage <- function(p) {
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE, repos = "http://cran.us.r-project.org")
  require(p, character.only = TRUE)
}

#--use of Rastrigin objective function to test optimization

Rastrigin <- function(x1, x2)
{
  20 + x1^2 + x2^2 - 10*(cos(2*pi*x1) + cos(2*pi*x2))
}

## Rosenbrock Banana function

fr <- function(x1, x2) {   
  100 * (x2 - x1 * x1)^2 + (1 - x1)^2
}

grr <- function(x) { 
  x1 <- x[1]
  x2 <- x[2]
  c(-400 * x1 * (x2 - x1 * x1) - 2 * (1 - x1),
    200 *      (x2 - x1 * x1))
}

res <- optim(c(-1.2,1), fr, grr, method = "BFGS", control = list(trace=TRUE), hessian = TRUE)
res


## camel6 function

camel6 <- function(x1, x2)
{
 (4-2.1*x1^2+(x1^4)/3) * x1^2

}


x1 <- x2 <- seq(-5.12, 5.12, by = 0.1)
f <- outer(x1, x2, Rastrigin)
persp3D(x1, x2, f, theta = 50, phi = 20, col.palette = bl2gr.colors)

filled.contour(x1, x2, f, color.palette = bl2gr.colors)



monitor <- function(obj) 
{ 
  contour(x1, x2, f, drawlabels = FALSE, col = grey(0.5))
  title(paste("iteration =", obj@iter), font.main = 1)
  points(obj@population, pch = 20, col = 2)
  Sys.sleep(0.2)
}


#--GA

suggestedSol <- matrix(c(0.2,1.5,-1.5,0.5), nrow = 2, ncol = 2, byrow = TRUE)


GA <- ga(type = "real-valued", 
         fitness =  function(x) -fitness_fun(x[1], x[2]),
         lower = c(-5.12, -5.12), upper = c(5.12, 5.12), 
         suggestions = suggestedSol,
         popSize = 50, maxiter = 1000, run = 100, monitor = monitor)
summary(GA)

plot(GA)

head(GA@population)



}

