###################################################################################################
# Step 1: Divide the sample in n blocks of equal length
###################################################################################################
# -------------------------------------------------------------------------------------------------
require(timeSeries)

# Load the data 'danishClaims'.
xts <- readSeries("danishClaims.csv")
x <- as.vector(xts)
blockLength <- 30

# Split the data in blocks.
block <- split(x, ceiling(seq_along(x)/blockLength))

# Visualize the data.
plot(x, type = "h", col = "black",
     xlab = "days", ylab = "Claim Size")
i <- blockLength
while (i < length(x)){
  abline(v=i, col="red", lwd=1)
  i = i + blockLength
}


###################################################################################################
# Step 2: Compute the maximum value in each block
###################################################################################################
# -------------------------------------------------------------------------------------------------
N = length(block)
# maxima = vector(mode = "list",N)
maxima = vector(,N)
for (i in 1:N) {
  maxima[i] = max(block[[i]])
}


###################################################################################################
# Step 3: Fit the GEV distribution to the block maxima
###################################################################################################
# -------------------------------------------------------------------------------------------------
# Normalized probability function of the Generalized Extreme Value distribution.
H <- function(x, xi, mu, sigma) {exp(-(1+xi*(x-mu)/sigma)^(-1/xi))}
dH = function(x, xi, mu, sigma) {1/sigma * exp(-(xi*(x-mu)/sigma + 1)^(-1/xi)) * (xi*(x-mu)/sigma + 1)^(-1/xi -1)}



# Estimate xi, mu and sigma of the GEV by applying MLE.
nlogl <- function(par,x) {-sum(log(dH(x,par[1],par[2],par[3])))/N}
fit <- nlminb(c(1,1,1), nlogl, x = maxima,
              lower=c(-Inf,-Inf,0.00000001), upper=c(Inf,Inf,Inf),
              control=list(trace=TRUE))

# retrieve fitted values
xi <- fit$par[1]
mu <- fit$par[2]
sigma <- fit$par[3]

c(xi,mu,sigma)

# Plot figure: (Maximum Likelihood Estimation of xi, mu and sigma of the GEV)
Ge <- seq(1/N, 1, len = N)
plot(sort(maxima), Ge, xlab="y", ylab="H(y)", type="p", pch=20, log="x")
v <- seq(min(maxima), max(maxima), length = 101)
lines(v, H(v, xi, mu, sigma), col="red")

