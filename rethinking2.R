### Statistical Rethinking 2nd ed.

##Notes regarding bookclub and solutions to exercises

#First install rstan from mc-stan.org
#make sure that packages "coda", "mvtnorm" and "devtools" are installed
#install the rethinking package

### Need to run only once ################################################################################
library(devtools)
devtools::install_github("rmcelreath/rethinking") #The master branch should now contain the 2nd ed version
##########################################################################################################


### Load the libraries
library(rethinking)

### * Chapter 1

# "I have personally read hundreds of uses of Fisher’s exact test in scientific journals, but aside
# from Fisher’s original use of it, I have never seen it used appropriately."

### * Chapter 2

#Bookmark: section 2.3

dbinom(6, size = 9, prob = 0.5)

# define grid
p_grid <- seq( from=0 , to=1 , length.out=100 )
# define prior
prior <- rep( 1 , 100 ) #Flat prior
#prior <- ifelse( p_grid < 0.5 , 0 , 1 ) #Step prior
#prior <- exp( -5*abs( p_grid - 0.5 ) ) #Peaked prior

# compute likelihood at each value in grid
likelihood <- dbinom( 6 , size=9 , prob=p_grid )
# compute product of likelihood and prior
unstd.posterior <- likelihood * prior
# standardize the posterior, so it sums to 1
posterior <- unstd.posterior / sum(unstd.posterior)

plot( p_grid , posterior , type="l" ,
xlab="probability of water" , ylab="posterior probability" )
mtext( "100 points" )

##Quadratic approximation
globe.qa <- quap(
alist(
W ~ dbinom( W+L ,p) , # binomial likelihood
p ~ dunif(0,1)
# uniform prior
) ,
data=list(W=6,L=3) )

### ** Problems

### *** Difficulty level: Easy

#2E1
#2 and 4

#2E2
#3

#2E3
#1 and 4

#2E4
#When we toss the globe we never observe any probability, only the results of land or water

### *** Difficulty level: Medium

#2M1
# define grid
p_grid <- seq( from=0 , to=1 , length.out=100 )
# define prior
prior <- rep( 1 , 100 ) #Flat prior
# compute likelihood at each value in grid
#W, W, W
#likelihood <- dbinom(3, size=3 , prob=p_grid )
#W, W, W, L
#likelihood <- dbinom(3, size = 4, prob = p_grid)
#L, W, W, L, W, W, W
likelihood <- dbinom(5, size = 7, prob = p_grid)

# compute product of likelihood and prior
unstd.posterior <- likelihood * prior
# standardize the posterior, so it sums to 1
posterior <- unstd.posterior / sum(unstd.posterior)

plot( p_grid , posterior , type="l" ,
xlab="probability of water" , ylab="posterior probability" )
mtext( "100 points" )

#2M2
# define grid
p_grid <- seq( from=0 , to=1 , length.out=100 )
# define prior
prior <- ifelse(p_grid < 0.5, 0, 1) #Step prior
# compute likelihood at each value in grid
#W, W, W
#likelihood <- dbinom(3, size=3 , prob=p_grid )
#W, W, W, L
#likelihood <- dbinom(3, size = 4, prob = p_grid)
#L, W, W, L, W, W, W
likelihood <- dbinom(5, size = 7, prob = p_grid)

# compute product of likelihood and prior
unstd.posterior <- likelihood * prior
# standardize the posterior, so it sums to 1
posterior <- unstd.posterior / sum(unstd.posterior)

plot( p_grid , posterior , type="l" ,
xlab="probability of water" , ylab="posterior probability" )
mtext( "100 points" )

#2M3
# probability of land, given Earth
p_le <- 0.3
# probability of land, given Mars
p_lm <- 1.0
# probability of Earth
p_e <- 0.5
# probability of land
p_l <- (p_e * p_le) + ((1 - p_e) * p_lm)
# probability of Earth, given land
p_el <- (p_le * p_e) / p_l #Use Bayes theorem here
round(p_el,2) == .23


##Or alternatively
prior <- c(.5, .5)
likelihood <- c(.3, 1)
unstandardized.posterior <- prior * likelihood
posterior <- unstandardized.posterior / sum(unstandardized.posterior)
round( posterior[1], 2) == .23

#2M4
#What are the possibilities that a face of the card is black?
#There are 3 black card faces in the bag, 2 for B/B, 1 for B/W, 0 for W/W

prior <- c(1/3, 1/3, 1/3) #or 1, 1, 1
likelihood <- c(2, 1, 0) #B/B in first, B/W in second, W/W in third index
unstd.posterior <- likelihood * prior
posterior <- unstd.posterior / sum(unstd.posterior)
posterior[1] #probability of B/B

#2M5
#Two B/B cards
prior <- c(1/4, 1/4, 1/4, 1/4) #(B/B, B/B, B/W, W/W)
likelihood <- c(2, 2,  1, 0)
unstd.posterior <- likelihood * prior
posterior <- unstd.posterior / sum(unstd.posterior)
posterior[1] + posterior[2] #probability of B/B 1 or B/B 2

#2M6
#Now black card is pulled from the bag less often
prior <- c(1, 2, 3) #For every pull of B/B, 2 pulls of B/W, and 3 pulls of W/W
likelihood <- c(2, 1, 0)
unstd.posterior <- likelihood * prior
posterior <- unstd.posterior / sum(unstd.posterior)
posterior[1] #probability of B/B

#2M7
#Two cards are pulled, first showing black second showing white
prior <- c(1,1,1)
likelihood <- c(2*3, 1*2, 0) #[1] first B/B then three possibilities of W, [2] first B/W, then 2 W faces possible, [3] 0 B faces
unstd.posterior <- likelihood * prior
posterior <- unstd.posterior / sum(unstd.posterior)
posterior[1] #probability of B/B
### *** Difficulty level: Hard

#2H1
# find posterior for plausibility of each pandas species following the first birth of twins
species.1.likelihood <- .1
species.2.likelihood <- .2
likelihood <- c(species.1.likelihood, species.2.likelihood)
prior <- c(1, 1)
unstandardized.posterior <- likelihood * prior
posterior <- unstandardized.posterior / sum(unstandardized.posterior)

# probability next birth is set of twins
posterior[1] * .1 + posterior[2] * .2

#2H2
#Probability that panda is from species A
#We already calculated this
#This is the same as posterior[1] from above
posterior[1]

#2H3
#Second birth that is a singleton infant
#What is the probability that panda is species A?
prior <- c(.5, .5)
likelihood <- c(0.1 * (1-0.1), 0.2 * (1-0.2)) #Two births for each species
prior <- c(1, 1)
unstandardized.posterior <- likelihood * prior
posterior <- unstandardized.posterior / sum(unstandardized.posterior)
posterior[1]

#2H4
# without birth information
species.1.likelihood <- .8
species.2.likelihood <- 1 - .65
likelihood <- c(species.1.likelihood, species.2.likelihood)
prior <- c(1, 1)
unstandardized.posterior <- likelihood * prior
posterior.vet.test <- unstandardized.posterior / sum(unstandardized.posterior)

# probability pandas is from species 1, given veterinarian test
posterior.vet.test[1]

# with birth information
species.1.likelihood <- .1 * (1 - .1)
species.2.likelihood <- .2 * (1 - .2)
likelihood <- c(species.1.likelihood, species.2.likelihood)
prior <- c(1, 1)
unstandardized.posterior <- likelihood * prior
posterior.birth.info <- unstandardized.posterior / sum(unstandardized.posterior)

# probability pandas is from species 1, given veterinarian test and birth information
composite.unstandardized.posterior <- posterior.vet.test * posterior.birth.info
composite.posterior <- composite.unstandardized.posterior / sum(composite.unstandardized.posterior)
composite.posterior[1]
### * Chapter 3

p_grid <- seq( from=0 , to=1 , length.out=1000 )
prob_p <- rep( 1 , 1000 )
prob_data <- dbinom( 6 , size=9 , prob=p_grid )
posterior <- prob_data * prob_p
posterior <- posterior / sum(posterior)

samples <- sample( p_grid , prob=posterior , size=1e4 , replace=TRUE )
plot(samples)

dens(samples)

sum( samples < 0.5 ) / 1e4

sum( samples > 0.5 & samples < 0.75 ) / 1e4

HPDI(samples, prob = 0.95)

##Bookmark section 3.3

dummy_w <- rbinom( 1e5 , size=9 , prob=0.7 )
simplehist( dummy_w , xlab="dummy water count" )

w <- rbinom( 1e4 , size=9 , prob=samples )
simplehist(w)

### ** Practise
### *** Difficulty level: Easy
p_grid <- seq( from=0 , to=1 , length.out=1000 )
prior <- rep( 1 , 1000 )
likelihood <- dbinom( 6 , size=9 , prob=p_grid )
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)
set.seed(100)
samples <- sample( p_grid , prob=posterior , size=1e4 , replace=TRUE )

#3E1
sum(samples < 0.2) / 1e4
#3E2
sum(samples > 0.8) / 1e4
#3E3
sum(samples > 0.2 & samples < 0.8) / 1e4
#3E4
quantile(samples, 0.2)
#3E5
quantile(samples, 0.8)
#3E6
HPDI(samples, prob = 0.66)
#3E7
quantile(samples, prob = c( .165, 1-.165))

### *** Difficulty level: Medium

##Setting random number generator so answers can be repeated
set.seed(42)

#3M1
p_grid <- seq(from = 0, to = 1, length.out = 1000)
prior <- rep(1 , 1000)
likelihood <- dbinom(8 , size = 15, prob=p_grid)
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)

plot(p_grid, posterior, type = "l")
p_grid[posterior == max(posterior)] #p = 0.53

#3M2
samples <- sample(p_grid, prob = posterior, size = 1e4, replace = TRUE)
HPDI(samples, prob = 0.9) #[0.34, 0.73]

#3M3
w <- rbinom(1e4, size = 15, prob=samples)
sum(w == 8)/1e4 #P(8 water) = 0.1464

#3M4
#Using posterior predictive check
w <- rbinom(1e4, size = 9, prob = samples)
sum(w == 6)/1e4 #P(6 water) = 0.1711

#3M5
p_grid <- seq( from=0 , to=1 , length.out=1000 )
prior <- c(rep(0 , 500), rep(1,500))
likelihood <- dbinom( 6 , size=9 , prob=p_grid ) ##Change this to 8 out of 15 if needed
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)

samples <- sample( p_grid , prob=posterior , size=1e4 , replace=TRUE )
plot(p_grid, posterior, type = "l")
p_grid[posterior == max(posterior)] #p = 0.67

HPDI(samples, prob = 0.9) #[0.50, 0.82]

w <- rbinom(1e4, size = 15, prob = samples)
sum(w == 8)/1e4 #P(8 water) = 0.0997

w <- rbinom(1e4, size = 9, prob = samples)
sum(w == 6)/1e4 #P(6 water) = 0.2219

#3M6
#How many observations are needed for 0.05 length PI interval?

##Okay, lets write a function to do a bunch of simulations that returns to width of the PI interval
simulate.globetoss <- function(tosses, n.simulation) {

    #Initialize some parameters
    p_grid <- seq(from = 0, to = 1, length.out = 1000) #For grid approximation
    prior <- rep(1 , 1000) #Flat prior
    #Initialize results matrix
    results.mat <- matrix(rep(0, length(tosses)*n.simulation), ncol = n.simulation)

    #Function to make one simulation and return 99% interval width
    single.sim <- function(n.toss, prior, p_grid) {
               
        #True probability of water, p = 0.7
        data <- rbinom(n = n.toss, size = 1, prob = 0.7)
        likelihood <- dbinom(sum(data), size = length(data), prob = p_grid)
        posterior <- likelihood * prior
        posterior <- posterior / sum(posterior)

        samples <- sample(p_grid, prob = posterior, size = 1e4, replace = TRUE)
        int <- PI(samples, prob = 0.99)
        width <- int[2] - int[1]

        return(width)
    }

    ##Perform simulations, code not optimized :(
    for(i in 1:length(tosses)) {
            for(j in 1:n.simulation) {
                results.mat[i,j] <- single.sim(n.toss = tosses[i], prior, p_grid)
            }
        }

     sim.means <- apply(results.mat, 1, mean)
     final.results <- data.frame(tosses = tosses, mean.width = sim.means)
     return(final.results)
   
}

simu.data <- simulate.globetoss(tosses = c(10, 50, 100, 200, 500, 1000, 2000, 5000, 10000), n.simulation = 100)                         

##Cant plot without ggplot 2 ;)
library(ggplot2)
library(cowplot)
theme_set(theme_cowplot())

ggplot(simu.data, aes(x = log10(tosses), y = mean.width)) +
    geom_point() +
    geom_line() +
    geom_hline(yintercept = 0.05, lty = "dashed")

#Looks like we need more than 2000 but less than 5000, tosses. A more dense sampling from this area

simu.data <- simulate.globetoss(tosses = c(2000:2500), n.simulation = 100)

ggplot(simu.data, aes(x = log10(tosses), y = mean.width)) +
    geom_point() +
    #geom_line() +
    geom_hline(yintercept = 0.05, lty = "dashed")

simu.data[simu.data[,2] < 0.05,][1,] #More than 2206 tosses

### *** Difficulty level: Hard

data(homeworkch3)

#3H1
all.births <- c(birth1, birth2)
sum(all.births)

p_grid <- seq(from = 0, to = 1, length.out = 1000)
prior <- rep(1, 1000)
likelihood <- dbinom(111, size = 200, prob = p_grid)
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)

plot(p_grid, posterior, type = "l")
p_grid[which.max(posterior)] #p = 0.55

#3H2
samples <- sample(p_grid, prob = posterior, size = 1e4, replace = TRUE)

HPDI(samples, 0.5)
HPDI(samples, 0.89)
HPDI(samples, 0.97)

#3H3
pred <- rbinom(1e4, size = 200, prob = samples)

dens(pred)
abline( v = 111)

#3H4

pred <- rbinom(1e4, size = 100, prob = samples)

dens(pred)
abline( v = sum(birth1))
abline( v = mean(pred), col = "blue")

#Model model fits less well, but the observed data is still plausible

#Observed births = 51
sum(birth1)
PI(pred) #[46, 65]

#3H5
#Assumes that first and second births are independent

#Get the second male births following female births

#Female first births
100 - sum(birth1) #49

#First birth female followed by a male
obs1f2m <- sum(birth1 == 0 & birth2 == 1) #39 cases out of 100

pred <- rbinom(1e4, size = 49, prob = samples)

simplehist(pred) #Model prediction is 28 male births, 39 has very low probability
abline(v = obs1f2m)

sum(pred >= 39)/1e4 #Very low probability to observe 39 male births after 1 birth female
PI(pred) #[21, 33]

#Model is underestimating the number of male births when the first child is a female. Hard to speculate on the reasons before knowing anything else about the data.
### * Chapter 4

#bookmark 4.3
#bookmark 4.4

### ** Practise

### *** Difficulty level: Easy

#4E1
#First line is the likelihood

#4E2
#2 parameters, mu and sigma

#4E3
#Pr(mu, sigma|y) = Prod N(y_i|mu,sigma) N(mu|0,10) exp(sigma|1) / int int Prod N(y_i|mu,sigma) N(mu|0,10) exp(sigma|1) d mu d sigma

#4E4
#Second line: mu_i = alpha + beta x_i

#4E5
#3 paremeters, alpha, beta, and sigma

### *** Difficulty level: Medium

#4M1
sample.mu <- rnorm(10000, 0, 10)
sample.sigma <- rexp(10000, 1)
sim.prior <- rnorm(10000, sample.mu, sample.sigma)

dens(sim.prior)

#4M2
flist <- alist(
  y ~ dnorm(mu, sigma),
  mu ~ dnorm(0, 10),
  sigma ~ dexp(1)
)

#4M3
#y_i ~ N(mu, sigma)
#mu_i = a + Bx_i
#a ~ N(0, 10)
#B ~ U(0,1)
#sigma ~ exp(1)

#4M4
#Okay we don't actually know how old these students are. Assuming uni students, ~20 years old. Likely most have stopped growing already

#y_i ~ N(mu, sigma)
#mu_i = a + B(x_i-x_bar)
#a ~ N(170, 20)
#B ~ N(0, 5)
#sigma ~ exp(1)

N <- 1000
a <- rnorm(N, 170, 20)
b <- rnorm(N, 0, 5)

plot(NULL , xlim=range(year.seq) , ylim=c(-100,400) ,
xlab="weight" , ylab="height" )
xbar <- mean(year.seq)
for ( i in 1:N ) curve( a[i] + b[i]*(x - xbar) ,
from=min(year.seq) , to=max(year.seq) , add=TRUE ,
col=col.alpha("black",0.2) )

#4M5
#LogNormal prior for B
b <- rlnorm(N, 0, 1)

plot(NULL , xlim=range(year.seq) , ylim=c(-100,400) ,
xlab="weight" , ylab="height" )
xbar <- mean(year.seq)
for ( i in 1:N ) curve( a[i] + b[i]*(x - xbar) ,
from=min(year.seq) , to=max(year.seq) , add=TRUE ,
col=col.alpha("black",0.2) )

#4M6

#Variance of 64 implies sd = sqrt(64) = 8

dens(rexp(1e4, 1)) #exp(1) gives very little probability for values larger than 8. Looks pretty OK

### *** Difficulty level: Hard

#4H1
data(Howell1)
how_dat <- Howell1
how_data <- how_dat[how_dat$age > 18,]

#Model used in chapter
m4.3 <- quap(alist(height ~ dnorm(mu, sigma),
                   mu <- a + b * (weight - mean(weight)),
                   a ~ dnorm(178, 20),
                   b ~ dlnorm(0, 1),
                   sigma ~ dunif(0, 50)),
             data = how_dat)

weight.seq <- c(46.95, 43.72, 64.78, 32.59, 54.63)

#Prediction intervals for height
sim.height <- sim(m4.3 , data=list(weight=weight.seq) )
str(sim.height)

height.mean <- apply(sim.height, 2, mean)
height.PI <- apply( sim.height , 2 , PI , prob=0.89 ) #Prediction intervals

#4H2
#4H2
d <- Howell1
d3 <- d[d$age < 18,]

model <- quap(
alist(
    height ~ dnorm(mu, sigma),
    mu <- a + b*weight,
a ~ dnorm( 178 , 20 ) ,
b ~ dlnorm( 0 , 1) ,
sigma ~ dunif( 0 , 50 )
) ,
data=d3)

precis(model)

# define sequence of weights to compute predictions for
# these values will be on the horizontal axis
weight.seq <- seq( from=0 , to=50 , by=1 )

# use link to compute mu
# for each sample from posterior
# and for each weight in weight.seq
mu <- link( model , data=data.frame(weight=weight.seq) )
str(mu)

# summarize the distribution of mu
mu.mean <- apply( mu , 2 , mean )
mu.HPDI <- apply( mu , 2 , HPDI , prob=0.89 )

#Prediction intervals for height
sim.height <- sim( model , data=list(weight=weight.seq) )
str(sim.height)

height.PI <- apply( sim.height , 2 , PI , prob=0.89 )

# plot raw data
# fading out points to make line and interval more visible
plot( height ~ weight , data=d3 , col=col.alpha(rangi2,0.5) )
# plot the MAP line, aka the mean mu for each weight
lines( weight.seq , mu.mean )
# plot a shaded region for 89% HPDI
shade( mu.HPDI , weight.seq )
# draw PI region for simulated heights
shade( height.PI , weight.seq )

#4H3
#Using all data
#A) 
d$log.weight <- log(d$weight) #Transform weight to log scale

model <- quap(
    alist(
        height ~ dnorm(mu, sigma),
        mu <- a + b * log.weight,
        a ~ dnorm(178, 20),
        b ~ dnorm(0,10),
        sigma ~ dunif(0, 50)
    ),
    data = d)

precis(model)

#B)



weight.seq <- log(seq( from=1 , to=70 , by=1 ))

mu <- link( model , data=data.frame(log.weight=weight.seq) )
#str(mu)

mu.mean <- apply( mu , 2 , mean )
mu.HPDI <- apply( mu , 2 , HPDI , prob=0.97 )

#Prediction intervals for height
sim.height <- sim( model , data=list(log.weight=weight.seq) )

height.PI <- apply( sim.height , 2 , PI , prob=0.97 )

plot( height ~ weight , data=Howell1 ,
     col=col.alpha(rangi2,0.4) )

#Mean mu for each weight
lines(exp(weight.seq) , mu.mean )
#Interval
shade( mu.HPDI , exp(weight.seq) )
# draw PI region for simulated heights
shade( height.PI , exp(weight.seq) )

#4H4
d <- Howell1
d <- d[d$age > 18,]

d$weight_s <- ( d$weight - mean(d$weight) )/sd(d$weight)
d$weight_s2 <- d$weight_s^2
m4.5 <- quap(alist(
height ~ dnorm( mu , sigma ) ,
mu <- a + b1*weight_s + b2*weight_s2 ,
a ~ dnorm( 178 , 20 ) ,
b1 ~ dlnorm( 0 , 1 ) ,
b2 ~ dnorm( 0 , 1 ) ,
sigma ~ dunif( 0 , 50 )
) ,
data=d )


N <- 100
a <- rnorm(N, 170, 20)
b1 <- rlnorm(N, 0, 1)
b2 <- rnorm(N, 0, 1)

plot(NULL , xlim=range(d$weight) , ylim=c(-100,400) ,
xlab="weight" , ylab="height" )
xbar <- mean(d$weight)
for ( i in 1:N ) curve( a[i] + b1[i]*(x - xbar) + b2[i]*(x-xbar),
from=min(d$weight) , to=max(d$weight) , add=TRUE ,
col=col.alpha("black",0.2) )
### * Chapter 5

#"In large data sets, every pair of variables has a statistically discernible non-zero correlation"

data(WaffleDivorce)
d <- WaffleDivorce
d$A <- scale( d$MedianAgeMarriage )
d$D <- scale( d$Divorce )
d$M <- scale(d$Marriage) 

library(dagitty)
dag5.1 <- dagitty( "dag {
A -> D
A -> M
M -> D
}")
coordinates(dag5.1) <- list( x=c(A=0,D=1,M=2) , y=c(A=0,D=1,M=0) )
drawdag( dag5.1 )

DMA_dag2 <- dagitty('dag{ D <- A -> M }')
impliedConditionalIndependencies( DMA_dag2 )

#"Never use residuals as data!"

#bookmark 5.2
prior <- extract.prior( m5.5 )
xseq <- c(-2,2)
mu <- link( m5.5_draft , post=prior , data=list(N=xseq) )
plot( NULL , xlim=xseq , ylim=xseq )
for ( i in 1:50 ) lines( xseq , mu[i,] , col=col.alpha("black",0.3) )

#
xseq <- seq( from=min(dcc$M)-0.15 , to=max(dcc$M)+0.15 , length.out=30 )
mu <- link( m5.6 , data=list(M=xseq) )
mu_mean <- apply(mu,2,mean)
mu_PI <- apply(mu,2,PI)
plot( K ~ M , data=dcc )
lines( xseq , mu_mean , lwd=2 )
shade( mu_PI , xseq )

m5.7 <- quap(
alist(
K ~ dnorm( mu , sigma ) ,
mu <- a + bN*N + bM*M ,
a ~ dnorm( 0 , 0.2 ) ,
bN ~ dnorm( 0 , 0.5 ) ,
bM ~ dnorm( 0 , 0.5 ) ,
sigma ~ dexp( 1 )
) , data=dcc )
precis(m5.7)

#Counterfactual holding N = 0
xseq <- seq( from=min(dcc$M)-0.15 , to=max(dcc$M)+0.15 , length.out=30 )
mu <- link( m5.7 , data=data.frame( M=xseq , N=0 ) )
mu_mean <- apply(mu,2,mean)
mu_PI <- apply(mu,2,PI)
plot( NULL , xlim=range(dcc$M) , ylim=range(dcc$K) )
lines( xseq , mu_mean , lwd=2 )
shade( mu_PI , xseq )

#Counterfactual holding M = 0
xseq <- seq( from=min(dcc$N)-0.15 , to=max(dcc$N)+0.15 , length.out=30 )
mu <- link( m5.7 , data=data.frame( N=xseq , M=0 ) )
mu_mean <- apply(mu,2,mean)
mu_PI <- apply(mu,2,PI)
plot( NULL , xlim=range(dcc$N) , ylim=range(dcc$K) )
lines( xseq , mu_mean , lwd=2 )
shade( mu_PI , xseq )

dag5.7 <- dagitty( "dag{
M -> K <- N
M -> N }" )
coordinates(dag5.7) <- list( x=c(M=0,K=1,N=2) , y=c(M=0.5,K=1,N=0.5) )
MElist <- equivalentDAGs(dag5.7)
drawdag(MElist)

### ** Practise

### *** Difficulty level: Easy

#5E1
#2 and 4

#5E2
#Let A = animal diversity, P = plant diversity, and L = latitude
#A ~ (mu, sigma)
#mu_i = alpha + L_i*Beta_L + P_i*Beta_P

#5E3
#Let F = amount of funding, S = size of laboratory, T = time to degree
#T ~ (mu, sigma)
#mu_i = alpha + F_i*Beta_F + S_i*Beta_S
#Both Beta_F and Beta_S are positive andF_i and S_i have to be negatively correlated with each other

#5E4
#Ignoring any effects of priors
#Models 1, 3, 4, and 5 are inferentially equivalent, model 2 has an extra parameter

### *** Difficulty level: Medium

#5M1
#Spurious correlation: Association number of people drowned, ice cream sales, and temperature
#both ice cream sales and temperature are correlated with n. people drowned
#However, when both temperature and ice cream sales are included in the model only temp. has association

#5M2
#Masked relationship: Association between health, income and use of medicine. Richer people are more healthy, healthy people use less drugs, and richer people use more drugs (because they can afford them)

#5M3
#If people remarry after their divorce divorces could result in more marriages

#5M4
#Mormon data from wikipedia
data(WaffleDivorce)
d <- WaffleDivorce
d$pct_LDS <- c(0.75, 4.53, 6.18, 1, 2.01, 2.82, 0.43, 0.55, 0.38,
0.75, 0.82, 5.18, 26.35, 0.44, 0.66, 0.87, 1.25, 0.77, 0.64, 0.81,
0.72, 0.39, 0.44, 0.58, 0.72, 1.14, 4.78, 1.29, 0.61, 0.37, 3.34,
0.41, 0.82, 1.48, 0.52, 1.2, 3.85, 0.4, 0.37, 0.83, 1.27, 0.75,
1.21, 67.97, 0.74, 1.13, 3.99, 0.92, 0.44, 11.5 )

m1 <- quap(
alist(
Divorce ~ dnorm(mu,sigma),
mu <- a + bR*Marriage + bA*MedianAgeMarriage + bM*pct_LDS,
a ~ dnorm(0,100),
c(bA,bR,bM) ~ dnorm(0,10),
sigma ~ dunif(0,10)
),
data=d )
precis( m1 )

#5M5
#Let O be obesity, E the amount of exercise, P the price of gasoline, D amount of driving, R restaurant usage

#Hypothesis 1 (Higher P lead to less D, higher D leads to less E, and Higher E leads to less O)
#There have to be negative relationships between as above

#Hypothesis 2 (Higher P leads to less D, less D leads to less R, less R leads to less O)
#There have to be negative relationship between P and D, and positive relationships between D and R; R and O)
#Should probably control for things like income, education etc.

### *** Difficulty level: Hard

data(foxes)

#5H1
m5h1 <- quap(
    alist(
        weight ~ dnorm(mu, sigma),
        mu <- a + b1*area,
        a ~ dnorm(4, 1),
        b1 ~ dnorm(0,10),
        sigma ~ dunif(0, 10)
        ),
    data = foxes)

#Prior predictive check
prior <- extract.prior(m5h1)
xseq <- c(1,6)
mu <- link(m5h1 , post=prior , data=list(area=xseq) )
plot( NULL , xlim=xseq , ylim=c(0,8) )
for ( i in 1:50 ) lines( xseq , mu[i,] , col=col.alpha("black",0.3) )

precis(m5h1)

#Plot
area.seq <- seq(from = 0.5, to = 5.5, length.out = 200)
mu <- link(m5h1, data = data.frame(area=area.seq))
mu.PI <- apply(mu, 2, PI)
#plot
plot(weight ~ area, data = foxes, col=rangi2)
abline(m5h1)
shade(mu.PI, area.seq)

##Area has no relationship with weight

m5h1.2 <- quap(
    alist(
        weight ~ dnorm(mu, sigma),
        mu <- a + b1*groupsize,
        a ~ dnorm(4, 1),
        b1 ~ dnorm(0, 10),
        sigma ~ dunif(0,10) ),
    data = foxes)

#Prior predictive check
prior <- extract.prior(m5h1.2)
xseq <- c(1,10)
mu <- link(m5h1.2, post=prior , data=list(groupsize=xseq) )
plot( NULL , xlim=xseq , ylim=c(0,8) )
for ( i in 1:50 ) lines( xseq , mu[i,] , col=col.alpha("black",0.3) )

precis(m5h1.2)

#Plot
groupsize.seq <- seq(from = 1, to = 9, length.out = 200)
mu <- link(m5h1.2, data = data.frame(groupsize=groupsize.seq))
mu.PI <- apply(mu, 2, PI)
#plot
plot(weight ~ groupsize, data = foxes, col = rangi2)
abline(m5h1.2)
shade(mu.PI, groupsize.seq)

##Group size has only a small effect

#5H2
m5h2 <- quap(
    alist(
        weight ~ dnorm(mu, sigma),
        mu <- a + b1*area + b2*groupsize,
        a ~ dnorm(4, 1),
        b1 ~ dnorm(0, 10),
        b2 ~ dnorm(0, 10),
        sigma ~ dunif(0, 10) ),
    data = foxes)

#Prior predictive check
prior <- extract.prior(m5h2)
xseq <- c(1,10)
mu <- link(m5h2, post=prior , data=list(groupsize=xseq, area = c(1,6)) )
plot( NULL , xlim=xseq , ylim=c(0,8) )
for ( i in 1:50 ) lines( xseq , mu[i,] , col=col.alpha("black",0.3) )

precis(m5h2)

#Now area has a positive effect on weight and groupsize larger negative effect on weight

#Plotting the effects of both variables, while holding the other constant (at mean)

#Effect of area (groupsize held constant) (Counterfactual plot)
mean.groupsize <- mean(foxes$groupsize)
area.seq <- seq(from = 0.5, to = 5.5, length.out = 200)
pred.data <- data.frame(area = area.seq, groupsize = mean.groupsize)
mu <- link(m5h2, data = pred.data, n = 1e4)
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI)
plot(weight ~ area, data = foxes, type = "n")
lines(area.seq, mu.mean)
shade(mu.PI, area.seq)

#Effect of groupsize (area held constant) (Counterfactual plot)
mean.area <- mean(foxes$area)
groupsize.seq <- seq(from = 1, to = 9, length.out = 200)
pred.data <- data.frame(area = mean.area, groupsize = groupsize.seq)
mu <- link(m5h2, data = pred.data, n = 1e4)
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI)
plot(weight ~ groupsize, data = foxes, type = "n")
lines(groupsize.seq, mu.mean)
shade(mu.PI, groupsize.seq)

#We observe different results because area and groupsize are correlated
plot(groupsize ~ area, data=foxes)
#But since correlation is not perfect, in multiple regression we can distangle that in those areas with less foxes for are size the foxes tend to be heavier

#5H3
m5h3.1 <- quap(
    alist(
        weight ~ dnorm(mu, sigma),
        mu <- a + b1*avgfood + b2*groupsize,
        a ~ dnorm(4, 1),
        b1 ~ dnorm(0, 10),
        b2 ~ dnorm(0, 10),
        sigma ~ dunif(0, 10) ),
    data = foxes)

#Prior predictive check
prior <- extract.prior(m5h3.1)
xseq <- c(1,10)
mu <- link(m5h3.1, post=prior , data=list(groupsize=xseq, avgfood = c(0,2)) )
plot( NULL , xlim=xseq , ylim=c(0,8) )
for ( i in 1:50 ) lines( xseq , mu[i,] , col=col.alpha("black",0.3) )

precis(m5h3.1)

m5h3.2 <- quap(
    alist(
        weight ~ dnorm(mu, sigma),
        mu <- a + b1*avgfood + b2*area + b3*groupsize,
        a ~ dnorm(4, 1),
        b1 ~ dnorm(0, 10),
        b2 ~ dnorm(0, 10),
        b3 ~ dnorm(0, 10),
        sigma ~ dunif(0, 10) ),
    data = foxes)

precis(m5h3.2)

#When area is included in the model standard deviation of avgfood increases
plot(coeftab(m5h3.1, m5h3.2), pars = c("b1", "b2", "b3"))

pairs( ~ weight + avgfood + area + groupsize, data=foxes , col=rangi2 )
#average food and are also correlated with each other. Which is the better predictor? Could do some model comparison (introduced in a later chapter), or experiment. Biological reasoning suggests that avgfood is probably more important?


######################################################3
#Extra assignments that were not in our PDF version ## 
######################################################

#5H1
#In the divorce example, suppose the DAG is: M→A→D. What are the implied conditional independencies of the graph? Are the data consistent with it?
library(dagitty)

divorce_dag <- dagitty("dag{ M -> A -> D}")
impliedConditionalIndependencies(divorce_dag)

#   D _||_ M | A
#Divorce rate should be independent of marriage rate conditioning on age of marriage
data(WaffleDivorce)
d <- list()
d$A <- standardize( WaffleDivorce$MedianAgeMarriage )
d$D <- standardize( WaffleDivorce$Divorce )
d$M <- standardize( WaffleDivorce$Marriage )

mMA <- quap(
alist(
D ~ dnorm( mu , sigma ) ,
mu <- a + bM*M + bA*A ,
a ~ dnorm( 0 , 0.2 ) ,
bM ~ dnorm( 0 , 0.5 ) ,
bA ~ dnorm( 0 , 0.5 ) ,
sigma ~ dexp( 1 )),
data = d )

##The data are consistent with this implied conditional independency


#5H2
m5.3_A <- quap(
alist(
## M -> A -> D
D ~ dnorm( mu , sigma ) ,
mu <- a + bM*M + bA*A ,
a ~ dnorm( 0 , 0.2 ) ,
bM ~ dnorm( 0 , 0.5 ) ,
bA ~ dnorm( 0 , 0.5 ) ,
sigma ~ dexp( 1 ),
## M -> A
A ~ dnorm( mu_M , sigma_M ),
mu_M <- aM + bAM*M,
aM ~ dnorm( 0 , 0.2 ),
bAM ~ dnorm( 0 , 0.5 ),
sigma_M ~ dexp( 1 )
) , data = d )


M_seq <- seq( from=-2 , to=2 , length.out=30 )
sim_dat <- data.frame( M=M_seq )
# simulate A and then D, using M_seq
s <- sim( m5.3_A , data=sim_dat , vars=c("A","D") )

# display counterfactual predictions
plot( sim_dat$M , colMeans(s$D) , ylim=c(-2,2) , type="l" ,
xlab="manipulated M" , ylab="counterfactual D" )
shade( apply(s$D,2,PI) , sim_dat$M )
mtext( "Total counterfactual effect of M on D" )

#5H3
data(milk)
d <- milk
d$K <- scale( d$kcal.per.g )
d$N <- scale( d$neocortex.perc )
d$M <- scale( log(d$mass))
dcc <- d[ complete.cases(d$K,d$N,d$M) , ]

m5h3 <- quap(
    alist(
        ##K <- M -> N
        K ~ dnorm(mu, sigma),
        mu <- a + bM*M + bN*N,
        a ~ dnorm(0, 0.2),
        bM ~ dnorm(0, 0.5),
        bN ~ dnorm(0, 0.5),
        sigma ~ dexp(1),
        ##M -> N
        N ~ dnorm(mu_N, sigma_N),
        mu_N <- a_N + bM_N*M,
        a_N ~ dnorm(0, 0.2),
        bM_N ~ dnorm(0, 0.5),
        sigma_N ~ dexp(1)
        ), data = dcc )

M_seq <- seq(from = -2, to = 2, length.out = 30)
sim_dat <- data.frame(M=M_seq)
#simulate N and then K using M_seq
s <- sim(m5h3, data = sim_dat, vars = c("N", "K"))

# display counterfactual predictions
plot( sim_dat$M , colMeans(s$K) , ylim=c(-2,2) , type="l" ,
xlab="manipulated M" , ylab="counterfactual K" )
shade( apply(s$K,2,PI) , sim_dat$M )
mtext( "Total counterfactual effect of M on K" )

#Note that since mass is on log scale, manipulating the counterfactual effect would be non-linear if mass were on normal scale

#5H4
#In our model southerness influences M and A, which influence D, A influences also M
divorce_dag <- dagitty("dag{
A -> M -> D
A -> M
A <- S -> M
A -> D
}")
drawdag( divorce_dag )
impliedConditionalIndependencies(divorce_dag)
#D _||_ S | A, M
#Divorce rate should be independent of southernness when we condition on age of marriage and marriage rate

data(WaffleDivorce)
d <- list()
d$A <- standardize( WaffleDivorce$MedianAgeMarriage )
d$D <- standardize( WaffleDivorce$Divorce )
d$M <- standardize( WaffleDivorce$Marriage )
d$S <- WaffleDivorce$South + 1 #Southern states are index 2

m5h4 <- quap(
    alist(
        D ~ dnorm(mu, sigma),
        mu <- a[S] + bA*A + bM*M,
        a[S] ~ dnorm(0, 0.2),
        bA ~ dnorm(0, 0.5),
        bM ~ dnorm(0, 0.5),
        sigma ~ dexp(1)), data = d)

post <- extract.samples(m5h4)
post$diff_south <- post$a[,2] - post$a[,1]
precis(post , depth=2 )
#Inspecting the posterior sample of difference between non-Southern and Southern states. The 89% interval overlaps with zero, suggesting that divorce rate is mostly independent when conditioning on age and marriage rate. However, thu bulk of the difference is positive, which could give a hint that there are some unobservable variables associated with the South that still influence divorce rate.

precis(m5h4, depth = 2)



### * Chapter 6

# "Regression will no sort it out"
# "... and model selection does not help"
# "because causal inference and making out-of-sample predictions are not the same thing!"

### ** Some notes while reading

m6.1 <- quap(
alist(
height ~ dnorm( mu , sigma ) ,
mu <- a + bl*leg_left + br*leg_right ,
a ~ dnorm( 10 , 100 ) ,
bl ~ dnorm( 2 , 10 ) ,
br ~ dnorm( 2 , 10 ) ,
sigma ~ dexp( 1 )
) ,
data=d )
precis(m6.1)


data(milk)
d <- milk
d$K <- scale( d$kcal.per.g )
d$F <- scale( d$perc.fat )
d$L <- scale( d$perc.lactose )

set.seed(71)
# number of plants
N <- 100
# simulate initial heights
h0 <- rnorm(N,10,2)
# assign treatments and simulate fungus and growth
treatment <- rep( 0:1 , each=N/2 )
fungus <- rbinom( N , size=1 , prob=0.5 - treatment*0.4 )
h1 <- h0 + rnorm(N, 5 - 3*fungus)
# compose a clean data frame
d <- data.frame( h0=h0 , h1=h1 , treatment=treatment , fungus=fungus )
precis(d)

library(dagitty)
plant_dag <- dagitty( "dag {
H_0 -> H_1
F -> H_1
T -> F
}")
coordinates( plant_dag ) <- list( x=c(H_0=0,T=2,F=1.5,H_1=1) ,
y=c(H_0=0,T=0,F=0,H_1=0) )
drawdag( plant_dag )

#bookmark 6.4

dag_6.1 <- dagitty( "dag {
U [unobserved]
X -> Y
X <- U <- A -> C -> Y
U -> B <- C
}")
adjustmentSets( dag_6.1 , exposure="X" , outcome="Y" ) #Tells us which variables to conditin for


dag_6.2 <- dagitty( "dag {
A -> D
A -> M -> D
A <- S -> M
S -> W -> D
}")
adjustmentSets( dag_6.2 , exposure="W" , outcome="D" )

impliedConditionalIndependencies( dag_6.2 )

### ** Practise problems

### *** Difficulty level: Easy

#6E1
#Multicollinearity
#Post-treatment bias 
#Collider bias (two unrelated variables are both related to a third variable, inclusion of third variable in the model created statistical association between the two)

#6E2

#6E3
#Fork: variable Z is a common cause of both X and Y, X is independent of Y conditional on Z
#Pipe: X influences Z, which influences Y. X is independent of Y conditional on Z
#Collider: variable X and variable Y both influence Z. X is not independent from Y conditional on Z
#Descendant: X and Y both influence Z, and Z causes D. Z is a collider, so keeping Z out of the model but including D would cause partial collider bias.

#6E4
#With a collider an association is introduced between two variables, when a third variable is added. In the funding example there is conditioning on the selection status if we have data only on funded samples. Of course this is very tricky because the collider is not in the data!

### *** Difficulty level: Medium
library(dagitty)
#6M1

dag.m61 <- dagitty("dag{
U [unobserved]
V [unobserved]
X -> Y
X <- U -> B
U <- A -> C
B <- C -> Y
C <- V -> Y
}")

coordinates(dag.m61) <- list(x = c(X=1, U = 1, A = 2, B=2, C=3, Y=3, V=3.5), y = c(X=1, U=2, A=2.5, B=1.5, C=2, Y=1, V=1.5))
drawdag( dag.m61 )
#Four paths from X to Y:
#1. X <- U <- A -> C -> Y
#2. X <- U -> B <- C -> Y
#3. X <- U <- A -> C <- V -> Y
#4. X <- U -> B <- C <- V -> Y

#paths 2 and 4 are closed because B is a collider. Paths 1 and 3 are open because A is a fork.
#We must condition on A
adjustmentSets(dag.m61, exposure = "X", outcome = "Y") #{A}

#6M2
x <- rnorm(100)
z <- rnorm(100, mean = x, sd = 0.3)
y <- rnorm(100, mean = z)

x <- scale(x)
z <- scale(z)
y <- scale(y)

d <- data.frame(y, x ,z)

m6m2 <- quap(
    alist(
        y ~ dnorm(mu, sigma),
        mu <- a + bX*x + bZ*z,
        a ~ dnorm(0, 0.2),
        bX ~ dnorm(0, 0.5),
        bZ ~ dnorm(0, 0.5),
        sigma ~ dexp(1)),
    data = d)

precis(m6m2)

#No multicollinearity observed, multicollinearity depends on the causal model

#6M3
#upper left
dag1 <- dagitty("dag{X -> Y; X <- Z -> Y; Z <- A -> Y}")
adjustmentSets(dag1, exposure = "X", outcome = "Y") #Need to condition on Z

#upper right
dag2 <- dagitty("dag{Z <- X -> Y; Z -> Y; Z <- A -> Y}")
adjustmentSets(dag2, exposure = "X", outcome = "Y") #No need to include anything

#bottom left
dag3 <- dagitty("dag{Z <- X -> Y; Z <- Y; X <- A -> Z}")
adjustmentSets(dag3, exposure = "X", outcome = "Y") #No need to include anything

#bottom right
dag4 <- dagitty("dag{Y <- X -> Z; Y <- Z; X <- A -> Z}")
adjustmentSets(dag4, exposure = "X", outcome = "Y") #Need to condition on A

### *** Difficulty level: Hard

#6H1
#Find the causal influence on Waffle Houses on divorce rate

#Using the DAG from the chapter
waffle_dag <- dagitty("dag { S -> W -> D <- A <- S -> M -> D; A -> M }")
drawdag(waffle_dag)

adjustmentSets(waffle_dag, exposure = "W", outcome = "D") #We need to condition either on S or both A and M
#Lets use only S

data(WaffleDivorce)
d <- list()
d$A <- scale(WaffleDivorce$MedianAgeMarriage)
d$D <- scale(WaffleDivorce$Divorce)
d$M <- scale(WaffleDivorce$Marriage)
d$W <- scale(WaffleDivorce$WaffleHouses)
d$S <- WaffleDivorce$South

m6h1 <- quap(
    alist(
        D ~ dnorm(mu, sigma),
        mu <- a + bW*W + bS*S,
        a ~ dnorm(0, 0.2),
        bW ~ dnorm(0, 0.5),
        bS ~ dnorm(0, 0.5),
        sigma ~ dexp(1)), data = d)

precis(m6h1)

#Estimate of bW overlaps with zero. No causal influence of waffle houses on divorce

#6H2
impliedConditionalIndependencies(waffle_dag)
#A _||_ W | S
#D _||_ S | A, M, W
#M _||_ W | S

cond1 <- quap(
    alist(
        A ~ dnorm(mu, sigma),
        mu <- a + bW*W + bS*S,
        a ~ dnorm(0, 0.2),
        bW ~ dnorm(0, 0.5),
        bS ~ dnorm(0, 0.5),
        sigma ~ dexp(1)), data = d)

precis(cond1) #This is okay, waffles do not influenge age of marriage

cond2 <- quap(
    alist(
        D ~ dnorm(mu, sigma),
        mu <- a + bS*S + bW*W + bA*A + bM*M,
        a ~ dnorm(0, 0.2),
        bW ~ dnorm(0, 0.5),
        bS ~ dnorm(0, 0.5),
        bA ~ dnorm(0, 0.5),
        bM ~ dnorm(0, 0.5),
        sigma ~ dexp(1)), data = d)

precis(cond2) #This is borderline, as S does overlap with zero, but is still slightly positive. We probably have not taken everything else into account (education etc.).

cond3 <- quap(
    alist(
        M ~ dnorm(mu, sigma),
        mu <- a + bW*W + bS*S,
        a ~ dnorm(0, 0.2),
        bW ~ dnorm(0, 0.5),
        bS ~ dnorm(0, 0.5),
        sigma ~ dexp(1)), data = d)

precis(cond3) #This is OK

#6H3
data(foxes)
foxes$area <- scale(foxes$area)
foxes$avgfood <- scale(foxes$avgfood)
foxes$groupsize <- scale(foxes$groupsize)
foxes$weight <- scale(foxes$weight)

fox_dag <- dagitty("dag{ area -> avgfood -> groupsize -> weight <- avgfood }")
adjustmentSets(fox_dag, exposure = "area", outcome = "weight") #No need to condition on anything

m6h3 <- quap(
    alist(
        weight ~ dnorm(mu, sigma),
        mu <- a + bA*area,
        a ~ dnorm(0, 0.2),
        bA ~ dnorm(0, 0.5),
        sigma ~ dexp(1)), data = foxes)

#Prior predictive check
prior <- extract.prior(m6h3)
xseq <- seq(-2, 2, length.out = 30)
mu <- link(m6h3 , post=prior , data=list(area=xseq) )
plot(foxes$weight ~ foxes$area , type = "n", xlim=c(-2,2) , ylim=c(-3,3) )
abline(h = c(min(foxes$weight), max(foxes$weight)))
for ( i in 1:50 ) lines( xseq , mu[i,] , col=col.alpha("black",0.3) )

#Prior looks OK

precis(m6h3) #No effect of area on weight

#6H4
adjustmentSets(fox_dag, exposure = "avgfood", outcome = "weight") #No need to condition on anything

m6h4 <- quap(
    alist(
        weight ~ dnorm(mu, sigma),
        mu <- a + bF*avgfood,
        a ~ dnorm(0, 0.2),
        bF ~ dnorm(0, 0.5),
        sigma ~ dexp(1)), data = foxes)

#No effect of adding food. This makes sense when we look at the DAG. Food influences also groupsize, which influences weight. So the TOTAL causal effect is near zero, when we just add more food we increase groupsize which decreases weight

#6H5
adjustmentSets(fox_dag, exposure = "groupsize", outcome = "weight") #Need to condition on avgfood

m6h5 <- quap(
    alist(
        weight ~ dnorm(mu, sigma),
        mu <- a + bG*groupsize + bF*avgfood,
        a ~ dnorm(0, 0.2),
        bG ~ dnorm(0, 0.5),
        bF ~ dnorm(0, 0.5),
        sigma ~ dexp(1)), data = foxes)

precis(m6h5)

#I was ahead of myself and already wrote the explanation above...

#6H6
#I did not have time to do these exercises...

#6H7

### * Chapter 7

#Some notes
##"When we design any particular statistical model, we must decide whether we want to understand causes or rather just predict"
##"Information: The reduction in uncertainty when we learn the outcome"
##"... we have to use the entire posterior distribution. Otherwise, vengeful angels will descend upon you."

#bookmark 7.3

#"Cross-validation and WAIC aim to find models that make good predictions. They don't solve any causal inference problem."

#bookmark 7.7 Practise

### ** Practice problems

### *** Difficulty level: Easy

#7E1.
#Information is defined as: "The reduction in uncertainty when we learn an outcome
#1. Measure of uncertainty must be continuous. This is to prevent large changes in uncertainty measure resulting from relatively small changes in probabilities (think p-values)
#2. Measure of uncertainty should increases as the numbers of events increases
#3. The measure of uncertainty should be additive.

#7E2.
#Entropy is the average log-probability of an event
#H(p) = -sum_{i=1}^n p_i\log(p_i)

##Function to calculate information entropy
infentropy <- function(p) {
#
#Need to deal with cases when p_i = 0
p_logp <- function(p) {
    if(p == 0) return(0) else return(p*log(p))
    }
#
H <- -1*sum(sapply(p, p_logp))
return(H)
}

#coin with 0.3 tails and 0.7 heads
infentropy(c(0.3, 0.7)) #H = 0.61

#7E3.
infentropy(c(0.2, 0.25, 0.25, 0.3)) #H = 1.38

#7E4.
infentropy(c( 1/3, 1/3, 1/3, 0)) #H = 1.10

### *** Difficulty level: Medium

#7M1.
#AIC = -2lppd + 2p, where lppd = log-pointwise-predictive-density and p = number of free parameters
#WAIC = -2(lppd - sum(var_theta log p(y_i|theta) )
#WAIC is same formula as AIC, with the exception of the last term. WAIC uses 2 times the sum of log-probability variances from each observation instead of 2 times the number of free parameters

#AIC assumes that priors are flat or overwhelmed by likelihood, the posterior distribution is approximately multivariate Gaussian, and the sample size is much greater than the number of parameters. If these hold AIC approx WAIC.

#7M2.
#Model selection refers to just picking the best (lowest criterion value) model and discarding the other models. Information is lost about the relative model accuracy. This information can inform how confident we are in the models. Model selection cares only about predictive accuracy and ignores causal inference. Thus, a model that has confounds may be selected to be the "best".

#Model comparison uses multiple models to understand how the variables included influence prediction and affect the implied conditional independencies in a causal model.

#7M3.
#Models must be fit to the same observations because of the way lppd is calculated
#lppd(y, theta) = sum_i log (1/S) sum_s p(y_i|theta_s), where S = number of samples. Larger sample size will necessarely lead to smaller lppd, which will increase the information criteria

#7M4.
#when prior becomes more concentrated the penalty term in the WAIC formula sum(var_theta log p(y_i|theta) is the sum of variances. Thus, if we restrict the prior to have a smaller variance we get smaller variances for the log-probabilities and a smaller penalty term.

#7M5.
#Informative priors restrict the plausible values for parameters. By using informative priors we limit the model from learning too much from the data.

#7M6.
#If prior is too restrictive the model does not learn enough from data, and the model just returns our prior distributions.

### *** Difficulty level: Hard

#7H1.
data(Laffer)

d <- Laffer
d$rate <- scale(Laffer$tax_rate)
d$rev <- scale(Laffer$tax_revenue)

#Linear
m7h1.1 <- quap(
    alist(
        rev ~ dnorm(mu, sigma),
        mu <- a + bR*rate,
        a ~ dnorm(0,0.2),
        bR ~ dnorm(0,0.5),
        sigma ~ dexp(1)), data = d)

precis(m7h1.1)

xseq <- seq( from = min(d$rate)-0.15, to = max(d$rate)+0.15, length.out = 30)
mu <- link(m7h1.1, data = list(rate=xseq))
mu_mean <- apply(mu,2,mean)
mu_PI <- apply(mu,2,PI)
plot(rev ~ rate, data = d)
lines(xseq, mu_mean, lwd = 2)
shade(mu_PI, xseq)

#Quadratic
m7h1.2 <- quap(
    alist(
        rev ~ dnorm(mu, sigma),
        mu <- a + bR[1]*rate + bR[2]*rate^2,
        a ~ dnorm(0,0.2),
        bR ~ dnorm(0,0.5),
        sigma ~ dexp(1)), data = d, start = list(bR=rep(0,2)))

precis(m7h1.2, depth = 2)

xseq <- seq( from = min(d$rate)-0.15, to = max(d$rate)+0.15, length.out = 30)
mu <- link(m7h1.2, data = list(rate=xseq))
mu_mean <- apply(mu,2,mean)
mu_PI <- apply(mu,2,PI)
plot(rev ~ rate, data = d)
lines(xseq, mu_mean, lwd = 2)
shade(mu_PI, xseq)

#There could be slight curvature

#Comparing the models
compare(m7h1.1, m7h1.2) #But we don't have any evidence to prefer one over the other, dWAIC = 0.3, while dSE = 2.97

#7H2.
compare(m7h1.1, m7h1.2, func = PSIS)


psis_m7h1.1 <- PSIS(m7h1.1, pointwise = TRUE)
waic_m7h1.1 <- WAIC(m7h1.1, pointwise = TRUE)

waic_m7h1.2 <- WAIC(m7h1.2, pointwise = TRUE)
psis_m7h1.2 <- PSIS(m7h1.2, pointwise = TRUE)

plot(psis_m7h1.1$k , waic_m7h1.1$penalty , xlab="PSIS Pareto k" ,
ylab="WAIC penalty" , col=rangi2 , lwd=2 ) #Observation 12 is an outlier

plot(psis_m7h1.2$k , waic_m7h1.2$penalty , xlab="PSIS Pareto k" ,
ylab="WAIC penalty" , col=rangi2 , lwd=2 ) #Observation 12 is an outlier

#Using robust regression
m7h1.1t <- quap(
    alist(
        rev ~ dstudent(2, mu, sigma),
        mu <- a + bR*rate,
        a ~ dnorm(0,0.2),
        bR ~ dnorm(0,0.5),
        sigma ~ dexp(1)), data = d)

precis(m7h1.1t)

xseq <- seq( from = min(d$rate)-0.15, to = max(d$rate)+0.15, length.out = 30)
mu <- link(m7h1.1t, data = list(rate=xseq))
mu_mean <- apply(mu,2,mean)
mu_PI <- apply(mu,2,PI)
plot(rev ~ rate, data = d)
lines(xseq, mu_mean, lwd = 2)
shade(mu_PI, xseq)

#Robust quadratic
m7h1.2t <- quap(
    alist(
        rev ~ dstudent(2, mu, sigma),
        mu <- a + bR[1]*rate + bR[2]*rate^2,
        a ~ dnorm(0,0.2),
        bR ~ dnorm(0,0.5),
        sigma ~ dexp(1)), data = d, start = list(bR=rep(0,2)))

precis(m7h1.2t, depth = 2)

xseq <- seq( from = min(d$rate)-0.15, to = max(d$rate)+0.15, length.out = 30)
mu <- link(m7h1.2t, data = list(rate=xseq))
mu_mean <- apply(mu,2,mean)
mu_PI <- apply(mu,2,PI)
plot(rev ~ rate, data = d)
lines(xseq, mu_mean, lwd = 2)
shade(mu_PI, xseq)

#Model comparison
compare(m7h1.1t, m7h1.2t, func = PSIS)
compare(m7h1.1t, m7h1.2t) #quadratic has now slightly more evidence, but not very strong
#In order to distinguish between linear and quadratic we would need more data with higher tax rates

#7H3.
#Using my function earlier to calculate entropy

#Entering the data
birds <- data.frame( island = paste("Island", 1:3), A = c(0.2, 0.8, 0.05), B = c(0.2, 0.1, 0.15), C = c(0.2, 0.05, 0.7), D = c(0.2, 0.025, 0.05), E = c(0.2, 0.025, 0.05))

apply(birds[,-1], 1, infentropy)
     #Island 1    #Island 2   #Island 3
#H    1.61         0.74         0.98
#Island 1 has the highest entropy (bird distribution is uniform -> max entropy)

#Function to calculate K-L divergence
D_kl <- function(p, q) {
    sum(p*(log(p)-log(q)))
    }

#Initialize results mat
results.mat <- matrix(rep(0, 3*3), ncol = 3)
for(i in 1:3) {
    for(j in 1:3) {
        results.mat[i,j] <- D_kl(birds[i,-1], birds[j,-1])
    }
}

#View the distances
results.mat

#The distances are the shortest when we use island 1. We are least surprised when we start with an uniform distribution. In contrast island 2 has the highest distances to the other islands.

#7H4.
#Previous model from page 181
d <- sim_happiness( seed=1977 , N_years=1000 )
d2 <- d[ d$age>17 , ] # only adults
d2$A <- ( d2$age - 18 ) / ( 65 - 18 )
d2$mid <- d2$married + 1

m6.9 <- quap(
alist(
happiness ~ dnorm( mu , sigma ),
mu <- a[mid] + bA*A,
a[mid] ~ dnorm( 0 , 1 ),
bA ~ dnorm( 0 , 2 ),
sigma ~ dexp(1)
) , data=d2 )
precis(m6.9,depth=2)

m6.10 <- quap(
alist(
happiness ~ dnorm( mu , sigma ),
mu <- a + bA*A,
a ~ dnorm( 0 , 1 ),
bA ~ dnorm( 0 , 2 ),
sigma ~ dexp(1)
) , data=d2 )
precis(m6.10)

compare(m6.9, m6.10) #Model m6.9 is overwhelmingly preferred, it gets all the weight
#However, the correct causal inference is made when using m6.10, because marital status is a collider. Predictions are improved when marital status is included but the association is not causal.

#7H5.
data(foxes)

foxes$area <- scale(foxes$area)
foxes$avgfood <- scale(foxes$avgfood)
foxes$groupsize <- scale(foxes$groupsize)
foxes$weight <- scale(foxes$weight)

#avgfood + groupsize + area
model1 <-  quap(
    alist(
        weight ~ dnorm(mu, sigma),
        mu <- a + bF*avgfood + bG*groupsize + bA*area,
        a ~ dnorm(0, 0.2),
        bF ~ dnorm(0, 0.5),
        bG ~ dnorm(0, 0.5),
        bA ~ dnorm(0, 0.5),
        sigma ~ dexp(1)), data = foxes)

#avgfood + groupsize
model2 <-  quap(
    alist(
        weight ~ dnorm(mu, sigma),
        mu <- a + bF*avgfood + bG*groupsize,
        a ~ dnorm(0, 0.2),
        bF ~ dnorm(0, 0.5),
        bG ~ dnorm(0, 0.5),
        sigma ~ dexp(1)), data = foxes)

#groupsize + area
model3 <-  quap(
    alist(
        weight ~ dnorm(mu, sigma),
        mu <- a +  bG*groupsize + bA*area,
        a ~ dnorm(0, 0.2),
        bG ~ dnorm(0, 0.5),
        bA ~ dnorm(0, 0.5),
        sigma ~ dexp(1)), data = foxes)

#avgfood
model4 <-  quap(
    alist(
        weight ~ dnorm(mu, sigma),
        mu <- a + bF*avgfood,
        a ~ dnorm(0, 0.2),
        bF ~ dnorm(0, 0.5),
        sigma ~ dexp(1)), data = foxes)

#area
model5 <- quap(
    alist(
        weight ~ dnorm(mu, sigma),
        mu <- a + bA*area,
        a ~ dnorm(0, 0.2),
        bA ~ dnorm(0, 0.5),
        sigma ~ dexp(1)), data = foxes)

#comparing the different models
compare(model1, model2, model3, model4, model5) #Models 1,2,3 have about the same evidence, models 4 and 5 are a little worse but still within 2 SE.
plot(compare(model1, model2, model3, model4, model5))

#Consider the DAG we were given about the foxes
library(dagitty)
fox_dag <- dagitty("dag{ area -> avgfood -> groupsize -> weight <- avgfood }")
drawdag(fox_dag)

#Models 1 to 3 are nearly identical, since they contain groupsize and one both avgfood or area. From the DAG, the effect of area goes through avgfood so including one or the other in the model is enough

#Models 4 and 5 are nearly identical because adjusting for area or avgfood should be the same, these models are missing groupsize

### * Chapter 8

#bookmark 8.1

data(rugged)
d <- rugged
# make log version of outcome
d$log_gdp <- log( d$rgdppc_2000 )
# extract countries with GDP data
dd <- d[ complete.cases(d$rgdppc_2000) , ]
# rescale variables
dd$log_gdp_std <- dd$log_gdp / mean(dd$log_gdp)
dd$rugged_std <- dd$rugged / max(dd$rugged)

m8.1 <- quap(
alist(
log_gdp_std ~ dnorm( mu , sigma ) ,
mu <- a + b*( rugged_std - 0.215 ) ,
a ~ dnorm( 1 , 0.1 ) ,
b ~ dnorm( 0 , 0.3 ) ,
sigma ~ dexp(1)
) , data=dd )

precis(m8.1)

# make variable to index Africa (1) or not (2)
dd$cid <- ifelse( dd$cont_africa==1 , 1 , 2 )

m8.2 <- quap(
alist(
log_gdp_std ~ dnorm( mu , sigma ) ,
mu <- a[cid] + b*( rugged_std - 0.215 ) ,
a[cid] ~ dnorm( 1 , 0.1 ) ,
b ~ dnorm( 0 , 0.3 ) ,
sigma ~ dexp( 1 )
) , data=dd, start = list(a = c(1,1), b = 0 ))

compare( m8.1 , m8.2 )

precis( m8.2 , depth=2 )

post <- extract.samples(m8.2)
diff_a1_a2 <- post$a[,1] - post$a[,2]
PI( diff_a1_a2 )

m8.3 <- quap(
alist(
log_gdp_std ~ dnorm( mu, sigma),
mu <- a[cid] + b[cid]*(rugged_std - 0.215),
a[cid] ~ dnorm( 1 , 0.1),
b[cid] ~ dnorm( 0 , 0.3),
sigma ~ dexp( 1 )
) , data=dd )

compare( m8.1 , m8.2 , m8.3 , func=PSIS )

plot( PSIS( m8.3 , pointwise=TRUE )$k )

data(tulips)
d <- tulips
str(d)

d$blooms_std <- d$blooms / max(d$blooms)
d$water_cent <- d$water - mean(d$water)
d$shade_cent <- d$shade - mean(d$shade)

m8.5 <- quap(
alist(
blooms_std ~ dnorm( mu , sigma ) ,
mu <- a + bw*water_cent + bs*shade_cent + bws*water_cent*shade_cent ,
a ~ dnorm( 0.5 , 0.25 ) ,
bw ~ dnorm( 0 , 0.25 ) ,
bs ~ dnorm( 0 , 0.25 ) ,
bws ~ dnorm( 0 , 0.25 ) ,
sigma ~ dexp( 1 )
) , data=d )

##bookmark 8.5

### ** Practice

### *** Difficulty level: Easy

#8E1
#1) temperature
#2) field of study
#3) presence or absence of wheels

#8E2
#Interactions are invoked in statements: 1, (3)

#8E3
# C = a + Bh*heat + Bl*liquid + Bhl*heat*liquid, where C = degree of caramelization
# S = a[fi] + Bc*cylinders, where S = speed, fi denotes index for different fuel injectors
# P = a*parents + B*friends, where P = political belief, a = beliefs of parents, B = beliefs of friends, friends = indicators whether person has friends (could also be an interaction)
# I = a + B1*sociality + B2*appendages

### *** Difficulty level: Medium 

#8M1

#This means a three way interaction between water, shade, and temperature

#8M2

#mu = (a + Bw*W + Bs*S + Bws*W*S)*T, where is an indicator variable T = {0, 1}

#8M3
#For example raven population size could depend on deer population size and presence or absence of wolves.

### *** Difficulty level: Hard
#8H1
data(tulips)
d <- tulips
str(d)

#Centering water and shade variables and making an index variable of bed
d$blooms_std <- d$blooms / max(d$blooms)
d$shade.c <- d$shade - mean(d$shade)
d$water.c <- d$water - mean(d$water)
d$bed.ind <- coerce_index(d$bed)

#Modelling effect of bed (block) as a unique intercept for the different flower beds

#Start values for parameters added
#Model with bed
m1 <- quap(
alist(
blooms_std~ dnorm(mu , sigma),
mu <- a[bed.ind] + bW*water.c + bS*shade.c + bWS*water.c*shade.c,
a ~ dnorm(0, 0.25),
bW ~ dnorm(0, 0.25),
bS ~ dnorm(0, 0.25),
bWS ~ dnorm(0, 0.25),    
sigma ~ dexp(1)
),
data=d,
start=list(a= rep(0,3),bW=0,bS=0, bWS=0))

precis(m1, depth = 2)

post <- extract.samples(m1)
diff_a1_a2 <- post$a[,1] - post$a[,2]
PI(diff_a1_a2) #bed level 1 is different from the other two



#8H2
#Model without flowerbed
m2 <- quap(
alist(
blooms_std~ dnorm(mu , sigma),
mu <- a + bW*water.c + bS*shade.c + bWS*water.c*shade.c,
a ~ dnorm(0, 0.25),
bW ~ dnorm(0, 0.25),
bS ~ dnorm(0, 0.25),
bWS ~ dnorm(0, 0.25),    
sigma ~ dexp(1)
),
data=d,
start=list(a= 0,bW=0,bS=0, bWS=0))

precis(m2, depth = 2)

compare(m1, m2) #Model with bed included receives 68% of the weight, but difference to m2 is very small (less than 1 standard error)
plot(coeftab(m1, m2))

#Probably drop one of the bed levels?

#8H3
data(rugged)
d <- rugged
# make log version of outcome
d$log_gdp <- log( d$rgdppc_2000 )
# extract countries with GDP data
dd <- d[ complete.cases(d$rgdppc_2000) , ]
# rescale variables
dd$log_gdp_std <- dd$log_gdp / mean(dd$log_gdp)
dd$rugged_std <- dd$rugged / max(dd$rugged)
# make variable to index Africa (1) or not (2)
dd$cid <- ifelse( dd$cont_africa==1 , 1 , 2 )

m8.3 <- quap(
alist(
log_gdp_std ~ dnorm(mu, sigma),
mu <- a[cid] + b[cid]*(rugged_std - 0.215),
a[cid] ~ dnorm(1, 0.1),
b[cid] ~ dnorm(0, 0.3),
sigma ~ dexp(1)
) , data=dd )

precis(m8.3, depth = 2)

# plot Africa - cid=1
rugged_seq <- seq( from=-0.1 , to=1.1 , length.out=30 )
d.A1 <- dd[ dd$cid==1 , ]
plot( d.A1$rugged_std , d.A1$log_gdp_std , pch=16 , col=rangi2 ,
xlab="ruggedness (standardized)" , ylab="log GDP (as proportion of mean)" ,
xlim=c(0,1) )

mu <- link( m8.3 , data=data.frame( cid=1 , rugged_std=rugged_seq ) )
mu_mean <- apply( mu , 2 , mean )
mu_ci <- apply( mu , 2 , PI , prob=0.97 )
lines( rugged_seq , mu_mean , lwd=2 )
shade( mu_ci , rugged_seq , col=col.alpha(rangi2,0.3) )
mtext("African nations")

# plot non-Africa - cid=2
d.A0 <- dd[ dd$cid==2 , ]
plot( d.A0$rugged_std , d.A0$log_gdp_std , pch=1 , col="black" ,
xlab="ruggedness (standardized)" , ylab="log GDP (as proportion of mean)" ,
xlim=c(0,1) )
mu <- link( m8.3 , data=data.frame( cid=2 , rugged_std=rugged_seq ) )
mu_mean <- apply( mu , 2 , mean )
mu_ci <- apply( mu , 2 , PI , prob=0.97 )
lines( rugged_seq , mu_mean , lwd=2 )
shade( mu_ci , rugged_seq )
mtext("Non-African nations")

waic_m8.3 <- WAIC(m8.3, pointwise = TRUE)
psis_m8.3 <- PSIS(m8.3, pointwise = TRUE)
plot(psis_m8.3$k, waic_m8.3$penalty, xlab="PSIS Pareto k", ylab="WAIC penalty", col=rangi2, lwd=2)
abline(v = 0.5, lty = "dashed")

dd[which(psis_m8.3$k > 0.5),"country"] #Lesotho is an outlier, high ruggedness, quite small GDP
dd[which(waic_m8.3$penalty == max(waic_m8.3$penalty)),"country"] #Seuchelles has high WAIC penalty

##Model with robust regression
m8.3r <- quap(
alist(
log_gdp_std ~ dstudent(2, mu, sigma),
mu <- a[cid] + b[cid]*(rugged_std - 0.215),
a[cid] ~ dnorm(1, 0.1),
b[cid] ~ dnorm(0, 0.3),
sigma ~ dexp(1)
) , data=dd )

precis(m8.3, depth = 2)

plot(coeftab(m8.3, m8.3r)) #Conclusions do not really change when using robust regression

waic_m8.3r <- WAIC(m8.3r, pointwise = TRUE)
psis_m8.3r <- PSIS(m8.3r, pointwise = TRUE)
plot(psis_m8.3r$k, waic_m8.3r$penalty, xlab="PSIS Pareto k", ylab="WAIC penalty", col=rangi2, lwd=2)
abline(v = 0.5, lty = "dashed") #Much better 

#8H4
data(nettle)
d <- nettle
d$lang.per.cap <- d$num.lang / d$k.pop
d$log_langpc <- scale(log(d$lang.per.cap))
d$scaled.growseason <- scale(d$mean.growing.season)
d$scaled.sd.growseason <- scale(d$sd.growing.season)
d$scaled.log.area <- scale(log(d$area))

#A)
m1 <- quap(
    alist(
        log_langpc ~ dnorm(mu, sigma),
        mu <- a + B*scaled.growseason + Ba*scaled.log.area,
        a ~ dnorm(0, 0.2),
        B ~ dnorm(0, 0.5),
        Ba ~ dnorm(0, 0.5),
        sigma ~ dexp(1)),
    data = d)

precis(m1) #Language diversity is positively associated with growing season

m1.2 <- quap(
    alist(
        log_langpc ~ dnorm(mu, sigma),
        mu <- a + B*scaled.growseason,
        a ~ dnorm(0, 0.2),
        B ~ dnorm(0, 0.5),
        sigma ~ dexp(1)),
    data = d)

compare(m1, m1.2)
plot(coeftab(m1, m1.2))


#B)
m2 <- quap(
    alist(
        log_langpc ~ dnorm(mu, sigma),
        mu <- a + B*scaled.sd.growseason + Ba*scaled.log.area,
        a ~ dnorm(0, 0.2),
        B ~ dnorm(0, 0.5),
        Ba ~ dnorm(0, 0.5),
        sigma ~ dexp(1)),
    data = d)

m2.2 <- quap(
    alist(
        log_langpc ~ dnorm(mu, sigma),
        mu <- a + B*scaled.sd.growseason,
        a ~ dnorm(0, 0.2),
        B ~ dnorm(0, 0.5),
        sigma ~ dexp(1)),
    data = d)

precis(m2) #SD of growing season is negatively associated with language diversity, but estimate of B does overlap with 0

compare(m2, m2.2)
plot(coeftab(m2, m2.2))

#C) Mean and SD of growing season interact to reduce language diversity (if mean longer then high SD should reduce lang. div. more)
m3 <- quap(
    alist(
        log_langpc ~ dnorm(mu, sigma),
        mu <- a + Bm*scaled.growseason + Bs*scaled.sd.growseason + Bms*scaled.growseason*scaled.sd.growseason +  Ba*scaled.log.area,
        a ~ dnorm(0, 0.2),
        Bm ~ dnorm(0, 0.5),
        Bs ~ dnorm(0, 0.5),
        Bms ~ dnorm(0, 0.5),
        Ba ~ dnorm(0, 0.5),
        sigma ~ dexp(1)),
    data = d)

precis(m3) #Interaction is negative, the effect of area disappears in this model

m3.1 <- quap(
    alist(
        log_langpc ~ dnorm(mu, sigma),
        mu <- a + Bm*scaled.growseason + Bs*scaled.sd.growseason + Bms*scaled.growseason*scaled.sd.growseason,
        a ~ dnorm(0, 0.2),
        Bm ~ dnorm(0, 0.5),
        Bs ~ dnorm(0, 0.5),
        Bms ~ dnorm(0, 0.5),
        sigma ~ dexp(1)),
    data = d)

compare(m3, m3.1) #Model without area give better predictions
plot(coeftab(m3, m3.1)) #Although coefficients don't really change

precis(m3.1)

#Make a plot to interpret the continuous interaction

sd_seq <- seq(from = -2, to = 2, length.out = 30)

##Making a triptyrch plot for the continuous interaction
par(mfrow=c(1,3))
#For mean growing season = -1
mu1 <- link(m3.1, data = data.frame(scaled.sd.growseason = sd_seq, scaled.growseason = -1))
mu1_mean <- apply(mu1, 2, mean)
mu1_PI <- apply(mu1, 2, PI)
plot(log_langpc ~ scaled.sd.growseason, type = "none", xlim = c(-2,2), main = "mean growing season = -1", data = d)
lines(sd_seq, mu1_mean, lwd = 2)
shade(mu1_PI, sd_seq)
#For mean growing season = 0
mu2 <- link(m3.1, data = data.frame(scaled.sd.growseason = sd_seq, scaled.growseason = 0))
mu2_mean <- apply(mu2, 2, mean)
mu2_PI <- apply(mu2, 2, PI)
plot(log_langpc ~ scaled.sd.growseason, type = "none", xlim = c(-2,2), main = "mean growing season = 0", data = d)
lines(sd_seq, mu2_mean, lwd = 2)
shade(mu2_PI, sd_seq)
#For mean growing season = 1
mu3 <- link(m3.1, data = data.frame(scaled.sd.growseason = sd_seq, scaled.growseason = 1))
mu3_mean <- apply(mu3, 2, mean)
mu3_PI <- apply(mu3, 2, PI)
plot(log_langpc ~ scaled.sd.growseason, type = "none", xlim = c(-2,2), main = "mean growing season = 1", data = d)
lines(sd_seq, mu3_mean, lwd = 2)
shade(mu3_PI, sd_seq)

#We can see from the plot that as the mean growing season gets longer, the slope of sd growing decreases. With short mean growing seasons, the variability of growing season does not have an effect, but with longer mean growing seasons, increasing variability decreases language diversity.
