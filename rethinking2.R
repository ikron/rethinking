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
