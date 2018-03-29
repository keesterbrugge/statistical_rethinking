# E

# 1

# 2
                          
# p(u,s|y) = prod_i ( Normal(y_i|u,s) * Normal(u|0,10) * Uniform(s|0,10) ) / int_u int_s prod_i ( Normal(y_i|u,s) * Normal(u|0,10) * Uniform(s|0,10) ) du ds          

# 2

# 3


# M

mu_sample <- rnorm(n = 1e4, mean = 0, sd = 10)
sigma_sample <- runif(n = 1e4, min = 0, max = 10)
y_sample <- rnorm(n = 1e4, mean = mu_sample, sd = sigma_sample)

library(rethinking)
flist4m2 <- alist(
  y ~ dnorm(mu, sigma),
  mu ~ dnorm(0,10),
  sigma ~ dunif(0,10)
)

# look at 4e4

flist4m4 <- alist(
  y ~ dnorm(mu, sigma),
  mu = a + b*year,
  a ~ dnorm(130,50), #nonzero. at year 12 a child is around 130 cm. very weak prior
  b ~ dnorm(9, 5), # a child grows from 20 cm to around 1.80 in 18 years, hence a bit less than 10 cm a year. 
  sigma ~ dunif(0,20) # must be nonzero, bigger max because great varaiblilty in children
)

# a ~ dnorm(130,50),  --> a ~ dnorm(120,20),
# more informative prior and shifted center of mass. I already I included my knowledge that kids don't get smaller by choosing positive mean for b. 

# sigma = sqrt(64) = 8. Hence sigma always lower than 8. This informs me too use following prior:  sigma ~ dunif(0,20) --> sigma ~ dunif(0,10)

# H
# 1
library(rethinking)
data(Howell1)
d <- Howell1
d2 <- d[ d$age >= 18 , ]

mymodel <- map(
  alist(
    height ~ dnorm( mu , sigma ) ,
    mu <- a + b*weight ,
    a ~ dnorm( 178 , 100 ) ,
    b ~ dnorm( 0 , 10 ) ,
    sigma ~ dunif( 0 , 50 )
  ) , data=d2 )

## R code 4.49
# extract 20 samples from the posterior
post <- extract.samples( mymodel , n=1e4 )


for (w in list(46.95, 43.72, 64.78, 32.59, 54.63)){
  print(precis(post$a + post$b * w))
}
# ## R code 4.50
# mu_at_50 <- post$a + post$b * 50
# 
# ## R code 4.51
# dens( mu_at_50 , col=rangi2 , lwd=2 , xlab="mu|weight=50" )
# 
# ## R code 4.52
# HPDI( mu_at_50 , prob=0.89 )

d_young <- d[ d$age < 18 , ]

m_young <- map(
  alist(
    height ~ dnorm( mu , sigma ) ,
    mu <- a + b*weight ,
    a ~ dnorm( 178 , 100 ) ,
    b ~ dnorm( 0 , 10 ) ,
    sigma ~ dunif( 0 , 50 )
  ) , data=d_young )

precis(m_young)

# it predicts 10 increase in weight rsults in 27.2 increase in height. 

post_young <- extract.samples(m_young, n = 1e4)
mu.link <- function(weight) post_young$a + post_young$b*weight
weight.seq <- seq( from=4 , to=45 , by=1 )
mu <- sapply( weight.seq , mu.link )
mu.mean <- apply( mu , 2 , mean )
mu.HPDI <- apply( mu , 2 , HPDI , prob=0.89 )

## R code 4.59
sim.height <- sim( m_young , data=list(weight=weight.seq) )
str(sim.height)

## R code 4.60
height.PI <- apply( sim.height , 2 , PI , prob=0.89 )

## R code 4.61
# plot raw data
plot( height ~ weight , d_young , col=col.alpha(rangi2,0.5) )

# draw MAP line
lines( weight.seq , mu.mean )

# draw HPDI region for line
shade( mu.HPDI , weight.seq )

# draw PI region for simulated heights
shade( height.PI , weight.seq )

# The model appears to capture the curvature poorly. As weight increases there is a decreasing marginal added weight. Not additive. Perhaps multiplicative 
# with constant to the power parameter, where the parameter between 0 and 1.
# We could also make it piecewise linear. 

# h3
d_young[["log_weight"]] = sapply(d_young[["weight"]], log)

m_young2 <- map(
  alist(
    height ~ dnorm( mu , sigma ) ,
    mu <- a + b*log_weight ,
    a ~ dnorm( 178 , 100 ) ,
    b ~ dnorm( 0 , 100 ) ,
    sigma ~ dunif( 0 , 50 )
  ) , data=d_young )

precis(m_young2,corr = TRUE)

# it predicts 10 increase in weight rsults in 27.2 increase in height. 

post_young2 <- extract.samples(m_young2, n = 1e4)
mu.link2 <- function(log_weight) post_young2$a + post_young2$b*log_weight
weight.seq2 <- seq( from=0 , to=8 , by=.1 )
mu2 <- sapply( weight.seq2 , mu.link2 )
mu.mean2 <- apply( mu2 , 2 , mean )
mu.HPDI2 <- apply( mu2 , 2 , HPDI , prob=0.89 )

## R code 4.59
sim.height2 <- sim( m_young2 , data=list(log_weight=weight.seq2) )
str(sim.height2)

## R code 4.60
height.PI2 <- apply( sim.height2 , 2 , PI , prob=0.97 )

## R code 4.61
# plot raw data
plot( height ~ log_weight , d_young , col=col.alpha(rangi2,0.5) )

# draw MAP line
lines( weight.seq2 , mu.mean2 )

# draw HPDI region for line
shade( mu.HPDI2 , weight.seq2 )

# draw PI region for simulated heights
shade( height.PI2 , weight.seq2 )

# --------------------------
d[["log_weight"]] = sapply(d[["weight"]], log)

m<- map(
  alist(
    height ~ dnorm( mu , sigma ) ,
    mu <- a + b*log_weight ,
    a ~ dnorm( 178 , 100 ) ,
    b ~ dnorm( 0 , 100 ) ,
    sigma ~ dunif( 0 , 50 )
  ) , data=d )

precis(m,corr = TRUE)

# it predicts 10 increase in weight rsults in 27.2 increase in height. 

post <- extract.samples(m, n = 1e4)
mu.link <- function(log_weight) post$a + post$b*log_weight
weight.seq <- seq( from=0 , to=8 , by=.1 )
mu <- sapply( weight.seq , mu.link )
mu.mean <- apply( mu , 2 , mean )
mu.HPDI <- apply( mu , 2 , HPDI , prob=0.89 )

## R code 4.59
sim.height <- sim( m, data=list(log_weight=weight.seq) )
str(sim.height)

## R code 4.60
height.PI <- apply( sim.height , 2 , PI , prob=0.97 )

## R code 4.61
# plot raw data
plot( height ~ log_weight , d , col=col.alpha(rangi2,0.5) )

# draw MAP line
lines( weight.seq , mu.mean )

# draw HPDI region for line
shade( mu.HPDI , weight.seq)

# draw PI region for simulated heights
shade( height.PI , weight.seq )

#interpretation: a doubling of weight results in increase of 47 in height. 