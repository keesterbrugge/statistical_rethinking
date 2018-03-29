libary(rethinking)

## R code 3.27
p_grid <- seq( from=0 , to=1 , length.out=1000 )
prior <- rep( 1 , 1000 )
likelihood <- dbinom( 6 , size=9 , prob=p_grid )
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)
set.seed(100)
samples <- sample( p_grid , prob=posterior , size=1e4 , replace=TRUE )

# E
sum(samples < 0.2)/1e4

mean(samples > 0.8)

mean(0.2 < samples  & samples < 0.8)

quantile(samples, 0.2)
mean( samples < 0.52)

quantile( samples, 0.8)

HPDI(samples, .66)

PI(samples, .66)

# M

p_grid <- seq( from=0 , to=1 , length.out=1000 )
prior <- rep( 1 , 1000 )
likelihood <- dbinom( 8 , size=15 , prob=p_grid )
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)


set.seed(100)
samples <- sample( p_grid , prob=posterior , size=1e5 , replace=TRUE )
HPDI(samples, .9)

w <- rbinom(1e5, size = 15, prob = samples)
mean(w == 8)

w <- rbinom(1e5, size = 9, prob = samples)
mean(w == 6)

#--
p_grid <- seq( from=0 , to=1 , length.out=1000 )
prior <- ifelse(p_grid < 0.5, 0, 1)
likelihood <- dbinom( 8 , size=15 , prob=p_grid )
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)


set.seed(100)
samples <- sample( p_grid , prob=posterior , size=1e5 , replace=TRUE )
HPDI(samples, .9)

w <- rbinom(1e5, size = 15, prob = samples)
mean(w == 8)

w <- rbinom(1e5, size = 9, prob = samples)
mean(w == 6)

# much narrower hdpi. Even end of interval is lower than with uniform prior.
# the probabilitie of observing 8/15 and 6/9 are both higher. 
dbinom(size = 9, x = 6, prob = 0.7)
dbinom(size = 15, x = 8, prob = 0.7)
# even though "true" probability of obsering 8 / 15 is lower than both estimats. 


# H

data(homeworkch3)
sum(birth1) + sum(birth2)

p_grid <- seq( from=0 , to=1 , length.out=1000 )
prior <- rep( 1 , 1000 )
likelihood <- dbinom( x = 111 , size = 200, prob=p_grid )
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)
p_grid [ which.max ( posterior ) ]
 
samples <- sample( x = p_grid, size = 1e5, replace = TRUE, prob = posterior)
quantile(samples, c(.5, .89, .97))

births_sample <- rbinom(length(samples), size = 200, prob = samples)
dens(births_sample)
simplehist(births_sample)
# yes, model fits data (111 boys) well. In middle and most likely value

sum(birth1)
births_sample <- rbinom(length(samples), size = 100, prob = samples)
simplehist(births_sample)
mean(births_sample < 51)
# the model doesn't fit the data so well. There are less first born boys than predicted by the posterior based on first and second births.

sample_second_boys <- rbinom(size = 100 - sum(birth1), n = length(samples), prob = samples)
simplehist(sample_second_boys)
sum(birth2[birth1 == 0])
mean(sample_second_boys > 39)
# it is highly unlikely that out of 49 second born where first born is girl, there are 39 or more boys if gender of first and second birth is independent. 