library(tidyverse)

# 2e1 2,4 
# 2e2 3
# 2e3 1,4
# 2e4 Given the model and the data I estimte that 70 percent of the world is covered with water

# 2m1 
n = 7
w = 5 
# define grid
p_grid <- seq( from=0 , to=1 , length.out=200 )

# define prior
prior <- rep( 1 , 200 )

# compute likelihood at each value in grid
likelihood <- dbinom( w , size=n , prob=p_grid )

# compute product of likelihood and prior
unstd.posterior <- likelihood * prior

# standardize the posterior, so it sums to 1
posterior <- unstd.posterior / sum(unstd.posterior)

## R code 2.4
plot( p_grid , posterior , type="b" ,
      xlab="probability of water" , ylab="posterior probability" )


# 2m2
n = 7
w = 5 
n_point = 200
# define grid
p_grid <- seq( from=0 , to=1 , length.out=n_points )

# define prior
prior <- if_else(p_grid < 0.5, 0, 1)

# compute likelihood at each value in grid
likelihood <- dbinom( w , size=n , prob=p_grid )

# compute product of likelihood and prior
unstd.posterior <- likelihood * prior

# standardize the posterior, so it sums to 1
posterior <- unstd.posterior / sum(unstd.posterior)

## R code 2.4
plot( p_grid , posterior , type="b" ,
      xlab="probability of water" , ylab="posterior probability" )

# 2m3
dbinom( 0 , size=1 , prob=0.7 )/ ( dbinom( 0 , size=1 , prob=0 ) + dbinom( 0 , size=1 , prob=0.7 ))

#2m4
# so there are three cards. The question is: which card is lying on the table?
# We've got bb, bw, ww. 
# We have no prior info.
# We sample without replacement.
# We observe data b. bw can create this in 1 way. bb can create this in 2 ways. This would mean there is a chance of 2/3 that card is bb hence other side also b. 
#

#2m5
# 4/5. 

#2m6
# prior weight for bb, bw, ww is 1, 2, 3, respectively. p(b|bb) * 1/ ( p(b|bb) * 1 + p(b|bw) * 2) = 2/(2+2) = .5

# 2m7
# say c1 = bb. Then 2 ways to observe s1(c1) = b. Then 2 ways to observe s1(c2) = w if c2 = ww and 1 ways to observe s1(c2) = w if c2 = bw
# say c1 = bw. Then 1 ways to observe s1(c1) = b. Then 2 ways to observe s1(c2) = w if c2 = ww .
# 2*(2+1)/(2*(2+1) + 1*2) = 0.75

# 2h1
# 0.17
# You first update your belief about species of panda. After observing data = twin , Pr(species = A | data = twin) = 1/3. This becomes your new prior.
# Then P(twin) = p(twin|A) * p(A) + p(twin|B) * p(B) = .1 * 1/3 + .2 * 2/3 = .17
# or without likelihood 

# 2h2
# 10*1 / (10*1+20*1)
# P(A|twin) = p(twin|a) * p(a)/( p(twin|b) * p(b) + p(twin|b) * p(b)) = .1 * .5 / (.1 * .5 + .2 * .5)

# 2h3
# order is unimportant
# P(A|S) = P(S|A) * P(A|T)/ (P(S|A) * P(A|T) + P(S|B) * P(B|T)) = 0.9 * 1/3 / (0.9 * 1/3  + 0.8 * 2/3) 
# How do I use better notation for posterior that is being used for prior after?
# P(A|TS) = P(TS|A) * P(A)/ (P(TS|A) * P(A) + P(TS|B) * P(B)) = 0.9*.1 * .5 / (0.9*.1 * .5  + 0.8*.2 * .5) 


#2h4
# p(a|identify as A) = .8 * .5 / (.8 * .5 + .35 * .5 )= .8/1.15
# 8/1.15 * P(A|TS) / ( 8/1.15 * P(A|TS) + (1 - 8/1.15) * P(B|TS))

