# Suppose we have two students taking a 40-question 
# multiple-choice exam.
# We think they will do better than chance.

# 1. What are our parameters of interest? 

# Ad 1: Theta 1 and thata 2 - probailities that a given student gets a question right


# 2. what is our likelihood?

# It's Binom(40, theta), assuming that questions are independent and 
# probabilities of answers are the same for each question


# 3. What prior should we use? 
# Beta, as it's a conjucate prior for binom.
theta = seq(0, 1, .1)
plot(theta, dbeta(theta, 8, 4), type = 'l')
# beta(8, 4) gives a reasonable model of doing better than chance (P > .25)
prior_alpha = 8
prior_beta  = 4

# 4. What's the prior probability that theta > .25, > .5, > .8?
1 - pbeta(.25, prior_alpha, prior_beta)
1 - pbeta( .5, prior_alpha, prior_beta)
1 - pbeta( .8, prior_alpha, prior_beta)

# 5. Suppose the first student takes the test and gets 33 of the 40 questions right.
#    What's the postrior distribution for their parameter? 
#    What's P(theta > .25), > .5,  > .8?
#    What's a 95% credible interval for it? 
post_alpha_1 = prior_alpha + 33
post_beta_1  = prior_beta  + 7

post_mean_1  = post_alpha_1 / (post_alpha_1 + post_beta_1)
cat('Posterior mean:', post_mean_1)
cat('MLE:', 33/40)

1 - pbeta(.25, post_alpha_1, post_beta_1)
1 - pbeta( .5, post_alpha_1, post_beta_1)
1 - pbeta( .8, post_alpha_1, post_beta_1)

cat('95% CI:', qbeta(.025, post_alpha_1, post_beta_1), 
               qbeta(.975, post_alpha_1, post_beta_1))

# 6. Suppose the 2nd student gets 24 questions right.
#    What's the distribution for their parameter?
#    What's P(theta > .25), > .5, > .8?
#    What's a 95% credible interval for it? 
post_alpha_2 = prior_alpha + 24
post_beta_2  = prior_beta  + 40 - 24

post_mean_2  = post_alpha_2 / (post_alpha_2 + post_beta_2)
cat('Posterior mean:', post_mean_2)
cat('MLE:', 24/40)

1 - pbeta(.25, post_alpha_2, post_beta_2)
1 - pbeta( .5, post_alpha_2, post_beta_2)
1 - pbeta( .8, post_alpha_2, post_beta_2)

cat('95% CI:', qbeta(.025, post_alpha_2, post_beta_2), 
               qbeta(.975, post_alpha_2, post_beta_2))


# 7) Estimate by simulation: draw 1,000 samples from each and see how often 
#    we observe theta1 > theta2

theta_1 = rbeta(1000,41,11)
theta_2 = rbeta(1000,32,20)
mean(theta_1 > theta_2)

