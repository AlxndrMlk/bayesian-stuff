# Your friend investigates the three observations above 700 grams and discovers that she had ordered 
# the incorrect meal on those dates. 
# She removes these observations from the data set and proceeds with the analysis using n=27.
# 
# She assumes a normal likelihood for the data with unknown mean μ and unknown variance σ2. 
# She uses the model presented in Lesson 10.2 where, conditional on σ2, 
# the prior for μ is normal with mean mmm and variance σ2/w. 

# Next, the marginal prior for σ2 is Inverse-Gamma(a,b).
# 
# Your friend's prior guess on the mean dish weight is 500 grams, 
# so we set m=500. She is not very confident with this guess, 
# so we set the prior effective sample size w=0.1. Finally, she sets a=3 and b=200.
# 
# We can learn more about this inverse-gamma prior by simulating draws from it. If a random variable 
# X follows a Gamma(a,b) distribution, then 1/X follows an Inverse-Gamma(a,b) distribution. 
# Hence, we can simulate draws from a gamma distribution and take their reciprocals, 
# which will be draws from an inverse-gamma.


# Simulate a large number of draws (at least 300) from the prior for σ2
# and report your approximate prior mean from these draws. It does not need to be exact.

m = 500
w = .1
alpha = 3
beta  = 200

n = 27

x_mean = 609.7
x_var  = 401.8

alpha_prim = 16.5
beta_prim  = 6022.9
m_prim     = 609.3


prior_var_1 = 1 / rgamma(10000, alpha, beta)

posterior_var_1  = 1 / rgamma(10000, alpha_prim, beta_prim)

posterior_mean_1 = rnorm(10000, m_prim, sqrt((posterior_var_1 / (w + n))))

quantile(posterior_mean_1, c(.025, .975))

# You complete your experiment at Restaurant A with n=30 data points, 
# which appear to be normally distributed. 
# You calculate the sample mean y¯= 622.8 and sample variance s2 = 403.1.

# Repeat the analysis from Question 9 using the same priors and draw samples from the 
# posterior distribution of sigma_A^2 and μA (where the A denotes that these parameters are for Restaurant A).

# Treating the data from Restaurant A as independent from Restaurant B, 
# we can now attempt to answer your friend's original question: 
# is restaurant A more generous? 

# To do so, we can compute posterior probabilities of hypotheses 
# like μA > μB. 
# This is a simple task if we have simulated draws for μA and μB. 
# For i=1,…,N (the number of simulations drawn for each parameter), 
# make the comparison μA > μB using the i-th draw for μA and μB. 
# Then count how many of these return a TRUE value and divide by N, the total number of simulations.

n_A = 30

x_mean_A = 622.8
x_var_A  = 403.1

alpha_prim_A = alpha + (n_A / 2)
beta_prim_A  = beta + ((n_A - 1) / 2) * x_var_A + ((w * n_A) / (2 * (w + n_A))) * (x_mean_A - m)**2
m_prim_A     = (n_A * x_mean_A + w * m) / (w + n_A)


prior_var_A = 1 / rgamma(10000, alpha, beta)

posterior_var_A  = 1 / rgamma(10000, alpha_prim_A, beta_prim_A)

posterior_mean_A = rnorm(10000, m_prim_A, sqrt((posterior_var_A / (w + n_A))))


sum(posterior_mean_A > posterior_mean_1) / 10000
