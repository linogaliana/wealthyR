# https://notes.quantecon.org/submission/5b3db2ceb9eab00015b89f93

# Import packages and load the data
import numpy as np
import numpy.random as rnd
import numpy.linalg as lin
import scipy.stats as sts
import scipy.integrate as intgr
import scipy.optimize as opt
import matplotlib
import matplotlib.pyplot as plt
import requests
from mpl_toolkits.mplot3d import Axes3D
from matplotlib import cm
cmap1 = matplotlib.cm.get_cmap('summer')

url = ('https://raw.githubusercontent.com/rickecon/Notebooks/' +
       'master/SMM/data/Econ381totpts.txt')
data_file = requests.get(url, allow_redirects=True)
open('Econ381totpts.txt', 'wb').write(data_file.content)

pts = np.loadtxt('Econ381totpts.txt')

# Define function that gives PDF values from truncated normal distribution
def trunc_norm_pdf(xvals, mu, sigma, cut_lb, cut_ub):
    '''
    --------------------------------------------------------------------
    Generate PDF values from a truncated normal distribution based on a
    normal distribution with mean mu and standard deviation sigma and
    cutoffs (cut_lb, cut_ub).
    --------------------------------------------------------------------
    INPUTS:
    xvals  = (N, S) matrix, (N,) vector, or scalar in (cut_lb, cut_ub),
             value(s) in the support of s~TN(mu, sig, cut_lb, cut_ub)
    mu     = scalar, mean of the nontruncated normal distribution from
             which the truncated normal is derived
    sigma  = scalar > 0, standard deviation of the nontruncated normal
             distribution from which the truncated normal is derived
    cut_lb = scalar or string, ='None' if no lower bound cutoff is
             given, otherwise is scalar lower bound value of
             distribution. Values below this cutoff have zero
             probability
    cut_ub = scalar or string, ='None' if no upper bound cutoff is given
             given, otherwise is scalar lower bound value of
             distribution. Values below this cutoff have zero
             probability
    
    OTHER FUNCTIONS AND FILES CALLED BY THIS FUNCTION:
        scipy.stats.norm()
    
    OBJECTS CREATED WITHIN FUNCTION:
    cut_ub_cdf = scalar in [0, 1], cdf of N(mu, sigma) at upper bound
                 cutoff of truncated normal distribution
    cut_lb_cdf = scalar in [0, 1], cdf of N(mu, sigma) at lower bound
                 cutoff of truncated normal distribution
    unif2_vals = (N, S) matrix, (N,) vector, or scalar in (0,1),
                 rescaled uniform derived from original.
    pdf_vals   = (N, S) matrix, (N,) vector, or scalar in (0,1), PDF
                 values corresponding to xvals from truncated normal PDF
                 with base normal normal distribution N(mu, sigma) and
                 cutoffs (cut_lb, cut_ub)
    
    FILES CREATED BY THIS FUNCTION: None
    
    RETURNS: pdf_vals
    --------------------------------------------------------------------
    '''
    # No cutoffs: truncated normal = normal
    if (cut_lb == None) & (cut_ub == None):
        cut_ub_cdf = 1.0
        cut_lb_cdf = 0.0
    # Lower bound truncation, no upper bound truncation
    elif (cut_lb != None) & (cut_ub == None):
        cut_ub_cdf = 1.0
        cut_lb_cdf = sts.norm.cdf(cut_lb, loc=mu, scale=sigma)
    # Upper bound truncation, no lower bound truncation
    elif (cut_lb == None) & (cut_ub != None):
        cut_ub_cdf = sts.norm.cdf(cut_ub, loc=mu, scale=sigma)
        cut_lb_cdf = 0.0
    # Lower bound and upper bound truncation
    elif (cut_lb != None) & (cut_ub != None):
        cut_ub_cdf = sts.norm.cdf(cut_ub, loc=mu, scale=sigma)
        cut_lb_cdf = sts.norm.cdf(cut_lb, loc=mu, scale=sigma)
    
    pdf_vals = (sts.norm.pdf(xvals, loc=mu, scale=sigma) /
                (cut_ub_cdf - cut_lb_cdf))
    
    return pdf_vals
    

# Define function that draws N x S test score values from a truncated
# normal distribution
def trunc_norm_draws(unif_vals, mu, sigma, cut_lb, cut_ub):
    '''
    --------------------------------------------------------------------
    Draw (N x S) matrix of random draws from a truncated normal
    distribution based on a normal distribution with mean mu and
    standard deviation sigma and cutoffs (cut_lb, cut_ub). These draws
    correspond to an (N x S) matrix of randomly generated draws from a
    uniform distribution U(0,1).
    --------------------------------------------------------------------
    INPUTS:
    unif_vals = (N, S) matrix, (N,) vector, or scalar in (0,1), random
                draws from uniform U(0,1) distribution
    mu        = scalar, mean of the nontruncated normal distribution
                from which the truncated normal is derived
    sigma     = scalar > 0, standard deviation of the nontruncated
                normal distribution from which the truncated normal is
                derived
    cut_lb    = scalar or string, ='None' if no lower bound cutoff is
                given, otherwise is scalar lower bound value of
                distribution. Values below this cutoff have zero
                probability
    cut_ub    = scalar or string, ='None' if no upper bound cutoff is
                given, otherwise is scalar lower bound value of
                distribution. Values below this cutoff have zero
                probability
    
    OTHER FUNCTIONS AND FILES CALLED BY THIS FUNCTION:
        scipy.stats.norm()
    
    OBJECTS CREATED WITHIN FUNCTION:
    cut_ub_cdf  = scalar in [0, 1], cdf of N(mu, sigma) at upper bound
                  cutoff of truncated normal distribution
    cut_lb_cdf  = scalar in [0, 1], cdf of N(mu, sigma) at lower bound
                  cutoff of truncated normal distribution
    unif2_vals  = (N, S) matrix, (N,) vector, or scalar in (0,1),
                  rescaled uniform derived from original.
    tnorm_draws = (N, S) matrix, (N,) vector, or scalar in (0,1),
                  values drawn from truncated normal PDF with base
                  normal distribution N(mu, sigma) and cutoffs
                  (cut_lb, cut_ub)
    
    FILES CREATED BY THIS FUNCTION: None
    
    RETURNS: tnorm_draws
    --------------------------------------------------------------------
    '''
    # No cutoffs: truncated normal = normal
    if (cut_lb == None) & (cut_ub == None):
        cut_ub_cdf = 1.0
        cut_lb_cdf = 0.0
    # Lower bound truncation, no upper bound truncation
    elif (cut_lb != None) & (cut_ub == None):
        cut_ub_cdf = 1.0
        cut_lb_cdf = sts.norm.cdf(cut_lb, loc=mu, scale=sigma)
    # Upper bound truncation, no lower bound truncation
    elif (cut_lb == None) & (cut_ub != None):
        cut_ub_cdf = sts.norm.cdf(cut_ub, loc=mu, scale=sigma)
        cut_lb_cdf = 0.0
    # Lower bound and upper bound truncation
    elif (cut_lb != None) & (cut_ub != None):
        cut_ub_cdf = sts.norm.cdf(cut_ub, loc=mu, scale=sigma)
        cut_lb_cdf = sts.norm.cdf(cut_lb, loc=mu, scale=sigma)
    
    unif2_vals = unif_vals * (cut_ub_cdf - cut_lb_cdf) + cut_lb_cdf
    tnorm_draws = sts.norm.ppf(unif2_vals, loc=mu, scale=sigma)
    
    return tnorm_draws
    
    
mu_1 = 300.0
sig_1 = 30.0
cut_lb_1 = 0.0
cut_ub_1 = 450.0
unif_vals_1 = sts.uniform.rvs(0, 1, size=161)
draws_1 = trunc_norm_draws(unif_vals_1, mu_1, sig_1,
                           cut_lb_1, cut_ub_1)
print('Mean score =', draws_1.mean())
print('Variance of scores =', draws_1.var())
print('Standard deviation of scores =', draws_1.std())

# Plot data histogram vs. simulated data histogram
count_d, bins_d, ignored_d = \
    plt.hist(pts, 30, density=True, color='b', edgecolor='black',
             linewidth=0.8, label='Data')
count_m, bins_m, ignored_m = \
    plt.hist(draws_1, 30, density=True, color='r', edgecolor='black',
             linewidth=0.8, label='Simulated data')
xvals = np.linspace(0, 450, 500)
plt.plot(xvals, trunc_norm_pdf(xvals, mu_1, sig_1, cut_lb_1, cut_ub_1),
         linewidth=2, color='k', label='PDF')
plt.title('Econ 381 scores: 2011-2012', fontsize=20)
plt.xlabel('Total points')
plt.ylabel('Percent of scores')
plt.xlim([0, 550])  # This gives the xmin and xmax to be plotted"
plt.legend(loc='upper left')



def data_moments(xvals):
    '''
    --------------------------------------------------------------------
    This function computes the two data moments for SMM
    (mean(data), variance(data)) from both the actual data and from the
    simulated data.
    --------------------------------------------------------------------
    INPUTS:
    xvals = (N, S) matrix, (N,) vector, or scalar in (cut_lb, cut_ub),
            test scores data, either real world or simulated. Real world
            data will come in the form (N,). Simulated data comes in the
            form (N,) or (N, S).
    
    OTHER FUNCTIONS AND FILES CALLED BY THIS FUNCTION: None
    
    OBJECTS CREATED WITHIN FUNCTION:
    mean_data = scalar or (S,) vector, mean value of test scores data
    var_data  = scalar > 0 or (S,) vector, variance of test scores data
    
    FILES CREATED BY THIS FUNCTION: None
    
    RETURNS: mean_data, var_data
    --------------------------------------------------------------------
    '''
    if xvals.ndim == 1:
        mean_data = xvals.mean()
        var_data = xvals.var()
    elif xvals.ndim == 2:
        mean_data = xvals.mean(axis=0)
        var_data = xvals.var(axis=0)
    
    return mean_data, var_data



mean_data, var_data = data_moments(pts)
print('Data mean =', mean_data)
print('Data variance =', var_data)
mean_sim, var_sim = data_moments(draws_1)
print('Sim. mean =', mean_sim)
print('Sim. variance =', var_sim)
