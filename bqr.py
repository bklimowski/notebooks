import numpy as np
import matplotlib.pyplot as plt
from scipy.linalg import cho_factor, cho_solve



n = 200
x = np.array([np.random.uniform(0, 10, n),
            np.ones(n)]).T
y = 5 + 2 * x[:,0] + np.random.normal(0, 0.4*x[:,0], n)
runs = 10000
burnin = 1000
thin = 1
tau = 0.8

plt.scatter(x[:,0], y)
plt.plot(x[:,0], x[:,0]*2.12285354 + 4.40681758)
plt.plot(x[:,0], x[:,0]*2.08267602 + 4.82268304)


def bqr(x, y, tau, runs = 11000, burn = 1000, thin = 1):
    n = x.shape[0]
    p = x.shape[1]

    beta_draws = np.zeros((runs, p))
    mu_y = np.zeros((runs, n))
    var_y = np.zeros((runs, n))
    sigma_draws = np.zeros((runs, 1))

    xi = (1 - 2*tau) 
    zeta = tau*(1-tau)
    beta = np.repeat(0.99, p)
    v = np.repeat(1, n)
    sigma = 1


    def inv_gaussian(n, mu, lambd = 1):
        un = np.random.uniform(0, 1, n)
        xi = np.random.chisquare(1, n)
        f = mu/(2*lambd)*(2*lambd+mu+xi+np.sqrt(4*lambd*mu*xi+mu**2*xi**2))
        s = mu**2/f
        return np.where(un < mu/(mu+s), f, s)

    for ii in range(runs):
        lambd = 1/(2*sigma)
        mu = 1/(np.abs(y - np.matmul(x, beta)))
        v = inv_gaussian(n, mu, lambd)
        Mu = np.matmul(x, beta) + xi*v
        shape =   3/2*n 
        scale  = sum((y - Mu)**2/(4*v))+zeta*np.sum(v) 
        sigma = 1/np.random.gamma(shape, scale, 1)
        vsigma = 2*sigma*v
        V = np.diag(1/vsigma)
        cho_x = np.matmul(np.matmul(x.T, V), x)
        varcov = cho_solve(cho_factor(cho_x,lower=True), 
                           np.eye(cho_x.shape[0]))
        betam = np.matmul(varcov,
                          np.matmul(x.T,
                                    np.matmul(V, y - xi*v)))

        beta = betam + np.matmul(cho_factor(varcov,lower=True)[0].T,
                                 np.random.normal(0,1,p))

        beta_draws[ii, :]  = beta  
        mu_y[ii, :]     = Mu
        var_y[ii, :]    = vsigma
        sigma_draws[ii, :] = sigma

    coefficients = beta_draws[burnin:runs, :].mean(0)

    result = {
        "beta": beta_draws[burnin:runs:thin, :],
        "mu_y": mu_y[burnin:runs:thin, :],
        "var_y": var_y[burnin:runs:thin, :],
        "sigma": sigma_draws[burnin:runs:thin, :],
        "y": y,
        "coefficients": coefficients
    }

    return result

bqr(x,y, tau=0.95)
