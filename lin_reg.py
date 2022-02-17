import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
sns.set_context('notebook')

# sample size and number of covariates
N = 1000
D = 1

# sample the covariates
X = np.random.normal(0, 1, (N, D))

# add intercept term
X = np.c_[np.ones(N), X]

# sample true regression coefficients
beta = np.random.normal(0, 1, (D+1, 1))

# sample irreducible error
epsilon = np.random.normal(0, 1, (N, 1))

# compute true outcome
y = X @ beta + epsilon

# estimate regression coefficients
beta_hat = np.linalg.solve(X.T @ X, X.T @ y)

# predict outcome
y_hat = X @ beta_hat

# compute mean squared error
MSE = ((y - y_hat)**2).mean()
print(f'MSE = {MSE:.4f}')

# drop the intercept term
X = X[:,1:]

# plot the data and model
fig, ax = plt.subplots(figsize=(4,4))
ax.scatter(X, y, color='black', alpha=1/np.sqrt(N))
ax.plot(X, y_hat, color='black', label='model')
ax.legend(frameon=False, loc='upper right')
ax.set_xlim(-4, 4)
ax.set_ylim(-4, 4)
ax.set_xlabel('X')
ax.set_ylabel('y')
fig.savefig('lin_reg.png', bbox_inches='tight', dpi=200)
