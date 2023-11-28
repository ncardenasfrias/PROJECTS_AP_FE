import numpy as np
import matplotlib.pyplot as plt


# Option Parameters
S0 = 100  # Initial stock price
T = 1  # Time to maturity (in years)
r = 0.05  # Risk-free interest rate
sigma = 0.2  # Volatility

# Simulation Parameters
N = 10000  # Number of simulation paths
n = 252  # Number of time steps (daily steps for one year)

# Simulation with Antithetic Variates
np.random.seed(42)  # Set seed for reproducibility
dt = T / n
S_star = np.zeros((N, n + 1))

for i in range(N):
    path_star = np.zeros(n + 1)
    path_star[0] = S0

    for t in range(1, n + 1):
        Z_star = np.random.standard_normal()
        path_star[t] = path_star[t - 1] * np.exp((r - 0.5 * sigma**2) * dt + sigma * np.sqrt(dt) * Z_star)

    S_star[i, :] = path_star

# Calculate Maximum Stock Prices
max_stock_prices = np.max(S_star[:, 1:], axis=1)

# Generate Antithetic Variates
S_star_antithetic = np.zeros((N, n + 1))

for i in range(N):
    path_star_antithetic = np.zeros(n + 1)
    path_star_antithetic[0] = S0

    for t in range(1, n + 1):
        Z_star_antithetic = -np.random.standard_normal()  # Negate for antithetic variate
        path_star_antithetic[t] = path_star_antithetic[t - 1] * np.exp((r - 0.5 * sigma**2) * dt + sigma * np.sqrt(dt) * Z_star_antithetic)

    S_star_antithetic[i, :] = path_star_antithetic

# Calculate Maximum Stock Prices for Antithetic Variates
max_stock_prices_antithetic = np.max(S_star_antithetic[:, 1:], axis=1)

# Combine Original and Antithetic Variates
combined_max_stock_prices = np.concatenate((max_stock_prices, max_stock_prices_antithetic))

# Calculate Lookback Put Payoff
lookback_put_payoff = np.exp(-r * T) * (np.mean(combined_max_stock_prices) - S0)

# Confidence Interval using Central Limit Theorem
confidence_interval = 1.96 * np.exp(-r * T) * np.std(combined_max_stock_prices) / np.sqrt(2 * N)  # Factor of 2 for antithetic variates

# Calculate Fair Price Estimate
fair_price_estimate = lookback_put_payoff + confidence_interval

print(f"The estimated fair price of the lookback put option with antithetic variates is: ${fair_price_estimate:.2f}")
print(f"Confidence Interval: +/- ${confidence_interval:.2f}")


print(f"Mean of max stock prices: {np.mean(combined_max_stock_prices)}")
print(f"Standard deviation of max stock prices: {np.std(combined_max_stock_prices)}")

#Histogram of the max stock price
plt.figure(figsize=(8, 6))
plt.hist(combined_max_stock_prices, bins=50, color='blue', alpha=0.7, edgecolor='black')
plt.title('Histogram of Maximum Stock Prices')
plt.xlabel('Maximum Stock Prices')
plt.ylabel('Frequency')
plt.grid(True)
plt.show()


# Plot vertical lines for the confidence interval
lower_bound = lookback_put_payoff - confidence_interval
upper_bound = lookback_put_payoff + confidence_interval
plt.axvline(lower_bound, color='red', linestyle='dashed', linewidth=2, label='Lower Bound of CI')
plt.axvline(upper_bound, color='green', linestyle='dashed', linewidth=2, label='Upper Bound of CI')

plt.legend()
plt.grid(True)
plt.show()

# Time series plot for a few individual paths
plt.figure(figsize=(8, 6))
for i in range(5):  # Plotting 5 random paths for illustration
    plt.plot(S_star[i, :], label=f'Path {i + 1}')

plt.title('Individual Simulated Paths')
plt.xlabel('Time Steps')
plt.ylabel('Stock Price')
plt.legend()
plt.grid(True)
plt.show()




