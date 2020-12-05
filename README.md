# Financial network estimation for measuring bank interconnectedness
# Project description
The aim of this project is to explore and compare different methods to estimate the interconnectedness of financial institutions. We do this via estimating networks between the *returns* or *volatilities* of the stocks of these financial institutions. We also compare these networks with the directly observable network of *cross-holdings* between these financial institutions.

The different methods that we apply are the following:
* Diebold-Yilmaz variance decomposition
* Neighbourhood Selection
* SPACE
* GLASSO
* Sparse Bayesian networks (or directed acyclical graphs)


# Data
You can find the stock price time series data that we use for this project in the [Stock prices](https://github.com/paparon96/FinancialNetworks/tree/main/Data/Stock_prices) folder, along with the French-Fama factors, which we also utilise for this project.

The cross-holdings data was obtained from NASDAQ's website using web scraping methods and it is available in the [Data](https://github.com/paparon96/FinancialNetworks/tree/main/Data) folder, along with other metadata.

# Code for network estimation
The scripts (written mainly in *R*) can be found in the root directory of the repository and the file names indicate the method used.

# Estimated networks

The estimated networks from the various methods are available in the [Estimated networks](https://github.com/paparon96/FinancialNetworks/tree/main/Data/Estimated_networks) folder.

# Figures, visualisation

The visualisation of the different estimated networks is available in the [Figures](https://github.com/paparon96/FinancialNetworks/tree/main/Figures) folder and again, the file names indicate what method was used for that particular network.

# Comparison of estimated networks
A comparison of the estimated networks in terms of the centrality of the financial institutions is analysed in the [Network comparison](https://github.com/paparon96/FinancialNetworks/blob/main/Network_comparison_centrality.ipynb) notebook.
