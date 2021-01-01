import pandas as pd
import numpy as np
import networkx as nx
import matplotlib.pyplot as plt
import seaborn as sns
import copy
import sys
import forceatlas2
from fa2 import ForceAtlas2
import igraph
import cairocffi



def centrality_norm_diff(candidate_dict,true_dict):

    """Computes the difference of the centrality scores
    given 2 dictionaries"""

    score = 0
    for key, val in candidate_dict.items():
        score += np.linalg.norm(val-true_dict[key])


    return score


def network_preprocessing(date,method,ts_type,dy_threshold):

    """Import the network adjacency matrix from a .csv file, converts it into a networkx
    diGraph object"""

    # Convert date to string
    final_date_transformed = date.replace("-","_")


    # Import file
    filename = "./Data/Estimated_networks/" +\
                  method+"_"+ts_type+"_"+final_date_transformed+".csv"

    if (method == "DY") & (date == "2020-03-16") & (ts_type=="return"):
        network_matrix = np.genfromtxt(filename,
                              delimiter=',',skip_header = 1,usecols = np.arange(1,11),
                                   skip_footer=1)

        # Make the diagonal zero
        np.fill_diagonal(network_matrix, 0)

        # Delete link if the edge weight is too small in case of DY
        network_matrix[network_matrix<dy_threshold] = 0

        # Normalize values
        network_matrix = network_matrix / 100

        #print(network_matrix)

    else:
        network_matrix = np.genfromtxt(filename,
                              delimiter=',',skip_header = 1,usecols = np.arange(1,11))


    # Convert it into networkX object
    network = nx.from_numpy_matrix(network_matrix, create_using=nx.DiGraph)

    return network

def network_preprocessing_demirer_playground(date,method,ts_type):

    """Import the network adjacency matrix from a .csv file, converts it into a networkx
    diGraph object"""

    # Convert date to string
    final_date_transformed = date.replace("-","_")


    # Import file
    filename = "./Data/Estimated_networks/Demirer_" +\
                  method+"_"+ts_type+"_"+final_date_transformed+".csv"

    network_matrix = np.genfromtxt(filename,
                              delimiter=',',skip_header = 1,usecols = np.arange(1,107))

    # Zero out the diagonal
    np.fill_diagonal(network_matrix,0)


    # Convert it into networkX object
    network = nx.from_numpy_matrix(network_matrix, create_using=nx.DiGraph)

    return network

def network_preprocessing_general(date,method,ts_type,base_path):

    """Import the network adjacency matrix from a .csv file, converts it into a networkx
    diGraph object"""

    # Convert date to string
    final_date_transformed = date.replace("-","_")


    # Import file
    filename = base_path +\
                  method+"_"+ts_type+"_"+final_date_transformed+".csv"

    network_matrix = np.genfromtxt(filename,
                              delimiter=',',skip_header = 1,usecols = np.arange(1,77))

    # Zero out the diagonal
    np.fill_diagonal(network_matrix,0)


    # Convert it into networkX object
    network = nx.from_numpy_matrix(network_matrix, create_using=nx.DiGraph)

    return network


def network_matrix_preprocessing(date,method,ts_type,dy_threshold):

    """Import the network adjacency matrix from a .csv file, converts it into a numpy array"""

    # Convert date to string
    final_date_transformed = date.replace("-","_")


    # Import file
    filename = "./Data/Estimated_networks/" +\
                  method+"_"+ts_type+"_"+final_date_transformed+".csv"

    if (method == "DY") & (date == "2020-03-16")& (ts_type=="return"):
        network_matrix = np.genfromtxt(filename,
                              delimiter=',',skip_header = 1,usecols = np.arange(1,11),
                                   skip_footer=1)

        # Make the diagonal zero
        np.fill_diagonal(network_matrix, 0)

        # Delete link if the edge weight is too small in case of DY
        network_matrix[network_matrix<dy_threshold] = 0

        # Normalize values
        network_matrix = network_matrix / 100

        #print(network_matrix)

    else:
        network_matrix = np.genfromtxt(filename,
                              delimiter=',',skip_header = 1,usecols = np.arange(1,11))


    # Convert it into networkX object
    network = np.asarray(network_matrix)

    return network

def network_robustness_check(dates,method,ts_type,dy_threshold):

    """Creates the adjacency matrix with the specified parameters for 2 different dates and computes
    the Frobenius-norm of the difference of the matrices to check persistence/robustness of results"""



    # Get the adjacency matrix of the estimated networks
    network1 = network_matrix_preprocessing(dates[0],method,ts_type,dy_threshold)


    network2 = network_matrix_preprocessing(dates[1],method,ts_type,dy_threshold)

    # Get difference
    diff = np.linalg.norm(network1-network2)

    return diff


def centrality_ranking_df(centrality_type,networks,varnames,asset_sizes=None,cross_holding_network=None):

    """Creates a dataframe with the sorted centrality of stocks
    from different methods"""

    # Add the cross-holding network to the dictionary (optional)
    if cross_holding_network is not None:
        networks['CH'] = cross_holding_network



    # Loop over different methods and their networks
    for key,val in networks.items():

        print(key)


        if centrality_type == "katz-bonacich":

            centrality_dict = nx.katz_centrality(val)
            ticker_names = [varnames[k] for k, v in sorted(centrality_dict.items(),
                                                     key=lambda item: item[1],reverse=True)]

        elif centrality_type == "betweenness":

            centrality_dict = nx.betweenness_centrality(val)
            ticker_names = [varnames[k] for k, v in sorted(centrality_dict.items(),
                                                     key=lambda item: item[1],reverse=True)]

        elif centrality_type == "degree":

            centrality_dict = nx.degree_centrality(val)
            ticker_names = [varnames[k] for k, v in sorted(centrality_dict.items(),
                                                     key=lambda item: item[1],reverse=True)]

        elif centrality_type == "eigenvector":

            centrality_dict = nx.eigenvector_centrality(val)
            ticker_names = [varnames[k] for k, v in sorted(centrality_dict.items(),
                                                     key=lambda item: item[1],reverse=True)]

        elif centrality_type == "closeness":

            centrality_dict = nx.closeness_centrality(val)
            ticker_names = [varnames[k] for k, v in sorted(centrality_dict.items(),
                                                     key=lambda item: item[1],reverse=True)]

        else:
            ticker_names = None

        networks[key] = ticker_names


    # Add the asset sizes to the dictionary (optional)
    if asset_sizes is not None:
        networks['AS'] = asset_sizes


    # Create dataframe
    df = pd.DataFrame(networks)

    return df


def rank_viz(varnames,ranking_df,centrality_type,date,ts_type):

    fig = plt.figure(figsize=(8,3.))
    ax = fig.add_subplot(111)
    col = 'white'

    cols = ranking_df.columns
    #print(cols)
    #print(varnames)

    n = len(varnames)
    d = len(cols)
    palette = iter(sns.color_palette("Paired", n))

    y_max = np.ones(d)*n
    x = np.arange(0,d,1)

    names = np.asarray(copy.deepcopy(varnames))

    for stock in varnames:
        stock_list = []
        counter = 0
        #print(stock)
        for i in cols:
            #print(i)
            #print(stock)


            temp = ranking_df[ranking_df[i]==stock].index.values[0]

            if counter == 0:
                #print(stock)
                #print(temp)
                names[n-temp-1] = stock

            stock_list.append(temp)



            ax.annotate(str(temp+1), xy=(counter,n - temp), color=col,
                fontsize=8, weight='heavy',
                horizontalalignment='center',
                verticalalignment='center')

            counter += 1

        yn = y_max-np.asarray(stock_list)
        #print(yn)



        ax.plot(x,yn, color=next(palette), linewidth=5)

    palette = iter(sns.color_palette("Paired", n))
    for stock in varnames:
        stock_list = []
        counter = 0
        for i in cols:
            temp = ranking_df[ranking_df[i]==stock].index.values[0]

            stock_list.append(temp)

            counter += 1

        ax.plot(x,y_max - np.asarray(stock_list), '.', markersize=26, mec='w', mfc=next(palette))

    plt.xticks(range(d))
    plt.yticks(np.arange(1,n+1,1))
    plt.gca().get_yaxis().set_ticklabels(names)
    plt.gca().get_xaxis().set_ticklabels(cols)
    ax.set_facecolor('gainsboro')
    plt.tight_layout()

    # Convert date to string
    final_date_transformed = date.replace("-","_")
    plt.savefig('./Figures/ranking_comparison_{}_{}_{}.pdf'.format(ts_type,final_date_transformed,
                                                                 centrality_type),dpi=120)
    plt.show()



def rank_viz_demirer_playground(chosen_varnames,ranking_df,centrality_type,date,ts_type):

    fig = plt.figure(figsize=(8,3.))
    ax = fig.add_subplot(111)
    col = 'white'

    cols = ranking_df.columns

    n = len(chosen_varnames)
    d = len(cols)
    palette = iter(sns.color_palette("Paired", n))

    y_max = np.ones(d)*n
    x = np.arange(0,d,1)

    names = np.asarray(copy.deepcopy(chosen_varnames))

    score_data_dict = {}

    for i in cols:
        score_data_dict[i] = {}

        for stock in chosen_varnames:

            temp = ranking_df[ranking_df[i]==stock].index.values[0]
            score_data_dict[i][stock] = temp

    loc_data_dict = {}

    for i in cols:
        loc_data_dict[i] = {}

        array = np.asarray(list(score_data_dict[i].values()))
        tempnames = list(score_data_dict[i].keys())
        order = array.argsort()
        ranks = list(order.argsort())
        for counter in range(0,len(tempnames)):
            nn = tempnames[counter]
            loc_data_dict[i][nn] = ranks[counter]



    for stock in chosen_varnames:
        stock_list = []
        counter = 0
        for i in cols:


            if counter == 0:
                names[n-loc_data_dict[i][stock]-1] = stock

            stock_list.append(loc_data_dict[i][stock])



            ax.annotate(str(score_data_dict[i][stock]+1), xy=(counter,n - loc_data_dict[i][stock]), color=col,
                fontsize=8, weight='heavy',
                horizontalalignment='center',
                verticalalignment='center')

            counter += 1

        yn = y_max-np.asarray(stock_list)



        ax.plot(x,yn, color=next(palette), linewidth=5)

    palette = iter(sns.color_palette("Paired", n))
    for stock in chosen_varnames:
        stock_list = []
        counter = 0
        for i in cols:

            stock_list.append(loc_data_dict[i][stock])

            counter += 1

        ax.plot(x,y_max - np.asarray(stock_list), '.', markersize=26, mec='w', mfc=next(palette))

    plt.xticks(range(d))
    plt.yticks(np.arange(1,n+1,1))
    plt.gca().get_yaxis().set_ticklabels(names)
    plt.gca().get_xaxis().set_ticklabels(cols)
    ax.set_facecolor('gainsboro')
    plt.tight_layout()

    # Convert date to string
    final_date_transformed = date.replace("-","_")
    plt.savefig('./Figures/ranking_comparison_Demirer_playground_{}_{}_{}.pdf'.format(ts_type,final_date_transformed,
                                                                 centrality_type),dpi=120)
    plt.show()


def rank_viz_general(chosen_varnames,ranking_df,centrality_type,date,ts_type,
data_type):

    fig = plt.figure(figsize=(8,3.))
    ax = fig.add_subplot(111)
    col = 'white'

    cols = ranking_df.columns

    n = len(chosen_varnames)
    d = len(cols)
    palette = iter(sns.color_palette("Paired", n))

    y_max = np.ones(d)*n
    x = np.arange(0,d,1)

    names = np.asarray(copy.deepcopy(chosen_varnames))

    score_data_dict = {}

    for i in cols:
        score_data_dict[i] = {}

        for stock in chosen_varnames:

            temp = ranking_df[ranking_df[i]==stock].index.values[0]
            score_data_dict[i][stock] = temp

    loc_data_dict = {}

    for i in cols:
        loc_data_dict[i] = {}

        array = np.asarray(list(score_data_dict[i].values()))
        tempnames = list(score_data_dict[i].keys())
        order = array.argsort()
        ranks = list(order.argsort())
        for counter in range(0,len(tempnames)):
            nn = tempnames[counter]
            loc_data_dict[i][nn] = ranks[counter]



    for stock in chosen_varnames:
        stock_list = []
        counter = 0
        for i in cols:


            if counter == 0:
                names[n-loc_data_dict[i][stock]-1] = stock

            stock_list.append(loc_data_dict[i][stock])



            ax.annotate(str(score_data_dict[i][stock]+1), xy=(counter,n - loc_data_dict[i][stock]), color=col,
                fontsize=8, weight='heavy',
                horizontalalignment='center',
                verticalalignment='center')

            counter += 1

        yn = y_max-np.asarray(stock_list)



        ax.plot(x,yn, color=next(palette), linewidth=5)

    palette = iter(sns.color_palette("Paired", n))
    for stock in chosen_varnames:
        stock_list = []
        counter = 0
        for i in cols:

            stock_list.append(loc_data_dict[i][stock])

            counter += 1

        ax.plot(x,y_max - np.asarray(stock_list), '.', markersize=26, mec='w', mfc=next(palette))

    plt.xticks(range(d))
    plt.yticks(np.arange(1,n+1,1))
    plt.gca().get_yaxis().set_ticklabels(names)
    plt.gca().get_xaxis().set_ticklabels(cols)
    ax.set_facecolor('gainsboro')
    plt.tight_layout()

    # Convert date to string
    final_date_transformed = date.replace("-","_")
    plt.savefig('./Figures/ranking_comparison_{}_{}_{}_{}.pdf'.format(data_type, ts_type,final_date_transformed,
                                                                 centrality_type),dpi=120)
    plt.show()


def large_network_viz(date,method,ts_type,var_names, color_map):

    print("Method: {}".format(method))

    # Import network
    G = network_preprocessing_demirer_playground(date,method,ts_type)

    label_dict = {}

    for i in range(0,len(var_names)):
        label_dict[i] = var_names[i]

    # Relabel network
    G = nx.relabel_nodes(G, label_dict)

    # Get edge weights
    edges,weights = zip(*nx.get_edge_attributes(G,'weight').items())


    # Parameters
    plt.rcParams['figure.figsize'] = (12,12)

    # Get degrees
    degree_demirer = dict(G.degree)

    # Draw network
    nx.draw_networkx(G,
                         #nodelist=degree_mcc_network.keys(),
                         node_size=[v * 5 for v in degree_demirer.values()],
                         #node_color=colors,
                         font_size=8, node_color=color_map,
                     edgelist=edges,
                     edge_color=weights,
                     width=5.0, edge_cmap=plt.cm.Blues,
                         with_labels=True)

    plt.savefig('./Figures/Demirer_network_{}_{}_{}.pdf'.format(ts_type,method,date),dpi = 120)
    plt.show()


def network_viz_general(date,method,ts_type,var_names, color_map,
base_path,data_type):

    print("Method: {}".format(method))

    # Import network
    G = network_preprocessing_general(date,method,ts_type,base_path)

    label_dict = {}

    for i in range(0,len(var_names)):
        label_dict[i] = var_names[i]

    # Relabel network
    G = nx.relabel_nodes(G, label_dict)

    # Get edge weights
    edges,weights = zip(*nx.get_edge_attributes(G,'weight').items())


    # Parameters
    plt.rcParams['figure.figsize'] = (12,12)

    # Get degrees
    degree_demirer = dict(G.degree)

    # Draw network
    nx.draw_networkx(G,
                         #nodelist=degree_mcc_network.keys(),
                         node_size=[v * 5 for v in degree_demirer.values()],
                         #node_color=colors,
                         font_size=8, node_color=color_map,
                     edgelist=edges,
                     edge_color=weights,
                     width=5.0, edge_cmap=plt.cm.Blues,
                         with_labels=True)

    plt.savefig('./Figures/{}_{}_{}_{}.pdf'.format(data_type,ts_type,method,date),dpi = 120)
    plt.show()
