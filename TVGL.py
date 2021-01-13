#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Jan 13 09:23:09 2021

@author: MacBook
"""

#import TVGL as tvgl
import numpy as np 
from TVGL import *




Cov = np.array([[5, 1], [1, 7]])
data = np.random.multivariate_normal(np.zeros(2), Cov, 50)


#data = np.genfromtxt('./PaperCode/Datasets/finance.csv', delimiter=',')
data = data[0:30,0:10]
lamb = 2.5
beta = 12
lengthOfSlice = 10


thetaSet = TVGL(data, lengthOfSlice, lamb, beta, indexOfPenalty = 1, verbose=True)
