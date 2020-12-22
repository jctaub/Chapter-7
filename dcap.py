#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Jul 11 20:19:48 2019

@author: RaeChen
"""

import numpy as np
import pandas as pd 
from numpy.random import choice

def restructure(data, target_index):
    '''
        restructure data to Keys+Target format 
    '''
    new=data.copy()
    new[target_index], new[-1]=new[-1], new[target_index].copy()
    return new


def get_data(data_file, target_index):
    '''
        Getting original data from .cvs file
        Desired format of data: numeric array, 0-based indexed categories,
        in the format of key variables+target variable
    '''
    testdata=pd.read_csv(data_file)
    data=np.array(testdata).T
    ori_data=restructure(data, target_index)
    m=data.shape[1]
    return ori_data, m

def unique(data): 
    unique=[]
    for i in range(data.shape[0]):
        unique.append(len(list(np.unique(data[i]))))
    return tuple(unique)
   
def create_array(data, shape): #Written by DS: duncan.g.smith@manchester.ac.uk
    '''
        Function to create full contingency table
    '''
    arr = np.zeros(shape)
    for case in data.T:
        arr[tuple(case)] += 1
    return arr

ori_data, m = get_data('data1.csv', 1)
shape = unique(ori_data)
ori_full_table = create_array(ori_data, shape)

syn_data=np.vstack([choice(np.unique(row),size=ori_data.shape[1],replace=True) for row in ori_data])


'''
    Find uniques in synthetic data
'''
count=np.unique(syn_data.T, return_counts=True, axis=0)
unique_data=[]
for i in range(len(count[1])):
    if count[1][i]==1:
        unique_data.append(count[0][i])
unique_data=np.array(unique_data).T

'''
    cap table
'''
target=ori_data[-1] #default ori_data[-1] is target variable
prop=np.unique(target, return_counts=True)[1]/m #frequencies

baseline_cap=[]
for value in target:
    baseline_cap.append(prop[value])
    
baseline_cap=np.array(baseline_cap)

shape_key=shape[:-1] #shape of key variables (equivalence class)

ori_key=ori_data[:-1] #extract key vars from ori_data
syn_key=syn_data[:-1] #extract key vars from syn_data

ori_key_table=create_array(ori_key, shape_key) #getting full CT of key vars from ori_data
syn_key_table=create_array(syn_key, shape_key) #getting full CT of key vars from syn_data

syn_full_table=create_array(syn_data, shape)#getting full CT of all vars from syn_data

ori_cap, syn_cap=np.zeros(m), np.zeros(m)
for i in range(m):
    ori_cap[i]=ori_full_table[tuple(ori_data.T[i])]/ori_key_table[tuple(ori_key.T[i])]
    syn_cap[i]=syn_full_table[tuple(ori_data.T[i])]/syn_key_table[tuple(ori_key.T[i])]
    
cap_table=np.vstack((ori_data, ori_cap, syn_cap, baseline_cap))

'''
    dcap score
'''
dcap=(np.mean(cap_table[-2])-np.mean(cap_table[-1]))/(np.mean(cap_table[-3])-np.mean(cap_table[-1]))
