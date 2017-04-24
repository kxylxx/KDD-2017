# -*- coding: utf-8 -*-
"""
Spyder Editor

This is a temporary script file.
"""

print(__doc__)

# importing necessary libraries
import numpy as np
import matplotlib.pyplot as plt
from pylab import *
import pandas as pd
#import glob
from sklearn.tree import DecisionTreeRegressor

from sklearn.ensemble import AdaBoostRegressor
from pandas import DataFrame
from sklearn.model_selection import cross_val_score
from sklearn.model_selection import cross_val_predict
from sklearn.model_selection import train_test_split
from sklearn import metrics
from sklearn.datasets import load_iris
from sklearn.neighbors import KNeighborsClassifier
# Create the dataset
rng = np.random.RandomState(1)
iris = load_iris()
X = iris.data
y =iris.target
knn = KNeighborsClassifier(n_neighbors=5)
scores = cross_val_score(knn,X,y,cv=10)
scores.mean()
#input the dataset
train = pd.read_csv('E:\\KDD_CUP_2017\\dataSets\\dataSets\\train_kxy\\new1_tollgate1_0.csv').set_index(['tollgate_id','time_window','direction'])
#train = pd.read_csv('E:\\KDD_CUP_2017\\dataSets\\dataSets\\train_kxy\\new_tollgate2_0.csv').set_index(['tollgate_id','time_window','direction'])
train = pd.read_csv('E:\\KDD_CUP_2017\\dataSets\\dataSets\\train_kxy\\new1_tollgate2_0.csv').set_index(['tollgate_id','time_window','direction'])
train = pd.read_csv('E:\\KDD_CUP_2017\\dataSets\\dataSets\\train_kxy\\new_tollgate3_0.csv').set_index(['tollgate_id','time_window','direction'])
train = pd.read_csv('E:\\KDD_CUP_2017\\dataSets\\dataSets\\train_kxy\\new_tollgate1_1.csv').set_index(['tollgate_id','time_window','direction'])
train = pd.read_csv('E:\\KDD_CUP_2017\\dataSets\\dataSets\\train_kxy\\new_tollgate3_1.csv').set_index(['tollgate_id','time_window','direction'])
X_train = train.drop('volume',axis=1).values
Y_train = train['volume']
Y_train = np.log(np.sqrt(Y_train))
#==============================================================================
####这里面是将train拆分为train和test两部分。
depth = list(range(31))
es = [20,40,60,80,100,120,140,160,180,200,220,250]

all_X = train.drop('volume',axis=1).values
all_Y = train['volume']
all_Y = np.log(np.sqrt(all_Y))
scores_all = np.ones(shape=[19,12])

for i in depth[1:20]:
    n = 0
    for j in es:        
        regr_2 = AdaBoostRegressor(DecisionTreeRegressor(max_depth=i),n_estimators=(50+j), random_state=rng)
        #X_train, X_test, Y_train, Y_test = train_test_split(all_X, all_Y, test_size=0.1, random_state=0)
        scores = cross_val_score(regr_2,all_X, all_Y,cv=10)
        #scores.mean()
        scores_all[(i-1),(n)] = scores.mean()
        n = n+1
np.where(scores_all==np.max(scores_all))
np.max(scores_all)
scores_all.sort

regr_2 = AdaBoostRegressor(DecisionTreeRegressor(max_depth=7),n_estimators=65, random_state=rng)
#X_train, X_test, Y_train, Y_test = train_test_split(all_X, all_Y, test_size=0.3, random_state=0)
scores = cross_val_score(regr_2,all_X,all_Y,cv=10)
scores.mean()
#==============================================================================

test = pd.read_csv('E:\\KDD_CUP_2017\\dataSets\\dataSets\\test_kxy\\new_tollgate1_0.csv').set_index(['tollgate_id','time_window','direction'])
test = pd.read_csv('E:\\KDD_CUP_2017\\dataSets\\dataSets\\test_kxy\\new_tollgate2_0.csv').set_index(['tollgate_id','time_window','direction'])
test = pd.read_csv('E:\\KDD_CUP_2017\\dataSets\\dataSets\\test_kxy\\new_tollgate3_0.csv').set_index(['tollgate_id','time_window','direction'])
test = pd.read_csv('E:\\KDD_CUP_2017\\dataSets\\dataSets\\test_kxy\\new_tollgate1_1.csv').set_index(['tollgate_id','time_window','direction'])
test = pd.read_csv('E:\\KDD_CUP_2017\\dataSets\\dataSets\\test_kxy\\new_tollgate3_1.csv').set_index(['tollgate_id','time_window','direction'])
X_test = test
np.min(X_test)

regr_2 = AdaBoostRegressor(DecisionTreeRegressor(max_depth=5),n_estimators=190, random_state=rng)
regr_2.fit(X_train, Y_train)

#Y_predict = regr_2.predict(X_test)
test['volume'] = regr_2.predict(X_test)
test_all = (np.exp(test['volume']))^2#只是在求tollgate1_0时执行这一句

test_all = pd.concat([test_all,(np.exp(test['volume']))^2])#在求除tollgate1_0之外时执行这一句
test_all.reset_index().to_csv('E:\\KDD_CUP_2017\\dataSets\\dataSets\\Ada_scale_task2.csv',index = False)


#==============================================================================
# type(volume)
# volume.shape
# volume_new.shape
# volume.columns
# 
# volume.head()
# type(volume_new)
# 
# 
# 
# 
# # Plot the results
# plt.figure()
# plt.plot(x, volume[u'volume'], c='b', label="training samples",linewidth=2)
# #plt.plot(x, y_1, c="g", label="n_estimators=1", linewidth=2)
# plt.plot(x, y_2, c="r", label="n_estimators=30000", linewidth=2)
# plt.xlabel("data")
# plt.ylabel("target")
# plt.title("Boosted Decision Tree Regression")
# plt.legend()
# plt.show()
# 
# def MAPE(y_true, y_pred):
#     return np.mean(np.abs((y_true - y_pred) / y_true))
# MAPE(volume[u'volume'][-25:-1],y_2)
#==============================================================================
