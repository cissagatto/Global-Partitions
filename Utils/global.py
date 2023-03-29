##############################################################################
# GLOBAL PARTITIONS                                                          #
# Copyright (C) 2023                                                         #
#                                                                            #
# This code is free software: you can redistribute it and/or modify it under #
# the terms of the GNU General Public License as published by the Free       #
# Software Foundation, either version 3 of the License, or (at your option)  #
# any later version. This code is distributed in the hope that it will be    #
# useful, but WITHOUT ANY WARRANTY; without even the implied warranty of     #
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General   #
# Public License for more details.                                           #
#                                                                            #
# 1 - PhD Elaine Cecilia Gatto | Prof PhD Ricardo Cerri                      #
# 2 - Prof PhD Mauri Ferrandin                                               #
# 3 - Prof PhD Celine Vens | PhD Felipe Nakano Kenji                         #
# 4 - Prof PhD Jesse Read                                                    #
#                                                                            #
# 1 = Federal University of São Carlos - UFSCar - https://www2.ufscar.br     #
# Campus São Carlos | Computer Department - DC - https://site.dc.ufscar.br | #
# Post Graduate Program in Computer Science - PPGCC                          # 
# http://ppgcc.dc.ufscar.br | Bioinformatics and Machine Learning Group      #
# BIOMAL - http://www.biomal.ufscar.br                                       # 
#                                                                            #
# 2 - Federal University of Santa Catarina Campus Blumenau - UFSC            #
# https://ufsc.br/                                                           #
#                                                                            #
# 3 - Katholieke Universiteit Leuven Campus Kulak Kortrijk Belgium           #
# Medicine Department - https://kulak.kuleuven.be/                           #
# https://kulak.kuleuven.be/nl/over_kulak/faculteiten/geneeskunde            #
#                                                                            #
# 4 - Ecole Polytechnique | Institut Polytechnique de Paris | 1 rue Honoré   #
# d’Estienne d’Orves - 91120 - Palaiseau - FRANCE                            #
#                                                                            #
##############################################################################


import sys
import numpy as np
import pandas as pd
from sklearn.ensemble import RandomForestClassifier  
from sklearn.metrics import average_precision_score

if __name__ == '__main__':   
    
    # obtendo argumentos da linha de comando
    train = pd.read_csv(sys.argv[1]) # conjunto de treino
    valid = pd.read_csv(sys.argv[2]) # conjunto de validação
    test = pd.read_csv(sys.argv[3])  # conjunto de teste
    start = int(sys.argv[4])         # inicio do espaço de rótulos  
    directory = sys.argv[5]          # diretório para salvar as predições 
    
    # train = pd.read_csv("/dev/shm/grf-GpositiveGO/Global/Split-1/GpositiveGO-Split-Tr-1.csv")
    # test = pd.read_csv("/dev/shm/grf-GpositiveGO/Global/Split-1/GpositiveGO-Split-Ts-1.csv")
    # valid = pd.read_csv("/dev/shm/grf-GpositiveGO/Global/Split-1/GpositiveGO-Split-Vl-1.csv")
    # start = 912
    # directory = "/dev/shm/grf-GpositiveGO/Global/Split-1/"
     
    # juntando treino com validação
    train = pd.concat([train,valid],axis=0).reset_index(drop=True)
    
    # treino: separando os atributos e os rótulos
    X_train = train.iloc[:, :start]    # atributos 
    Y_train = train.iloc[:, start:]    # rótulos 
    
    # teste: separando os atributos e os rótulos
    X_test = test.iloc[:, :start]    # atributos 
    Y_test = test.iloc[:, start:]    # rótulos 
    
    # obtendo os nomes dos rótulos
    labels_y_train = list(Y_train.columns)
    labels_y_test = list(Y_test.columns)
    
    # obtendo os nomes dos atributos
    attr_x_train = list(X_train.columns)
    attr_x_test = list(X_test.columns)
    
    # parametros do classificador base
    random_state = 0    
    n_estimators = 200

    # inicializa o classificador base
    rf = RandomForestClassifier(n_estimators = n_estimators, random_state = random_state)
    
    # treino
    rf.fit(X_train, Y_train)
    
    # teste
    y_pred_bin_o = rf.predict(X_test)
    # y_pred_bin_o.shape[1]
    # y_pred_bin_o.shape[0]

    # predições binárias
    y_pred_bin_d = pd.DataFrame(rf.predict(X_test)) 
    # y_pred_bin_d.shape[1]
    # y_pred_bin_d.shape[0]
    
    # renomeando as colunas
    y_pred_bin_d.columns = labels_y_test

    # predições probabilísticas
    y_proba_o = rf.predict_proba(X_test)
    
    # obtendo os rótulos do teste
    y_true = pd.DataFrame(Y_test)
    
    # setando nome do diretorio e arquivo para salvar
    name_true = (directory + "/y_true.csv")          # rótulos do teste 
    name_pred = (directory + "/y_pred_bin.csv")      # predições do predict
    name_proba_o = (directory + "/y_proba_o.csv") # predições do predict_proba, todas
    
    # salvando rotulos do teste e das predições binárias
    y_pred_bin_d.to_csv(name_pred, index=False)
    y_true.to_csv(name_true, index=False)    

    # construindo a tabela com as predições probabilisticas
    # para salvar num formato de dataframe que pode ser usado no R
    ldf1 = []
    ldf2 = []
    for n in range(0, len(y_proba_o)):
      # print(" ", n)
      res = y_proba_o[n]
      res1 = pd.DataFrame(res)
      res1.columns = [f'prob_{n}_0', f'prob_{n}_1']
      ldf1.append(res1)
    
    # salvando
    final = pd.concat(ldf1, axis=1)
    final.to_csv(name_proba_o, index=False)
