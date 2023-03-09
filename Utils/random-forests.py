##############################################################################
# RANDOM FORESTS IN PYTHON                                                   #
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
# Elaine Cecilia Gatto | Prof. Dr. Ricardo Cerri | Prof. Dr. Mauri           #
# Ferrandin | Prof. Dr. Celine Vens | PhD Felipe Nakano Kenji                #
#                                                                            #
# Federal University of São Carlos - UFSCar - https://www2.ufscar.br         #
# Campus São Carlos - Computer Department - DC - https://site.dc.ufscar.br   #
# Post Graduate Program in Computer Science - PPGCC                          # 
# http://ppgcc.dc.ufscar.br - Bioinformatics and Machine Learning Group      #
# BIOMAL - http://www.biomal.ufscar.br                                       #
#                                                                            #
# Katholieke Universiteit Leuven Campus Kulak Kortrijk Belgium               #
# Medicine Department - https://kulak.kuleuven.be/                           #
# https://kulak.kuleuven.be/nl/over_kulak/faculteiten/geneeskunde            #
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
     
    # juntando treino com validação
    train = pd.concat([train,valid],axis=0).reset_index(drop=True)
    
    # treino: separando os atributos e os rótulos
    X_train_att = train.iloc[:, :start]    # atributos 
    Y_train_labels = train.iloc[:, start:] # rótulos 
    
    # teste: separando os atributos e os rótulos
    X_test_att = test.iloc[:, :start]     # atributos
    Y_test_labels  = test.iloc[:, start:] # rótulos verdadeiros
    
    # obtendo os nomes dos rótulos
    labels_y_train = list(Y_train_labels.columns)
    labels_y_test = list(Y_test_labels.columns)
    
    # obtendo os nomes dos atributos
    attr_x_train = list(X_train_att.columns)
    attr_x_test = list(X_test_att.columns)
    
    # parametros do classificador base
    random_state = 0    
    n_estimators = 200

    # inicializa o classificador base
    rf = RandomForestClassifier(n_estimators = n_estimators, random_state = random_state)
    
    # treino
    rf.fit(X_train_att, Y_train_labels)
    
    # teste
    y_pred_a = rf.predict(X_test_att)

    # predições binárias
    y_pred_d = pd.DataFrame(rf.predict(X_test_att)) 

    # predições probabilísticas
    probabilities = rf.predict_proba(X_test_att)    
    
    # renomeando as colunas
    y_pred_d.columns = labels_y_test
    
    # obtendo os rótulos verdadeiros
    y_true_a = np.array(Y_test_labels)      # array
    y_true_d = pd.DataFrame(Y_test_labels)  # dataframe
    
    # setando nome do diretorio e arquivo para salvar
    true = (directory + "/y_true.csv")          # salva os rótulos verdadeiros
    pred = (directory + "/y_pred.csv")          # salva as predições binárias
    probaname1 = (directory + "/y_proba.csv")   # salva todas as predições probabilísticas
    probaname2 = (directory + "/y_proba_1.csv") # salva as predições probabilísticas para 1
    
    # salvando true labels and predict labels
    y_pred_d.to_csv(pred, index=False)
    y_true_d.to_csv(true, index=False)    

    # construindo a tabela com as predições probabilisticas
    # para salvar num formato de dataframe que pode ser usado no R
    ldf = []
    ldf2 = []
    for n in range(0, len(probabilities)):
      # print(" ", n)
      res = probabilities[n]
      res = pd.DataFrame(res)
      res.columns = [f'prob_{n+1}',f'prob_{n+1}']
      ldf.append(res)
      res2 = res.iloc[:, :1] # atributos
      ldf2.append(res2)
    
    
    # salvando
    print(" ")
    final = pd.concat(ldf, axis=1)
    final.to_csv(probaname1, index=False)
    
    final2 = pd.concat(ldf2, axis=1)
    final2.to_csv(probaname2, index=False)

    y_true = pd.read_csv(true)
    y_pred = pd.read_csv(pred)
      
    micro = average_precision_score(y_true, y_pred, average = "micro")
    macro = average_precision_score(y_true, y_pred, average = "macro")
      
    y_proba = pd.DataFrame([micro,macro]).T
    y_proba.columns = ["Micro-AUPRC", "Macro-AUPRC"]
    name = (directory + "/y_proba_mami.csv")
    y_proba.to_csv(name, index=False)


    

