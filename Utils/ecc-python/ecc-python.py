import sys
import pandas as pd
import numpy as np
from ecc import ECC
from skmultilearn.problem_transform import BinaryRelevance
from sklearn.tree import DecisionTreeRegressor


if __name__ == '__ecc-python__':

    n_chains = 5
    random_state = 0
    baseModel = DecisionTreeRegressor(random_state = random_state)
    
    #train = pd.read_csv("/dev/shm/eg-GpositiveGO/Dataset/GpositiveGO/CrossValidation/Tr/GpositiveGO-Split-Tr-1.csv")
    #valid = pd.read_csv("/dev/shm/eg-GpositiveGO/Dataset/GpositiveGO/CrossValidation/Vl/GpositiveGO-Split-Vl-1.csv")
    #test = pd.read_csv("/dev/shm/eg-GpositiveGO/Dataset/GpositiveGO/CrossValidation/Ts/GpositiveGO-Split-Ts-1.csv")
    
    train = pd.read_csv(sys.argv[1])
    valid = pd.read_csv(sys.argv[2])
    test = pd.read_csv(sys.argv[3])
    diretorio = sys.argv[4]
    
    # junto o dataset de validação com o treino
    train = pd.concat([train,valid],axis=0).reset_index(drop=True)
    
    train.shape[0] # numero de linhas
    train.shape[1] # número de colunas
    train.shape # dimensão do dataframe
    train.info
    train.describe
    
    test.shape[0] # numero de linhas
    test.shape[1] # número de colunas
    test.shape # dimensão do dataframe
    test.info
    test.describe
    
    # atributos de treino
    X_train = train.iloc[:, :start]
    X_train.shape
    
    # rótulos de treino
    Y_train = train.iloc[:, start:]
    Y_train.shape
    
    # atributos de teste
    X_test = test.iloc[:, :start]
    X_test.shape
    
    # rótulos de test
    Y_test = test.iloc[:, start:]
    Y_test.shape
    
    # obtendo os nomes dos rótulos do dataset
    labels_y_train = list(Y_train.columns)
    labels_y_test = list(Y_test.columns)
    attr_x_train = list(X_train.columns)
    attr_x_test = list(X_test.columns)
    
    true = (diretorio + "/y_true.csv")
    pred = (diretorio + "/y_pred.csv")
    
    test_predictions = pd.DataFrame(ecc.predict(x_test))
    train_predictions = pd.DataFrame(ecc.predict(x_train))
    test_predictions.to_csv("y_pred.csv", index=False)
    test[allLabels].to_csv("y_true.csv", index=False)
    
