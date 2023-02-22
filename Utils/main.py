import sys
from sklearn.ensemble import RandomForestClassifier
import numpy as np
import pandas as pd

if __name__ == '__main__':   
    
   
    train = pd.read_csv(sys.argv[1])
    valid = pd.read_csv(sys.argv[2])
    test = pd.read_csv(sys.argv[3])
    start = int(sys.argv[4])
    directory = sys.argv[5]
    
    # train = pd.read_csv("/dev/shm/eg-GpositiveGO/Global/Split-1/GpositiveGO-Split-Tr-1.csv")
    # test = pd.read_csv("/dev/shm/eg-GpositiveGO/Global/Split-1/GpositiveGO-Split-Ts-1.csv")
    # validation = pd.read_csv("/dev/shm/eg-GpositiveGO/Global/Split-1/GpositiveGO-Split-Vl-1.csv")
    # start = 912
    # directory = "/dev/shm/eg-GpositiveGO/Global/Split-1"
    
    train = pd.concat([train,valid],axis=0).reset_index(drop=True)
    
    # TREINO
    X_train = train.iloc[:, :start] # atributos 
    Y_train = train.iloc[:, start:] # rótulos 
    
    # TESTE
    X_test = test.iloc[:, :start] # atributos
    Y_test = test.iloc[:, start:] # rótulos verdadeiros
    
    labels_y_train = list(Y_train.columns)
    labels_y_test = list(Y_test.columns)
    attr_x_train = list(X_train.columns)
    attr_x_test = list(X_test.columns)
    
    random_state = 0
    n_estimators = 200
    rf = RandomForestClassifier(n_estimators = n_estimators, random_state = random_state)
    rf.fit(X_train, Y_train)
    
    y_pred = pd.DataFrame(rf.predict(X_test)) 
    y_pred.columns = labels_y_test
    y_true = pd.DataFrame(Y_test)
    
    true = (directory + "/y_true.csv")
    pred = (directory + "/y_pred.csv")
    
    y_pred.to_csv(pred, index=False)
    y_true.to_csv(true, index=False)

