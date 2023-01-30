"""
=================================
Ensemble Classifier Chain Example
=================================

An example of :class:`skml.ensemble.EnsembleClassifierChain`
"""

from sklearn.metrics import hamming_loss
from sklearn.metrics import accuracy_score
from sklearn.metrics import f1_score
from sklearn.metrics import precision_score
from sklearn.metrics import recall_score
from sklearn.model_selection import train_test_split
from sklearn.datasets import fetch_openml
from sklearn.ensemble import RandomForestClassifier
from sklearn.tree import DecisionTreeRegressor
from sklearn import datasets
from skml.ensemble import EnsembleClassifierChain
import numpy as np



X, Y = fetch_openml("yeast", version=4, return_X_y=True, parser="pandas")
Y = Y == "TRUE"
X_train, X_test, Y_train, Y_test = train_test_split(X, Y, test_size=0.2, random_state=0)


# X, y = load_dataset('yeast')
# X_train, X_test, y_train, y_test = train_test_split(X, Y)


n_chains = 5
random_state = 0
baseModel = DecisionTreeRegressor(random_state = random_state)
ensemble = EnsembleClassifierChain(baseModel)
ensemble.fit(X_train.values, Y_train.values)
y_pred = pd.DataFrame(ensemble.predict(X_test.values))


test_predictions.to_csv("y_pred.csv", index=False)
test[allLabels].to_csv("y_true.csv", index=False)

