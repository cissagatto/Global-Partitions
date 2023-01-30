import sys
from chai import Chai
from sklearn.ensemble import RandomForestClassifier
from sklearn.model_selection import GridSearchCV
from sklearn.pipeline import Pipeline
from sklearn.metrics import hamming_loss
import scipy.sparse as sp
from skml.ensemble import EnsembleClassifierChain
from skmultilearn.problem_transform import ClassifierChain
from sklearn.linear_model import LogisticRegression
from sklearn.tree import DecisionTreeRegressor
import numpy as np
import pandas as pd


train = pd.read_csv("/home/biomal/skml/data/train.csv")
valid = pd.read_csv("/home/biomal/skml/data/valid.csv")
test = pd.read_csv("/home/biomal/skml/data/test.csv")
diretorio = "/home/biomal/skml"
train = pd.concat([train,valid],axis=0).reset_index(drop=True)

X_train = train.iloc[:, :912]
Y_train = train.iloc[:, 912:]
X_test = test.iloc[:, :912]
Y_test = test.iloc[:, 912:]

n_chains = 5
random_state = 0
baseModel = DecisionTreeRegressor(random_state = random_state)


class TestECC(Chai):
    def test_ecc_fit_predict(self):
        ensemble = EnsembleClassifierChain(RandomForestClassifier(),
                                           threshold=.6,
                                           max_features=1.0)
        ensemble.fit(X_train.values, Y_train.values)
        y_pred = ensemble.predict(X_test.values)
        hamming_loss(Y_test, y_pred)

    def test_ecc_pipeline(self):
        pl = Pipeline([("cc",
                        EnsembleClassifierChain(RandomForestClassifier()))])
        pl.fit(X, y)

    def test_ecc_gridsearch(self):
        ecc = EnsembleClassifierChain(RandomForestClassifier())
        cv = GridSearchCV(ecc,
                          {'estimator__n_estimators': [10, 20]},
                          n_jobs=-1)
        cv.fit(X, y)

    def test_ecc_always_present(self):
        # Test that ecc works with classes that are always present or absent.
        ecc = EnsembleClassifierChain(RandomForestClassifier())
        X_2 = np.array([[2, 3], [4, 0]])
        y_2 = np.array([[1, 1], [1, 0]])
        ecc.fit(X, y)

    def test_ecc_predict_multi_instances(self):
        clf = EnsembleClassifierChain(RandomForestClassifier())
        clf.fit(X, y)

        y_pred = clf.predict(X)
        assert_true(y_pred.shape[0] == y.shape[0])

    def test_ecc_fit_predict_sparse(self):
        # test fit/predict of sparse matrices
        for sparse in [sp.csr_matrix, sp.csc_matrix, sp.coo_matrix,
                       sp.dok_matrix, sp.lil_matrix]:
            clf = EnsembleClassifierChain(RandomForestClassifier())
            clf.fit(X, sparse(y))
            y_pred = clf.predict(X)
            assert_true(sp.issparse(y_pred))
