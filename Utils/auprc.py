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
from sklearn.metrics import average_precision_score
import matplotlib.pyplot as plt
import pandas as pd

y_true = pd.read_csv(sys.argv[1])
y_proba = pd.read_csv(sys.argv[2])
y_pred_bin = pd.read_csv(sys.argv[3])
name_proba = sys.argv[4]
name_bin = sys.argv[5]

# y_true = pd.read_csv("/dev/shm/grf-GpositiveGO/Global/Split-1/y_true.csv")
# y_proba = pd.read_csv("/dev/shm/grf-GpositiveGO/Global/Split-1/y_proba_1.csv")
# y_pred_bin = pd.read_csv("/dev/shm/grf-GpositiveGO/Global/Split-1/y_pred_bin.csv")
# name_proba= "/dev/shm/grf-GpositiveGO/Global/Split-1/auprc_proba.csv"
# name_bin = "/dev/shm/grf-GpositiveGO/Global/Split-1/auprc_bin.csv"

# usando probabilidades
micro_proba = average_precision_score(y_true, y_proba, average = "micro")
macro_proba = average_precision_score(y_true, y_proba, average = "macro")

# usando predições binárias
# micro_bin = average_precision_score(y_true, y_pred_bin, average = "micro")
# macro_bin = average_precision_score(y_true, y_pred_bin, average = "macro")

res_proba = pd.DataFrame([micro_proba, macro_proba]).T
res_proba .columns = ["Micro-AUPRC", "Macro-AUPRC"]
res_proba.to_csv(name_proba, index=False)

# res_bin = pd.DataFrame([micro_bin, macro_bin]).T
# res_bin.columns = ["Micro-AUPRC", "Macro-AUPRC"]
# res_bin.to_csv(name_bin, index=False)

