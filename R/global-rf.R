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

###############################################################################
# SET WORKSAPCE                                                               #
###############################################################################
FolderRoot = "~/Global-Partitions"
FolderScripts = "~/Global-Partitions/R"


###############################################################################
# FUNCTION GATHER FILES FOLDS GLOBAL                                          #
#   Objective                                                                 #
#       Joins the configuration, training and test files in a single folder   #
#     running the PYTHON                                                        #
#   Parameters                                                                #
#       ds: specific dataset information                                      #
#       dataset_name: dataset name. It is used to save files                  #
#       number_folds: number of folds created                                 #
#       FolderConfifFiles: folder path                                        #
#   Return                                                                    #
#       configurations files                                                  #
###############################################################################
gather.files.python <- function(ds, 
                                dataset_name,
                                number_dataset, 
                                number_cores, 
                                number_folds, 
                                folderResults){
  
  f = 1
  # foldsParalel <- foreach(f = 1:number_folds) %dopar% {
  while(f<=number_folds){
    
    cat("\nFold: ", f)
    
    ###########################################################################
    FolderRoot = "~/Global-Partitions"
    FolderScripts = "~/Global-Partitions/R"
    
    ###########################################################################
    setwd(FolderScripts)
    source("libraries.R")
    
    setwd(FolderScripts)
    source("utils.R")
    
    ###########################################################################
    diretorios = directories(dataset_name, folderResults)
    
    ###########################################################################
    FolderSplit = paste(diretorios$folderGlobal, "/Split-", f, sep="")
    if(dir.exists(FolderSplit)==FALSE){dir.create(FolderSplit)}
    
    # names files
    nome.tr.csv = paste(dataset_name, "-Split-Tr-", f, ".csv", sep="")
    nome.ts.csv = paste(dataset_name, "-Split-Ts-", f, ".csv", sep="")
    nome.vl.csv = paste(dataset_name, "-Split-Vl-", f, ".csv", sep="")
    
    # train
    setwd(diretorios$folderCVTR)
    if(file.exists(nome.tr.csv) == TRUE){
      setwd(diretorios$folderCVTR)
      copia = paste(diretorios$folderCVTR, "/", nome.tr.csv, sep="")
      cola = paste(FolderSplit, "/", nome.tr.csv, sep="")
      file.copy(copia, cola, overwrite = TRUE)
    }
    
    # test
    setwd(diretorios$folderCVTS)
    if(file.exists(nome.ts.csv) == TRUE){
      setwd(diretorios$folderCVTS)
      copia = paste(diretorios$folderCVTS, "/", nome.ts.csv, sep="")
      cola = paste(FolderSplit, "/", nome.ts.csv, sep="")
      file.copy(copia, cola, overwrite = TRUE)
    }
    
    # validation
    setwd(diretorios$folderCVVL)
    if(file.exists(nome.vl.csv) == TRUE){
      setwd(diretorios$folderCVVL)
      copia = paste(diretorios$folderCVVL, "/", nome.vl.csv, sep="")
      cola = paste(FolderSplit, "/", nome.vl.csv, sep="")
      file.copy(copia, cola, overwrite = TRUE)
    }
    
    f = f + 1
    gc()
  }
  
  gc()
  cat("\n###############################################################")
  cat("\n# GLOBAL RF: END OF THE GATHER FILES FOLDS FUNCTION           #")
  cat("\n###############################################################")
  cat("\n\n")
}


##############################################################################
# FUNCTION EXECUTE PYTHON GLOBAL                                               #
#   Objective                                                                #
#       Tests global partitions                                              #
#   Parameters                                                               #
#       ds: specific dataset information                                     #
#       dataset_name: dataset name. It is used to save files.                #
#       number_folds: number of folds created                                #
#       Folder: folder path                                                  #
#   Return                                                                   #
#       configurations files                                                 #
##############################################################################
execute.global.python <- function(parameters,
                                  ds, 
                                  dataset_name, 
                                  number_folds, 
                                  number_cores, 
                                  folderResults){
  
  f = 1
  PYTHONGlobalParalel <- foreach(f = 1:number_folds) %dopar%{
  # while(f<=number_folds){
    
    #########################################################################
    cat("\nFold: ", f)
    
    ##########################################################################
    FolderRoot = "~/Global-Partitions"
    FolderScripts = "~/Global-Partitions/R"
    
    ##########################################################################
    setwd(FolderScripts)
    source("libraries.R")
    
    setwd(FolderScripts)
    source("utils.R")
    
    ##########################################################################
    diretorios = directories(dataset_name, folderResults)
    
    
    ##########################################################################
    FolderSplit = paste(diretorios$folderGlobal, "/Split-", f, sep="")
    
    
    ##########################################################################
    train.file.name = paste(FolderSplit, "/", dataset_name, 
                            "-Split-Tr-", f , ".csv", sep="")
    
    test.file.name = paste(FolderSplit, "/", dataset_name, 
                           "-Split-Ts-", f, ".csv", sep="")
    
    val.file.name = paste(FolderSplit, "/", dataset_name, 
                          "-Split-Vl-", f , ".csv", sep="")
    
    ##########################################################################
    setwd(FolderSplit)
    train = data.frame(read.csv(train.file.name))
    test = data.frame(read.csv(test.file.name))
    val = data.frame(read.csv(val.file.name))
    tv = rbind(train, val)
    
    
    ##########################################################################
    labels.indices = seq(parameters$Dataset.Info$LabelStart, parameters$Dataset.Info$LabelEnd, by=1)
    
    
    ##########################################################################
    mldr.treino = mldr_from_dataframe(train, labelIndices = labels.indices)
    mldr.teste = mldr_from_dataframe(test, labelIndices = labels.indices)
    
    
    ##########################################################################
    names.rotulos = colnames(train[,parameters$Dataset.Info$LabelStart:parameters$Dataset.Info$LabelEnd])
    
    
    ##################################################################
    # EXECUTE ECC PYTHON
    str.execute = paste("python3 ", diretorios$folderUtils,
                        "/random-forests.py ", 
                        train.file.name, " ",
                        val.file.name,  " ",
                        test.file.name, " ", 
                        start = as.numeric(ds$AttEnd), " ", 
                        FolderSplit,
                        sep="")
    
    # EXECUTA
    res = print(system(str.execute))
    cat("\n\n")
    
    if(res!=0){
      break
    }
    
    
    ###################
    setwd(FolderSplit)
    y_preds = data.frame(read.csv("y_pred.csv"))
    y_trues = data.frame(read.csv("y_true.csv"))
    y_probas = data.frame(read.csv("y_proba_1.csv"))
    
    
    #############################
    nomes.rotulos = colnames(y_trues)
    
    
    #####################################################################
    cat("\nSave original and pruned predictions")
    pred.o = paste(colnames(y_preds), "-pred", sep="")
    names(y_preds) = pred.o
    
    true.labels = paste(colnames(y_trues), "-true", sep="")
    names(y_trues) = true.labels
    
    proba = paste(names.rotulos, "-proba", sep="")
    names(y_probas) = proba
    
    all.predictions = cbind(y_preds, y_trues, y_probas)
    
    setwd(FolderSplit)
    write.csv(all.predictions, "folder-predictions.csv", row.names = FALSE)
    
    
    #####################################################################
    cat("\nPredictions")
    # predicoes <- function(y_trues, y_preds, folder){
    predicoes(y_trues, y_preds, FolderSplit, nomes.rotulos)
    
    
    #####################################################################
    cat("\nPlot ROC curve")
    # y_pred, y_proba, mldr.teste, folder
    plote.curva.roc(y_pred = y_preds, 
                    y_proba = y_probas, 
                    teste.dataset = mldr.teste, 
                    folder = FolderSplit)
    
    
    #####################################################################
    cat("\nApagando arquivos")
    unlink(train.file.name)
    unlink(test.file.name)
    unlink(val.file.name)
    
    # f = f + 1
    gc()
  }
  
  gc()
  cat("\n############################################################")
  cat("\n# GLOBAL RF: END OF FUNCTION EXECUTE                       #")
  cat("\n############################################################")
  cat("\n\n")
}



##################################################################################################
# FUNCTION EVALUATE GENERAL                                                                      #
#   Objective:                                                                                   #
#       Evaluate Multilabel                                                                      #
#   Parameters:                                                                                  #
#       ds: specific dataset information                                                         #
#       dataset_name: dataset name. It is used to save files.                                    #
#       number_folds: number of folds to be created                                              #
#       Folder: folder where the folds are                                                       #
#   Return:                                                                                      #
#       Confusion Matrix                                                                         #
##################################################################################################
evaluate.global.python <- function(ds,
                                   dataset_name,
                                   number_folds,
                                   number_cores,
                                   folderResults){
  
  apagar = c(0)
  resConfMatFinal = data.frame(apagar)
  
  f = 1
  avaliaParalel <- foreach (f = 1:number_folds) %dopar%{
    #while(f<=number_folds){
    
    #########################################################################
    cat("\nFold: ", f)
    
    FolderRoot = "~/Global-Partitions/"
    FolderScripts = paste(FolderRoot, "/R/", sep="")
    
    #########################################################################
    setwd(FolderScripts)
    source("utils.R")
    
    setwd(FolderScripts)
    source("libraries.R")
    
    ####################################################################
    diretorios = directories(dataset_name, folderResults)
    
    ####################################################################
    FolderSplit = paste(diretorios$folderGlobal, "/Split-", f, sep="")
    
    ####################################################################################
    # cat("\nAbrindo pred and true")
    setwd(FolderSplit)
    y_pred = data.frame(read.csv("y_pred.csv"))
    y_true = data.frame(read.csv("y_true.csv"))
    
    # cat("\nConvertendo em numerico")
    y_true2 = data.frame(sapply(y_true, function(x) as.numeric(as.character(x))))
    y_true3 = mldr_from_dataframe(y_true2 , labelIndices = seq(1,ncol(y_true2 )), name = "y_true2")
    y_pred2 = sapply(y_pred, function(x) as.numeric(as.character(x)))
    
    
    ###############################################################
    # cat("\nsalvando")
    salva3 = paste("ConfMatFold-", f, ".txt", sep="")
    setwd(FolderSplit)
    sink(file=salva3, type="output")
    confmat = multilabel_confusion_matrix(y_true3, y_pred2)
    print(confmat)
    sink()
    
    ###############################################################
    # cat("\nmatriz de confusão")
    resConfMat = multilabel_evaluate(confmat)
    resConfMat = data.frame(resConfMat)
    names(resConfMat) = paste("Fold-", f, sep="")
    setwd(FolderSplit)
    write.csv(resConfMat, "ResConfMat.csv")
    
    
    ###############################################################
    conf.mat = data.frame(confmat$TPl, confmat$FPl,
                          confmat$FNl, confmat$TNl)
    names(conf.mat) = c("TP", "FP", "FN", "TN")
    
    
    # porcentagem
    conf.mat.perc = data.frame(conf.mat/nrow(y_true))
    names(conf.mat.perc) = c("TP.perc", "FP.perc", "FN.perc", "TN.perc")
    
    # calculando o total de rótulos classificados errados
    wrong = conf.mat$FP + conf.mat$FN
    
    # calculando a porcentagem de rótulos classificados errados
    wrong.perc = wrong/nrow(y_true)
    
    # calculando o total de rótulos classificados corretamente
    correct = conf.mat$TP + conf.mat$TN
    
    # calculando a porcentagem de rótulos classificados corretamente
    correct.perc = correct/nrow(y_true)
    
    conf.mat = data.frame(conf.mat, conf.mat.perc, wrong, correct, 
                          wrong.perc, correct.perc)
    
    setwd(FolderSplit)
    write.csv(conf.mat, "matrix-confusion.csv")
    
    #f = f + 1
    gc()
  }
  
  gc()
  cat("\n####################################################################")
  cat("\n# GLOBAL RF: END OF THE EVALUATION MISCELLANEOUS FUNCTION          #")
  cat("\n####################################################################")
  cat("\n\n\n\n")
}




##################################################################################################
# FUNCTION GATHER PREDICTS GLOBAL PARTITIONS                                                     #
#   Objective                                                                                    #
#      Evaluates the global partitions                                                           #
#   Parameters                                                                                   #
#       ds: specific dataset information                                                         #
#       dataset_name: dataset name. It is used to save files.                                    #
#       number_folds: number of folds created                                                    #
#       Folder: path of global partition results                                                 #
#   Return                                                                                       #
#       Assessment measures for each global partition                                            #
##################################################################################################
gather.eval.global.python <- function(ds, 
                                      dataset_name, 
                                      number_folds, 
                                      number_cores, 
                                      folderResults){
  
  diretorios = directories(dataset_name, folderResults) 
  
  retorno = list()
  
  # vector with names measures
  measures = c("accuracy","average-precision","clp",
               "coverage","F1","hamming-loss","macro-AUC",
               "macro-F1","macro-precision","macro-recall",
               "margin-loss","micro-AUC","micro-F1",
               "micro-precision","micro-recall","mlp",
               "one-error","precision","ranking-loss",
               "recall","subset-accuracy","wlp")
  
  # dta frame
  confMatFinal = data.frame(measures)
  folds = c("")
  final.proba.micro.auc = c(0)
  final.proba.macro.auc = c(0)
  final.proba.auc = c(0)
  final.proba.ma.mi.auc = c(0)
  
  # from fold = 1 to number_labels
  f = 1
  while(f<= number_folds){
    cat("\nFold: ", f)
    
    FolderSplit = paste(diretorios$folderGlobal, "/Split-", f, sep="")
    setwd(FolderSplit)
    
    # cat("\n\tOpen ResConfMat ", f)
    confMat = data.frame(read.csv(paste(FolderSplit, "/ResConfMat.csv", sep="")))
    names(confMat) = c("Measures", "Fold")
    confMatFinal = cbind(confMatFinal, confMat$Fold) 
    
    folds[f] = paste("Fold-", f, sep="")
    
    setwd(FolderSplit)
    proba.auc = data.frame(read.csv("proba-auc.csv"))
    names(proba.auc) = c("fold", "value")
    final.proba.auc = rbind(final.proba.auc, proba.auc)
    
    setwd(FolderSplit)
    proba.micro.auc = data.frame(read.csv("proba-micro-auc.csv"))
    names(proba.micro.auc) = c("fold", "value")
    final.proba.micro.auc = rbind(final.proba.micro.auc, proba.micro.auc)
    
    setwd(FolderSplit)
    proba.macro.auc = data.frame(read.csv("proba-macro-auc.csv"))
    names(proba.macro.auc) = c("fold", "value")
    final.proba.macro.auc = rbind(final.proba.macro.auc, proba.macro.auc)
    
    setwd(FolderSplit)
    proba.ma.mi.auc = data.frame(read.csv("y_proba_mami.csv"))
    final.proba.ma.mi.auc = rbind(final.proba.ma.mi.auc, proba.ma.mi.auc)
    
    f = f + 1
    gc()
  } 
  
  cat("\nsave measures")
  names(confMatFinal) = c("Measures", folds)
  write.csv(confMatFinal, 
            paste(diretorios$folderGlobal, "/All-Folds-Global.csv", sep=""),
            row.names = FALSE)
  
  final.proba.auc = final.proba.auc[-1,]
  fold = seq(1, parameters$Number.Folds, by =1)
  final.proba.auc = data.frame(fold, auc = final.proba.auc$value)
  
  final.proba.micro.auc = final.proba.micro.auc[-1,]
  fold = seq(1, parameters$Number.Folds, by =1)
  final.proba.micro.auc = data.frame(fold, micro.auc = final.proba.micro.auc$value)
  
  final.proba.macro.auc = final.proba.macro.auc[-1,]
  fold = seq(1, parameters$Number.Folds, by =1)
  final.proba.macro.auc = data.frame(fold, macro.auc = final.proba.macro.auc$value)
  
  final.proba.ma.mi.auc = final.proba.ma.mi.auc[-1,]
  fold = seq(1, parameters$Number.Folds, by =1)
  final.proba.ma.mi.auc = data.frame(fold, final.proba.ma.mi.auc)
  
  setwd(diretorios$folderGlobal)
  write.csv(final.proba.auc, "proba-auc.csv", row.names = FALSE)  
  write.csv(final.proba.macro.auc, "proba-macro-auc.csv", row.names = FALSE)  
  write.csv(final.proba.micro.auc, "proba-micro-auc.csv", row.names = FALSE)
  write.csv(final.proba.ma.mi.auc, "proba-ma-mi-auprc.csv", row.names = FALSE)  
  
  # calculando a média dos 10 folds para cada medida
  media = data.frame(apply(confMatFinal[,-1], 1, mean))
  media = cbind(measures, media)
  names(media) = c("Measures", "Mean10Folds")
  
  setwd(diretorios$folderGlobal)
  write.csv(media, 
            paste(diretorios$folderGlobal, "/Mean10Folds.csv", sep=""), 
            row.names = FALSE)
  
  mediana = data.frame(apply(confMatFinal[,-1], 1, median))
  mediana = cbind(measures, mediana)
  names(mediana) = c("Measures", "Median10Folds")
  
  setwd(diretorios$folderGlobal)
  write.csv(mediana, 
            paste(diretorios$folderGlobal, "/Median10Folds.csv", sep=""),
            row.names = FALSE)
  
  dp = data.frame(apply(confMatFinal[,-1], 1, sd))
  dp = cbind(measures, dp)
  names(dp) = c("Measures", "SD10Folds")
  
  setwd(diretorios$folderGlobal)
  write.csv(dp,  
            paste(diretorios$folderGlobal, 
                  "/standard-deviation-10-folds.csv", sep=""), 
            row.names = FALSE)
  
  
  gc()
  cat("\n#############################################################")
  cat("\n# RF GLOBAL: END OF THE FUNCTION GATHER EVALUATED           #") 
  cat("\n#############################################################")
  cat("\n\n\n\n")
}



##################################################################################################
# Please, any errors, contact us: elainececiliagatto@gmail.com                                   #
# Thank you very much!                                                                           #
################################################################################################
