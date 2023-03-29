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



###############################################################################
# SET WORKSAPCE                                                               #
###############################################################################
FolderRoot = "~/Global-Partitions"
FolderScripts = "~/Global-Partitions/R"


###############################################################################
#
###############################################################################
gather.files.python <- function(parameters){
  
  f = 1
  foldsParalel <- foreach(f = 1:parameters$Config.File$Number.Folds) %dopar% {
  # while(f<=number_folds){
    
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
    FolderSplit = paste(parameters$Directories$FolderGlobal, "/Split-", 
                        f, sep="")
    if(dir.exists(FolderSplit)==FALSE){dir.create(FolderSplit)}
    
    ###########################################################################
    # names files
    nome.tr.csv = paste(parameters$Config.File$Dataset.Name , 
                        "-Split-Tr-", f, ".csv", sep="")
    nome.ts.csv = paste(parameters$Config.File$Dataset.Name, 
                        "-Split-Ts-", f, ".csv", sep="")
    nome.vl.csv = paste(parameters$Config.File$Dataset.Name, 
                        "-Split-Vl-", f, ".csv", sep="")
    
    # train
    setwd(parameters$Directories$FolderCVTR)
    if(file.exists(nome.tr.csv) == TRUE){
      setwd(parameters$Directories$FolderCVTR)
      copia = paste(parameters$Directories$FolderCVTR, "/", 
                    nome.tr.csv, sep="")
      cola = paste(FolderSplit, "/", nome.tr.csv, sep="")
      file.copy(copia, cola, overwrite = TRUE)
    }
    
    # test
    setwd(parameters$Directories$FolderCVTS)
    if(file.exists(nome.ts.csv) == TRUE){
      setwd(parameters$Directories$FolderCVTS)
      copia = paste(parameters$Directories$FolderCVTS, "/", 
                    nome.ts.csv, sep="")
      cola = paste(FolderSplit, "/", nome.ts.csv, sep="")
      file.copy(copia, cola, overwrite = TRUE)
    }
    
    # validation
    setwd(parameters$Directories$FolderCVVL)
    if(file.exists(nome.vl.csv) == TRUE){
      setwd(parameters$Directories$FolderCVVL)
      copia = paste(parameters$Directories$FolderCVVL, "/", 
                    nome.vl.csv, sep="")
      cola = paste(FolderSplit, "/", nome.vl.csv, sep="")
      file.copy(copia, cola, overwrite = TRUE)
    }
    
    # f = f + 1
    gc()
  }
  
  gc()
  cat("\n###############################################################")
  cat("\n# GLOBAL RF: END OF THE GATHER FILES FOLDS FUNCTION           #")
  cat("\n###############################################################")
  cat("\n\n")
}


##############################################################################
#
##############################################################################
execute.global.python <- function(parameters){
  
  f = 1
  RfGlobalParalel <- foreach(f = 1:parameters$Config.File$Number.Folds) %dopar%{
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
    
    
    ###########################################################################
    FolderSplit = paste(parameters$Directories$FolderGlobal, "/Split-", f, sep="")
    if(dir.exists(FolderSplit)==FALSE){dir.create(FolderSplit)}
    
    
    ###########################################################################
    # names files
    nome.tr.csv = paste(FolderSplit, "/", parameters$Config.File$Dataset.Name , 
                        "-Split-Tr-", f, ".csv", sep="")
    nome.ts.csv = paste(FolderSplit, "/", parameters$Config.File$Dataset.Name, 
                        "-Split-Ts-", f, ".csv", sep="")
    nome.vl.csv = paste(FolderSplit, "/", parameters$Config.File$Dataset.Name, 
                        "-Split-Vl-", f, ".csv", sep="")
    
    
    ##########################################################################
    train = data.frame(read.csv(nome.tr.csv))
    test = data.frame(read.csv(nome.ts.csv))
    val = data.frame(read.csv(nome.vl.csv))
    tv = rbind(train, val)
    
    
    ##########################################################################
    labels.indices = seq(parameters$Dataset.Info$LabelStart, 
                         parameters$Dataset.Info$LabelEnd, by=1)
    
    
    ##########################################################################
    mldr.treino = mldr_from_dataframe(train, labelIndices = labels.indices)
    mldr.teste = mldr_from_dataframe(test, labelIndices = labels.indices)
    
    
    ##################################################################
    str.execute = paste("python3 ", parameters$Directories$FolderUtils,
                        "/global.py ", 
                        nome.tr.csv, " ",
                        nome.vl.csv,  " ",
                        nome.ts.csv, " ", 
                        start = as.numeric(parameters$Dataset.Info$AttEnd), " ", 
                        FolderSplit,
                        sep="")
    
    # EXECUTA
    start <- proc.time()
    res = print(system(str.execute))
    tempo = data.matrix((proc.time() - start))
    tempo = data.frame(t(tempo))
    write.csv(tempo, paste(FolderSplit, "/runtime-fold.csv", sep=""))
    
    
    if(res!=0){
      break
    }
    
    
    ###################
    setwd(FolderSplit)
    y_pred_bin = data.frame(read.csv("y_pred_bin.csv"))
    y_test_true = data.frame(read.csv("y_true.csv"))
    y_proba_o = data.frame(read.csv("y_proba_o.csv"))     
    
    
    ###################
    # reorganizando as predições do PROBA
    
    # pego os nomes das colunas originais do proba
    nomes.proba = colnames(y_proba_o)
    
    # total de linhas
    linhas = ncol(y_proba_o)
    
    # total de nomes que quero gerar
    m = (linhas/2)+1
    
    # gerando os nomes das colunas que estou interessada
    nomes.2 = c("")
    a = 0
    while(a<m){
      nomes.2[a] = paste("prob_", a-1, "_1", sep="")
      a = a + 1
    }
    
    probabilidades = y_proba_o %>% select(all_of(nomes.2))
    names(probabilidades) = parameters$Names.Labels$Labels
    
    if(nrow(probabilidades)!=nrow(test)){
      cat("\nSomething is wrong with probabilities predictions!")
      break
    }
    
    # salvando as probabilidades corretas
    write.csv(probabilidades, paste(FolderSplit, "/y_proba_1.csv", sep=""),
              row.names = FALSE)
    
    
    #####################################################################
    # Computing AU PRC MICRO AND MACRO
    nome.true = paste(FolderSplit, "/y_true.csv", sep="")
    nome.proba = paste(FolderSplit, "/y_proba_1.csv", sep="")
    nome.pred.bin = paste(FolderSplit, "/y_pred_bin.csv", sep="")
    save.proba = paste(FolderSplit, "/auprc_bin.csv", sep="")
    save.bin = paste(FolderSplit, "/auprc_proba.csv", sep="")
    
    str.execute = paste("python3 ",
                        parameters$Directories$FolderUtils,
                        "/auprc.py ",
                        nome.true, " ",
                        nome.proba, " ",
                        nome.pred.bin, " ",
                        save.proba, " ",
                        save.bin, " ",
                        sep="")
    
    # EXECUTA
    res = print(system(str.execute))
    
    if(res!=0){
      break
    }
    
    
    #####################################################################
    cat("\nPlot ROC curve")
    roc.curva(pred_bin = y_pred_bin,
              pred_proba = probabilidades,
              test = mldr.teste,
              f = f,
              Folder = FolderSplit)

    
    ####################################################################
    cat("\nApagando arquivos")
    unlink(nome.tr.csv)
    unlink(nome.ts.csv)
    unlink(nome.vl.csv)
    
    # f = f + 1
    gc()
  }
  
  gc()
  cat("\n############################################################")
  cat("\n# GLOBAL RF: END OF FUNCTION EXECUTE                       #")
  cat("\n############################################################")
  cat("\n\n")
}



############################################################################
#
############################################################################
evaluate.global.python <- function(parameters){
  
  f = 1
  avaliaParalel <- foreach (f = 1:parameters$Config.File$Number.Folds) %dopar%{
  # while(f<=parameters$Config.File$Number.Folds){
    
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
    
    
    ###########################################################################
    FolderSplit = paste(parameters$Directories$FolderGlobal, "/Split-", f, sep="")
    if(dir.exists(FolderSplit)==FALSE){dir.create(FolderSplit)}
    
    ####################################################################################
    setwd(FolderSplit)
    y_pred = data.frame(read.csv("y_pred_bin.csv"))
    y_true = data.frame(read.csv("y_true.csv"))
    y_proba = data.frame(read.csv("y_proba_1.csv"))
    
    ####################################################################################
    y_true2 = data.frame(sapply(y_true, function(x) as.numeric(as.character(x))))
    y_true3 = mldr_from_dataframe(y_true2, 
                                  labelIndices = seq(1,ncol(y_true2 )), 
                                  name = "y_true2")
    y_pred_bin = sapply(y_pred, function(x) as.numeric(as.character(x)))
    y_proba2 = sapply(y_proba, function(x) as.numeric(as.character(x)))
    
    
    ####################################################################################
    y_pred_threshold <- data.frame(as.matrix(fixed_threshold(y_proba2, 
                                                             threshold = 0.5)))
    
    # identical(y_pred_bin, y_pred_threshold)
    
    ##############################################
    cat("\nInformações das predições")
    predictions.information(nomes.rotulos = parameters$Names.Labels$Labels, 
                            proba = y_proba, 
                            preds = y_pred, 
                            trues = y_true,
                            thr = y_pred_threshold,
                            folder = FolderSplit)
    
    
    
    ###############################################################
    salva.4 = paste("Bin-ConfMatFold-", f, ".txt", sep="")
    setwd(FolderSplit)
    sink(file=salva.4, type="output")
    confmat.bin = multilabel_confusion_matrix(y_true3, y_pred_bin)
    print(confmat.bin)
    sink()
    
    
    ###############################################################
    salva.5 = paste("Threshdol-ConfMatFold-", f, ".txt", sep="")
    setwd(FolderSplit)
    sink(file=salva.5, type="output")
    confmat.thr = multilabel_confusion_matrix(y_true3, y_pred_threshold)
    print(confmat.thr)
    sink()
    
    
    ###############################################################
    resConfMat = multilabel_evaluate(confmat.bin)
    resConfMat = data.frame(resConfMat)
    names(resConfMat) = paste("Fold-", f, sep="")
    setwd(FolderSplit)
    write.csv(resConfMat, "Bin-ResConfMat.csv")
    
    
    ###############################################################
    rm(resConfMat)
    resConfMat = multilabel_evaluate(confmat.thr)
    resConfMat = data.frame(resConfMat)
    names(resConfMat) = paste("Fold-", f, sep="")
    setwd(FolderSplit)
    write.csv(resConfMat, "Thr-ResConfMat.csv")
    
    
    ###############################################################
    conf.mat.bin = data.frame(confmat.bin$TPl, confmat.bin$FPl,
                              confmat.bin$FNl, confmat.bin$TNl)
    names(conf.mat.bin) = c("TP", "FP", "FN", "TN")
    conf.mat.perc = data.frame(conf.mat.bin/nrow(y_true))
    names(conf.mat.perc) = c("TP.perc", "FP.perc", "FN.perc", "TN.perc")
    wrong = conf.mat.bin$FP + conf.mat.bin$FN
    wrong.perc = wrong/nrow(y_true)
    correct = conf.mat.bin$TP + conf.mat.bin$TN
    correct.perc = correct/nrow(y_true)
    conf.mat.bin.2 = data.frame(conf.mat.bin, conf.mat.perc, wrong, correct, 
                          wrong.perc, correct.perc)
    setwd(FolderSplit)
    write.csv(conf.mat.bin.2, "utiml-bin-matrix-confusion.csv")
    
    
    ###############################################################
    conf.mat.thr = data.frame(confmat.thr$TPl, confmat.thr$FPl,
                          confmat.thr$FNl, confmat.thr$TNl)    
    names(conf.mat.thr) = c("TP", "FP", "FN", "TN")
    conf.mat.perc = data.frame(conf.mat.thr/nrow(y_true))
    names(conf.mat.perc) = c("TP.perc", "FP.perc", "FN.perc", "TN.perc")
    wrong = conf.mat.thr$FP + conf.mat.thr$FN
    wrong.perc = wrong/nrow(y_true)
    correct = conf.mat.thr$TP + conf.mat.thr$TN
    correct.perc = correct/nrow(y_true)
    conf.mat.thr.2 = data.frame(conf.mat.thr, conf.mat.perc, wrong, correct, 
                          wrong.perc, correct.perc)
    setwd(FolderSplit)
    write.csv(conf.mat.thr.2, "utiml-thr-matrix-confusion.csv")
    
    
    #####################################################################
    cat("\nSave original and pruned predictions")
    bin = paste(parameters$Names.Labels$Labels, "-pred", sep="")
    names(y_pred) = bin
    
    thr = paste(parameters$Names.Labels$Labels, "-thr", sep="")
    names(y_pred_threshold) = thr
    
    test.labels = paste(parameters$Names.Labels$Labels, "-true", sep="")
    names(y_true) = test.labels
    
    proba = paste(parameters$Names.Labels$Labels, "-proba", sep="")
    names(y_proba) = proba
    
    all.predictions = cbind(y_proba, y_pred_threshold, y_pred, y_true)
    
    setwd(FolderSplit)
    write.csv(all.predictions, "folder-predictions.csv", row.names = FALSE)
    
    
    ###############################################################
    #f = f + 1
    gc()
  }
  
  gc()
  cat("\n####################################################################")
  cat("\n# GLOBAL RF: END OF THE EVALUATION MISCELLANEOUS FUNCTION          #")
  cat("\n####################################################################")
  cat("\n\n\n\n")
}




###########################################################################
#
###########################################################################
gather.eval.global.python <- function(parameters){
  
  # vector with names measures
  measures = c("accuracy", "average-precision", "clp", "coverage",
               "F1", "hamming-loss", "macro-AUC", "macro-F1", 
               "macro-precision", "macro-recall", "margin-loss", 
               "micro-AUC","micro-F1", "micro-precision",
               "micro-recall", "mlp", "one-error", "precision", 
               "ranking-loss", "recall", "subset-accuracy", "wlp")
  
  
  # dta frame
  folds = c(0)
  
  confMatFinalTHR = data.frame(measures)
  confMatFinalBIN = data.frame(measures)
  
  final.proba.auc = data.frame()
  final.proba.micro.auc =  data.frame()
  final.proba.macro.auc =  data.frame()
  
  final.bin.auc =  data.frame()
  final.bin.micro.auc =  data.frame()
  final.bin.macro.auc =  data.frame()
  
  final.auprc = data.frame()
  
  final.runtime =  data.frame()
  
  # from fold = 1 to number_labels
  f = 1
  while(f<=parameters$Config.File$Number.Folds){
    
    cat("\nFold: ", f)
        
    folderSplit = paste(parameters$Directories$FolderGlobal, "/Split-", f, sep="")
    
    setwd(folderSplit)
    
    #########################################################################
    confMatTHR = data.frame(read.csv(paste(folderSplit, "/Thr-ResConfMat.csv", sep="")))
    names(confMatTHR) = c("Measures", "Fold")
    
    confMatBIN = data.frame(read.csv(paste(folderSplit, "/Bin-ResConfMat.csv", sep="")))
    names(confMatBIN) = c("Measures", "Fold")
    
    #########################################################################
    confMatTHR[is.na(confMatTHR)] <- 0
    confMatBIN[is.na(confMatBIN)] <- 0
    
    #########################################################################
    confMatFinalTHR = cbind(confMatFinalTHR, confMatTHR$Fold) 
    folds[f] = paste("Fold-", f, sep="")
    
    confMatFinalBIN = cbind(confMatFinalBIN, confMatBIN$Fold) 
    folds[f] = paste("Fold-", f, sep="")
    
    #########################################################################
    auprc  = data.frame(read.csv("auprc_bin.csv"))       
    final.auprc = rbind(final.auprc, auprc)
    
    ##################
    pred.auc = data.frame(read.csv("roc-bin-auc.csv"))
    names(pred.auc) = c("fold", "value")
    final.bin.auc = rbind(final.bin.auc, pred.auc)
    
    pred.micro.auc = data.frame(read.csv("roc-bin-micro-auc.csv"))
    names(pred.micro.auc) = c("fold", "value")
    final.bin.micro.auc = rbind(final.bin.micro.auc, pred.micro.auc)
    
    pred.macro.auc = data.frame(read.csv("roc-bin-macro-auc.csv"))
    names(pred.macro.auc) = c("fold", "value")
    final.bin.macro.auc = rbind(final.bin.macro.auc, pred.macro.auc)
    
    
    #################################
    proba.auc = data.frame(read.csv("roc-proba-auc.csv"))
    names(proba.auc) = c("fold", "value")
    final.proba.auc = rbind(final.proba.auc, proba.auc)
    
    proba.micro.auc = data.frame(read.csv("roc-proba-micro-auc.csv"))
    names(proba.micro.auc) = c("fold", "value")
    final.proba.micro.auc = rbind(final.proba.micro.auc, proba.micro.auc)
    
    proba.macro.auc = data.frame(read.csv("roc-proba-macro-auc.csv"))
    names(proba.macro.auc) = c("fold", "value")
    final.proba.macro.auc = rbind(final.proba.macro.auc, proba.macro.auc)
    
    #################################
    runtime = data.frame(read.csv("runtime-fold.csv"))
    names(runtime) = c("fold", "user.self", "sys.self",
                       "elapsed","user.child","sys.child")
    final.runtime = rbind(final.runtime, runtime)
    
    #################################
    f = f + 1
    gc()
  } 
  
  
  ###########################################
  setwd(parameters$Directories$FolderGlobal)
  names(confMatFinalTHR) = c("Measures", folds)
  confMatFinalTHR[is.na(confMatFinalTHR)] <- 0
  write.csv(confMatFinalTHR, "Thr-Test-Evaluated.csv", row.names = FALSE)
  
  
  ###########################################
  setwd(parameters$Directories$FolderGlobal)
  names(confMatFinalBIN) = c("Measures", folds)
  confMatFinalBIN[is.na(confMatFinalBIN)] <- 0
  write.csv(confMatFinalBIN, "Bin-Test-Evaluated.csv", row.names = FALSE)
  
  # identical(confMatFinalTHR, confMatFinalBIN)
  
  ###########################################
  fold = seq(1, number_folds, by =1)
  
  ###########################################
  final.micro.auprc = data.frame(fold, Micro.AUPRC = final.auprc$Micro.AUPRC)
  setwd(parameters$Directories$FolderGlobal)
  write.csv(final.micro.auprc, "auprc-micro.csv", row.names = FALSE)  
  
  ###########################################
  final.macro.auprc = data.frame(fold, Macro.AUPRC = final.auprc$Macro.AUPRC)
  setwd(parameters$Directories$FolderGlobal)
  write.csv(final.macro.auprc, "auprc-macro.csv", row.names = FALSE)  
  
  ###########################################
  final.proba.auc.2 = data.frame(fold, auc = final.proba.auc$value)
  final.proba.micro.auc.2 = data.frame(fold, micro.auc = final.proba.micro.auc$value)
  final.proba.macro.auc.2 = data.frame(fold, macro.auc = final.proba.macro.auc$value)
  setwd(parameters$Directories$FolderGlobal)
  write.csv(final.proba.auc.2, "roc-proba-auc.csv", row.names = FALSE)  
  write.csv(final.proba.macro.auc.2, "roc-proba-macro-auc.csv", row.names = FALSE)  
  write.csv(final.proba.micro.auc.2, "roc-proba-micro-auc.csv", row.names = FALSE)
  
  
  #################
  final.bin.auc.2 = data.frame(fold, auc = final.bin.auc$value)
  final.bin.micro.auc.2 = data.frame(fold, micro.auc = final.bin.micro.auc$value)
  final.pred.macro.auc.2 = data.frame(fold, macro.auc = final.bin.macro.auc$value)
  setwd(parameters$Directories$FolderGlobal)
  write.csv(final.bin.auc.2, "roc-bin-auc.csv", row.names = FALSE)  
  write.csv(final.bin.micro.auc.2, "roc-bin-macro-auc.csv", row.names = FALSE)  
  write.csv(final.pred.macro.auc.2, "roc-bin-micro-auc.csv", row.names = FALSE)  
  
  #################
  runtime.final = data.frame(fold, final.runtime)
  runtime.final = runtime.final[,-2]
  setwd(parameters$Directories$FolderGlobal)
  write.csv(runtime.final , "runtime-folds.csv", row.names = FALSE)  
  
  #######################
  media.BIN = data.frame(apply(confMatFinalBIN[,-1], 1, mean))
  media.BIN = cbind(measures, media.BIN)
  names(media.BIN) = c("Measures", "Mean10Folds")
  setwd(parameters$Directories$FolderGlobal)
  write.csv(media.BIN, "BIN-Mean10Folds.csv", row.names = FALSE)
  
  #######################
  mediana.BIN = data.frame(apply(confMatFinalBIN[,-1], 1, median))
  mediana.BIN = cbind(measures, mediana.BIN)
  names(mediana.BIN) = c("Measures", "Median10Folds")
  setwd(parameters$Directories$FolderGlobal)
  write.csv(mediana.BIN, "BIN-Median10Folds.csv", row.names = FALSE)
  
  #######################
  dp.BIN = data.frame(apply(confMatFinalBIN[,-1], 1, sd))
  dp.BIN = cbind(measures, dp.BIN)
  names(dp.BIN) = c("Measures", "SD10Folds")
  setwd(parameters$Directories$FolderGlobal)
  write.csv(dp.BIN, "standard-deviation-10-folds.csv", row.names = FALSE)
  
  #############################################################################
  media.THR = data.frame(apply(confMatFinalTHR[,-1], 1, mean))
  media.THR = cbind(measures, media.THR)
  names(media.THR) = c("Measures", "Mean10Folds")
  setwd(parameters$Directories$FolderGlobal)
  write.csv(media.THR, "THR-Mean10Folds.csv", row.names = FALSE)
  
  #######################
  mediana.THR = data.frame(apply(confMatFinalTHR[,-1], 1, median))
  mediana.THR = cbind(measures, mediana.THR)
  names(mediana.THR) = c("Measures", "Median10Folds")
  setwd(parameters$Directories$FolderGlobal)
  write.csv(mediana.THR, "THR-Median10Folds.csv", row.names = FALSE)
  
  #######################
  dp.THR = data.frame(apply(confMatFinalTHR[,-1], 1, sd))
  dp.THR = cbind(measures, dp.THR)
  names(dp.THR) = c("Measures", "SD10Folds")
  setwd(parameters$Directories$FolderGlobal)
  write.csv(dp.THR, "THR-standard-deviation-10-folds.csv", row.names = FALSE)
  
  gc()
  cat("\n########################################################")
  cat("\n# END EVALUATED                                        #") 
  cat("\n########################################################")
  cat("\n\n\n\n")
}



###############################################################################
# Please, any errors, contact us: elainececiliagatto@gmail.com                #
# Thank you very much!                                                        #
###############################################################################
