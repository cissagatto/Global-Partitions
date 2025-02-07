##############################################################################
# GLOBAL PARTITIONS MULTI-LABEL CLASSIFICATION                               #
# Copyright (C) 2025                                                         #
#                                                                            #
# This code is free software: you can redistribute it and/or modify it under #
# the terms of the GNU General Public License as published by the Free       #
# Software Foundation, either version 3 of the License, or (at your option)  #
# any later version. This code is distributed in the hope that it will be    #
# useful, but WITHOUT ANY WARRANTY; without even the implied warranty of     #
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General   #
# Public License for more details.                                           #
#                                                                            #
# 1 - Prof PhD Elaine Cecilia Gatto                                          #
# 2 - Prof PhD Ricardo Cerri                                                 #
# 3 - Prof PhD Mauri Ferrandin                                               #
# 4 - Prof PhD Celine Vens                                                   #
# 5 - PhD Felipe Nakano Kenji                                                #
# 6 - Prof PhD Jesse Read                                                    #
#                                                                            #
# 1 = Federal University of São Carlos - UFSCar - https://www2.ufscar.br     #
# Campus São Carlos | Computer Department - DC - https://site.dc.ufscar.br | #
# Post Graduate Program in Computer Science - PPGCC                          # 
# http://ppgcc.dc.ufscar.br | Bioinformatics and Machine Learning Group      #
# BIOMAL - http://www.biomal.ufscar.br                                       # 
#                                                                            # 
# 1 = Federal University of Lavras - UFLA                                    #
#                                                                            # 
# 2 = State University of São Paulo - USP                                    #
#                                                                            # 
# 3 - Federal University of Santa Catarina Campus Blumenau - UFSC            #
# https://ufsc.br/                                                           #
#                                                                            #
# 4 and 5 - Katholieke Universiteit Leuven Campus Kulak Kortrijk Belgium     #
# Medicine Department - https://kulak.kuleuven.be/                           #
# https://kulak.kuleuven.be/nl/over_kulak/faculteiten/geneeskunde            #
#                                                                            #
# 6 - Ecole Polytechnique | Institut Polytechnique de Paris | 1 rue Honoré   #
# d’Estienne d’Orves - 91120 - Palaiseau - FRANCE                            #
#                                                                            #
##############################################################################

#getwd()

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
    mldr.test = mldr_from_dataframe(test, labelIndices = labels.indices)
    mldr.tv = mldr_from_dataframe(tv, labelIndices = labels.indices)
    
    
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
    y_pred_bin = data.frame(read.csv(paste(FolderSplit, "/y_pred_bin.csv", sep="")))
    y_pred_proba = data.frame(read.csv(paste(FolderSplit, "/y_pred_proba.csv", sep="")))
    y_true = data.frame(read.csv(paste(FolderSplit, "/y_true.csv", sep="")))
    
    
    ###################
    # pego os nomes das colunas originais do proba
    nomes.proba = colnames(y_pred_proba)
    
    # total de linhas
    linhas = ncol(y_pred_proba)
    
    # total de nomes que quero gerar
    m = (linhas/2)+1
    
    # gerando os nomes das colunas que estou interessada
    nomes.2 = c("")
    a = 0
    while(a<m){
      nomes.2[a] = paste("prob_", a-1, "_1", sep="")
      a = a + 1
    }
    
    y_pred_proba = y_pred_proba %>% select(all_of(nomes.2))
    names(y_pred_proba) = parameters$Names.Labels$Labels
    
    if(nrow(y_pred_proba)!=nrow(test)){
      cat("\nSomething is wrong with probabilities predictions!")
      break
    }
    
    # salvando as probabilidades corretas
    write.csv(y_pred_proba, paste(FolderSplit, "/y_pred_proba.csv", sep=""),
              row.names = FALSE)
    
    
    ####################################################################################
    y.true.2 = data.frame(sapply(y_true, function(x) as.numeric(as.character(x))))
    y.true.3 = mldr_from_dataframe(y.true.2, 
                                   labelIndices = seq(1,ncol(y.true.2)), 
                                   name = "y.true.2")
    y_pred_bin = sapply(y_pred_bin, function(x) as.numeric(as.character(x)))
    y_pred_proba = sapply(y_pred_proba, function(x) as.numeric(as.character(x)))
    
    
    ########################################################################
    y_threshold_05 <- data.frame(as.matrix(fixed_threshold(y_pred_proba,
                                                           threshold = 0.5)))
    write.csv(y_threshold_05, 
              paste(FolderSplit, "/y_pred_thr05.csv", sep=""),
              row.names = FALSE)
    
    
    ########################################################################
    y_threshold_card = lcard_threshold(as.matrix(y_pred_proba), 
                                       mldr.tv$measures$cardinality,
                                       probability = F)
    write.csv(y_threshold_card, 
              paste(FolderSplit, "/y_pred_thrLC.csv", sep=""),
              row.names = FALSE)
    
    y_threshold_card = data.frame(as.matrix(y_threshold_card))
    
    #####################################################################
    nome.true = paste(FolderSplit, "/y_true.csv", sep="")
    nome.pred.proba = paste(FolderSplit, "/y_pred_proba.csv", sep="")
    nome.pred.bin = paste(FolderSplit, "/y_pred_bin.csv", sep="")
    nome.thr.05 = paste(FolderSplit, "/y_pred_thr05.csv", sep="")
    nome.thr.LC = paste(FolderSplit, "/y_pred_thrLC.csv", sep="")
    
    save.pred.bin = paste(FolderSplit, "/pred-bin-auprc.csv", sep="")
    save.pred.proba = paste(FolderSplit, "/pred-proba-auprc.csv", sep="")
    save.thr05 = paste(FolderSplit, "/thr-05-auprc.csv", sep="")
    save.thrLC = paste(FolderSplit, "/thr-lc-auprc.csv", sep="")
    
    
    #################################################################
    str.execute = paste("python3 ",
                        parameters$Directories$FolderUtils,
                        "/auprc.py ",
                        nome.true, " ",
                        nome.pred.bin, " ",
                        save.pred.bin, " ",
                        sep="")
    res = print(system(str.execute))
    if(res!=0){
      break
    }
    
    #################################################################
    str.execute = paste("python3 ",
                        parameters$Directories$FolderUtils,
                        "/auprc.py ",
                        nome.true, " ",
                        nome.pred.proba, " ",
                        save.pred.proba, " ",
                        sep="")
    res = print(system(str.execute))
    if(res!=0){
      break
    }
    
    #################################################################
    str.execute = paste("python3 ",
                        parameters$Directories$FolderUtils,
                        "/auprc.py ",
                        nome.true, " ",
                        nome.thr.05, " ",
                        save.thr05, " ",
                        sep="")
    res = print(system(str.execute))
    if(res!=0){
      break
    }
    
    #################################################################
    str.execute = paste("python3 ",
                        parameters$Directories$FolderUtils,
                        "/auprc.py ",
                        nome.true, " ",
                        nome.thr.LC, " ",
                        save.thrLC, " ",
                        sep="")
    res = print(system(str.execute))
    if(res!=0){
      break
    }
    
    ####################################################
    names = paste(parameters$Names.Labels$Labels, "-proba", sep="")
    y_pred_proba = data.frame(y_pred_proba)
    names(y_pred_proba) = names
    rm(names)
    
    names = paste(parameters$Names.Labels$Labels, "-bin", sep="")
    y_pred_bin = data.frame(y_pred_bin)
    names(y_pred_bin) = names
    rm(names)
    
    names  = paste(parameters$Names.Labels$Labels, "-true", sep="")
    true = data.frame(y_true)
    names(y_true) = names 
    rm(names)
    
    names  = paste(parameters$Names.Labels$Labels, "-thr-05", sep="")
    y_threshold_05 = data.frame(y_threshold_05)
    names(y_threshold_05) = names 
    rm(names)
    
    names  = paste(parameters$Names.Labels$Labels, "-thr-lc", sep="")
    y_threshold_card = data.frame(as.matrix(y_threshold_card))
    names(y_threshold_card) = names 
    rm(names)
    
    all.predictions = cbind(y_true, y_pred_bin, y_pred_proba,
                            y_threshold_05, y_threshold_card)
    write.csv(all.predictions, 
              paste(FolderSplit, "/folder-predictions.csv", sep=""), 
              row.names = FALSE)
    
    
    ##############################################
    matrix.confusao(true = y_true, pred = y_threshold_05, 
                    type = "thr-05", salva = FolderSplit, 
                    nomes.rotulos = parameters$Names.Labels$Labels)
    
    matrix.confusao(true = y_true, pred = y_threshold_card, 
                    type = "thr-lc", salva = FolderSplit, 
                    nomes.rotulos = parameters$Names.Labels$Labels)
    
    matrix.confusao(true = y_true, pred = y_pred_bin , 
                    type = "pred-bin", salva = FolderSplit, 
                    nomes.rotulos = parameters$Names.Labels$Labels)
    
    
    #########################################################################    
    roc.curva(f = f, y_pred = y_pred_bin, test = mldr.test,
              Folder = FolderSplit, nome = "pred-bin")

    roc.curva(f = f, y_pred = y_pred_proba, test = mldr.test,
              Folder = FolderSplit, nome = "pred-proba")

    roc.curva(f = f, y_pred = y_threshold_card, test = mldr.test,
              Folder = FolderSplit, nome = "thr-lc")

    roc.curva(f = f, y_pred = y_threshold_05, test = mldr.test,
              Folder = FolderSplit, nome = "thr-05")
    
    #########################################################################   
    
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
    
    #####################################################################
    nome.true = paste(FolderSplit, "/y_true.csv", sep="")
    nome.pred.proba = paste(FolderSplit, "/y_pred_proba.csv", sep="")
    nome.pred.bin = paste(FolderSplit, "/y_pred_bin.csv", sep="")
    nome.thr.05 = paste(FolderSplit, "/y_pred_thr05.csv", sep="")
    nome.thr.LC = paste(FolderSplit, "/y_pred_thrLC.csv", sep="")
    
    #####################################################################
    y_pred_proba = data.frame(read.csv(nome.pred.proba))
    y_pred_bin = data.frame(read.csv(nome.pred.bin))
    y_pred_thr_05 = data.frame(read.csv(nome.thr.05))
    y_pred_thr_lc = data.frame(read.csv(nome.thr.LC))
    y_true = data.frame(read.csv(nome.true))
    
    
    ##########################################################################
    y.true.2 = data.frame(sapply(y_true, function(x) as.numeric(as.character(x))))
    y.true.3 = mldr_from_dataframe(y.true.2, 
                                   labelIndices = seq(1,ncol(y.true.2)), 
                                   name = "y.true.2")
    y_pred_bin = sapply(y_pred_bin, function(x) as.numeric(as.character(x)))
    y_pred_proba = sapply(y_pred_proba, function(x) as.numeric(as.character(x)))
    y_pred_thr_05 = sapply(y_pred_thr_05, function(x) as.numeric(as.character(x)))
    y_pred_thr_lc = sapply(y_pred_thr_lc, function(x) as.numeric(as.character(x)))
    
    
    ##########################################################################    
    avaliacao(f = f, y_true = y.true.3, y_pred = y_pred_bin,
              salva = FolderSplit, nome = "pred-bin")
    
    avaliacao(f = f, y_true = y.true.3, y_pred = y_pred_proba,
              salva = FolderSplit, nome = "pred-proba")
    
    avaliacao(f = f, y_true = y.true.3, y_pred = y_pred_thr_05,
              salva = FolderSplit, nome = "thr-05")
    
    avaliacao(f = f, y_true = y.true.3, y_pred = y_pred_thr_lc,
              salva = FolderSplit, nome = "thr-lc")
    
    # f = f + 1
    gc()
  }
  
  gc()
  cat("\n##################################")
  cat("\n# END FUNCTION EVALUATE          #")
  cat("\n##################################")
  cat("\n\n\n\n")
}





###########################################################################
#
###########################################################################
gather.eval.global.python <- function(parameters){
  
  measures = c("accuracy", "average-precision", "clp", "coverage",
               "F1", "hamming-loss", "macro-AUC", "macro-F1", 
               "macro-precision", "macro-recall", "margin-loss", 
               "micro-AUC","micro-F1", "micro-precision",
               "micro-recall", "mlp", "one-error", "precision", 
               "ranking-loss", "recall", "subset-accuracy", "wlp")
  
  folds = c(0)
  
  nomes.preds = c("pred-bin", "pred-proba", 
                  "thr-05", "thr-lc")
  
  i = 1
  while(i<=length(nomes.preds)){
    
    cat("\n\npredicao: ", i)
    
    final.roc.auc = data.frame()
    final.roc.auc.micro = data.frame()
    final.roc.auc.macro = data.frame()
    
    final.auprc.macro = data.frame(fold = c(0), value=c(0))
    final.auprc.micro = data.frame(fold = c(0), value=c(0))
    
    final.runtime = data.frame()
    final.conf.mat = data.frame(measures)
    final.models = data.frame()
    
    f = 1
    while(f<=parameters$Config.File$Number.Folds){
      
      cat("\nFold: ", f)
      
      #########################################################################
      folderSplit = paste(parameters$Directories$FolderGlobal,
                          "/Split-", f, sep="")
      
      #########################################################################
      confMat = data.frame(read.csv(paste(folderSplit, "/", nomes.preds[i], 
                                          "-evaluated.csv", sep="")))
      names(confMat) = c("Measures", "Fold")
      
      #########################################################################
      confMat[is.na(confMat)] <- 0
      
      #########################################################################
      final.conf.mat = cbind(final.conf.mat, confMat$Fold) 
      folds[f] = paste("Fold-", f, sep="")
      
      #########################################################################
      roc.auc = data.frame(read.csv(paste(folderSplit, "/", nomes.preds[i], 
                                          "-roc-auc.csv", sep="")))       
      final.roc.auc = rbind(final.roc.auc, roc.auc)
      
      #########################################################################
      roc.micro.auc = data.frame(read.csv(paste(folderSplit, "/", nomes.preds[i], 
                                                "-roc-auc-micro.csv", sep="")))       
      final.roc.auc.micro = rbind(final.roc.auc.micro, roc.micro.auc)
      
      #########################################################################
      roc.macro.auc = data.frame(read.csv(paste(folderSplit, "/", nomes.preds[i], 
                                                "-roc-auc-macro.csv", sep="")))       
      final.roc.auc.macro = rbind(final.roc.auc.macro, roc.macro.auc)
      
      #########################################################################
      auprc = data.frame(read.csv(paste(folderSplit, "/", nomes.preds[i], 
                                        "-auprc.csv", sep="")))       
      final.auprc.macro = rbind(final.auprc.macro, 
                                data.frame(fold = f, value = auprc$Macro.AUPRC))
      final.auprc.micro = rbind(final.auprc.micro, 
                                data.frame(fold = f, value = auprc$Micro.AUPRC))
      
      #################################
      runtime = data.frame(read.csv(paste(folderSplit, "/runtime-fold.csv", sep="")))
      names(runtime) = c("fold", "user.self", "sys.self",
                         "elapsed","user.child","sys.child")
      final.runtime = rbind(final.runtime, runtime)
      
      #########################################################################
      name_model = paste(parameters$Directories$FolderGlobal,
                         "/Split-", f, "/model_sizes.csv", sep="")
      models = data.frame(read.csv(name_model))
      models = data.frame(fold = f, models)
      final.models = rbind(final.models, models)
      
      f = f + 1
      gc()
    } 
    
    ###########################################
    write.csv(final.models, 
              paste(parameters$Directories$FolderGlobal, 
                    "/models_sizes.csv", sep=""), 
              row.names = FALSE)
    
    
    names(final.conf.mat) = c("Measures", folds)
    names(final.roc.auc) = c("Fold", "Value")
    names(final.roc.auc.micro) = c("Fold", "Value")
    names(final.roc.auc.macro) = c("Fold", "Value")
    names(final.auprc.micro) = c("Fold", "Value")
    names(final.auprc.macro) = c("Fold", "Value")
    final.auprc.macro = final.auprc.macro[-1,]
    final.auprc.micro = final.auprc.micro[-1,]
    
    ###########################################
    fold = seq(1, parameters$Config.File$Number.Folds, by =1)
    
    ###########################################
    names(final.conf.mat) = c("Measures", folds)
    final.conf.mat[is.na(final.conf.mat)] <- 0
    write.csv(final.conf.mat, 
              paste(parameters$Directories$FolderGlobal, "/", nomes.preds[i], 
                    "-Test-Evaluated.csv", sep=""), 
              row.names = FALSE)
    
    #######################
    media = data.frame(apply(final.conf.mat[,-1], 1, mean))
    media = cbind(measures, media)
    names(media) = c("Measures", "Mean10Folds")
    write.csv(media, 
              paste(parameters$Directories$FolderGlobal, "/", 
                    nomes.preds[i], "-Mean10Folds.csv", sep=""), 
              row.names = FALSE)
    
    #######################
    mediana = data.frame(apply(final.conf.mat[,-1], 1, median))
    mediana = cbind(measures, mediana)
    names(mediana) = c("Measures", "Median10Folds")
    write.csv(mediana, 
              paste(parameters$Directories$FolderGlobal, "/", 
                    nomes.preds[i], "-Median10Folds.csv", sep=""), 
              row.names = FALSE)
    
    
    #######################
    desvio = data.frame(apply(final.conf.mat[,-1], 1, sd))
    desvio  = cbind(measures, desvio)
    names(desvio ) = c("Measures", "Deviation10Folds")
    write.csv(desvio , 
              paste(parameters$Directories$FolderGlobal, "/", 
                    nomes.preds[i], "-Deviation10Folds.csv", sep=""), 
              row.names = FALSE)
    
    ###########################################
    write.csv(final.roc.auc, 
              paste(parameters$Directories$FolderGlobal, "/", nomes.preds[i], 
                    "-roc-auc.csv", sep=""), 
              row.names = FALSE)
    
    ###########################################
    write.csv(final.roc.auc.micro, 
              paste(parameters$Directories$FolderGlobal, "/", nomes.preds[i], 
                    "-roc-auc-micro.csv", sep=""), 
              row.names = FALSE)
    
    ###########################################
    write.csv(final.roc.auc.macro, 
              paste(parameters$Directories$FolderGlobal, "/", nomes.preds[i], 
                    "-roc-auc-macro.csv", sep=""), 
              row.names = FALSE)
    
    ###########################################
    write.csv(final.auprc.micro, 
              paste(parameters$Directories$FolderGlobal, "/", nomes.preds[i], 
                    "-roc-auprc-micro.csv", sep=""), 
              row.names = FALSE)
    
    ###########################################
    final.runtime$fold = fold
    write.csv(final.runtime, 
              paste(parameters$Directories$FolderGlobal, 
                    "/runtime-folds.csv", sep=""), 
              row.names = FALSE)
    
    ################
    i = i + 1
    gc()
  }
  
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
