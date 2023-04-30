###############################################################################
# Global Partitions with Clus                                                 #
# Copyright (C) 2022                                                          #
#                                                                             #
# This code is free software: you can redistribute it and/or modify it under  #
# the terms of the GNU General Public License as published by the Free        #
# Software Foundation, either version 3 of the License, or (at your option)   #
# any later version. This code is distributed in the hope that it will be     #
# useful, but WITHOUT ANY WARRANTY; without even the implied warranty of      #
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General    #
# Public License for more details.                                            #
#                                                                             #
# Elaine Cecilia Gatto | Prof. Dr. Ricardo Cerri | Prof. Dr. Mauri Ferrandin  #
# Federal University of Sao Carlos (UFSCar: https://www2.ufscar.br/) |        #
# Campus Sao Carlos | Computer Department (DC: https://site.dc.ufscar.br/)    #
# Program of Post Graduation in Computer Science                              #
# (PPG-CC: http://ppgcc.dc.ufscar.br/) | Bioinformatics and Machine Learning  #
# Group (BIOMAL: http://www.biomal.ufscar.br/)                                #                                                                                                #
###############################################################################


###############################################################################
# SET WORKSAPCE                                                               #
###############################################################################
FolderRoot = "~/Global-Partitions"
FolderScripts = "~/Global-Partitions/R"


###############################################################################
#
###############################################################################
gather.files.clus <- function(parameters){
  
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
                        "-Split-Tr-", f, ".arff", sep="")
    nome.ts.csv = paste(parameters$Config.File$Dataset.Name, 
                        "-Split-Ts-", f, ".arff", sep="")
    nome.vl.csv = paste(parameters$Config.File$Dataset.Name, 
                        "-Split-Vl-", f, ".arff", sep="")
    
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
  cat("\n# GLOBAL CLUS: END OF THE GATHER FILES FOLDS FUNCTION         #")
  cat("\n###############################################################")
  cat("\n\n")
}



##############################################################################
# 
##############################################################################
execute.clus <- function(parameters){
  
  # from fold = 1 to number_folds
  i = 1
  clusGlobalParalel <- foreach(i = 1:parameters$Config.File$Number.Folds) %dopar% {
    #while(i<=number_folds){
    
    ########################################################################
    FolderRoot = "~/Global-Partitions/"
    FolderScripts = paste(FolderRoot, "/R/", sep="")
    
    ##########################################################################
    setwd(FolderScripts)
    source("libraries.R")
    
    setwd(FolderScripts)
    source("utils.R")
    
    ##########################################################################
    cat("\nFold: ", i)
    
    ##########################################################################
    FolderSplit = paste(parameters$Directories$FolderGlobal, "/Split-", i, sep="")
    
    #######################################################################
    inicio = parameters$Dataset.Info$LabelStart
    fim = parameters$Dataset.Info$LabelEnd
    
    ########################################################################
    trainFileName = paste(FolderSplit, "/", 
                          parameters$Config.File$Dataset.Name, 
                          "-Split-Tr-", i , ".arff", sep="")
    # cat("\n\t", trainFileName)
    
    testFileName = paste(FolderSplit, "/", 
                         parameters$Config.File$Dataset.Name, 
                         "-Split-Ts-", i, ".arff", sep="")
    # cat("\n\t", testFileName)
    
    #########################################################################
    cat("\nCreate config file clus\n")
    setwd(FolderSplit)
    nome_config = paste(FolderSplit, "/",
                        parameters$Config.File$Dataset.Name, 
                        "-Split-", i, ".s", sep="")
    sink(nome_config, type = "output")
    
    cat("[General]")
    cat("\nCompatibility = MLJ08")
    
    cat("\n")
    cat("\n[Data]")
    cat(paste("\nFile = ", trainFileName, sep=""))
    cat(paste("\nTestSet = ", testFileName, sep=""))
    
    cat("\n")
    cat("\n[Attributes]")
    cat("\nReduceMemoryNominalAttrs = yes")
    
    cat("\n")
    cat("\n[Attributes]")
    cat(paste("\nTarget = ", inicio, "-", fim, sep=""))
    cat("\nWeights = 1")
    
    cat("\n")
    cat("\n[Tree]")
    cat("\nHeuristic = VarianceReduction")
    cat("\nFTest = [0.001,0.005,0.01,0.05,0.1,0.125]")
    
    cat("\n")
    cat("\n[Model]")
    cat("\nMinimalWeight = 5.0")
    
    cat("\n")
    cat("\n[Output]")
    cat("\nWritePredictions = {Test}")
    cat("\n")
    sink()
    
    ###########################################################################
    cat("\nExecute CLUS\n")
    setwd(FolderSplit)
    str = paste("java -jar ", parameters$Directories$FolderUtils, 
                "/Clus.jar ", nome_config, sep="")
    cat("\n")
    print(system(str))
    cat("\n")
    
    ###########################################################################
    setwd(FolderSplit)
    zero = paste(parameters$Config.File$Dataset.Name, "-Split-", i, ".out", sep="")
    um = paste(parameters$Config.File$Dataset.Name, "-Split-", i, ".model", sep="")
    dois = paste(parameters$Config.File$Dataset.Name, "-Split-", i, ".s", sep="")
    tres = paste(parameters$Config.File$Dataset.Name, "-Split-Tr-", i, ".arff", sep="")
    quatro = paste(parameters$Config.File$Dataset.Name, "-Split-Ts-", i, ".arff", sep="")
    cinco = paste(parameters$Config.File$Dataset.Name, "-Split-Vl-", i, ".arff", sep="")
    seis = paste("Variance_RHE_1.csv")
    sete = paste(parameters$Config.File$Dataset.Name, "-Split-", i, ".", sep="")
    
    setwd(FolderSplit)
    unlink(zero, recursive = TRUE)
    unlink(um, recursive = TRUE)
    unlink(dois, recursive = TRUE)
    unlink(tres, recursive = TRUE)
    unlink(quatro, recursive = TRUE)
    unlink(cinco, recursive = TRUE)
    unlink(seis, recursive = TRUE)
    unlink(sete, recursive = TRUE)
    
    #i =i + 1
    gc()
  }
  
  gc()
  cat("\n#################################################################")
  cat("\n# GLOBAL CLUS: END OF FUNCTION EXECUTE CLUS                     #") 
  cat("\n#################################################################")
  cat("\n\n\n\n")
}


##############################################################################
# 
##############################################################################
gather.predicts.clus <- function(parameters){
  
  # from fold = 1 to number_folds
  f = 1
  predGlobalParalel <- foreach(f = 1:1:parameters$Config.File$Number.Folds) %dopar% {    
    #while(f<=number_folds){
    
    cat("\nFold: ", f)
    
    FolderRoot = "~/Global-Partitions/"
    FolderScripts = paste(FolderRoot, "/R/", sep="")
    
    setwd(FolderScripts)
    source("libraries.R")
    
    setwd(FolderScripts)
    source("utils.R")
    
    # specifying folder
    FolderSplit = paste(parameters$Directories$FolderGlobal, "/Split-", f, sep="")    
    
    cat("\n\tOpen Test.Pred.Arff ", f)
    setwd(FolderSplit)    
    nome = paste(FolderSplit, "/", parameters$Config.File$Dataset.Name, 
                 "-Split-" , f, ".test.pred.arff", sep="")
    predicoes = data.frame(foreign::read.arff(nome))
    
    inicio = parameters$Dataset.Info$LabelStart
    fim = parameters$Dataset.Info$LabelEnd
    comeco = 1+(fim - inicio)
    
    cat("\n\tSave Y_true")
    classes = data.frame(predicoes[,1:comeco])
    write.csv(classes, "y_true.csv", row.names = FALSE)    
    
    rotulos = c(colnames(classes))
    n_r = length(rotulos)
    nomeColuna = c()
    a = 1 
    while(a <= n_r){
      nomeColuna[a] = paste("Pruned.p.", rotulos[a], sep="")
      a = a + 1
      gc()
    }
    
    cat("\n\tSave Y_pred")
    setwd(FolderSplit)
    pred = data.frame(predicoes[nomeColuna])
    names(pred) = rotulos
    write.csv(pred, "y_predict.csv", row.names = FALSE)  
    
    unlink("inicioFimRotulos.csv")
    
    #f = f + 1
    gc()
  }
  
  gc()
  cat("\n###################################################################")
  cat("\n# CLUS GLOBAL: END OF THE FUNCTION GATHER PREDICTS                #") 
  cat("\n###################################################################")
  cat("\n\n\n\n")
}


##############################################################################
# 
##############################################################################
evaluate.clus <- function(parameters){    
  
  
  apagar = c(0)
  resConfMatFinal = data.frame(apagar)
  
  f = 1
  avaliaParalel <- foreach (f = 1:1:parameters$Config.File$Number.Folds) %dopar%{    
    #while(f<=number_folds){
    
    FolderRoot = "~/Global-Partitions/"
    FolderScripts = paste(FolderRoot, "/R/", sep="")
    
    #########################################################################
    setwd(FolderScripts)
    source("utils.R")
    
    library("mldr")
    library("utiml")
    
    cat("\n\nSplit: ", f)    
    
    FolderSplit = paste(parameters$Directories$FolderGlobal, "/Split-", f, sep="")
    
    #######################################################################
    cat("\nAbrindo pred and true")
    setwd(FolderSplit)
    y_pred = data.frame(read.csv("y_predict.csv"))
    y_true = data.frame(read.csv("y_true.csv"))
    
    cat("\nConvertendo em numerico")
    y_true2 = data.frame(sapply(y_true, function(x) as.numeric(as.character(x))))
    y_true3 = mldr_from_dataframe(y_true2 , labelIndices = seq(1,ncol(y_true2 )), name = "y_true2")
    y_pred2 = sapply(y_pred, function(x) as.numeric(as.character(x)))
    
    cat("\nsalvando")
    salva3 = paste("ConfMatFold-", f, ".txt", sep="")
    setwd(FolderSplit)
    sink(file=salva3, type="output")
    confmat = multilabel_confusion_matrix(y_true3, y_pred2)
    print(confmat)
    sink()
    
    cat("\nmatriz de confusão")
    resConfMat = multilabel_evaluate(confmat)
    resConfMat = data.frame(resConfMat)
    names(resConfMat) = paste("Fold-", f, sep="")
    setwd(FolderSplit)
    write.csv(resConfMat, "ResConfMat.csv")   
    
    #f = f + 1
    
    # setwd(FolderSplit)
    # unlink("y_true.csv")
    # unlink("y_predict.csv")
    
    gc()
  }
  
  gc()
  cat("\n##################################################################")
  cat("\n# END ")
  cat("\n#################################################################")
  cat("\n\n\n\n")
}



##############################################################################
# 
##############################################################################
gather.eval.clus <- function(parameters){
  
  retorno = list()
  
  # vector with names measures
  measures = c("accuracy","average-precision","clp","coverage","F1",
               "hamming-loss","macro-AUC",
               "macro-F1","macro-precision","macro-recall","margin-loss",
               "micro-AUC","micro-F1",
               "micro-precision","micro-recall","mlp","one-error","precision",
               "ranking-loss",
               "recall","subset-accuracy","wlp")
  
  # dta frame
  confMatFinal = data.frame(measures)
  folds = c("")
  
  # from fold = 1 to number_labels
  f = 1
  while(f<=parameters$Config.File$Number.Folds){
    cat("\nFold: ", f)
    
    FolderSplit = paste(parameters$Directories$FolderGlobal, "/Split-", f, sep="")
    setwd(FolderSplit)
    
    # cat("\n\tOpen ResConfMat ", f)
    confMat = data.frame(read.csv(paste(FolderSplit, "/ResConfMat.csv", sep="")))
    names(confMat) = c("Measures", "Fold")
    confMatFinal = cbind(confMatFinal, confMat$Fold) 
    
    folds[f] = paste("Fold-", f, sep="")
    
    f = f + 1
    gc()
  } 
  
  cat("\nsave measures")
  names(confMatFinal) = c("Measures", folds)
  setwd(parameters$Directories$FolderGlobal)
  write.csv(confMatFinal, "All-Folds-Global.csv", row.names = FALSE)
  
  # calculando a média dos 10 folds para cada medida
  media = data.frame(apply(confMatFinal[,-1], 1, mean))
  media = cbind(measures, media)
  names(media) = c("Measures", "Mean10Folds")
  
  setwd(parameters$Directories$FolderGlobal)
  write.csv(media, "Mean10Folds.csv", row.names = FALSE)
  
  mediana = data.frame(apply(confMatFinal[,-1], 1, median))
  mediana = cbind(measures, mediana)
  names(mediana) = c("Measures", "Median10Folds")
  
  setwd(parameters$Directories$FolderGlobal)
  write.csv(mediana, "Median10Folds.csv", row.names = FALSE)
  
  dp = data.frame(apply(confMatFinal[,-1], 1, sd))
  dp = cbind(measures, dp)
  names(dp) = c("Measures", "SD10Folds")
  
  setwd(parameters$Directories$FolderGlobal)
  write.csv(dp, "desvio-padrão-10-folds.csv", row.names = FALSE)
  
  gc()
  cat("\n##################################################################################################")
  cat("\n# CLUS GLOBAL: END OF THE FUNCTION GATHER EVALUATED                                              #") 
  cat("\n##################################################################################################")
  cat("\n\n\n\n")
}



##################################################################################################
# Please, any errors, contact us: elainececiliagatto@gmail.com                                   #
# Thank you very much!                                                                           #
##################################################################################################
