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


##################################################################################################
# FUNCTION GATHER FILES FOLDS GLOBAL                                                             #
#   Objective                                                                                    #
#       Joins the configuration, training and test files in a single folder for running the clus #
#   Parameters                                                                                   #
#       ds: specific dataset information                                                         #
#       dataset_name: dataset name. It is used to save files.                                    #
#       number_folds: number of folds created                                                    #
#       FolderConfifFiles: folder path                                                           #
#   Return                                                                                       #
#       configurations files                                                                     #
##################################################################################################
gather.files.clus <- function(ds, dataset_name, 
                              number_folds, folderResults){
  
  # from fold = 1 to number_folds
  s = 1
  # foldsParalel <- foreach(s = 1:number_folds) %dopar% {
  while(s<=number_folds){
    
    cat("\nFold: ", s)
    
    FolderRoot = "~/Global-Partitions/"
    FolderScripts = paste(FolderRoot, "/R/", sep="")
    
    setwd(FolderScripts)
    source("libraries.R")
    
    setwd(FolderScripts)
    source("utils.R")
    
    diretorios = directories(dataset_name, folderResults)
    
    # specifying folder
    FolderSplit = paste(diretorios$folderGlobal, "/Split-", s, sep="")
    if(dir.exists(FolderSplit)==FALSE){dir.create(FolderSplit)}
    
    # names files
    nome_tr = paste(dataset_name, "-Split-Tr-", s, ".arff", sep="")
    nome_ts = paste(dataset_name, "-Split-Ts-", s, ".arff", sep="")
    nome_vl = paste(dataset_name, "-Split-Vl-", s, ".arff", sep="")
    nome_config = paste(dataset_name, "-Split-", s, ".s", sep="")
    
    # train
    setwd(diretorios$folderCVTR)
    if(file.exists(nome_tr) == TRUE){
      #cat("\n\n=========================================================")
      setwd(diretorios$folderCVTR)
      
      copia = paste(diretorios$folderCVTR, "/", nome_tr, sep="")
      #cat("\n ", copia)
      
      cola = paste(FolderSplit, "/", nome_tr, sep="")
      #cat("\n ", cola)
      
      file.copy(copia, cola, overwrite = TRUE)
      # cat("\nTransfer Train", s)
      
    } else {
      cat("\n")
    }
    
    # train
    setwd(diretorios$folderCVTS)
    if(file.exists(nome_ts) == TRUE){
      #cat("\n\n=========================================================")
      setwd(diretorios$folderCVTS)
      
      copia = paste(diretorios$folderCVTS, "/", nome_ts, sep="")
      #cat("\n ", copia)
      
      cola = paste(FolderSplit, "/", nome_ts, sep="")
      #cat("\n ", cola)
      
      file.copy(copia, cola, overwrite = TRUE)
      #cat("\nTransfer Train", s)
      
    } else {
      cat("\n")
    }
    
    # train
    setwd(diretorios$folderCVVL)
    if(file.exists(nome_vl) == TRUE){
      #cat("\n\n=========================================================")
      setwd(diretorios$folderCVVL)
      
      copia = paste(diretorios$folderCVVL, "/", nome_vl, sep="")
      #cat("\n ", copia)
      
      cola = paste(FolderSplit, "/", nome_vl, sep="")
      #cat("\n ", cola)
      
      file.copy(copia, cola, overwrite = TRUE)
      #cat("\nTransfer Train", s)
      
    } else {
      cat("\n")
    }
    
    #cat("\nJuntando treino com validação!")
    setwd(FolderSplit)
    validation = data.frame(foreign::read.arff(nome_vl))
    train = data.frame(foreign::read.arff(nome_tr))
    treino = rbind(train, validation)
    
    #unlink(nome_vl)
    #unlink(nome_tr)
    
    nome_tr_2 = paste(dataset_name, "-Split-Tr-", s, ".csv", sep="")
    write.csv(treino, nome_tr_2, row.names = FALSE)
    
    ############################################################################################################
    converteArff <- function(arg1, arg2, arg3, FolderUtils){  
      str = paste("java -jar ", diretorios$folderUtils, 
                  "/R_csv_2_arff.jar ", arg1, " ", arg2, " ", arg3, sep="")
      print(system(str))
      cat("\n\n")  
    }
    
    ####################################################################################
    # Targets
    #cat("\nDEFININDO OS TARGETS")
    inicio = ds$LabelStart
    fim = ncol(treino)
    ifr = data.frame(inicio, fim)
    write.csv(ifr, "inicioFimRotulos.csv", row.names = FALSE)
    
    
    ####################################################################################
    # TRAIN: Convert CSV to ARFF
    #cat("\nCONVERTENDO ARFF EM CSV")
    arg1Tr = nome_tr_2
    arg2Tr = nome_tr
    arg3Tr = paste(inicio, "-", fim, sep="")
    converteArff(arg1Tr, arg2Tr, arg3Tr, diretorios$FolderUtils)
    
    ####################################################################################
    #cat("\nVERIFICANDO 0 E 1")
    str0 = paste("sed -i 's/{0}/{0,1}/g;s/{1}/{0,1}/g' ", nome_tr, sep="")
    print(system(str0))
    
    unlink(nome_tr_2)
    
    s = s + 1
    gc()
  }
  
  gc()
  cat("\n##################################################################################################")
  cat("\n# GLOBAL CLUS: END OF THE GATHER FILES FOLDS FUNCTION                                            #")
  cat("\n##################################################################################################")
  cat("\n\n\n\n")
}


##################################################################################################
# FUNCTION EXECUTE CLUS GLOBAL                                                                   #
#   Objective                                                                                    #
#       Tests global partitions                                                                  #
#   Parameters                                                                                   #
#       ds: specific dataset information                                                         #
#       dataset_name: dataset name. It is used to save files.                                    #
#       number_folds: number of folds created                                                    #
#       Folder: folder path                                                                      #
#   Return                                                                                       #
#       configurations files                                                                     #
##################################################################################################
execute.clus <- function(ds, 
                         dataset_name, 
                         number_folds, 
                         number_cores, 
                         folderResults){
  
  # from fold = 1 to number_folds
  i = 1
  clusGlobalParalel <- foreach(i = 1:number_folds) %dopar% {
    #while(i<=number_folds){
    
    ####################################################################################
    FolderRoot = "~/Global-Partitions/"
    FolderScripts = paste(FolderRoot, "/R/", sep="")
    
    ####################################################################################
    setwd(FolderScripts)
    source("libraries.R")
    
    setwd(FolderScripts)
    source("utils.R")
    
    ####################################################################################
    diretorios = directories(dataset_name, folderResults)
    
    ####################################################################################
    cat("\nFold: ", i)
    
    ####################################################################################
    FolderSplit = paste(diretorios$folderGlobal, "/Split-", i, sep="")
    
    ####################################################################################
    inicio = ds$LabelStart
    fim = ds$LabelEnd
    
    ####################################################################################
    trainFileName = paste(FolderSplit, "/", 
                          dataset_name, "-Split-Tr-", i , ".arff", sep="")
    # cat("\n\t", trainFileName)
    
    testFileName = paste(FolderSplit, "/", 
                         dataset_name, "-Split-Ts-", i, ".arff", sep="")
    # cat("\n\t", testFileName)
    
    ####################################################################################
    cat("\nCreate config file clus\n")
    setwd(FolderSplit)
    nome_config = paste(FolderSplit, "/",
                        dataset_name, "-Split-", i, ".s", sep="")
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
    
    ####################################################################################
    cat("\nExecute CLUS\n")
    setwd(FolderSplit)
    str = paste("java -jar ", diretorios$folderUtils , 
                "/Clus.jar ", nome_config, sep="")
    cat("\n")
    print(system(str))
    cat("\n")
    
    ####################################################################################
    um = paste(dataset_name, "-Split-", i, ".model", sep="")
    dois = paste(dataset_name, "-Split-", i, ".s", sep="")
    tres = paste(dataset_name, "-Split-Tr-", i, ".arff", sep="")
    quatro = paste(dataset_name, "-Split-Ts-", i, ".arff", sep="")
    cinco = paste(dataset_name, "-Split-Vl-", i, ".arff", sep="")
    seis = paste("Variance_RHE_1.csv")
    sete = paste(dataset_name, "-Split-", i, ".", sep="")
    
    setwd(FolderSplit)
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
  cat("\n##################################################################################################")
  cat("\n# GLOBAL CLUS: END OF FUNCTION EXECUTE CLUS                                                      #") 
  cat("\n##################################################################################################")
  cat("\n\n\n\n")
}


##################################################################################################
# FUNCTION GATHER PREDICTS GLOBAL                                                                #
#   Objective                                                                                    #
#      From the file "test.pred.arff", separates the real labels and the predicted labels to     # 
#      generate the confusion matrix to evaluate the partition.                                  #
#   Parameters                                                                                   #
#       ds: specific dataset information                                                         #
#       dataset_name: dataset name. It is used to save files.                                    #
#       number_folds: number of folds created                                                    #
#       Folder: folder path                                                                      #
#   Return                                                                                       #
#       true labels and predicts labels                                                          #
##################################################################################################
gather.predicts.clus <- function(ds, 
                                 dataset_name, 
                                 number_folds, 
                                 number_cores, 
                                 folderResults){
  
  diretorios = directories(dataset_name, folderResults)
  
  # from fold = 1 to number_folds
  f = 1
  predGlobalParalel <- foreach(f = 1:number_folds) %dopar% {    
    #while(f<=number_folds){
    
    cat("\nFold: ", f)
    
    FolderRoot = "~/Global-Partitions/"
    FolderScripts = paste(FolderRoot, "/R/", sep="")
    
    setwd(FolderScripts)
    source("libraries.R")
    
    setwd(FolderScripts)
    source("utils.R")
    
    # specifying folder
    FolderSplit = paste(diretorios$folderGlobal, "/Split-", f, sep="")    
    
    cat("\n\tOpen Test.Pred.Arff ", f)
    setwd(FolderSplit)    
    nome = paste(FolderSplit, "/", dataset_name, 
                 "-Split-" , f, ".test.pred.arff", sep="")
    predicoes = data.frame(foreign::read.arff(nome))
    
    inicio = ds$LabelStart
    fim = ds$LabelEnd
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
  cat("\n##################################################################################################")
  cat("\n# CLUS GLOBAL: END OF THE FUNCTION GATHER PREDICTS                                               #") 
  cat("\n##################################################################################################")
  cat("\n\n\n\n")
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
evaluate.clus <- function(ds, 
                          dataset_name, 
                          number_folds, 
                          number_cores, 
                          folderResults){    
  
  
  apagar = c(0)
  resConfMatFinal = data.frame(apagar)
  
  f = 1
  avaliaParalel <- foreach (f = 1:number_folds) %dopar%{    
    #while(f<=number_folds){
    
    FolderRoot = "~/Global-Partitions/"
    FolderScripts = paste(FolderRoot, "/R/", sep="")
    
    ####################################################################################
    setwd(FolderScripts)
    source("utils.R")
    
    library("mldr")
    library("utiml")
    
    ####################################################################################
    diretorios = directories(dataset_name, folderResults)
    
    cat("\n\nSplit: ", f)    
    
    FolderSplit = paste(diretorios$folderGlobal, "/Split-", f, sep="")
    
    ####################################################################################
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
    
    setwd(FolderSplit)
    unlink("y_true.csv")
    unlink("y_predict.csv")
    
    gc()
  }
  
  gc()
  cat("\n##################################################################################################")
  cat("\n# END OF THE EVALUATION MISCELLANEOUS FUNCTION                                                   #") 
  cat("\n##################################################################################################")
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
gather.eval.clus <- function(ds, 
                             dataset_name, 
                             number_folds, 
                             number_cores, 
                             folderResults){
  
  diretorios = directories(dataset_name, folderResults) 
  
  retorno = list()
  
  # vector with names measures
  measures = c("accuracy","average-precision","clp","coverage","F1","hamming-loss","macro-AUC",
               "macro-F1","macro-precision","macro-recall","margin-loss","micro-AUC","micro-F1",
               "micro-precision","micro-recall","mlp","one-error","precision","ranking-loss",
               "recall","subset-accuracy","wlp")
  
  # dta frame
  confMatFinal = data.frame(measures)
  folds = c("")
  
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
    
    f = f + 1
    gc()
  } 
  
  cat("\nsave measures")
  names(confMatFinal) = c("Measures", folds)
  setwd(diretorios$folderGlobal)
  write.csv(confMatFinal, "All-Folds-Global.csv", row.names = FALSE)
  
  # cat("\nadjust")
  # confMatFinal2 = data.frame(t(confMatFinal))
  # confMatFinal3 = confMatFinal2[-1,]
  # colnames(confMatFinal3) = medidas
  # teste = data.frame(sapply(confMatFinal3, function(x) as.numeric(as.character(x))))
  # write.csv(confMatFinal3, paste(dataset_name, "-Global-fold-per-measure.csv", sep=""), row.names = FALSE)
  # 
  # cat("\nsummary")
  # sumary = apply(teste,2,mean)
  # sumary2 = data.frame(sumary)
  # sumary3 = cbind(medidas, sumary2)
  # names(sumary3) = c("Measures", "Mean")
  # write.csv(sumary3, paste(dataset_name, "-Global-Mean-10-folds.csv", sep=""), row.names = FALSE)
  
  # calculando a média dos 10 folds para cada medida
  media = data.frame(apply(confMatFinal[,-1], 1, mean))
  media = cbind(measures, media)
  names(media) = c("Measures", "Mean10Folds")
  
  setwd(diretorios$folderGlobal)
  write.csv(media, "Mean10Folds.csv", row.names = FALSE)
  
  mediana = data.frame(apply(confMatFinal[,-1], 1, median))
  mediana = cbind(measures, mediana)
  names(mediana) = c("Measures", "Median10Folds")
  
  setwd(diretorios$folderGlobal)
  write.csv(mediana, "Median10Folds.csv", row.names = FALSE)
  
  dp = data.frame(apply(confMatFinal[,-1], 1, sd))
  dp = cbind(measures, dp)
  names(dp) = c("Measures", "SD10Folds")
  
  setwd(diretorios$folderGlobal)
  write.csv(dp, "desvio-padrão-10-folds.csv", row.names = FALSE)
  
  gc()
  cat("\n##################################################################################################")
  cat("\n# CLUS GLOBAL: END OF THE FUNCTION GATHER EVALUATED                                              #") 
  cat("\n##################################################################################################")
  cat("\n\n\n\n")
}


##################################################################################################
# FUNCTION DELETE GLOBAL PARTITION                                                               #
#   Objective                                                                                    #
#       deletes all unnecessary files                                                            #
#   Parameters                                                                                   #
#       dataset_name: name dataset                                                               #
#       ds: specific dataset information                                                         #
#       number_folds: number of folds created                                                    #
#       FolderGlobal path of hybrid partition results                                            #
#   Return                                                                                       #
##################################################################################################
deleteGlobal <-function(ds, dataset_name, number_folds, folderResults){
  
  diretorios = directories(dataset_name, folderResults)   
  
  # from fold = 1 to number_labes
  f = 1
  apagaGlobal <- foreach (f = 1:number_folds) %dopar%{
    cat("\nFold  ", f)
    FolderSplit = paste(diretorios$FolderGlobal, "/Split-", f, sep="")
    
    str = paste("rm -r ", FolderSplit, sep="")
    system(str)
    
    #setwd(FolderSplit)
    
    #nome1 = paste(dataset_name, "-Split-", f , ".s", sep="")
    #unlink(nome1)
    
    #nome2 = paste(dataset_name, "-Split-", f ,".test.pred.arff", sep="")
    #unlink(nome2)
    
    gc()
  }
  
  gc()
  cat("\n##################################################################################################")
  cat("\n# CLUS GLOBAL: FIM DA FUNCAO DELETE                                                              #") 
  cat("\n##################################################################################################")
  cat("\n\n\n\n")
}



##################################################################################################
# Please, any errors, contact us: elainececiliagatto@gmail.com                                   #
# Thank you very much!                                                                           #
##################################################################################################
