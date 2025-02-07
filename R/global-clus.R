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


###############################################################################
# SET WORKSAPCE                                                               #
###############################################################################
FolderRoot = "~/Global-Partitions"
FolderScripts = "~/Global-Partitions/R"


###############################################################################
#
###############################################################################


############################################################################
#
############################################################################
gather.files.clus <- function(parameters){
  
  # from fold = 1 to number_folds
  s = 1
  # gfflParalel <- foreach(s = 1:number_folds) %dopar% {
  while(s<=parameters$Config.File$Number.Folds){
    
    cat("\nFold: ", s)
    # creating folder
    FS = paste(parameters$Directories$FolderGlobal, "/Split-", s, sep="")
    if(dir.exists(FS)==FALSE){dir.create(FS)}
    
    # names files
    nome_tr = paste(parameters$Config.File$Dataset.Name, "-Split-Tr-", s, ".arff", sep="")
    nome_ts = paste(parameters$Config.File$Dataset.Name, "-Split-Ts-", s, ".arff", sep="")
    nome_vl = paste(parameters$Config.File$Dataset.Name, "-Split-Vl-", s, ".arff", sep="")
    
    # copying train files
    setwd(parameters$Directories$FolderCVTR)
    if(file.exists(nome_tr) == TRUE){
      setwd(parameters$Directories$FolderCVTR)
      copia = paste(parameters$Directories$FolderCVTR, "/", nome_tr, sep="")
      cola = paste(FS, "/", nome_tr, sep="")
      file.copy(copia, cola, overwrite = TRUE)
    } else {
      break
    }
    
    # copying test files
    setwd(parameters$Directories$FolderCVTS)
    if(file.exists(nome_ts) == TRUE){
      setwd(parameters$Directories$FolderCVTS)
      copia = paste(parameters$Directories$FolderCVTS, "/", nome_ts, sep="")
      cola = paste(FS, "/", nome_ts, sep="")
      file.copy(copia, cola, overwrite = TRUE)
    } else {
      break
    }
    
    # copying test files
    setwd(parameters$Directories$FolderCVVL)
    if(file.exists(nome_vl) == TRUE){
      setwd(parameters$Directories$FolderCVVL)
      copia = paste(parameters$Directories$FolderCVVL, "/", nome_vl, sep="")
      cola = paste(FS, "/", nome_vl, sep="")
      file.copy(copia, cola, overwrite = TRUE)
    } else {
      break
    }
    
    setwd(parameters$Directories$FolderCVVL)
    validation = data.frame(foreign::read.arff(nome_vl))
    
    setwd(parameters$Directories$FolderCVTR)
    train = data.frame(foreign::read.arff(nome_tr))
    
    treino = rbind(train, validation)
    
    #unlink(nome_ts)
    unlink(nome_vl)
    unlink(nome_tr)
    
    nome_tr_2 = paste(FS, "/", parameters$Config.File$Dataset.Name, "-Split-Tr-", s, ".csv", sep="")
    write.csv(treino, nome_tr_2, row.names = FALSE)
    
    ########################################################################
    converteArff <- function(arg1, arg2, arg3, folderUtils){
      str = paste("java -jar ", parameters$Directories$FolderUtils,
                  "/R_csv_2_arff.jar ", arg1, " ", arg2, " ", arg3, sep="")
      print(system(str))
      cat("\n\n")
    }
    
    #########################################################################
    # Targets
    inicio = parameters$Dataset.Info$LabelStart
    fim = ncol(treino)
    ifr = data.frame(inicio, fim)
    write.csv(ifr, "inicioFimRotulos.csv", row.names = FALSE)
    
    
    ########################################################################
    # TRAIN: Convert CSV to ARFF
    arg1Tr = nome_tr_2
    arg2Tr = paste(FS, "/", nome_tr, sep="")
    arg3Tr = paste(inicio, "-", fim, sep="")
    converteArff(arg1Tr, arg2Tr, arg3Tr, parameters$Directories$FolderUtils)
    
    
    #########################################################################
    str0 = paste("sed -i 's/{0}/{0,1}/g;s/{1}/{0,1}/g' ", arg2Tr, sep="")
    print(system(str0))
    
    # unlink(nome_tr_2)
    
    s = s + 1
    gc()
  }
  
  gc()
  cat("\n#########################################################")
  cat("\n# END FUNCTION COPY                                     #") 
  cat("\n#########################################################")
  cat("\n\n\n\n")
  
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
