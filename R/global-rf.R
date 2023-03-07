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
    train = rbind(train, val)
    
    ##########################################################################
    labels.indices = seq(parameters$Dataset.Info$LabelStart, parameters$Dataset.Info$LabelEnd, by=1)
    mldr.treino = mldr_from_dataframe(train, labelIndices = labels.indices)
    mldr.teste = mldr_from_dataframe(test, labelIndices = labels.indices)
    
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
    y_proba = data.frame(read.csv("y_proba_1.csv"))
    
    
    #############################
    nomes.rotulos = colnames(y_trues)
    
    
    #####################################################################
    cat("\n\tSave original and pruned predictions\n")
    pred.o = paste(colnames(y_preds), "-pred", sep="")
    names(y_preds) = pred.o
    
    true.labels = paste(colnames(y_trues), "-true", sep="")
    names(y_trues) = true.labels
    
    proba = paste(names.rotulos, "-proba", sep="")
    names(y_proba) = proba
    
    all.predictions = cbind(y_preds, y_trues, y_proba)
    setwd(FolderSplit)
    write.csv(all.predictions, "folder-predictions.csv", row.names = FALSE)
    
    
    #####################################################################
    y_pred2 = sapply(y_preds, function(x) as.numeric(as.character(x)))
    res = mldr_evaluate(mldr.teste, y_pred2)
    
    # MEDIDAS DE AVALIAÇÃO
    accuracy = res$accuracy
    example.auc = res$example_auc
    average.precision = res$average_precision
    coverage = res$coverage
    f1 = res$fmeasure
    hamming.loss = res$hamming_loss
    macro.auc = res$macro_auc 
    macro.f1 = res$macro_fmeasure
    macro.precision = res$macro_precision
    macro.recall = res$macro_recall
    micro.auc = res$micro_auc
    micro.f1 = res$micro_fmeasure
    micro.precision = res$micro_precision
    micro.recall = res$micro_recall
    one.error = res$one_error
    precision = res$precision
    ranking.loss = res$ranking_loss
    recall = res$recall
    subset.accuracy = res$subset_accuracy
    
    evaluated = data.frame(accuracy, example.auc, average.precision,
                           coverage, f1, hamming.loss, 
                           macro.auc, macro.f1, macro.precision, macro.recall,
                           micro.auc, micro.f1, micro.precision, micro.recall,
                           one.error, precision, ranking.loss, recall,
                           subset.accuracy)
    measures = colnames(evaluated)
    evaluated.2 = t(evaluated)
    evaluated.3 = data.frame(measures, evaluated.2)
    names(evaluated.3) = c("measures", "evaluated")
    name = paste(FolderSplit, "/fold-", f, "-evaluated-a.csv", sep="")
    write.csv(evaluated.3, name, row.names = FALSE)
    
    
    ###############################################################
    # PLOTANDO ROC CURVE
    name = paste(FolderSplit, "/plot-roc-", f, ".pdf", sep="")
    pdf(name, width = 10, height = 8)
    print(plot(res$roc, print.thres = 'all', print.auc=TRUE, 
               print.thres.cex=0.7, grid = TRUE, identity=TRUE,
               axes = TRUE, legacy.axes = TRUE, 
               identity.col = "#a91e0e", col = "#1161d5"))
    dev.off()
    cat("\n")
    
    
    ###############################################################
    # SALVANDO AS INFORMAÇÕES DO ROC SEPARADAMENTE
    name = paste(FolderSplit, "/roc-fold-", f, ".txt", sep="")
    output.file <- file(name, "wb")
    
    write(" ", file = output.file, append = TRUE)
    write("percent: ", file = output.file, append = TRUE)
    write(res$roc$percent, file = output.file, append = TRUE)
    
    write(" ", file = output.file, append = TRUE)
    write("sensitivities: ", file = output.file, append = TRUE)
    write(res$roc$sensitivities, file = output.file, append = TRUE)
    
    write(" ", file = output.file, append = TRUE)
    write("specificities: ", file = output.file, append = TRUE)
    write(res$roc$specificities, file = output.file, append = TRUE)
    
    write(" ", file = output.file, append = TRUE)
    write("thresholds: ", file = output.file, append = TRUE)
    write(res$roc$thresholds, file = output.file, append = TRUE)
    
    write(" ", file = output.file, append = TRUE)
    write("direction: ", file = output.file, append = TRUE)
    write(res$roc$direction, file = output.file, append = TRUE)
    
    write(" ", file = output.file, append = TRUE)
    write("cases: ", file = output.file, append = TRUE)
    write(res$roc$cases, file = output.file, append = TRUE)
    
    write(" ", file = output.file, append = TRUE)
    write("controls: ", file = output.file, append = TRUE)
    write(res$roc$controls, file = output.file, append = TRUE)
    
    write(" ", file = output.file, append = TRUE)
    write("auc: ", file = output.file, append = TRUE)
    write(res$roc$auc, file = output.file, append = TRUE)
    
    write(" ", file = output.file, append = TRUE)
    write("original predictor: ", file = output.file, append = TRUE)
    write(res$roc$original.predictor, file = output.file, append = TRUE)
    
    write(" ", file = output.file, append = TRUE)
    write("original response: ", file = output.file, append = TRUE)
    write(res$roc$original.response, file = output.file, append = TRUE)
    
    write(" ", file = output.file, append = TRUE)
    write("predictor: ", file = output.file, append = TRUE)
    write(res$roc$predictor, file = output.file, append = TRUE)
    
    write(" ", file = output.file, append = TRUE)
    write("response: ", file = output.file, append = TRUE)
    write(res$roc$response, file = output.file, append = TRUE)
    
    write(" ", file = output.file, append = TRUE)
    write("levels: ", file = output.file, append = TRUE)
    write(res$roc$levels, file = output.file, append = TRUE)
    
    close(output.file)
    
    
    ###############################################################
    # SALVANDO AS OUTRAS INFORMAÇÕES
    name = paste(FolderSplit, "/roc-info-fold-", f, ".txt", sep="")
    sink(name, type = "output")
    print(res$roc)
    cat("\n\n")
    str(res)
    sink()
    
    ###############################################
    bipartition = data.frame(y_trues, y_preds)
    
    # número de instâncias do conjunto
    num.instancias = nrow(bipartition)
    
    # número de rótulos do conjunto
    num.rotulos = ncol(y_trues)
    
    # número de instâncias positivas
    num.positive.instances = apply(bipartition, 2, sum)
    
    # número de instâncias negativas
    num.negative.instances = num.instancias - num.positive.instances 
    
    # salvando
    res = rbind(num.positive.instances, num.negative.instances)
    name = paste(FolderSplit, "/instances-pn-", f, ".csv", sep="")
    write.csv(res, name)
    
    # calcular rótulo verdadeiro igual a 1
    true_1 = data.frame(ifelse(y_trues==1,1,0))
    total_true_1 = apply(true_1, 2, sum)
    
    # calcular rótulo verdadeiro igual a 0
    true_0 = data.frame(ifelse(y_trues==0,1,0))
    total_true_0 = apply(true_0, 2, sum)
    
    # calcular rótulo predito igual a 1
    pred_1 = data.frame(ifelse(y_preds==1,1,0))
    total_pred_1 = apply(pred_1, 2, sum)
    
    # calcular rótulo verdadeiro igual a 0
    pred_0 = data.frame(ifelse(y_preds==0,1,0))
    total_pred_0 = apply(pred_0, 2, sum)
    
    matriz_totais = cbind(total_true_0, total_true_1, total_pred_0, total_pred_1)
    row.names(matriz_totais) = nomes.rotulos
    name = paste(FolderSplit, "/trues-preds-", f, ".csv", sep="")
    write.csv(matriz_totais, name)
    
    # Verdadeiro Positivo: O modelo previu 1 e a resposta correta é 1
    TPi  = data.frame(ifelse((true_1 & true_1),1,0))
    tpi = paste(nomes.rotulos, "-TP", sep="")
    names(TPi) = tpi
    
    # Verdadeiro Negativo: O modelo previu 0 e a resposta correta é 0
    TNi  = data.frame(ifelse((true_0 & pred_0),1,0))
    tni = paste(nomes.rotulos, "-TN", sep="")
    names(TNi) = tni
    
    # Falso Positivo: O modelo previu 1 e a resposta correta é 0
    FPi  = data.frame(ifelse((true_0 & pred_1),1,0))
    fpi = paste(nomes.rotulos, "-FP", sep="")
    names(FPi) = fpi
    
    # Falso Negativo: O modelo previu 0 e a resposta correta é 1
    FNi  = data.frame(ifelse((true_1 & pred_0),1,0))
    fni = paste(nomes.rotulos, "-FN", sep="")
    names(FNi) = fni
    
    fpnt = data.frame(TPi, FPi, FNi, TNi)
    name = paste(FolderSplit, "/fpnt-", f, ".csv", sep="")
    write.csv(fpnt, name, row.names = FALSE)
    
    # total de verdadeiros positivos
    TPl = apply(TPi, 2, sum)
    tpl = paste(nomes.rotulos, "-TP", sep="")
    names(TPl) = tpl
    
    # total de verdadeiros negativos
    TNl = apply(TNi, 2, sum)
    tnl = paste(nomes.rotulos, "-TN", sep="")
    names(TNl) = tnl
    
    # total de falsos negativos
    FNl = apply(FNi, 2, sum)
    fnl = paste(nomes.rotulos, "-FN", sep="")
    names(FNl) = fnl
    
    # total de falsos positivos
    FPl = apply(FPi, 2, sum)
    fpl = paste(nomes.rotulos, "-FP", sep="")
    names(FPl) = fpl
    
    matriz_confusao_por_rotulos = data.frame(TPl, FPl, FNl, TNl)
    colnames(matriz_confusao_por_rotulos) = c("TP","FP", "FN", "TN")
    row.names(matriz_confusao_por_rotulos) = nomes.rotulos
    name = paste(FolderSplit, "/mc-", f, ".csv", sep="")
    write.csv(matriz_confusao_por_rotulos, name)
    
    # apagando arquivos
    unlink(train.file.name)
    unlink(test.file.name)
    unlink(val.file.name)
    
    
    #f = f + 1
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
    
    total.col = apply(conf.mat, 2, sum)
    total.row = apply(conf.mat, 1, sum)
    
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
    
    conf.mat = data.frame(conf.mat, conf.mat.perc, total.col,
                          wrong, correct, wrong.perc, correct.perc)
    
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
  write.csv(confMatFinal, 
            paste(diretorios$folderGlobal, "/All-Folds-Global.csv", sep=""),
            row.names = FALSE)
  
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
            paste(diretorios$folderGlobal, "/standard-deviation-10-folds.csv", sep=""), 
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
