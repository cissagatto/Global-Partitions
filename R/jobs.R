rm(list=ls())

###############################################################################
# Global Partitions with Ensemble of classifier chain                         #
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
FolderScripts = paste(FolderRoot, "/R", sep = "")


###############################################################################
# LOAD LIBRARY/PACKAGE                                                        #
###############################################################################
library(stringr)


###############################################################################
# READING DATASET INFORMATION FROM DATASETS-ORIGINAL.CSV                      #
###############################################################################
datasets = data.frame(read.csv("datasets-original.csv"))
n = nrow(datasets)


###############################################################################
# CREATING FOLDER TO SAVE CONFIG FILES                                        #
###############################################################################
FolderJob = paste(FolderRoot, "/jobs", sep = "")
if (dir.exists(FolderJob) == FALSE) {dir.create(FolderJob)}

FolderCF = paste(FolderRoot, "/config-files", sep="")
if(dir.exists(FolderCF)==FALSE){dir.create(FolderCF)}


###############################################################################
# QUAL PACOTE USAR
###############################################################################
pacote = c("utiml", "mulan")


###############################################################################
# CREATING CONFIG FILES FOR EACH DATASET                                      #
###############################################################################
w = 1
while(w<=2){
  
  FolderPa = paste(FolderJob, "/", pacote[w], sep="")
  if(dir.exists(FolderPa)==FALSE){dir.create(FolderPa)}
  
  FolderCa = paste(FolderCF, "/", pacote[w], sep="")
  if(dir.exists(FolderCa)==FALSE){dir.create(FolderCa)}
  
  cat("\n================================================")
  cat("\nPackage: \t", pacote[w])
  
  i = 1
  while (i <= n) {
    
    # select the specific dataset
    dataset = datasets[i, ]
    
    # print dataset name
    cat("\n\tDataset: \t", dataset$Name)
    
    # job name - 
    job_name = paste("eg-", dataset$Name, sep = "")
    
    # directory name - "/scratch/eg-3s-bbc1000"
    folder_name = paste("/scratch/", job_name, sep = "")
    
    # Confi File Name - "eg-3s-bbc1000.csv"
    file_name = paste("eg-", dataset$Name, ".csv", sep="")
    
    # sh file name - "~/Global-Partitions/jobs/utiml/eg-3s-bbc1000.sh
    sh_name = paste(FolderPa, "/", job_name, ".sh", sep = "")
    
    # config file name - "~/Global-Partitions/config-files/utiml/eg-3s-bbc1000.csv"
    config_name = paste(FolderCa, "/", file_name, sep = "")
    
    # start writing
    output.file <- file(sh_name, "wb")
    
    # bash parameters
    write("#!/bin/bash", file = output.file)
    
    str.1 = paste("#SBATCH -J ", job_name, sep = "")
    write(str.1, file = output.file, append = TRUE)
    
    write("#SBATCH -o %j.out", file = output.file, append = TRUE)
    
    # number of processors
    write("#SBATCH -n 1", file = output.file, append = TRUE)
    
    # number of cores
    write("#SBATCH -c 10", file = output.file, append = TRUE)
    
    # uncomment this line if you are using slow partition
    # write("#SBATCH --partition slow", file = output.file, append = TRUE)
    
    # uncomment this line if you are using slow partition
    # write("#SBATCH -t 720:00:00", file = output.file, append = TRUE)
    
    # comment this line if you are using slow partition
    write("#SBATCH -t 128:00:00", file = output.file, append = TRUE)
    
    # uncomment this line if you need to use all node memory
    # write("#SBATCH --mem=0", file = output.file, append = TRUE)
    
    # amount of node memory you want to use
    # comment this line if you are using -mem=0
    write("#SBATCH --mem-per-cpu=30GB", file = output.file, append = TRUE)
    # write("#SBATCH -mem=0", file = output.file, append = TRUE)
    
    # email to receive notification
    write("#SBATCH --mail-user=elainegatto@estudante.ufscar.br",
          file = output.file, append = TRUE)
    
    # type of notification
    write("#SBATCH --mail-type=ALL", file = output.file, append = TRUE)
    write("", file = output.file, append = TRUE)
    
    # FUNCTION TO CLEAN THE JOB
    str.2 = paste("local_job=",  "\"/scratch/", job_name, "\"", sep = "")
    write(str.2, file = output.file, append = TRUE)
    write("function clean_job(){", file = output.file, append = TRUE)
    str.3 = paste(" echo", "\"CLEANING ENVIRONMENT...\"", sep = " ")
    write(str.3, file = output.file, append = TRUE)
    str.4 = paste(" rm -rf ", "\"${local_job}\"", sep = "")
    write(str.4, file = output.file, append = TRUE)
    write("}", file = output.file, append = TRUE)
    write("trap clean_job EXIT HUP INT TERM ERR", 
          file = output.file, append = TRUE)
    write("", file = output.file, append = TRUE)
    
    
    # MANDATORY PARAMETERS
    write("set -eE", file = output.file, append = TRUE)
    write("umask 077", file = output.file, append = TRUE)
    
    
    write("", file = output.file, append = TRUE)
    write("echo =============================================================", 
          file = output.file, append = TRUE)
    str.5 = paste("echo SBATCH: RUNNING GLOBAL PARTITIONS FOR ", 
                 " ", pacote[w], " ", dataset$Name, sep="")
    write(str.5, file = output.file, append = TRUE)
    write("echo =============================================================", 
          file = output.file, append = TRUE)
    
    
    write("", file = output.file, append = TRUE)
    write("echo DELETING THE FOLDER", file = output.file, append = TRUE)
    str.6 = paste("rm -rf ", folder_name, sep = "")
    write(str.6, file = output.file, append = TRUE)
    
    
    write("", file = output.file, append = TRUE)
    write("echo CREATING THE FOLDER", file = output.file, append = TRUE)
    str.7 = paste("mkdir ", folder_name, sep = "")
    write(str.7, file = output.file, append = TRUE)
    
    
    write("", file = output.file, append = TRUE)
    write("echo COPYING CONDA ENVIRONMENT", file = output.file, append = TRUE)
    str.8 = paste("cp /home/u704616/miniconda3.tar.gz ", folder_name, sep ="")
    write(str.8 , file = output.file, append = TRUE)
    
    
    write(" ", file = output.file, append = TRUE)
    write("echo UNPACKING MINICONDA", file = output.file, append = TRUE)
    str.9 = paste("tar xzf ", folder_name, "/miniconda3.tar.gz -C ", 
                  folder_name, sep = "")
    write(str.9 , file = output.file, append = TRUE)
    
    
    write(" ", file = output.file, append = TRUE)
    write("echo DELETING MINICONDA TAR.GZ", file = output.file, append = TRUE)
    str.10 = paste("rm -rf ", folder_name, "/miniconda3.tar.gz", sep = "")
    write(str.10, file = output.file, append = TRUE)
    
    
    write(" ", file = output.file, append = TRUE)
    write("echo SOURCE", file = output.file, append = TRUE)
    str.11 = paste("source ", folder_name,
                  "/miniconda3/etc/profile.d/conda.sh ", sep = "")
    write(str.11, file = output.file, append = TRUE)
    
    
    write(" ", file = output.file, append = TRUE)
    write("echo ACTIVATING MINICONDA ", file = output.file, append = TRUE)
    write("conda activate AmbienteTeste", file = output.file, append = TRUE)
    write(" ", file = output.file, append = TRUE)
    
    
    write("echo RUNNING", file = output.file, append = TRUE)
    str.12 = paste("Rscript /home/u704616/Global-ECC/R/global.R ", 
                 config_name, sep = "")
    write(str.12, file = output.file, append = TRUE)
    write(" ", file = output.file, append = TRUE)
    
    
    write("echo DELETING JOB FOLDER", file = output.file, append = TRUE)
    str.13 = paste("rm -rf ", folder_name, sep = "")
    write(str.13, file = output.file, append = TRUE)
    
    
    write("", file = output.file, append = TRUE)
    write("echo ==================================", 
          file = output.file, append = TRUE)
    write("echo SBATCH: ENDED SUCCESSFULLY", file = output.file, append = TRUE)
    write("echo ==================================", 
          file = output.file, append = TRUE)
    
    close(output.file)
    
    i = i + 1
    gc()
  }
  
  cat("\n================================================")
  
  w = w + 1
  gc()
}

cat("\n================================================")


###############################################################################
# Please, any errors, contact us: elainececiliagatto@gmail.com                #
# Thank you very much!                                                        #                                #
###############################################################################
