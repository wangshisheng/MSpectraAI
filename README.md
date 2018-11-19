# MSpectraAI<img src="www/MSpectraAI_logo.jpg" align="right" height="200" width="200"/>
#### A powerful platform for deciphering proteome profiling of multi-tumour mass spectrometry data using deep learning 

## Brief Description
**<font size='5'> MSpectraAI </font>** is a free, user-friendly and comprehensive web tool for mining and classifying raw LC-MS<sup>2</sup>-based proteomics or metabolomics data of different samples using deep learning models. Users can also built your own deep neural network model in this software. It is developed with [R](https://www.r-project.org/) (https://www.r-project.org/) and an example is shown here: [http://www.omicsolution.org/wukong/MSpectraAI/](http://www.omicsolution.org/wukong/MSpectraAI/).

## Friendly Tips
* **Run this tool locally**. As we know, the raw data from mass spectrometer are usually very large. You can analyze your data on our web server, but the analysis speed will be slower.
* **Be familiar with the basic usage of R language**. This web tool is developed with R, therefore, if you know some basic knowledge about R, it will help you understand this tool better. However, you need not worry if you know nothing about R, and you can learn to use our tool expertly as well after reading our manual.

## Preparation
- **Install R**. You can download R from here:[https://www.r-project.org/](https://www.r-project.org/).
- **Install RStudio** (Recommendatory but not necessary). You can download RStudio from here:[https://www.rstudio.com/](https://www.rstudio.com/).

## Install Packages
```r
#Packages
needpackages<-c("devtools","shiny","shinyjs","shinyBS","ggplot2","ggjoy","openxlsx","gdata","DT","gtools","ggsci","mzR",
                "plyr","tidyr","abind","data.table","parallel","ggrastr","ggthemes","viridis","glue","ComplexHeatmap",
                "impute","circlize","ROCR","keras")
#Check and install function
CheckInstallFunc <- function(x){
  for( i in x ){
    #  require returns TRUE invisibly if it was able to load package
    if( ! require( i , character.only = TRUE ) ){
      #  If package was not able to be loaded then re-install
      BiocManager::install(i, dependencies = TRUE)
      if( ! require( i , character.only = TRUE ) ) install.packages( i , dependencies = TRUE )
      if(i=="ggrastr"){
        devtools::install_github('VPetukhov/ggrastr')
      }
    }
  }
}
#Start to check and install
CheckInstallFunc(needpackages)
#R interface to Keras: https://keras.rstudio.com/
library(keras)
install_keras()
```

## Download and Run locally
You can download our tool from this github and unzip the file
