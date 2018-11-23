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
#Packages
needpackages<-c("devtools","shiny","shinyjs","shinyBS","ggplot2","ggjoy","openxlsx","gdata","DT","gtools","ggsci","mzR",
                "plyr","tidyr","abind","data.table","parallel","ggrastr","ggthemes","viridis","glue","ComplexHeatmap",
                "impute","circlize","ROCR","keras")
#
CheckInstallFunc(needpackages)
