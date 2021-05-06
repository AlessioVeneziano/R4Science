################################################################Install packages
if (!requireNamespace("BiocManager",quietly=T))
  install.packages("BiocManager")
BiocManager::install("EBImage")

install.packages("mmand",dependencies=T)
install.packages("Rdimtools",dependencies=T)
install.packages("Morpho",dependencies=T)

install.packages("devtools",dependencies=T)
devtools::install_github("https://github.com/AlessioVeneziano/IndianaBones",local=F)


##################################################################Call packages
library(EBImage)
library(mmand)
library(Rdimtools)
library(Morpho)
library(indianaBones)


###################################################Isolation of the cancellous bone
data(exampleStack)
Stack<-exampleStack

strel<-makeBrush(5,"disc")
seg<-splitBone(Stack,strel,3,2,3,0)

image(Stack[,,30],col=c(1,0))
image(seg$comp[,,30],col=c(1,0))
image(seg$trab[,,30],col=c(1,0))


######################################END OF SCRIPT
