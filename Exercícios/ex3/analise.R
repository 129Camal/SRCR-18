set.seed(1234567890)
library(neuralnet)
library(hydroGOF)
library( leaps )
library(arules) 

dados <- read.csv(file="/Users/Asus/Desktop/SRCR/Trabalho/bank-additional-full1.csv",header=TRUE,sep=",")



treino <- dados[1:25000,]

teste <- dados[25001:41188,]

ca <- c(0.1,0.2,0.)

income1 <- cut(dados$pdays,seq(0,30,5), labels=c(1:6))


View(dados$pdays)
dados$pdays <- as.numeric(income1)
summary(dados$pdays)

formula <- y ~ age+job+marital+education+default+housing+loan+campaign+pdays+previous+poutcome+emp.var.rate+cons.price.idx+cons.conf.idx+euribor3m

formula1 <- y ~ pdays+poutcome+emp.var.rate+cons.price.idx+cons.conf.idx+euribor3m

formula2 <- y ~ age+education+default+pdays+poutcome+emp.var.rate+cons.price.idx+cons.conf.idx+euribor3m

formula3 <- y ~ age+marital+education+default+campaign+pdays+previous+poutcome+emp.var.rate+cons.price.idx+cons.conf.idx+euribor3m


rede1 <- neuralnet(formula,treino,hidden=c(3,2),linear.output=FALSE,lifesign = "full",threshold = 0.1)
rede2 <- neuralnet(formula1,treino,hidden=c(3,2),linear.output=FALSE,lifesign = "full",threshold = 0.1)
rede3 <- neuralnet(formula2,treino,hidden=c(3,2),linear.output=FALSE,lifesign = "full",threshold = 0.1)
rede4 <- neuralnet(formula3,treino,hidden=c(3,2),linear.output=FALSE,lifesign = "full",threshold = 0.1)


set1 <- subset(teste,select=c("age","job","marital","education","default","housing","loan","campaign","pdays","previous","poutcome","emp.var.rate","cons.price.idx","cons.conf.idx","euribor3m"))
set2 <-subset(teste,select=c("pdays","poutcome","emp.var.rate","cons.price.idx","cons.conf.idx","euribor3m"))
set3 <- subset(teste,select=c("age","education","default","pdays","poutcome","emp.var.rate","cons.price.idx","cons.conf.idx","euribor3m"))
set4 <- subset(teste,select=c("age","marital","education","default","campaign","pdays","previous","poutcome","emp.var.rate","cons.price.idx","cons.conf.idx","euribor3m"))


rede1.resultados <- compute(rede1,set1)
rede2.resultados <- compute(rede2,set2)
rede3.resultados <- compute(rede3,set3)
rede4.resultados <- compute(rede4,set4)


resultados1 <- data.frame(atual=teste$y,previsao=rede1.resultados$net.result)
resultados2 <- data.frame(atual=teste$y,previsao=rede2.resultados$net.result)
resultados3 <- data.frame(atual=teste$y,previsao=rede3.resultados$net.result)
resultados4 <- data.frame(atual=teste$y,previsao=rede4.resultados$net.result)



rmse(c(teste$y),c(resultados4$previsao))












