set.seed(1234567890)
library(neuralnet)
library(hydroGOF)
library( leaps )
library(arules) 

dados <- read.csv(file="/Users/Asus/Desktop/SRCR/Trabalho/bank-additional-full1.csv",header=TRUE,sep=";")

summary(dados)

treino <- dados[1:25000,]

teste <- dados[25001:41188,]
summary(teste)
View(dados$pdays)
formula <- y ~ age+job+marital+education+default+housing+loan+campaign+pdays+previous+poutcome+emp.var.rate+cons.price.idx+cons.conf.idx+euribor3m

selecao <- regsubsets(formula,dados,nvmax=15)
summary(selecao)

ex <- neuralnet(formula,treino,hidden = c(4), lifesign = "full", linear.output=FALSE, threshold = 0.1)

rede <- neuralnet(formula,treino,hidden=c(3,2),linear.output=FALSE,lifesign = "full",threshold = 0.1)

plot(ex,rep="best")

teste1 <-subset(teste,select=c("age","job","marital","education","default","housing","loan","campaign","pdays","previous"))

ex.resultados <- compute(ex,teste1)


resultados <- data.frame(atual=teste$y,previsao=ex.resultados$net.result)

View(resultados)

rmse(c(teste$y),c(resultados$previsao))
