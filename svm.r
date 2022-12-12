library(readxl)
library(e1071)
library(rpart)

##################################################
###############SIMULAÇÃO##########################
##################################################

dataset_svm_train=matrix(NA,ncol=31,nrow=1000)

for (k in 1:nrow(dataset_svm_train)){
  if(k<=200)dataset_svm_train[k,]=c(arima.sim(model = list(ar=0.1),n=30),1)
  if(k>200)dataset_svm_train[k,]=c(arima.sim(model = list(ar=0.8),n=30),0)
}

#Separando dados para teste
dataset_svm_train=as.data.frame(dataset_svm_train)
dataset_svm_test=data.frame(dataset_svm_train[156:185,])
dataset_svm_test=rbind(dataset_svm_test,dataset_svm_train[(nrow(dataset_svm_train)-29):(nrow(dataset_svm_train)),])
dataset_svm_train=dataset_svm_train[-(156:185),]
dataset_svm_train=dataset_svm_train[-((nrow(dataset_svm_train)-29):(nrow(dataset_svm_train))),]

#################################################
####################SVM##########################
#################################################
classifier = svm(formula =  V31~ .,
                 data = dataset_svm_train,
                 type = 'one-classification',
                 kernel = 'radial')
#summary(classifier)
rpart.model = rpart(V31~ ., data = dataset_svm_train,method = "class")
pred=predict(classifier,newdata = dataset_svm_test[,-31])
summary(rpart.model)
par(mfrow=c(1,1))
plot(pred)
plot(as.numeric(classifier$fitted),type='l')
plot(classifier$decision.values)




##################################################
###############APLICAÇÃO##########################
##################################################
setwd("C:/Users/vitor/OneDrive/Área de Trabalho/Bases Contratos W/ARQ WIN/DATA")
mar2022=read_excel("C:/Users/vitor/OneDrive/Área de Trabalho/Bases Contratos W/ARQ WIN/DATA/MAR2022.xlsx")
fev2022=read_excel("C:/Users/vitor/OneDrive/Área de Trabalho/Bases Contratos W/ARQ WIN/DATA/FEB2022.xlsx")
jan2022=read_excel("C:/Users/vitor/OneDrive/Área de Trabalho/Bases Contratos W/ARQ WIN/DATA/JAN2022.xlsx")
dec2021=read_excel("C:/Users/vitor/OneDrive/Área de Trabalho/Bases Contratos W/ARQ WIN/DATA/DEC2021.xlsx")
nov2021=read_excel("C:/Users/vitor/OneDrive/Área de Trabalho/Bases Contratos W/ARQ WIN/DATA/NOV2021.xlsx")
out2021=read_excel("C:/Users/vitor/OneDrive/Área de Trabalho/Bases Contratos W/ARQ WIN/DATA/OUT2021.xlsx")

historico_data=c(out2021$Data,nov2021$Data,dec2021$Data,jan2022$Data,fev2022$Data)
historico_fechamento=c(out2021$Fechamento,nov2021$Fechamento,dec2021$Fechamento,jan2022$Fechamento,fev2022$Fechamento)
historico_minima=c(out2021$Mínima,nov2021$Mínima,dec2021$Mínima,jan2022$Mínima,fev2022$Mínima)
historico_maxima=c(out2021$Máxima,nov2021$Máxima,dec2021$Máxima,jan2022$Máxima,fev2022$Máxima)
historico_abertura=c(out2021$Abertura,nov2021$Abertura,dec2021$Abertura,jan2022$Abertura,fev2022$Abertura)
historico_ma=c(out2021$`Média Móvel A [100]`,nov2021$`Média Móvel A [100]`,dec2021$`Média Móvel A [100]`,jan2022$`Média Móvel A [100]`,fev2022$`Média Móvel A [100]`)

Data=data.frame(historico_fechamento,historico_minima,historico_maxima,historico_abertura,historico_ma,historico_data)

Data$historico_data=format(Data$historico_data, "%d-%m-%Y %H:%M:%S")
Data=Data[order(as.Date(Data$historico_data,format = "%d-%m-%Y %H:%M:%S"),decreasing = F),]

Data$Retorno=c(0,Data$historico_fechamento[2:length(historico_fechamento)]-Data$historico_fechamento[1:(length(historico_fechamento)-1)])

plot(Data$historico_fechamento,type = 'l')
plot(Data$Retorno,type = 'l')

#Calcula vetor de volatilidade do retorno por x candle de 2 min
x=30 #volatilidade em 1 hr se x=30
p=31
i=0
vetor_volatil=vector()
while (p <= length(Data$historico_fechamento)){
  i=i+1
  vetor_volatil[i]=sqrt(var(Data$Retorno[(p-30):(p-1)]))
  p=p+(30)
}
plot(vetor_volatil,type = 'l')
vetor_volatil_final=vetor_volatil[vetor_volatil<200]
vetor_volatil_final_index=which(vetor_volatil<200)

plot(vetor_volatil_final)

#Calcula distancia entre fechamento da janela anterior e o max e min na janela de x candles de 2 min (se dist for maior pro max selecionala max, se nao min)
max_retorno_janela=vector()
p=31
c=0
max_retorno_janela_index=vector()
for (i in vetor_volatil_final_index){
  c=c+1
  c_index=vetor_volatil_final_index[i]
  max_retorno_janela[c]=max(Data$historico_abertura[c_index]-min(Data$historico_minima[c_index:(c_index+(p-1)-1)]),
                           max(Data$historico_maxima[c_index:(c_index+(p-1)-1)])-Data$historico_abertura[c_index])
  max_retorno_janela_index[c]=c_index
}
plot(max_retorno_janela)
vetor_mx_retorno_final_index=max_retorno_janela_index[max_retorno_janela>600]
par(mfrow=c(5,2))
for (i in 21:30){
  c_index=vetor_mx_retorno_final_index[i]
  plot(Data$Retorno[(c_index+30):(c_index+1)],type='l')
}

#####################################################
#Criando matriz de dados de treinamento
N=30 #quantos periodos para traz vao ser usados para classificaçao
vetor_mx_retorno_final_index=vetor_mx_retorno_final_index[vetor_mx_retorno_final_index>N]
c=0
vetor_mx_retorno_final_index_train=vetor_mx_retorno_final_index[1:(length(vetor_mx_retorno_final_index)-30)]
vetor_mx_retorno_final_index_test=vetor_mx_retorno_final_index[(length(vetor_mx_retorno_final_index)-30+1):length(vetor_mx_retorno_final_index)]
dataset_svm_train=matrix(NA,nrow=length(vetor_mx_retorno_final_index_train),ncol=N+1)
for (i in 1:length(vetor_mx_retorno_final_index_train)){
  c=c+1
  c_index=vetor_mx_retorno_final_index_train[i]
  dataset_svm_train[c,1:N]=Data$Retorno[(c_index-N+1):c_index]
  dataset_svm_train[c,(N+1)]=1
}
vetor_index_failgroup=seq(31,length(Data$Retorno),1)
vetor_index_failgroup=vetor_index_failgroup[-vetor_mx_retorno_final_index]
vetor_index_failgroup=vetor_index_failgroup[-(vetor_mx_retorno_final_index+1)]
c=0
for (i in 1:length(vetor_index_failgroup)){
  c=c+1
  c_index=vetor_index_failgroup[i]
  dataset_svm_train=rbind(dataset_svm_train,c(Data$Retorno[(c_index-N+1):c_index],0))
}

#Separando dados para teste
dataset_svm_train=as.data.frame(dataset_svm_train)
dataset_svm_train=dataset_svm_train[-((nrow(dataset_svm_train)-20800):nrow(dataset_svm_train)),]
dataset_svm_test=data.frame(dataset_svm_train[156:185,])
dataset_svm_test=rbind(dataset_svm_test,dataset_svm_train[(nrow(dataset_svm_train)-29):(nrow(dataset_svm_train)),])
dataset_svm_train=dataset_svm_train[-(156:185),]
dataset_svm_train=dataset_svm_train[-((nrow(dataset_svm_train)-29):(nrow(dataset_svm_train))),]

#################################################
####################SVM##########################
#################################################
classifier = svm(formula =  V31~ .,
                 data = dataset_svm_train,
                 type = 'C-classification',
                 kernel = 'polynomial',degree=5)
#summary(classifier)
rpart.model = rpart(V31~ ., data = dataset_svm_train,method = "class")
pred=predict(classifier,newdata = dataset_svm_test[,-31])
summary(rpart.model)
par(mfrow=c(1,1))
plot(pred)
plot(as.numeric(classifier$fitted),type='l')
plot(classifier$decision.values)

