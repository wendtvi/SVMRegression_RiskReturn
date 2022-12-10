library(readxl)
library(plotly)

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

plot(x=Data$historico_data,y=Data$historico_fechamento,type = 'l',)
plot(Data$Retorno,type = 'l')


fig = Data$ %>% plot_ly(x = ~historico_data, type="candlestick", open = ~historico_abertura, close = ~historico_fechamento,high = ~historico_maxima, low = ~historico_minima) 
fig <- fig %>% add_lines(x = ~historico_data, y = ~historico_ma, name = "Mv Avg",
                         line = list(color = '#E377C2', width = 1),
                         hoverinfo = "none", inherit = F) 
fig <- fig %>% layout(title = "Basic Candlestick Chart")

fig
