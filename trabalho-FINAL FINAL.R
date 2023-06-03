library(ggplot2)
library(readr)
library(dplyr)


# Definir a URL do arquivo CSV
#url <- "https://api.bcb.gov.br/dados/serie/bcdata.sgs.24363/dados?formato=csv"

caminhoPasta <- "C:/Dados/conjuntoDados.csv"
meusDados <- read.csv(caminhoPasta, sep = ";")
print(meusDados)
meusDados$valor

#Separei os valores de 2009 a 2011 e coloquei nesse conjunto de dados valoresMensais e meses

#001 - Dataset

valoresMensais <- c(115.2,115.26,127.81, 123.09,124.41,125.61,131.44,130.7,129.81,132.48,129.62,129.23,125.81,127.61,143.44,136.87,136.52,136.09,141.64,141.55,139.46,139.33,139.68,136.69,132.66,136.18,144.93,139.89,143.23,141.75,145.19,147.51,142.3,142.02,141.87,139.23)

meses <- c("01/01/2009","01/02/2009"," 01/03/2009 "," 01/04/2009"," 01/05/2009"," 01/06/2009","01/07/2009","01/08/2009","01/09/2009","01/10/2009","01/11/2009","01/12/2009","01/01/2010","01/02/2010","01/03/2010","01/04/2010","01/05/2010","01/06/2010","01/07/2010","01/08/2010","01/09/2010", "01/10/2010","01/11/2010","01/12/2010"  
            ,"01/01/2011","01/02/2011","01/03/2011","01/04/2011","01/05/2011","01/06/2011","01/07/2011","01/08/2011","01/09/2011","01/10/2011","01/11/2011","01/12/2011")
  
ano <- c(2009,2010,2011) 

#001 - Gráfico 

barplot(valoresMensais, names.arg = meses, xlab = "Mês", ylab = "Lucro Mensal", main = "Lucro Mensal ")


#002 - Dataframe GGPLOT
dadosBarra <- data.frame(ano = c(2009,2010,2011), valoresMensais = c(115.2,115.26,127.81, 123.09,124.41,125.61,131.44,130.7,129.81,132.48,129.62,129.23,125.81,127.61,143.44,136.87,136.52,136.09,141.64,141.55,139.46,139.33,139.68,136.69,132.66,136.18,144.93,139.89,143.23,141.75,145.19,147.51,142.3,142.02,141.87,139.23))

#002 - GGplot
ggplot(dadosBarra,  aes(x= ano, y = valoresMensais)) + geom_line() + geom_point() + labs(x = "Ano", y = "Valor", title = "Gráfico - 01") + theme_minimal()


#Grafico Barra002
lucroMensal <- c(100,150,300)
meses <- c("Janeiro","Fevereiro","Março")

barplot(lucroMensal, names.arg = meses, xlab = "Mês", ylab = "Lucro Mensal", main = "Lucro Mensal ")


#003 - Dataset

valoresMensais <- c(115.2,115.26,127.81, 123.09,124.41,125.61,131.44,130.7,129.81,132.48,129.62,129.23,125.81,127.61,143.44,136.87,136.52,136.09,141.64,141.55,139.46,139.33,139.68,136.69,132.66,136.18,144.93,139.89,143.23,141.75,145.19,147.51,142.3,142.02,141.87,139.23)

#003 - Outlier
boxplot(valoresMensais, outline = TRUE)



# Dados mensais dos anos de 2009 e 2011
dadosMensais <- c(115.2,115.26,127.81, 123.09,124.41,125.61,131.44,130.7,129.81,132.48,129.62,129.23,125.81,127.61,143.44,136.87,136.52,136.09,141.64,141.55,139.46,139.33,139.68,136.69,132.66,136.18,144.93,139.89,143.23,141.75,145.19,147.51,142.3,142.02,141.87,139.23)

#Variaveis
minimo <- min(dadosMensais)

maximo <- max(dadosMensais)

desvio_padrao <- sd(dadosMensais)

mediana <- median(dadosMensais)

quartis <- quantile(dadosMensais)

outliers <- boxplot(dadosMensais)

#Imprimindo Valores
print(minimo)
print(maximo)
print(desvio_padrao)
print(mediana)
print(quartis)

#Regressão Linear


v10 <- c(125.81,127.61,143.44,136.87,136.52,136.09,141.64,141.55,139.46,139.33,139.68,136.69)

v11 <- c(132.66,136.18,144.93,139.89,143.23,141.75,145.19,147.51,142.3,142.02,141.87,139.23)

data <- data.frame(x = v10, y = v11)

# Ajustar o modelo de regressão linear
modelo <- lm(y ~ x, data = data)
print("exercicio 1")
summary(modelo)

#
print('exercicio 2')
modelo2 <- lm(y ~ poly(x, 2), data = data)
summary(modelo2)