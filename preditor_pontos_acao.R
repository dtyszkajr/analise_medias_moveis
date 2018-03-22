
require(quantmod) # puxa dados financeiros
require(xts) # transforma xts em data.fram???
require(TTR) # medias moveis
require(lattice) # grafs
require(latticeExtra) # grafs plus

# BAIXANDO DADOS DO INDICE BOVESPA
load("data-ibov-20180320.RData")
# lines <- readLines("http://bvmf.bmfbovespa.com.br/indices/ResumoCarteiraTeorica.aspx?Indice=IBOV&idioma=en-us") #leitura do site do indice
# l <- grep("lblCodigo.*>(.{5})<.*$",lines)
# lines <- lines[l]
# nomes <- gsub("^.*>(.*)<.*$","\\1",lines)
# nomes <- paste(nomes,".SA", sep="") #codigos das acoes do indice
# getSymbols(nomes, from="2013-01-01") #dados das funcoes do ibovespa
# ibov <- mget(nomes, envir = globalenv()) # criando lista com as acoes

# PARAMETROS DAS MEDIAS MOVEIS
SMA_VALUE = 40
EMA_VALUE = 9
PLOT_DIAS = 240

# EXEMPLO PLOT GRAF COM ACOES E MEDIAS MOVEIS
getSymbols("GGBR4.SA")
da <- na.omit(GGBR4.SA)
da <- da[(nrow(da)-PLOT_DIAS):nrow(da),]
da$ma <- SMA(da[,4], n=SMA_VALUE) #media aritmetica
da$me <- WMA(da[,4], n=EMA_VALUE, wts=1:9) #media exponencial
dif <- as.numeric(da[(nrow(da)-10):nrow(da),7] - da[(nrow(da)-10):nrow(da),8])
dif <- sign(dif) # sinal da diferenca
dif <- dif-c(dif[1],dif[-10]) # encontrar pontos de cruzamento
plot(da[,4], type="l")
lines(da$ma,col="green")
lines(da$me,col="red")

# SEPARANDO ACOES QUE CRUZAM NOS ULTIMOS 10 DIAS
z <- list()
for(i in 1:length(ibov)){
    x <- na.omit(ibov[[i]])
    da <- reclass(x)
    da$ma <- SMA(da[,4], n=SMA_VALUE)
    da$me <- WMA(da[,4], n=EMA_VALUE, wts=1:9)
    dif <- as.numeric(da[(nrow(da)-10):nrow(da),7] - da[(nrow(da)-10):nrow(da),8])
    dif <- sign(dif)
    dif <- dif-c(dif[1],dif[-10])
    if ( sum(dif) != 0 ) {
        z[[length(z)+1]] <- x}
}

# PLOTANDO TODOS OS GRAFICOS (NAO ESTA FUNFANDO - TODO: CONSERTAR)
par(mfrow=c(3,5)) # grafs das que cruzam
for (i in 1:length(z)){
    dd <- na.omit(z[[i]])
    dd <- dd[max((nrow(dd)-PLOT_DIAS),0):nrow(dd),]
    dd$ma <- SMA(dd[,4], n=SMA_VALUE)
    dd$me <- WMA(dd[,4], n=EMA_VALUE, wts=1:9)
    plot(dd[,4], type="l")
    lines(dd$ma,col="green")
    lines(dd$me,col="red")
}
layout(1)

# PREDIZENDO SENTIDOS DAS MEDIAS MOVEIS PARA PROX DIAS
DIAS_REGRESSAO = 10
DIAS_PREDICAO = 10
DIAS_PLOT = 120
da <- GGBR4.SA
da <- as.data.frame(da)
da <- da[complete.cases(da), ]  # investigar casos de NA
colnames(da)[4] <- "val"
da$ma <- SMA(da[,4], n=SMA_VALUE)
da$me <- WMA(da[,4], n=EMA_VALUE, wts=1:EMA_VALUE)
da$index <- 1:nrow(da)
m1 <- lm(ma~index, data=da[(nrow(da)-DIAS_REGRESSAO):nrow(da),])
pred1 <- data.frame(index=(nrow(da)-DIAS_REGRESSAO):(nrow(da)+DIAS_REGRESSAO+DIAS_PREDICAO))
aux1 <- predict(m1, newdata=pred1)
pred1 <- cbind(pred1,aux1)
m2 <- lm(me~index, data=da[(nrow(da)-DIAS_REGRESSAO):nrow(da),])
pred2 <- data.frame(index=(nrow(da)-DIAS_REGRESSAO):(nrow(da)+DIAS_REGRESSAO+DIAS_PREDICAO))
aux2 <- predict(m2, newdata=pred2)
pred2 <- cbind(pred2,aux2)
plot(val~index,data=da[(nrow(da)-DIAS_PLOT):nrow(da),],
       type="l",xlim=c((nrow(da)-DIAS_PLOT),(nrow(da)+DIAS_PREDICAO+5)))
lines(aux1~index, data=pred1, type="l", col=2)
lines(aux2~index, data=pred2, type="l", col=3)
lines(da$ma,col="4")
lines(da$me,col="5")

# TODO: transformar chunk acima em funcao para varrer acoes buscando aquelas que vao cruzar nos prox X dias
