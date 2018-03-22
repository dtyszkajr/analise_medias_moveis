
require(quantmod) # puxa dados financeiros
#require(xts) # transforma xts em data.fram???
#require(TTR) # medias moveis
require(lattice) # grafs
#require(latticeExtra) # grafs plus

# BAIXANDO DADOS DO INDICE BOVESPA
load("data-ibov-20180320.RData")
# lines <- readLines("http://bvmf.bmfbovespa.com.br/indices/ResumoCarteiraTeorica.aspx?Indice=IBOV&idioma=en-us") #leitura do site do indice
# l <- grep("lblCodigo.*>(.{5})<.*$",lines)
# lines <- lines[l]
# nomes <- gsub("^.*>(.*)<.*$","\\1",lines)
# nomes <- paste(nomes,".SA", sep="") #codigos das acoes do indice
# getSymbols(nomes, from="2013-01-01") #dados das funcoes do ibovespa
# ibov <- mget(nomes, envir = globalenv()) # criando lista com as acoes

# funcao que calcula o resultado (lucro ou prej) de uma acao
# para determinados parametros de medias moveis exponenciais e aritmeticas
resultado.acao <- function(x, na, ne){
  da <- as.data.frame(x)
  da <- da[,4]
  da <- da[(length(da)-DIAS_CORTE):length(da)]
  dama <- SMA(da, n=na) #media aritmetica
  dame <- WMA(da, n=ne, wts=1:ne) #media exponencial
  da <- data.frame(fe=da, ma=dama, me=dame)
  dif <- as.numeric(da$me - da$ma)
  dif <- sign(dif) # sinal da diferenca
  dif <- dif-c(dif[1],dif[-length(dif)]) # encontrar pontos de cruzamento
  i <- max(na, ne)+1
  compra <- "zero"
  result <- 0
  while(i<nrow(da)){
    if(dif[i] == 2){compra <- da$fe[i+1]}
    if(dif[i] == -2){
      if(compra!="zero"){
        venda <- da$fe[i+1]
        result <- result + ((venda - compra)/venda)
        compra <- "zero"
      }
    }
    i <- i+1
  }
  return(result)
}

# calcula resultado (lucro ou prej) para varias acoes ao mesmo tempo
# usa a funcao de calculo de resultado por acao resultado.acao
resultado.conjunto <- function(lista, na, ne){
  somas <- data.frame(acao=0, resultado=0)
  for(i in 1:length(lista)){
    r <- resultado.acao(lista[[i]], na=na, ne=ne)
    somas <- rbind(somas, c(i,r))
  }
  return(sum(somas$resultado))
}

## criando grid de resultados

DIAS_CORTE = 180

# load("data-grid_geral_results.RData")

tab_grid <- expand.grid(na=3:120, ne=3:120, res=0)
for(i in 1:nrow(tab_grid)){
  tab_grid$res[i] <- resultado.conjunto(lista=ibov, na=tab_grid$na[i], ne=tab_grid$ne[i])}

# plotando superficie
wireframe(res ~ na*ne, data = tab_grid,
          xlab = "na", ylab = "ne", zlab = "result",
          main = "Resultado por Combinacao de Parametros",
          drape = TRUE,
          colorkey = TRUE
)

# printando melhores combiancoes de parametros
tab_grid[order(tab_grid$res, decreasing=TRUE)[1:5],]

# save.image("data-grid_geral_results.RData")
