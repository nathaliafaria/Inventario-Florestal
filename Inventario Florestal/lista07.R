setwd('C:/Users/naaht/Desktop/UFSCar/Inventario/lista07')
parc<-read.csv2('parcelas.csv'); View(parc)

#Valores constantes
areafrag<-157
largura<-10
comparc<-50
comprimento<-2130
sig<-0.01

comp_frag <- 2130
area_frag <- 157
larg_frag <- 10

volumeporfaixa<-aggregate(list(volume=parc$volume), list(faixa=parc$faixa), sum); View(volumeporfaixa)
teste<-aggregate(list(parcelas=parc$parcela),list(faixa=parc$faixa), length); View(teste)
base<-merge(volumeporfaixa,teste); View(base)
base$areafrag<-area_frag
base$comprimento<-comp_frag
base$largura<-larg_frag 
base$areafaixa<-base$parcelas*50*10/10000 #mudar


x<-base$areafaixa
y<-base$volume #vari?vel de interesse

nt <- comp_frag/larg_frag
nt


# Plotando os gr?ficos da rela??o entre ?rea de parcela e volume
plot(x, y, xlab='Área da parcela', ylab='vtcc', pch='*', col='purple')

# Ajuste linear entre as duas vari?veis
aj <- lm(y~x)

# Adicionando ao gr?fico, linha da rela??o linear entre parcela e volume
lines(x, predict(aj))

# M?dia estimada de x (em ha)
xme <- mean(x)
xme

# M?dia populacional da vari?vel auxiliar
xm <- area_frag/nt
xm

# M?dia estimada de y (em m?)
yme <- mean(y)
yme

# Quantidade de amostras
n <- length(y)
n

# Estimador de regress?o (j? em m?/ha)
b <- sum((y-yme)*(x-xme))/sum((x-xme)^2)
b
# Mesma coisa, muito mais f?cil:
b <- as.numeric(coef(aj)[2])
b

# M?dia estimada pelo estimador de regress?o
ymreg <- yme+b*(xm-xme)
ymreg

# Por??o da vari?ncia de Y que n?o pode ser explicada pela rela??o linear com X (S?yx)
varyx <- sum((y-yme-b*(x-xme))^2)/(n-2)
varyx

# Vari?ncia da m?dia estimada [m^6]
varrmedia <- (1-n/nt)*(1/n)*varyx
varrmedia

# Erro padr?o da m?dia estimada [m?]
erro_pad_med <- sqrt(varrmedia)
erro_pad_med

# Erro invent?rio em m?
erro_inv <- qt(0.995, n-1)*erro_pad_med
erro_inv

# Erro do invent?rio em porcentagem
erro_inv_perc <- (erro_inv/ymreg)*100
erro_inv_perc

# Passando para hectare (utilizar m?dia , utilizada para estimar)
ymreg_ha <- ymreg/xm
ymreg_ha

# Sem corre??o
yme/xm

# ------------------------------ SA?DA ----------------------------- #
cat(paste('Media(m?/parc) = ', round(ymreg,2), sep=''),'\n');
# cat -> para mudar de linha
# paste -> concatenar informa??es

total_fazenda <- ymreg_ha*area_frag
total_fazenda # Em m?

erro_inv_fazenda <- erro_inv*area_frag
erro_inv_fazenda # Em m?

paste('IC:',(total_fazenda-erro_inv_fazenda),'m?/parcela a',(total_fazenda+erro_inv_fazenda),'m?/parcela') #Intevalo de confian?a

library(plyr)
conglomerado<-subset(parc, npa1==1); View(conglomerado)

volumeporfaixacon<-aggregate(list(volume=conglomerado$volume), list(faixa=conglomerado$faixa), sum);
testecon<-aggregate(list(parcelas=conglomerado$parcela),list(faixa=conglomerado$faixa), length);
baseconglomerado<-merge(volumeporfaixacon,testecon); View(baseconglomerado)
baseconglomerado<-rename(baseconglomerado,replace = c('volume'='yi')); baseconglomerado<-rename(baseconglomerado,replace = c('parcelas'='mi'))


(NC<-comprimento/largura) #n?mero de clusters cab?veis na popula??o
(MC<-areafrag*10000/(comparc*largura)) #quantas parcelas cabem dentro da ?rea 
(nc<-nrow(baseconglomerado)) #n?mero de clusters sorteados e medidos em campo
(mic<-sum(baseconglomerado$mi)) #n?mero de elementos medidos nos clusters cluster


#total populacional
(ymc<-sum(baseconglomerado$yi)/mic); #m?dia ponderada dos yi, porque cada cluster tem uma quantidade diferente de elementos
(Y<-MC*ymc); #total populacional

#vari?ncia da popula??o
(s2Mymc<-(NC^2)*((NC-nc)/NC)*(1/nc)*sum((baseconglomerado$yi-baseconglomerado$mi*ymc)^2)/(nc-1))

#erro padr?o populacional
(sMymc<-sqrt(s2Mymc))

#absoluto
(erroabscon<-qt(0.995,n-1)*sMymc)

#relativo
(erroperccon<-erroabscon/Y*100)

#Intervalo de confian?a
(licon<-Y-erroabscon); (lscon<-Y+erroabscon)
cat(paste(round(licon),'<=total<=',round(lscon),'m?',sep=''),'\n') #\n para pular linha; sep='' ? para tirar os espa?os do vetor



####################################################################################
###########       Terceiro plano amostral -> Subamostragem          ################
####################################################################################
subamostragem<-subset(parc, npa2==1); View(subamostragem)

volumeporfaixasub<-aggregate(list(volume=subamostragem$volume), list(faixa=subamostragem$faixa), sum); 
testesub<-aggregate(list(mi=subamostragem$parcela),list(faixa=subamostragem$faixa), length); #quantas parcelas foram sorteadas
teste2<-aggregate(list(Mi=parc$parcela),list(faixa=parc$faixa), length); #quantas parcelas cabem em cada cluster
int<-merge(testesub,teste2)
suba<-merge(volumeporfaixasub,int); View(suba)
suba<-rename(suba,replace = c('volume'='yi')); suba<-rename(suba, replace=c('faixa'='cluster'))

(Ns<-comprimento/largura) #n?mero de clusters cab?veis na popula??o
(ns<-nrow(suba)) #n?mero de clusters sorteados e medidos em campo

#m?dia das parcelas na i-?sima unidade do primeiro est?gio
calc<-aggregate(list(ym=subamostragem$volume),
                list(cluster=subamostragem$faixa),mean) 
suba<-merge(suba, calc)

#vari?ncia das parcelas na i-?sima unidade do primeiro est?gio
calc<-aggregate(list(s2w=subamostragem$volume),
                list(cluster=subamostragem$faixa),var) 
suba<-merge(suba, calc)

#n?mero m?dio de parcelas do 2? estagio cabiveis por unidade do 1? est?gio
(Mm<-sum(suba$Mi)/ns) #quantas parcelas cabem por unidade

#m?dia por parcela
(ym<-mean(subamostragem$volume))

#estimador do total populacional com o valor de Mi de todos os clusters desconhecidos
(Ysub<-(Ns/ns)*sum(suba$Mi*suba$ym))

#varianvicia entre compartimentos
(s2b<-(1/(ns-1))*sum(((suba$Mi/Mm)*(suba$ym-ym))^2))

#variancia da popula??o
(s2ym<-(Ns/ns)*((Mm^2)*(Ns-ns)*s2b+with(suba,sum(Mi*(Mi-mi)*(s2w/mi)))))

#erro padrao do total populacional
(sym<-sqrt(s2ym))

#erro do invent?rio
(erroabssub<-qt(0.995,ns-1)*sym) #o n ? o n?mero de clusters

#erro percentual
(erropercsub<-erroabssub/Ysub*100)

#intervalo de confian?a
paste('IC (m?):', round(Ysub-erroabssub,2),'<=total estimado pela subamostragem<=',round(Ysub+erroabssub,2), 'para n.s.=',sig*100,'%')






######################## Resultados ######################################################
paste('IC (m?):', round(ytraz-erroinvtot,2),'<=total estimado pelo estimador de raz?o<=',round(ytraz+erroinvtot,2), 'para n.s.=',sig*100,'%')
paste('IC (m?):', round(licon),'<=Total estimado pelo conglomerado<=',round(lscon), 'para n.s.=', sig*100,'%')
paste('IC (m?):', round(Ysub-erroabssub,2),'<=total estimado pela subamostragem<=',round(Ysub+erroabssub,2), 'para n.s.=',sig*100,'%')
paste('Erro do invent?rio em porcentagem pelo estimador de raz?o:', round(erroinvperc,4), '%')
paste('Erro do invent?rio em porcentagem pelo conglomerado:', round(erroperccon,4), '%')
paste('Erro do invent?rio em porcentagem pela subamostragem:', round(erropercsub,4), '%')



parc<-read.csv2('parcelas.csv')
length(parc$npa1)
um=subset(parc,parc$npa1==1)
length(um$npa1)
dois=subset(parc,parc$npa2==1)
length(dois$npa1)
