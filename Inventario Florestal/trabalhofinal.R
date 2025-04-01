##TRABALHO FINAL INVENTARIO# #Nathalia Faria
##Sabendo-se que as parcelas foram selecionadas de forma sistemática e que a variável de interesse é o 
#volume comercial com casca
#diâmetro mínimo de aproveitamento igual a 3cm com casca e altura de toco igual a 0, 1m,
# (α = 5%).

library(readxl)

#  Rodar o arquivo cubagem, para descobrir o volume comercial sem casca (diametro minimo 3 cm)
setwd('C:/Users/naaht/Desktop/UFSCar/Inventario/trabalho/dados')
dir();
dados<-read_excel("C:/Users/naaht/Desktop/UFSCar/Inventario/trabalho/dados/cubagem.xlsx");
View(dados);
names(dados);
dados$dicc<- ((dados$dicc1+dados$dicc2)/2); dados$dicc1<- NULL; dados$dicc2<- NULL;
View(dados); 
cub<-dados[,c('arv','dap','ht','hi','dicc')]; #se for sem casca manter o espscasca
selarv=subset(cub,!ht<hi);
View(selarv);

#--------------------------------------------------------------------------------------------------------

#Cubagem rigorosa - dcomcc: diametro minimo comercial, 3 cm o meu

library(cmrinvflor);
vsmal<-smalian(selarv,dcoms=3,htoco=10,comcasca=T,di_ou_ci='di', dbase_ponta = 3);
View(vsmal);
vsmal$vcomcc<- vsmal$vprod_3;
cubagem <- vsmal[,c('arv','dap','ht','vcomcc')]
View(cubagem);

#--------------------------------------------------------------------------------------------------------

# escolah de modelo volumetrico
#TIREI o matgen dessa etapa 
dados<-cubagem[,c('arv','dap','ht','vcomcc')];
View(dados);

#Modelo volumétrico de Berkhout: v=b0*dap^b1
modelo_berk<-'vcomcc~b0*dap^b1';
ajnlin_berk=nls(modelo_berk,dados,start=list(b0=pi/40000*0.45,b1=2));
coef(ajnlin_berk);
sumario=summary(ajnlin_berk);
syx_berk=sumario$sigma; # erro padrão residual (m³)
syx_berk_perc=syx_berk/mean(dados$vcomcc)*100; # erro padrão residual (%)

#Modelo volumétrico de Spurr: v=b0+b1*dap^2*ht
modelo_spurr<-'vcomcc~I(dap^2*ht)';
ajlin_spurr=lm(modelo_spurr,dados);
coef(ajlin_spurr);
sumario<-summary(ajlin_spurr);
anova(ajlin_spurr);
syx_spurr=sumario$sigma; # erro padrão residual (m³) R2aj = Adjusted R-squared (quanto maior, melhor o modelo)
syx_spurr_perc=syx_spurr/mean(dados$vcomcc)*100; # erro padrão residual (%)

#Modelo volumétrico de Spurr Logarítmico: ln(v)=b0+b1*ln(dap^2*ht)
modelo_spurrlog='log(vcomcc)~I(log(dap^2*ht))';
ajlin_spurrlog=lm(modelo_spurrlog,dados);
anova(ajlin_spurrlog);
sumario=summary(ajlin_spurrlog);
coef(ajlin_spurrlog);
R2adj=sumario$adj.r.squared
R2adjp=round(R2adj*100,2)
y=dados$vcomcc;
D(expression(log(y)),'y');
dy=1/y;
medgeo=exp(mean(log(dy)));
syx_spurrlog=sumario$sigma; #Erro padrão residual em ln (m³)
IF_spurrlog=1/medgeo*syx_spurrlog; #Indice de Furnival (pode ser comparado com o syx de ajustes lineares em m³)
IF_spurrlog_perc=IF_spurrlog/mean(dados$vcomcc)*100;
fc = exp(0.5*syx_spurrlog^2);

#Modelo Volumétrico de Schumacher e Hall: v=b0*dap^b1*ht^b2
modelo_schu='vcomcc~b0*dap^b1*ht^b2';
ajnlin_schu=nls(modelo_schu,dados,start=list(b0=pi/40000*0.45,b1=2,b2=1));
coef(ajnlin_schu);
sumario=summary(ajnlin_schu);
syx_schu=sumario$sigma; # erro padrão residual (m³)
syx_schu_perc=syx_schu/mean(dados$vcomcc)*100; # erro padrão residual (%)

#Modelo Volumétrico de Schumacher e Hall Logarítmico: ln(v)=b0+b1*ln(dap)+b2*ln(ht)
modelo_schulog='log(vcomcc)~log(dap)+log(ht)';
ajlin_schulog=lm(modelo_schulog,dados);
anova(ajlin_schulog);
sumario=summary(ajlin_schulog);
coef(ajlin_schulog) #Intercept = b0;  log(dap) =  b1;    log(ht) = b2
y=dados$vcomcc;
D(expression(log(y)),'y');
dy=1/y;
medgeo=exp(mean(log(dy)));
syx_schulog=sumario$sigma; #Erro padrão residual em ln (m³)
IF_schulog=1/medgeo*syx_schulog; #Indice de Furnival (pode ser comparado com o syx de ajustes lineares)
IF_schulog_perc=IF_schulog/mean(dados$vcomcc)*100;

#Modelo Volumétrico de Takata: v=dap^2*ht/(b0+b1*dap)
#linearição para obter betas
takata='I((dap^2*ht)/vcomcc)~(dap)';
ajlin_tak=lm(takata,dados);
summary(ajlin_tak);
coef(ajlin_tak);
b0<- coef(ajlin_tak)[1]; b1<- coef(ajlin_tak)[2];

#definição do modelo não linear de takata
modelo_takata='vcomcc~(dap^2*ht)/(b0+b1*dap)';
ajnlin_tak=nls(modelo_takata,dados,start=list(b0=b0,b1=b1));
sumario=summary(ajnlin_tak);
coef(ajnlin_tak)
syx_tak=sumario$sigma; # erro padrão residual (m³)
syx_tak_perc=syx_tak/mean(dados$vcomcc)*100; # erro padrão residual (%)

# Tabulação dos dados, para escolha do melhor modelo (menor erro)
Erro_m3<- c(syx_berk, syx_spurr, IF_spurrlog, syx_schu, IF_schulog, syx_tak);
Erro_perc<- c(syx_berk_perc, syx_spurr_perc, IF_spurrlog_perc, syx_schu_perc, IF_schulog_perc, syx_tak_perc);
Modelos<- c('Berkhout','Spurr','Spurr Logarítmico','Schumacher e Hall','Schumacher e Hall Logarítmico','Takata');
Tabela<- data.frame(Modelos,Erro_m3,Erro_perc); View(Tabela);

# Análise gráfica (plot e qqnorm) dos modelos

x11()
par(mfrow=c(3,2))

plot(predict(ajnlin_berk),residuals(ajnlin_berk)/syx_berk,
     main= 'Berkhout',
     pch='*',col='blue',xlab='Volume estimado (m³)',ylab='Residuos padronizados (m³)',
     ylim=c(-5,5))
abline(h=0);
tsup=qt(0.975,nrow(dados)-1); tinf=qt(0.025,nrow(dados)-1);
abline(h=tsup,lty=2);abline(h=tinf,lty=2);

plot(predict(ajlin_spurr),residuals(ajlin_spurr)/syx_spurr,
     main= 'Spurr',
     pch='*',col='blue',xlab='Volume estimado (m³)',ylab='Residuos padronizados (m³)',
     ylim=c(-5,5));
abline(h=0);
tsup=qt(0.975,nrow(dados)-1); tinf=qt(0.025,nrow(dados)-1);
abline(h=tsup,lty=2);abline(h=tinf,lty=2);

plot(exp(predict(ajlin_spurrlog)), residuals(ajlin_spurrlog)/IF_spurrlog,
     main= 'Spurr (Log)',
     pch='*', col='blue',
     xlab='Volume estimado (m³)', ylab='Resíduos padronizados',
     ylim=c(-4,4))
abline(h=0, col='black')
tsup = 2
tinf = -2
abline(h=tsup, lty=2, col='black')
abline(h=tinf, lty=2, col='black')



plot(predict(ajnlin_schu),residuals(ajnlin_schu)/syx_schu,
     main= 'Schumacher & Hall',
     pch='*',col='blue',xlab='Volume estimado (m³)',ylab='Residuos padronizados (m³)',
     ylim=c(-5,5))
abline(h=0);
tsup=qt(0.975,nrow(dados)-1); tinf=qt(0.025,nrow(dados)-1);
abline(h=tsup,lty=2);abline(h=tinf,lty=2);
#CORRIGIDO
plot(exp(predict(ajlin_schulog)), residuals(ajlin_schulog)/IF_schulog,
     main= 'Schumacher & Hall (Log)',
     pch='*', col='blue',
     xlab='Volume estimado (m³)', ylab='Resíduos padronizados',
     ylim=c(-4,4))
abline(h=0, col='black')
tsup = 2
tinf = -2
abline(h=tsup, lty=2, col='black')
abline(h=tinf, lty=2, col='black')

plot(predict(ajnlin_tak),residuals(ajnlin_tak)/syx_tak,
     main= 'Takata',
     pch='*',col='blue',xlab='Volume estimado (m³)',
     ylab='Residuos padronizados (m³)',
     ylim=c(-5,5))
abline(h=0);
tsup=qt(0.975,nrow(dados)-1); tinf=qt(0.025,nrow(dados)-1);
abline(h=tsup,lty=2);abline(h=tinf,lty=2);

x11()
par(mfrow=c(3,2))

library(fBasics); 
qqnormPlot(as.vector(residuals(ajnlin_berk)),ylim= c(-2,2),title= T,mtext = T)
qqnormPlot(as.vector(residuals(ajlin_spurr)),ylim= c(-2,2),title=F)
qqnormPlot(as.vector(residuals(ajlin_spurrlog)),ylim= c(-2,2),title=F)
qqnormPlot(as.vector(residuals(ajnlin_schu)),ylim= c(-2,2),title=F)
qqnormPlot(as.vector(residuals(ajlin_schulog)),ylim= c(-2,2),title=F)
qqnormPlot(as.vector(residuals(ajnlin_tak)),ylim= c(-2,2),title=F)

## HIPSOMETRIA POR PARCELA
#AMOSTRAGEM CASUAL SIMPLES - HIPSOMETRIA POR PARCELA

library(readxl)
arv<- read_excel("C:/Users/naaht/Desktop/UFSCar/Inventario/trabalho/dados/fustes2.xlsx");
View(arv);
names(arv);

#Hipsometria por parcela

hipso<-data.frame(
  parcela=unique(arv$parcela),
  b0=NA,
  b1=NA
)

for(i in 1:nrow(hipso)){
  selarv<-subset(arv,
                 parcela==hipso$parcela[i] &
                   dap>0 & !is.na(dap) &
                   ht>0 & !is.na(ht)   
  );
  ajhipso<-lm('log(ht)~I(1/dap)',selarv);
  bs<-as.vector(coef(ajhipso));
  hipso$b0[i]<-bs[1];
  hipso$b1[i]<-bs[2];
}
View(hipso)

arv<-merge(arv,hipso,by='parcela');


#Estimando as alturas totais
arv$htest<-with(arv,exp(b0+b1/dap));

x11();

with(arv,plot(ht~dap,xlab='dap(cm)',
              ylab='ht(m)',pch='*',col='green'))
with(arv,points(htest~dap,pch='*',col='red'));

arv$htre<-arv$ht;
ii<-is.na(arv$ht) | arv$ht==0;
arv$htre[ii]<-arv$htest[ii];

arv$vicc<-with(arv,6.436159e-05*dap^1.852143*htre^9.530665e-01);

names(arv);

parc<-aggregate(list(vol=arv$vicc),
                list(fazenda=arv$fazenda,
                     talhao=arv$plantio,
                     areatal=arv$areaplantio,
                     parcela=arv$parcela,
                     areaparc=arv$areaparc
                ),sum);
View(parc);

#Variável de interesse
y<-parc$vol;

#Média
(ymed<-mean(y));

#Variância
(yvar<-var(y));

#Desvio padrão
(ydesv<-sd(y));
#ou
(ydesv<-sqrt(yvar));

#Tamanho da amostra
(n<-length(y));

#Área da parcela
(areaparc<-mean(parc$areaparc));

#Área da fazenda
talhao<-subset(parc,!duplicated(talhao),c('talhao','areatal'));
View(talhao);
(areafaz<-sum(talhao$areatal));

#Intensidade amostral
(ia<-areafaz/n);

#Número de parcelas cabíveis
(N<-areafaz*10000/areaparc);

#Variância da media
(yvarmed<-(yvar/n)*(1-n/N));

#Erro padrão da média
(ydesvmed<-sqrt(yvarmed));

#Erro do inventário
(erroinv<-qt(0.975,n-1)*ydesvmed)

#Erro do inventário em porcentagem

(erroinvperc<-erroinv/ymed *100);

#Média por ha
(yha<-ymed*(10000/areaparc));
#Erro inventário por ha
(erroinvha<-erroinv*(10000/areaparc));

#Total populacional
(ytot<-yha*areafaz);
#Erro inventário para a população
(erroinvtot<-erroinvha*areafaz);

#Intervalo de confiança para a população

cat(paste(
  round(ytot-erroinvtot,0),
  '<= total <=',
  round(ytot+erroinvtot,0),'m³\n'))

#-----------------------------------------------------------------------------------------------------------
# PASSO 4 - Calcular o volume das árvores medidas, a partir do modelo escolhido previamente

#Modelo Escolhido: Schumacher e Hall - v=b0*dap^b1*ht^b2

bs<-as.vector(coef(ajnlin_schu));
paste(bs[1],bs[2],bs[3]);

#Estimando o volume
cubagem$vest=predict(ajnlin_schu); 
View(cubagem);
x11();
par(mfrow=c(1,2))
with(cubagem,plot(vest~dap,xlab='dap(cm)',
                  ylab='vest(m³)',pch='*',col='red'))
with(cubagem,plot(vcomcc~dap,xlab='dap(cm)',
                  ylab='vcomcc(m³)',pch='*',col='green'))

#Juntando o vcomcc na base de dados "arv"   b0*dap^b1*ht^b2
arv<- subset(arv,!is.na(dap) & !is.na(htre) 
             & dap>0 & htre>0)
arv$vcomcc<- with(arv,bs[1]*(dap)^bs[2]*(htre^bs[3])); #SE O MODELO DE VCS FOR DIFERENTE, ALTERAR A FUNÇÃO DO AJUSTE
View(arv);

#AGREGANDO OS DADOS (somando o talhão, area talhão, volume....)
parc=aggregate(list(vcomcc=arv$vcomcc), list(matgen=arv$matgen, krigman=arv$krigman, talhao=arv$plantio, areatal=arv$areaplantio, parcela=arv$parcela, areaparc=arv$areaparc, x=arv$x, y=arv$y),sum,na.rm=TRUE)
View(parc) #consigo visualizar o volume de cada parcela
#write.csv(parc, "fazenda3.csv", row.names = FALSE)
##----------------------------------------------------------------------------------------------------
# TENTANDO COM ACS SIMPLES

dados=parc;
View(dados);
names(dados);

dados$vary=dados$vcomcc #variavel de interesse chamada de vary
#Variável de interesse
y<-dados$vcomcc;

#Média
(ymed<-mean(y));

#Variância
(yvar<-var(y));

#Desvio padrão
(ydesv<-sd(y));
#ou
(ydesv<-sqrt(yvar));

#Tamanho da amostra
(n<-length(y));

#Área da parcela
(areaparc<-mean(dados$areaparc));

#Área da fazenda
talhao<-subset(dados,!duplicated(talhao),c('talhao','areatal'));
View(talhao);
(areafaz<-136.18);

#Intensidade amostral
(ia<-areafaz/n);

#Número de parcelas cabíveis
(N<-areafaz*10000/areaparc);

#Variância da media
(yvarmed<-(yvar/n)*(1-n/N));

#Erro padrão da média
(ydesvmed<-sqrt(yvarmed));

#Erro do inventário
(erroinv<-qt(0.975,n-1)*ydesvmed)

#Erro do inventário em porcentagem

(erroinvperc<-erroinv/ymed *100);

#Média por ha
(yha<-ymed*(10000/areaparc));
#Erro inventário por ha
(erroinvha<-erroinv*(10000/areaparc));

#Total populacional
(ytot<-yha*areafaz);
#Erro inventário para a população
(erroinvtot<-erroinvha*areafaz);

#Intervalo de confiança para a população
cat(paste(round(ytot-erroinvtot,0),'<= total <=', round(ytot+erroinvtot,0),'m³\n'))

####ACE KRIGAGEM----------------------------------------------------------------------
#OBS. ACE pode ser por genetica, espacamento, talhao. etc, eu fiz por estratos de produtividade medidos no
#qgis a partir do mapa de krigagem gerado na etapa seguinte (KRIGAGEM) deste codigo.
#alem disso, algumas alteracoes foram feitas na planilha do excel que usei como base de dados, atente-se as mudanças

# PASSO 5 - Rodar o inventário do povoamento, de forma estratificada
dados<-read.csv("C:/Users/naaht/Desktop/UFSCar/Inventario/trabalho/dados/dadoskrig.csv")
View(dados);
names(dados);

dados$vary=dados$vcomcc; #variavel de interesse chamada de vary

krigman=subset(dados,!duplicated(kricalc),c('kricalc'));
View(krigman)

calc=subset(dados,!duplicated(kricalc),c('areaestr','kricalc'));
View(calc)

krig1=subset(calc,kricalc==1);krig1;
areakrig1=sum(krig1$areaestr);areakrig1;

krig2=subset(calc,kricalc==2);krig2;
areakrig2=sum(krig2$areaestr);areakrig2;

krig3=subset(calc,kricalc==3);krig3;
areakrig3=sum(krig3$areaestr);areakrig3;

areamattot=c(areakrig1,areakrig2,areakrig3); #importante verificar se essa area condiz com a area total da sua base de dados
areatot=sum(areamattot); areatot;

dados$areaest=NA
dados$areaest[dados$kricalc==1]<-areakrig1
dados$areaest[dados$kricalc==2]<-areakrig2
dados$areaest[dados$kricalc==3]<-areakrig3

View(dados)

#escolhendo estratificao

dados$estrato=NULL
dados$estrato=dados$kricalc
sig=0.05 #alfa=5% ALTERAR CASO O NIVEL DE SIGNIFICANCIA FOR DIFERENTE

#VOLUME DE INDIVIDUO SOMADO P/ OBTER VOLUME POR PARCELA
dados2=with(dados,aggregate(list(vary=vary),
                            list(areaest=areaest,areaparc=areaparc,estrato=estrato,parcela=parcela),sum,na.rm=TRUE));
View(dados2);

# PROCESSAMENTO
estrato=with(dados2,aggregate(
  list(areaest=areaest,areaparc=areaparc,ym=vary),
  list(estrato=estrato),
  mean
)); estrato

#Numero de parcelas lancadas por estrato
calc=with(dados2,aggregate(
  list(anj=parcela),
  list(estrato=estrato),
  length
)); calc

estrato=merge(estrato,calc);

#Variancia e desvio padrao por estrato. atentar-se para a substituicao de vetores de mesmo nome
calc=with(dados2,aggregate(
  list(s2y=vary),
  list(estrato=estrato),
  var
)); calc
calc$sy=sqrt(calc$s2y);

estrato=merge(estrato,calc);

estrato$areaparc=estrato$areaparc/10000;
estrato$pnj=estrato$areaest/estrato$areaparc; #n de parcelas por estrato

#Nomeando a populacaoo
estrato$populacao='cota farm by nat'

#area, n. de amostras e n. de amostras cabiveis na populacao
populacao=with(estrato,aggregate(
  list(area=areaest,an=anj,pn=pnj),
  list(populacao=populacao),
  sum
)); populacao

estrato=merge(estrato,populacao);

#Peso de cada estrato na popula??o, ligado ao n?mero de parcelas lan?adas em cada estrato
estrato$pwj=estrato$pnj/estrato$pn;

#C?lculo da m?dia estratificada/ponderada
calc=with(estrato,aggregate(
  list(ymstr=pwj*ym),
  list(populacao=populacao),
  sum
));

populacao=merge(populacao,calc);

#Vari?ncia da m?dia estratificada
calc=with(estrato,aggregate(
  list(calc1=pwj^2*(s2y/anj)),
  list(populacao=populacao),
  sum
));

populacao=merge(populacao,calc);

calc=with(estrato,aggregate(
  list(calc2=(pwj*s2y)/pn),
  list(populacao=populacao),
  sum
));

populacao=merge(populacao,calc);
populacao$s2ystr=with(populacao,calc1-calc2);
populacao$calc1=NULL;
populacao$calc2=NULL;

#Grau de liberdade efetivo
estrato$calcgl=with(estrato,pnj*(pnj-anj)/anj); #Representa??o de um peso

calc=with(estrato,aggregate(
  list(calc1=calcgl*s2y,
       calc2=(calcgl*s2y)^2/(anj-1)),
  list(populacao=populacao),
  sum
));

calc$gle=calc$calc1^2/calc$calc2;
calc$calc1=NULL;
calc$calc2=NULL;

populacao=merge(populacao,calc);populacao;
View(estrato)
#Erro na unidade
populacao$errounid=with(populacao,qt(1-sig/2,gle)*sqrt(s2ystr));

#Erro percentual
populacao$erroperc=with(populacao,errounid/ymstr*100);

#Total e erro populacional 
populacao$ytstr=with(populacao,ymstr*pn); 
populacao$errototal=with(populacao,errounid*pn);
total<-with(populacao,ymstr*pn); #volume total
etotal<-with(populacao,errounid*pn);  #erro total

##Intervalo de confiança da população
litot<-total-etotal;
lstot<-total+etotal;

print(paste('IC:',round(litot,2),'<=T<=',round(lstot,2),'m³',sep=''));

#Total e erro por hectare
populacao$ymha=with(populacao,ytstr/area); #volume total por ha
populacao$erroha=with(populacao,errototal/area); #erro total por ha
View(populacao);

###KRIGAGEM----------------------------------------------------------------------------------------------
#KRIGAGEM ORDINÁRIA -----------------------------------------------------------
setwd('C:/Users/naaht/Desktop/UFSCar/Inventario/trabalho/dados')
library(raster);
library(sf);
library(geoR);

# Ler o arquivo CSV
invparc <- read.csv('fazenda3.csv', stringsAsFactors = FALSE)
shpplt <- as_Spatial(st_read('fazenda.shp'))
# Visualizar os dados
View(invparc)
head(invparc)

# Selecionar colunas relevantes
invparc <- invparc[, c('x', 'y', 'vcomcc')]

coordinates(invparc)<-~x+y;
crs(invparc)<-st_crs(4326)$wkt
invparc<-sp::spTransform(invparc, st_crs(31982)$wkt);
points(invparc, col='red', pch=16);
##Informações gerais
resolucao<-10;
#Conversão dos dados para o formato geodata
vgeo<-as.geodata(invparc);
#Criação do raster da área de interesse
rasterplt<-raster(shpplt,resolution=resolucao);
rasterplt<-rasterize(shpplt,rasterplt);
reskrige<-as.data.frame(rasterToPoints(rasterplt)); #depois de converter em pontos converte em data frame
nrow(reskrige)
head(reskrige)
#Análise variográfica - calcula a variância entre os pontos por elipse de distância
svar<-variog(vgeo); #faz o grafico sem uma distância máxima - análise do comportamento da semivariância
svar<-variog(vgeo, max.dist = 2000); #estabelece uma distância máxima (evita dependência entre áreas não correlatas)
graphics.off(); x11();
plot(
  svar,
  xlab='distância (m)',
  ylab='Semivariância(m^6)'
);

##Ajuste utilizando mínimos quadrados
tau<-0; #Efeito pepita - neste caso um bom parâmetro inicial é zero (quanto maior o efeito, pior é o modelo)
sigma<-3-tau; #Contribuição - quanto maior a contribuição, melhor o efeito da geoestatística
phi<-1200; #Alcance - distância em que uma parcela ainda apresenta efeito espacial (dependência)
ajuste<-variofit( ##variofit é a função do geoR que ajusta o modelo
  svar,ini=c(sigma,phi),
  nugget=tau,
  cov.model = 'exp'##ta usando o modelo exp
);
lines(ajuste);
kvgeo<-krige.conv( ##krig.conv = krigagem convencional
  vgeo,
  loc=as.matrix(reskrige[,1:2]),
  krige=krige.control(obj=ajuste)
);
reskrige$vcom<-kvgeo$predict;
head(reskrige)
###Exportando dos resultados da krigagem
coordinates(reskrige)=~x+y;
gridded(reskrige)<-T;
reskrige$layer<-NULL;
crs(reskrige) <- crs(shpplt); #Adicionando Projeção
class(reskrige)
reskrige <- raster(reskrige);
class(reskrige)
writeRaster(reskrige,'reskrige.tif',"GTiff", overwrite=TRUE);
###Exibição do mapa
reskrige <- mask(reskrige, shpplt, inverse=FALSE) #recortando o raster com o plantio
graphics.off();x11();
plot(reskrige, legend=TRUE, asp=1, main="KRIGAGEM: VCOM(m³/400m2)");
plot(shpplt,lwd=2,border='darkblue',col='transparent',add=TRUE,asp=1);
points(invparc, pch = '+', cex=0.6)
#sp::compassRose(identify(reskrige,plot=F));
sp::compassRose(x=405500, y=7724000, cex=0.5) ##rosa dos ventos

