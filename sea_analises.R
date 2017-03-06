# SEA - Estratificação e Desigualdade
# Final Work
# Neylson Crepalde
# ANALISES

#Puxando as alteracoes e limpezas realizadas antes
source("/home/neylson/Documentos/Neylson Crepalde/Doutorado/SEA Desigualdade/Trabalho Final/sea_clean_data.R")
source("/home/neylson/Documentos/Neylson Crepalde/Doutorado/SEA Desigualdade/Trabalho Final/sea_clean_data2.R")
source("/home/neylson/Documentos/Neylson Crepalde/Doutorado/SEA Desigualdade/Trabalho Final/sea_clean_data3.R")
source("/home/neylson/Documentos/Neylson Crepalde/Doutorado/SEA Desigualdade/Trabalho Final/sea_clean_data4.R")
source("/home/neylson/Documentos/Neylson Crepalde/Doutorado/SEA Desigualdade/Trabalho Final/sea_clean_data5.R")
source("/home/neylson/Documentos/Neylson Crepalde/Doutorado/SEA Desigualdade/Trabalho Final/sea_clean_data6.R")
source("/home/neylson/Documentos/Neylson Crepalde/Doutorado/SEA Desigualdade/Trabalho Final/sea_clean_data7_atributos.R")
source("/home/neylson/Documentos/Neylson Crepalde/Doutorado/SEA Desigualdade/Trabalho Final/sea_clean_data8_gp.R")
source("/home/neylson/Documentos/Neylson Crepalde/Doutorado/SEA Desigualdade/Trabalho Final/sea_clean_data9_disciplinas.R")

# Pacotes
library(magrittr)
library(descr)
library(xtable)
library(ggplot2)
library(ggthemes)
#library(grid)
#library(gridExtra)

##################
# Função MULTIPLOT
# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
########################################################


# Estatísticas Descritivas
tabela.quali <- summary(dados[,c(4,6,7,10,12)])
tabela.quanti <- summary(dados[,c(5,9)])
xtable(tabela.quanti)
xtable(tabela.quali)

# Investigando em que ponto do curso estão os doutorandos e mestrandos
xtable(freq(dados$`Qual é a sua ocupação principal?`))
freq(dados$`Em que ano do curso você se encontra?`[dados$`Qual é a sua situação acadêmica neste momento?`=="Doutorando"])
freq(dados$`Em que ano do curso você se encontra?`[dados$`Qual é a sua situação acadêmica neste momento?`=="Mestrando"])

# Como auto-avaliação
freq(dados$`Como você avalia o seu desempenho com relação às suas notas?`)
freq(dados$`Como você avalia seu desempenho quanto à produção e publicação de artigos científicos?`)

gg4 <- ggplot(dados, aes(factor(dados$`Como você avalia o seu desempenho com relação às suas notas?`,
                         levels = c(5,4,3))))+geom_bar()+coord_flip()+theme_bw(base_size = 14)+
  labs(x="Self evaluation - GRADES",y="")
gg5 <- ggplot(dados, aes(factor(dados$`Como você avalia seu desempenho quanto à produção e publicação de artigos científicos?`,
                         levels = c(5,4,3,2,1))))+geom_bar()+coord_flip()+theme_bw(base_size = 14)+
  labs(x="Self evaluation - Paper PUBLISHING",y="")
multiplot(gg4, gg5, cols = 2)


# Produtividade
freq(dados$`Quantos artigos você publicou desde 2014?`)
freq(dados$`Quantos trabalhos você apresentou em congresso desde 2014?`)
freq(dados$`Quantos cursos ou disciplinas você ministrou desde 2014?`)

gg1 <- ggplot(dados, aes(factor(dados$`Quantos artigos você publicou desde 2014?`,
                         levels = c(0,1,2,3,5))))+geom_bar()+theme_bw(base_size = 14)+
  labs(x="Papers Published",y="")

gg2 <- ggplot(dados, aes(factor(dados$`Quantos trabalhos você apresentou em congresso desde 2014?`,
                         levels = c(0,1,2,3,4,5))))+geom_bar()+theme_bw(base_size = 14)+
  labs(x="Papers Presented",y="")

gg3 <- ggplot(dados, aes(factor(dados$`Quantos cursos ou disciplinas você ministrou desde 2014?`,
                         levels = c(0,1,2,3,4,5,6,8))))+geom_bar()+theme_bw(base_size = 14)+
  labs(x="Classes Taught",y="")
multiplot(gg1,gg2,gg3,cols = 3)
grid.arrange(gg1,gg2,gg3, ncol=3)

#Criando um escore de produtividade com peso 2 para artigos publicados
dados$produtividade <- (dados[[19]]*2)+dados[[20]]+dados[[21]]
ggplot(dados, aes(produtividade))+geom_histogram(bins = 12)

#Criando uma variável binária BRANCO/NÃO BRANCO
names(dados)[6] <- "raca"
dados$branco <- ifelse(dados$raca=="Branco", 1,0)


# Fiz vários e vários testes de média. Nenhum deu significativo.
#t.test(dados$`De quanto foi a sua renda mensal nos últimos dois meses?`~dados$Sexo)

#Correlações
correlacoes <- cor(cbind(dados$`Qual é a sua idade em 01 de maio de 2016?`,dados$`De quanto foi a sua renda mensal nos últimos dois meses?`,
    dados$`Como você avalia o seu desempenho com relação às suas notas?`,
    dados$`Como você avalia seu desempenho quanto à produção e publicação de artigos científicos?`,
    dados$`Quantos artigos você publicou desde 2014?`,dados$`Quantos trabalhos você apresentou em congresso desde 2014?`,
    dados$`Quantos cursos ou disciplinas você ministrou desde 2014?`,dados$produtividade))
row.names(correlacoes) = c("Age","Income","Grades (eval)","Pubs (eval)","Pubs","Congress","Classes","Productivity")
colnames(correlacoes) = c("Age","Income","Grades (eval)","Pubs (eval)","Pubs","Congress","Classes","Productivity")
xtable(correlacoes)

library(stargazer)
stargazer(correlacoes, type="text")

############################################################
# Modelo linear

dados$produtiv1 <- dados$produtividade+1
#dados$produtiv2 <- dados$produtiv1
#dados$produtiv2[5] <- NA
#t.test(dados$produtiv1~dados$Sexo)
dados$participagp <- c(0,1,0,1,1,1,1,0,0,0,1,0,1,0,0,0,0,0,1,0,1,1,0,0,1,0,1,1,0,0,1,1,0,0,0,1,1,1,0,1,0,1,0,0,1,0,1)

dados$age.center <- dados[[5]]-mean(dados[[5]])

linear1 <- lm(produtiv1~participagp+Sexo+branco+dados[[18]]+age.center+dados[[7]]+dados[[9]]+dados[[12]], data = dados)
summary(linear1)
par(mfrow=c(2,2))
plot(linear1)


gamma1 <- glm(produtiv1~participagp+Sexo+branco+dados[[18]]+age.center+dados[[7]]+dados[[9]]+dados[[12]],
              data = dados, family = Gamma(link = "identity"))
summary(gamma1)
plot(gamma1)
par(mfrow=c(1,1))

texreg::texreg(list(linear1,gamma1), single.row = T, caption = "Regression Models", caption.above = T)

deviance(linear1)-deviance(gamma1)
xtable::xtable(lmtest::lrtest(linear1,gamma1))


dados$grau.metod <- c(0,5,1,1,11,12,0,1,1,1,1,2,1,1,2,2,1,1,3,5,1,0,1,1,8,2,2,8,1,1,0,11,1,2,0,1,4,2,1,3,0,0,
                      1,2,1,1,2)
length(dados$grau.metod)
dados$grau.rev <- c(0,1,1,0,2,4,1,2,0,0,0,0,1,0,1,1,0,3,4,0,0,0,1,0,3,1,1,3,1,1,0,2,1,0,0,0,1,1,0,0,0,1,1,1,1,
                    0,1)
length(dados$grau.rev)

dados$grau.colab <- c(2,0,0,0,1,1,0,1,0,1,4,1,1,0,0,1,0,2,2,0,1,0,2,0,0,0,0,1,3,0,0,3,0,0,0,3,0,0,1,1,0,0,0,0,0,0,0)
length(dados$grau.colab)


linear2 <- lm(produtiv1~grau.colab+participagp+Sexo+branco+dados[[18]]+age.center+dados[[7]]+dados[[9]]+dados[[12]], data = dados)
summary(linear2)

gamma2 <- glm(produtiv1~grau.colab+participagp+Sexo+branco+dados[[18]]+age.center+dados[[7]]+dados[[9]]+dados[[12]],
              data = dados, family = Gamma(link = "log"))
summary(gamma2)
deviance(linear2)-deviance(gamma2)


#######################################################
# Elaborando medidas de rede

#GRAU
grau.colab <- degree(colaboracao)
sort(grau.colab)

grau.rev.in <- degree(revisao, mode="in")
grau.rev.out <- degree(revisao, mode="out")
sort(grau.rev.out)

grau.metod.in <- degree(metodologica, mode = "in")
sort(grau.metod.in)

grau.teor.in <- degree(teorica, mode="in")
sort(grau.teor.in)

grau.trab.in <- degree(trabalho, mode="in")
sort(grau.trab.in)

grau.trab.out <- degree(trabalho, mode="out")
sort(grau.trab.out)

grau.amizade <- degree(amizade)
sort(grau.amizade)

#par(mfrow=c(2,3))
#hist(grau.colab, col = "red", main="Degree Distribution\nCollaboration Network", xlab="Degre")
h1 <- ggplot(data=NULL, aes(x=grau.colab))+geom_histogram(binwidth=1, color="black",fill="red")+
  labs(title="Collaboration\nNetwork", x="Degree", y="")+theme_bw(base_size = 12)
#hist(grau.rev.in, col = "green", main="InDegree Distribution\nReview Network", xlab="InDegre")
h2 <- ggplot(data=NULL, aes(x=grau.rev.in))+geom_histogram(binwidth=1, color="black",fill="green")+
  labs(title="Review\nNetwork", x="InDegree", y="")+theme_bw(base_size = 12)
#hist(grau.metod.in, col = "yellow", main="InDegree Distribution\nMethodological Network", xlab="InDegre")
h3 <- ggplot(data=NULL, aes(x=grau.metod.in))+geom_histogram(binwidth=1, color="black",fill="yellow")+
  labs(title="Methodological\nNetwork", x="InDegree", y="")+theme_bw(base_size = 12)
#hist(grau.teor.in, col = "lightblue", main="InDegree Distribution\nTheoretical Network", xlab="InDegre")
h4 <- ggplot(data=NULL, aes(x=grau.teor.in))+geom_histogram(binwidth=1, color="black",fill="lightblue")+
  labs(title="Theoretical\nNetwork", x="InDegree", y="")+theme_bw(base_size = 12)

#hist(grau.trab.in, col="pink", main="InDegree Distribution\nProfessional Indic. Network", xlab="InDegree")
h5 <- ggplot(data=NULL, aes(x=grau.trab.in))+geom_histogram(binwidth=1, color="black",fill="pink")+
  labs(title="Professional\nNetwork", x="InDegree", y="")+theme_bw(base_size = 12)

#hist(grau.amizade, col="grey", main="Degree Distribution\nFriendship Network", xlab="Degree")
h6 <- ggplot(data=NULL, aes(x=grau.amizade))+geom_histogram(binwidth=2, color="black",fill="orange")+
  labs(title="Friendship\nNetwork", x="Degree", y="")+theme_bw(base_size = 12)

#hist(grau.rev.out, col = "orange", main="OutDegree Distribution\nReview Network", xlab="OutDegre")
h7 <- ggplot(data=NULL, aes(x=grau.rev.out))+geom_histogram(binwidth=1, color="black",fill="darkgreen")+
  labs(title="Review\nNetwork", x="OutDegree", y="")+theme_bw(base_size = 12)

h8 <- ggplot(data=NULL, aes(x=grau.trab.out))+geom_histogram(binwidth=5, color="black",fill="darkred")+
  labs(title="Professional\nNetwork", x="OutDegree", y="")+theme_bw(base_size = 12)


multiplot(h2,h1,h7,h6,h5,h3,h8,h4,cols = 4)
#par(mfrow=c(1,1))


#DENSIDADE
dens.colab    <- edge_density(colaboracao)
dens.rev      <- edge_density(revisao)
dens.trab     <- edge_density(trabalho)
dens.amizade  <- edge_density(amizade)
dens.metod    <- edge_density(metodologica)
dens.teor     <- edge_density(teorica)

#############################################################
#ADICIONANDO ATRIBUTOS

nomes <- V(colaboracao)$name
renda <- dados[[9]]
renda[48:55] = NA
nomes.db <- dados[[3]]
nomes.db[48:55] = NA
att <- cbind(nomes,nomes.db,renda)
View(att)
View(cbind(dados$`Nome completo`,dados$`De quanto foi a sua renda mensal nos últimos dois meses?`))
View(dados[,c(3,28,30)])
dados$id = 1:47

#################################################
# ATRIBUTOS REDE COLABORACAO
renda = c(2200,3500,1500,1500,2200,7094.05,2200,2200,1500,1500,3000,3500,NA,
          3000,NA,NA,6200,NA,2200,3082.15,2460,2850,3000,NA,8200,2200,1500,1500,
          2200,1500,1500,1850,4000,2200,2000,NA,1500,NA,2230,1500,2200,1560,750,
          2200,7500,1500,5030,NA,1500,2000,1500,2200,5200,0,2200)
renda.nona=renda
for (i in 1:length(renda)){ 
  if (is.na(renda[i])==T){
    renda.nona[i]=0
  }
}
V(colaboracao)$renda = renda.nona

sexo <- c(1,1,0,0,0,1,1,0,0,0,0,0,0,0,1,0,1,1,0,1,0,0,0,1,0,1,0,0,1,1,1,1,0,1,
          0,0,0,0,0,0,0,0,0,1,1,1,0,1,0,0,0,1,0,0,1)
V(colaboracao)$sexo = sexo

grupos.pesquisa = c(NA,NA,"GE Metodologias Qualitativas",NA,"GE Metodologias Qualitativas",
                      "GIARS","CPEQS","INCITE",NA,NA,NA,NA,NA,"CRISP",NA,NA,"CRISP",NA,"CRISP",NA,
                      NA,NA,NA,NA,NA,"CRISP","CRISP",NA,"CRISP","GE Metodologias Qualitativas",
                      NA,"GIARS",NA,"INCITE","CPEQS",NA,NA,NA,NA,"CONEX",NA,NA,NA,
                      "GIARS","GIARS",NA,"LAPEST",NA,NA,"GE Metodologias Qualitativas",NA,
                      NA,"GE Metodologias Qualitativas",NA,"CRISP")
grupos.pesquisa <- as.factor(grupos.pesquisa)
grupos.pes.num = as.numeric(grupos.pesquisa)
V(colaboracao)$GP = grupos.pesquisa

dados$produtividade
#View(cbind(dados$`Nome completo`,dados$produtividade))
produt <- c(7,2,2,2,0,19,6,6,1,8,2,1,NA,0,NA,NA,12,0,10,0,1,0,5,NA,9,7,7,0,5,1,0,1,3,
            6,5,NA,3,NA,0,7,6,0,3,3,4,10,0,NA,3,5,1,1,8,8,5)
produtiv <- produt+2
for (i in 1:length(produtiv)){ 
  if (is.na(produtiv[i])==T){
    produtiv[i]=1
  }
}

V(colaboracao)$produtividade <- produtiv

branco.colab <- c(1,0,1,1,1,0,1,1,1,1,0,0,NA,1,NA,NA,1,NA,1,1,1,0,1,NA,0,0,0,0,1,1,0,0,0,NA,1,NA,1,NA,1,0,1,1,
                  1,0,1,0,1,NA,0,0,0,0,1,1,0)
V(colaboracao)$branco <- branco.colab

####################################################
# ATRIBUTOS REDE REVISAO
#incluido <- V(revisao)$name %in% V(colaboracao)$name
#which(incluido)
#V(revisao)$name[]

#nomes.revisao <- V(revisao)$name
#nomes[56:58] <- NA
#nomes.df <- data.frame(nomes, 1:length(nomes))
#nomes.rev.df <- data.frame(nomes.revisao)



###########################################################################################
# MODELANDO

library(intergraph)
library(statnet)
colab   <- asNetwork(colaboracao)
rev     <- asNetwork(revisao)
teor    <- asNetwork(teorica)
metod   <- asNetwork(metodologica)
friend  <- asNetwork(amizade)
trab    <- asNetwork(trabalho)
gp      <- asNetwork(GP)
di      <- asNetwork(DI)


############################################################################
# BLOCKMODEL da rede REVISAO

revisao
library(mixer)
mix <- mixer(as.matrix(get.adjacency(revisao)), qmin = 4, qmax = 8, directed = T)
bm.output <- getModel(mix)
bm.output$Pis # Class connectivity matrix
plot(mix)

grupos <-c()

for (i in 1:ncol(bm.output$Taus)){
  grupos[i] <- which.max(bm.output$Taus[,i])
}

plot(revisao, vertex.label=NA, vertex.color=grupos+2, edge.arrow.size=.3, vertex.size=8, edge.color="black",
     layout=layout_with_fr)
title(main="Review Network\nBlockmodelling")
legend("bottomleft", legend = c("Blue = Block 1   Yellow = Block 2\nRed = Block 3    Green = Block 4"), box.col = "white")

#######################################
# CORRELACOES
gcor(colab,rev)
gcor(colab,metod)
gcor(colab,teor)

#gcor.array[1,,] <- colab
#gcor.array[2,,] <- rev
#qt1 <- qaptest(list(colab,rev),gcor, g1=1, g2=2)  #Consertar esse código


# ERGM
# COLAB
fit1.colab <- ergm(colab~edges+edgecov(rev)+edgecov(teor)+edgecov(metod)+edgecov(trab)+edgecov(friend))
summary(fit1.colab)

model2 <- formula(colab~edges+isolates+gwesp(1,fixed=T)+twopath+
                    edgecov(rev)+edgecov(teor)+edgecov(metod)+edgecov(trab)+edgecov(friend))
summary.statistics(model2)
fit2 <- ergm(model2)
summary(fit2)
gof2.colab <- gof(model2, coef = coef(fit2))
par(mfrow=c(1,3))
plot(gof2.colab)

model3 <- formula(colab~edges+isolates+gwesp(1,fixed=T)+twopath+
                    edgecov(rev)+edgecov(teor)+edgecov(metod)+edgecov(trab)+edgecov(friend)+
                    nodematch("GP")+nodematch("sexo")+nodematch("branco")+nodecov("produtividade")+
                    nodecov("renda"))
summary.statistics(model3)
fit3 <- ergm(model3)
summary(fit3)

gof3 <- gof(model3, coef = coef(fit3))
par(mfrow=c(1,3))
plot(gof3)

model4 <- formula(colab~edges+isolates+gwesp(1,fixed=T)+twopath+
                    edgecov(rev)+edgecov(teor)+edgecov(metod)+edgecov(trab)+edgecov(friend)+
                    nodematch("GP")+nodematch("sexo")+nodematch("branco")+absdiff("produtividade")+absdiff("renda"))
summary.statistics(model4)
fit4 <- ergm(model4)
summary(fit4)
gof4 <- gof(model4, coef = coef(fit4))
par(mfrow=c(1,3))
plot(gof4)

#library(stargazer)
#stargazer(fit1.colab,fit2,fit3,no.space = T,type="latex", 
#          column.labels=c("Model 1","Model 2","Model 3"),
#          dep.var.labels = "Collaboration Network")
library(texreg)
texreg(list(fit1.colab,fit2,fit4), single.row = T,
       custom.model.names = c("Model 1","Model 2","Model 3"),
       caption = "Collaboration Network -- ERGM's",
       caption.above = T,center = F)

############################################################
#REV
#fit1.rev <- ergm(rev~edges+mutual+edgecov(teor)+edgecov(metod)+edgecov(trab)+edgecov(friend))
#summary(fit1.rev)

#model2.rev <- formula(rev~edges+mutual+isolates+gwesp(1,fixed=F,cutoff=2)+twopath+
#                   edgecov(teor)+edgecov(metod)+edgecov(trab)+edgecov(friend))
#summary.statistics(model2.rev)
#fit2.rev <- ergm(model2.rev)
#summary(fit2.rev)
#gof2.rev <- gof(fit2.rev)
#par(mfrow=c(1,4))
#plot(gof2.rev)
