# SEA - Estratificação e Desigualdade
# Final Work
# Neylson Crepalde

#No windows
library(readr)
dados <- read_csv("C:/Users/neyls/Documents/Neylson Crepalde/Doutorado/sea_desigualdade/Trabalho Final/dados_rede_teste.csv")


#library(readr)
#dados <- read_csv("/home/neylson/Documentos/Neylson Crepalde/Doutorado/SEA Desigualdade/Trabalho Final/dados_rede_teste.csv")

rede1 <- dados[,c(3,22)]
names(rede1) <- c("nome","indicacao")

####################################################################
# Limpando os dados
rede1$indicacao[rede1$indicacao=="nenhum"] = "none"
rede1$indicacao[rede1$indicacao=="Não se aplica"] = "none"
rede1$indicacao[rede1$indicacao=="Não houve"] = "none"
rede1$indicacao[rede1$indicacao=="Ninguém"] = "none"
rede1$indicacao[rede1$indicacao=="nenhum destes"] = "none"
rede1$indicacao[rede1$indicacao=="Nenhum"] = "none"
rede1$indicacao[rede1$indicacao=="Nennhum"] = "none"
rede1$indicacao[rede1$indicacao=="nunca escrevi ou publiquei artigo com nenhum dos colegas acima"] = "none"
rede1$indicacao[rede1$indicacao=="Nenhum desses"] = "none"
rede1$indicacao[rede1$indicacao=="Nenhum destes"] = "none"
rede1$indicacao[rede1$indicacao=="NENHUM"] = "none"
rede1$indicacao[rede1$indicacao=="sozinha"] = "none"
rede1$indicacao[rede1$indicacao=="x"] = "none"
rede1$indicacao[rede1$indicacao=="Nao publiquei com os colegas"] = "none"
rede1$indicacao[rede1$indicacao=="."] = "none"
rede1$indicacao[rede1$indicacao=="0"] = "none"
rede1$indicacao[rede1$indicacao=="Nunca pedi revisão"] = "none"
############################################################################

#####################################################################
# Limpando acentos e colocando letras minúsculas
rede1 <- sapply(rede1[,c(1,2)], tolower)
rede1 <- gsub("ã","a", rede1[,c(1,2)])
rede1 <- gsub("á","a", rede1[,c(1,2)])
rede1 <- gsub("é","e", rede1[,c(1,2)])
rede1 <- gsub("í","i", rede1[,c(1,2)])
rede1 <- gsub("ó","o", rede1[,c(1,2)])
rede1 <- gsub("ú","u", rede1[,c(1,2)])
###################################################################

############################################################################
# Separando os nomes e montando edgelist com o comando unnest()
library(tidyr)
rede1 <- as.data.frame(rede1, stringsAsFactors = F)

colaboracao <- rede1 %>% unnest(indicacao=strsplit(indicacao,", "))
#View(colaboracao)
############################################################################

###########################################################################
# Formatando o edgelist usando o pacote reshape2
#library(reshape2)
#indicacao <- setNames(strsplit(df$indicacao, split = ", "), df$nome)
#long <- melt(indicacao)
#names(long) <- c("indicacao", "nome")
#long
##########################################################################



###################################################################
# Preparando a rede
library(igraph)
colaboracao <- as.matrix(colaboracao)
colaboracao <- graph_from_edgelist(colaboracao, directed = F)
colaboracao <- delete_vertices(colaboracao, "none")
colaboracao <- simplify(colaboracao)

#plot(colaboracao, vertex.label.cex=0.7, vertex.size=4, edge.arrow.size=0.3)
#plot(colaboracao, vertex.label=NA, vertex.size=8, edge.arrow.size=0.3, 
#     edge.color="black", main="Collaboration", xlab="Density = 0.014")
