# SEA - Estratificação e Desigualdade
# Final Work
# Neylson Crepalde
# REDES BIPARTIDAS DISCIPLINAS - rodar apos sea_clean_data.R

rede.di <- dados[,c(3,13)]
names(rede.di) <- c("nome","afiliacao")
#View(rede.di)

# Limpando os dados
rede.di$afiliacao[rede.di$afiliacao=="Conclui o mestrado"] = "none"
rede.di$afiliacao[rede.di$afiliacao=="Nenhuma"] = "none"
rede.di$afiliacao[rede.di$afiliacao=="Já terminou os créditos"] = "none"
rede.di$afiliacao[rede.di$afiliacao=="Elaboração de Trabalho Final"] = "none"
rede.di$afiliacao[rede.di$afiliacao=="elaboração de dissertação"] = "none"
rede.di$afiliacao[rede.di$afiliacao=="Elaboração de tese"] = "none"
rede.di$afiliacao[rede.di$afiliacao=="trabalho final"] = "none"
rede.di$afiliacao[rede.di$afiliacao=="Já finalizei crėditos"] = "none"
rede.di$afiliacao[37] = "none"
rede.di$afiliacao[rede.di$afiliacao=="Conclui o curso"] = "none"
rede.di$afiliacao[rede.di$afiliacao=="Nenhuma, já cumpri meus créditos"] = "none"
rede.di$afiliacao[rede.di$afiliacao=="Já defendi atese, mas ainda não entreguei versão final, então ainda estou ligada ao programa."] = "none"
rede.di$afiliacao[rede.di$afiliacao=="Elaboração de Trabalho Final da Tese"] = "none"
rede.di$afiliacao

#Limpando os nomes
# Limpando acentos e colocando letras minúsculas
rede.di$nome <- sapply(rede.di$nome, tolower)
rede.di$afiliacao <- gsub("Polícia, ","Polícia ", rede.di$afiliacao)
rede.di$nome <- gsub("ã","a", rede.di$nome)
rede.di$nome <- gsub("á","a", rede.di$nome)
rede.di$nome <- gsub("é","e", rede.di$nome)
rede.di$nome <- gsub("í","i", rede.di$nome)
rede.di$nome <- gsub("ó","o", rede.di$nome)
rede.di$nome <- gsub("ú","u", rede.di$nome)

library(tidyr)
rede.di <- as.data.frame(rede.di, stringsAsFactors = F)

DI <- rede.di %>% unnest(afiliacao=strsplit(afiliacao,", "))
#View(DI)
####################################################################

library(igraph)
DI <- as.matrix(DI)
DI <- graph_from_edgelist(DI, directed = F)
type <- bipartite_mapping(DI)$type
V(DI)$type = type
DI <- delete_vertices(DI, "none")
DI <- simplify(DI)

#plot(DI, vertex.label.cex=0.7, vertex.size=4, edge.arrow.size=0.3, vertex.color=as.numeric(V(DI)$type)+1,
#     main="Participação em Disciplinas 2016/1", layout=layout_with_fr)
#title(xlab="Rede 2-mode")
