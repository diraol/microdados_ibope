library(plyr)
library(graphics)
library(memisc)
setwd("~/devel/microdados_ibope/arquivos_originais")
data = as.data.frame(as.data.set(spss.system.file('ago2014_2.sav')))
#Removendo variáveis que não serão utilizadas
selecionados = select(data, -(quest))
selecionados = select(selecionados, -(idade))
selecionados = select(selecionados, -(alfab))
selecionados = select(selecionados, -(p1:p3))
selecionados = select(selecionados, -(p5:p9))
selecionados = select(selecionados, -(p12:p18))
selecionados = select(selecionados, -(p20:p21))
#Adiciona uma coluna "com_bolsa" baseada nas respostas da p22
selecionados = mutate(selecionados,
    sem_bolsa = factor(
      (
        (p22a01=="Não participa de nenhum" | p22a01=="Não sabe/ Não respondeu" | is.na(p22a01)) & 
        (p22a02=="Não participa de nenhum" | p22a02=="Não sabe/ Não respondeu" | is.na(p22a02)) & 
        (p22a03=="Não participa de nenhum" | p22a03=="Não sabe/ Não respondeu" | is.na(p22a03)) & 
        (p22a04=="Não participa de nenhum" | p22a04=="Não sabe/ Não respondeu" | is.na(p22a04)) & 
        (p22a05=="Não participa de nenhum" | p22a05=="Não sabe/ Não respondeu" | is.na(p22a05)) & 
        (p22a06=="Não participa de nenhum" | p22a06=="Não sabe/ Não respondeu" | is.na(p22a06)) & 
        (p22a07=="Não participa de nenhum" | p22a07=="Não sabe/ Não respondeu" | is.na(p22a07)) & 
        (p22a08=="Não participa de nenhum" | p22a08=="Não sabe/ Não respondeu" | is.na(p22a08)) & 
        (p22a09=="Não participa de nenhum" | p22a09=="Não sabe/ Não respondeu" | is.na(p22a09))
      ) & 
      (
        (p22b01=="Ninguém da sua família" | p22b01=="Não sabe/ Não respondeu" | is.na(p22b01)) & 
        (p22b02=="Ninguém da sua família" | p22b02=="Não sabe/ Não respondeu" | is.na(p22b02)) & 
        (p22b03=="Ninguém da sua família" | p22b03=="Não sabe/ Não respondeu" | is.na(p22b03)) & 
        (p22b04=="Ninguém da sua família" | p22b04=="Não sabe/ Não respondeu" | is.na(p22b04)) & 
        (p22b05=="Ninguém da sua família" | p22b05=="Não sabe/ Não respondeu" | is.na(p22b05)) & 
        (p22b06=="Ninguém da sua família" | p22b06=="Não sabe/ Não respondeu" | is.na(p22b06)) & 
        (p22b07=="Ninguém da sua família" | p22b07=="Não sabe/ Não respondeu" | is.na(p22b07)) & 
        (p22b08=="Ninguém da sua família" | p22b08=="Não sabe/ Não respondeu" | is.na(p22b08)) & 
        (p22b09=="Ninguém da sua família" | p22b09=="Não sabe/ Não respondeu" | is.na(p22b09)) & 
        (p22b10=="Ninguém da sua família" | p22b10=="Não sabe/ Não respondeu" | is.na(p22b10)) &
        (p22b11=="Ninguém da sua família" | p22b11=="Não sabe/ Não respondeu" | is.na(p22b11)) &
        (p22b12=="Ninguém da sua família" | p22b12=="Não sabe/ Não respondeu" | is.na(p22b12))
      ))
    )
#Removendo variáveis de bolsa não mais necessárias
selecionados = select(selecionados,-(p22a01:p22b12))
#Filtrando a amostra de cada candidato a ser analisado
dilma = filter(selecionados, p4 == 'Dilma')
dilma = select(dilma, -(p4))
aecio = filter(selecionados, p4 == 'Aécio Neves')
aecio = select(aecio, -(p4))
marina = filter(selecionados, p4 == 'Marina Silva')
marina = select(marina, -(p4))

#Iniciando a Análise de Correspondência Múltipla ('HOMALS')
#Vamos utilizar o pacote homals. Para instalá-lo, é preciso instalar
# o pacote libglu1-mesa-dev no sistema operacional (no caso do Linux deb)
library(homals)
res_marina = homals(marina, active = TRUE, ndim = 3)
?plot.homals
plot3d(res_marina, plot.type="starplot")







hc_dilma = hclust(dist(dilma),"ward.D2")
hc_aecio = hclust(dist(aecio),"ward.D2")
hc_marina = hclust(dist(marina),"ward.D2")
plot(as.dendrogram(hc_dilma),horiz=T)
plot(as.dendrogram(hc_aecio),horiz=T)
plot(as.dendrogram(hc_marina),horiz=T)
marina_clusters = kmeans(marina,2)
