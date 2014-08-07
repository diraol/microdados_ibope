final = ibope

for (i in unique(ibope$data)) {
  teste = ibope[ibope$data==i,]
  for (j in unique(teste$variavel)) {
    outros = 0
    teste2 = teste[teste$variavel==j,]
    recorte = unique(teste2$recorte)
    for (k in unique(teste2$candidato)) {
      if ((k != "Aécio Neves") & (k != "Dilma Rousseff") & (k != "Eduardo Campos") & (k != "Nulo") & (k != "Não Sabe") & (k != "Pastor Everaldo") & (k != "Outros")) {
        teste3 = teste2[teste2$candidato == k,]
        outros = outros + teste3$intencao
      }
    }
    nova_linha = data.frame(data=i,instituto="Ibope",recorte=as.vector(recorte)[1],variavel=j,candidato="Outros",partido="Outros",intencao=outros,turno=1)
    names(nova_linha) = names(final)
    final = rbind(final,nova_linha)
  }  
}

final = final[!(final$candidato != "Aécio Neves" & final$candidato != "Dilma Rousseff" & final$candidato != "Eduardo Campos" & final$candidato != "Nulo" & final$candidato != "Não Sabe" & final$candidato != "Pastor Everaldo" & final$candidato != "Outros"),]
