source('~/github/microdados_ibope/analisa_microdados.R')
options(stringsAsFactors = FALSE) 

#DF DE MARCO
perg_marco = c("p1","p2","p3","p4","p8a","p8c","p10")
trad_marco = c("vida_hoje","interesse","intencao_espontanea","intencao_estimulada","2turno_aecio","2turno_campos","desejo_mudanca")
marco = cria_arquivo("ibopemar2014.sav",perg_marco,trad_marco)
resultado_marco = calcula_tudo(marco,"2014-03-17")

#DF DE ABRIL
perg_abril = c("p1","p2","p3","p4","p9a","p9c","p12","p13","p15","p11")
trad_abril = c("vida_hoje","interesse","intencao_espontanea","intencao_estimulada","2turno_aecio","2turno_campos","avaliacao_governo","aprova_dilma","desejo_mudanca","rejeicao")
abril = cria_arquivo("ibopeabr2014.sav",perg_abril,trad_abril)
resultado_abril = calcula_tudo(abril,"2014-04-14")

#DF de MAIO
perg_maio = c("p1","p2","p3","p4","p7a","p7b","p10","p11","p13","p8")
trad_maio = c("vida_hoje","interesse","intencao_espontanea","intencao_estimulada","2turno_aecio","2turno_campos","avaliacao_governo","aprova_dilma","desejo_mudanca","rejeicao")
maio = cria_arquivo("ibopemai2014.sav",perg_maio,trad_maio)
resultado_maio = calcula_tudo(maio,"2014-05-19")

#DF de JUNHO_1
perg_junho = c("p1","p2","p3","p801","p802","p11","p12","p13")
trad_junho = c("interesse","intencao_espontanea","intencao_estimulada","2turno_aecio","2turno_campos","rejeicao","avaliacao_governo","aprova_dilma")
junho = cria_arquivo("ibopejun2014.sav",perg_junho,trad_junho)
resultado_junho = calcula_tudo(junho,"2014-06-08")

#DF de JUNHO_2
perg_junho2 = c("p1","p2","p3","p401","p402","p6","p7")
trad_junho2 = c("interesse","intencao_espontanea","intencao_estimulada","2turno_aecio","2turno_campos","avaliacao_governo","aprova_dilma")
junho2 = cria_arquivo("ibopejun2014_2.sav",perg_junho2,trad_junho2)
resultado_junho2 = calcula_tudo(junho2,"2014-06-15")

#DF de JULHO
perg = c("p1","p2","p3","p4","p501","p502","p8","p10","p11","p21a01","p21b01","p7","p12","p1501","p1503","p1504","p1506","p20","p6")
trad = c("vida_hoje","interesse","intencao_espontanea","intencao_estimulada","2turno_aecio","2turno_campos","desejo_mudanca","avaliacao_governo","aprova_dilma","bolsa1","bolsa2","favorito","nota","poder_compra","saude","emprego","educacao","partido","rejeicao")
jul = cria_arquivo("ibopejul2014.sav",perg,trad)
resultado_jul = calcula_tudo(jul,"2014-07-21")

#DF de AGOSTO
perg = c("p1","p2","p3","p4","p501","p502","p7","p8","p9","p10","p11","p12","p13a01","p13b01","p6")
trad = c("vida_hoje","interesse","intencao_espontanea","intencao_estimulada","2turno_aecio","2turno_campos","favorito","desejo_mudanca","avaliacao_governo","aprova_dilma","nota","partido","bolsa1","bolsa2","rejeicao")
ago = cria_arquivo("ibopeago2014.sav",perg,trad)
resultado_ago = calcula_tudo(ago,"2014-08-06")

#DF de AGOSTO_2
perg = c("p1","p2","p3","p4","p601","p602","p7","p9","p10","p11","p12","p13","p21","p22a01","p22b01")
trad = c("vida_hoje","interesse","intencao_espontanea","intencao_estimulada","2turno_aecio","2turno_marina","rejeicao","favorito","desejo_mudanca","avaliacao_governo","aprova_dilma","nota","partido","bolsa1","bolsa2")
ago2 = cria_arquivo("ago2014_2.sav",perg,trad)
resultado_ago2 = calcula_tudo(ago2,"2014-08-25")

#DF de AGOSTO_2 - RINGUE
perg = c("p801","p802","p803","porte")
trad = c("aecio","dilma","marina","porte")
ago2 = cria_arquivo("ago2014_2.sav",perg,trad)
resultado_ago2 = calcula_tudo(ago2,"2014-08-25")

resultado=rbind(resultado_marco,resultado_abril,resultado_maio,resultado_junho,resultado_junho2,resultado_jul,resultado_ago)
resultado = resultado[complete.cases(resultado),]
write.csv(resultado,"resultado.csv", row.names=FALSE)

#############
perg_out = c("p8")
trad_out = c("avaliacao_governo")
perg_nov = c("p8")
trad_nov = c("avaliacao_governo")
out = cria_arquivo("ibopeout2013.sav",perg_out,trad_out)
nov = cria_arquivo("ibopenov2013.sav",perg_nov,trad_nov)
resultado_out = calcula_tudo(out,"2013-10-21")
resultado_nov = calcula_tudo(nov,"2013-11-11")

###############
maio2 <- as.data.frame(as.data.set(spss.system.file("ibopemai2014.sav")))
jul2 <- as.data.frame(as.data.set(spss.system.file("ibopejul2014.sav")))
ago2 <- as.data.frame(as.data.set(spss.system.file("ago2014_2.sav")))
round(normaliza(cruza_respostas(data,"P1","reg")),1)
