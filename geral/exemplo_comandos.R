
perg_maio = c("p1","p2","p3","p4","p7a","p7b","p10","p11","p13")
trad_maio = c("vida_hoje","interesse","intencao_espontanea","intencao_estimulada","2turno_aecio","2turno_campos","avaliacao_governo","aprova_dilma","desejo_mudanca")
perg_abril = c("p1","p2","p3","p4","p9a","p9c","p12","p13","p15")
trad_abril = c("vida_hoje","interesse","intencao_espontanea","intencao_estimulada","2turno_aecio","2turno_campos","avaliacao_governo","aprova_dilma","desejo_mudanca")
perg_marco = c("p1","p2","p3","p4","p8a","p8c","p10")
trad_marco = c("vida_hoje","interesse","intencao_espontanea","intencao_estimulada","2turno_aecio","2turno_campos","desejo_mudanca")
marco = cria_arquivo("ibopemar2014.sav",perg_marco,trad_marco,FALSE)
abril = cria_arquivo("ibopeabr2014.sav",perg_abril,trad_abril,TRUE)
maio = cria_arquivo("ibopemai2014.sav",perg_maio,trad_maio,TRUE)
resultado_marco = calcula_tudo(marco,"2014-03-17")
resultado_abril = calcula_tudo(abril,"2014-04-14")
resultado_maio = calcula_tudo(maio,"2014-05-19")
resultado=rbind(resultado_marco,resultado_abril,resultado_maio)
resultado = resultado[complete.cases(resultado),]
write.csv(resultado,"resultado.csv", row.names=FALSE)

#############
perg_out = c("p8")
trad_out = c("avaliacao_governo")
perg_nov = c("p8")
trad_nov = c("avaliacao_governo")
out = cria_arquivo("ibopeout2013.sav",perg_out,trad_out,FALSE)
nov = cria_arquivo("ibopenov2013.sav",perg_nov,trad_nov,FALSE)
resultado_out = calcula_tudo(out,"2013-10-21")
resultado_nov = calcula_tudo(nov,"2013-11-11")

###############
teste <- as.data.frame(as.data.set(spss.system.file("ibopenov2013.sav")))
