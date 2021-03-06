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
perg = c("p1","p2","p3","p4","p601","p602","p7","p9","p10","p11","p12","p13","p21","p22a01","p22b01","p801","p802","p803")
trad = c("vida_hoje","interesse","intencao_espontanea","intencao_estimulada","2turno_aecio","2turno_marina","rejeicao","favorito","desejo_mudanca","avaliacao_governo","aprova_dilma","nota","partido","bolsa1","bolsa2","aecio","dilma","marina")
ago2 = cria_arquivo("ago2014_2.sav",perg,trad)
resultado_ago2 = calcula_tudo(ago2,"2014-08-25")

#DF de AGOSTO_2 - RINGUE
perg = c("p801","p802","p803","porte")
trad = c("aecio","dilma","marina","porte")
ago2 = cria_arquivo("ago2014_2.sav",perg,trad)
resultado_ago2 = calcula_tudo(ago2,"2014-08-25")

#DF de SETEMBRO
perg = c("p1","p2","p3","p4","p601","p602","p7","p8","p9","p10","p11","p12","p1301","p1303","p1304","p1306","p15","p16a01","p16b01")
trad = c("vida_hoje","interesse","intencao_espontanea","intencao_estimulada","2turno_aecio","2turno_marina","rejeicao","favorito","desejo_mudanca","avaliacao_governo","aprova_dilma","nota","poder_compra","saude","emprego","educacao","partido","bolsa1","bolsa2")
set = cria_arquivo("set2014.sav",perg,trad)
resultado_set = calcula_tudo(set,"2014-09-02")
perfil = perfil_candidatos(set)
write.csv(perfil,"perfil.csv",row.names=FALSE)

#DF de SETEMBRO_2
perg = c("p1","p2","p3","p4","p501","p502","p7","p8")
trad = c("vida_hoje","interesse","intencao_espontanea","intencao_estimulada","2turno_aecio","2turno_marina","avaliacao_governo","aprova_dilma")
set2 = cria_arquivo("set2014_2.sav",perg,trad)
resultado_set2 = calcula_tudo(set2,"2014-09-08")
write.csv(resultado_set2,"resultado.csv", row.names=FALSE)
perfil = perfil_candidatos(set2)
write.csv(perfil,"perfil.csv",row.names=FALSE)

#DF de SETEMBRO_3
perg = c("p1","p2","p3","p4","p601","p602","p7","p9","p10","p11","p12","p13","p16","p17a01","p17b01","p801","p802","p803")
trad = c("vida_hoje","interesse","intencao_espontanea","intencao_estimulada","2turno_aecio","2turno_marina","rejeicao","favorito","desejo_mudanca","avaliacao_governo","aprova_dilma","nota","partido","bolsa1","bolsa2","aecio","dilma","marina")
set3 = cria_arquivo("set_3.sav",perg,trad)
resultado_set3 = calcula_tudo(set3,"2014-09-15")
write.csv(resultado_set3,"resultado.csv", row.names=FALSE)
perfil = perfil_candidatos(set3)
write.csv(perfil,"perfil.csv",row.names=FALSE)

#DF de SET_3 - RINGUE
perg = c("p801","p802","p803","porte")
trad = c("aecio","dilma","marina","porte")
set3 = cria_arquivo("set_3.sav",perg,trad)
ringue = calcula_tudo(set3,"2014-09-15")
write.csv(ringue,"ringue.csv") #- DEPOIS DELETAR DUAS PRIMERIAS COLUNAS

#DF de SETEMBRO_4
perg = c("p1","p2","p3","p4","p601","p602","p7","p9","p10","p11","p12","p13","p19","p20a01","p20b01","p801","p802","p803")
trad = c("vida_hoje","interesse","intencao_espontanea","intencao_estimulada","2turno_aecio","2turno_marina","rejeicao","favorito","desejo_mudanca","avaliacao_governo","aprova_dilma","nota","partido","bolsa1","bolsa2","aecio","dilma","marina")
set4 = cria_arquivo("set_4.sav",perg,trad)
resultado_set4 = calcula_tudo(set4,"2014-09-22")
write.csv(resultado_set4,"resultado.csv", row.names=FALSE)
perfil = perfil_candidatos(set4)
write.csv(perfil,"perfil.csv",row.names=FALSE)

#DF de SET_4 - RINGUE
perg = c("p801","p802","p803","porte")
trad = c("aecio","dilma","marina","porte")
set4 = cria_arquivo("set_4.sav",perg,trad)
ringue = calcula_tudo(set4,"2014-09-22")
write.csv(ringue,"ringue.csv") #- DEPOIS DELETAR DUAS PRIMERIAS COLUNAS

#DF de SETEMBRO_5
perg = c("p1","p2","p3","p4a","p601","p602","p7","p9","p10","p11","p12","p13","p19","p20a01","p20b01","p801","p802","p803")
trad = c("vida_hoje","interesse","intencao_espontanea","intencao_estimulada","2turno_aecio","2turno_marina","rejeicao","favorito","desejo_mudanca","avaliacao_governo","aprova_dilma","nota","partido","bolsa1","bolsa2","aecio","dilma","marina")
set5 = cria_arquivo("set_5.sav",perg,trad)
resultado_set5 = calcula_tudo(set5,"2014-09-29")
write.csv(resultado_set5,"resultado.csv", row.names=FALSE)
perfil = perfil_candidatos(set5)
write.csv(perfil,"perfil.csv",row.names=FALSE)

#DF de SET_5 - RINGUE
perg = c("p801","p802","p803","porte")
trad = c("aecio","dilma","marina","porte")
set5 = cria_arquivo("set_5.sav",perg,trad)
ringue = calcula_tudo(set5,"2014-09-29")
write.csv(ringue,"ringue.csv") #- DEPOIS DELETAR DUAS PRIMERIAS COLUNAS ANTES DE PASSAR O PYTHON

#DF de OUTUBRO_1
perg = c("p1","p2","p3","p4a","p601","p602","p7","p9","p10","p11","p12","p13","p15","p16a01","p16b01","p801","p802","p803")
trad = c("vida_hoje","interesse","intencao_espontanea","intencao_estimulada","2turno_aecio","2turno_marina","rejeicao","favorito","desejo_mudanca","avaliacao_governo","aprova_dilma","nota","partido","bolsa1","bolsa2","aecio","dilma","marina")
out1 = cria_arquivo("out_1.sav",perg,trad)
resultado_out1 = calcula_tudo(out1,"2014-10-01")
write.csv(resultado_out1,"resultado.csv", row.names=FALSE)
perfil = perfil_candidatos(out1)
write.csv(perfil,"perfil.csv",row.names=FALSE)

#DF de OUTUBRO_1 - RINGUE
perg = c("p801","p802","p803","porte")
trad = c("aecio","dilma","marina","porte")
out1 = cria_arquivo("out_1.sav",perg,trad)
ringue = calcula_tudo(out1,"2014-10-01")
write.csv(ringue,"ringue.csv") #- DEPOIS DELETAR DUAS PRIMERIAS COLUNAS ANTES DE PASSAR O PYTHON

#DF de OUTUBRO_2
perg = c("p1","p2","p3","p4a","p501","p502","p6","p7","p8","p9","p10")
trad = c("vida_hoje","interesse","intencao_espontanea","intencao_estimulada","2turno_aecio","2turno_marina","rejeicao","favorito","avaliacao_governo","aprova_dilma","partido")
out2 = cria_arquivo("out_2.sav",perg,trad)
resultado_out2 = calcula_tudo(out2,"2014-10-04")
write.csv(resultado_out2,"resultado.csv", row.names=FALSE)
perfil = perfil_candidatos(out2)
write.csv(perfil,"perfil.csv",row.names=FALSE)


#DF de OUTUBRO_3 -- PRIMEIRO DO SEGUNDO TURNO
perg = c("p1","p2","p3","p4","p5","p6","p7","p10","p11","p12","p13","p15","p16a01","p16b01")
trad = c("vida_hoje","interesse","intencao_espontanea","2turno_aecio","certeza_voto","rejeicao","favorito","voto_1turno","avaliacao_governo","aprova_dilma","nota","partido","bolsa1","bolsa2")
out3 = cria_arquivo("out_3.sav",perg,trad)
resultado_out3 = calcula_tudo(out3,"2014-10-08")
write.csv(resultado_out3,"resultado.csv", row.names=FALSE)
perfil = perfil_candidatos(out2)
write.csv(perfil,"perfil.csv",row.names=FALSE)


#DF de OUTUBRO_4 -- SEGUNDO DO SEGUNDO TURNO
perg = c("p1","p2","p3","p4","p5","p6","p7","p9","p10","p11","p12","p13","p21a01","p21b01")
trad = c("vida_hoje","interesse","intencao_espontanea","2turno_aecio","certeza_voto","rejeicao","favorito","voto_1turno","avaliacao_governo","aprova_dilma","nota","partido","bolsa1","bolsa2")
out4 = cria_arquivo("out4.sav",perg,trad)
resultado_out4 = calcula_tudo(out4,"2014-10-22")
write.csv(resultado_out4,"resultado.csv", row.names=FALSE)
perfil = perfil_candidatos(out2)
write.csv(perfil,"perfil.csv",row.names=FALSE)



resultado=rbind(resultado_ago2,resultado_set3,resultado_set4)
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
set <- as.data.frame(as.data.set(spss.system.file("set2014_2.sav")))
round(normaliza(cruza_respostas(data,"P1","reg")),1)
