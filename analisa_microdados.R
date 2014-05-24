cruza_respostas = function (bd,pergunta,pergunta2) {
  
#faz o cruzamento das colunas
cruzamento = as.data.frame(table(bd[[pergunta]],bd[[pergunta2]]))

#monta o dataframe de saída
respostas = unique(cruzamento$Var1)
respostas2 = unique(cruzamento$Var2)
final = data.frame(matrix(0, nrow = length(respostas), ncol = length(respostas2)))
names(final) = respostas2
row.names(final) = respostas

#preenche o dataframe de saída
for (i in unique(cruzamento$Var1)) {
  for (j in unique(cruzamento$Var2)) {
    final[[i,j]] = cruzamento$Freq[cruzamento$Var1 == i & cruzamento$Var2 ==j]
  }
}

#acha a porcentagem correspondente a cada opção
final = final*100/length(bd[[pergunta]])

#retira candidatos com 0 de menção
if ((pergunta == "intencao_espontanea") | (pergunta == "intencao_estimulada")) {
  for (i in names(final)) {
    final = final[final[[i]] != 0,]
  }  
}

return(final)
}

normaliza = function (final) {
  for (i in names(final)) {
    final[[i]] = final[[i]]*100/sum(final[[i]])
  }
  return(final)
}


normaliza2 = function (final) {
  for (i in names(final)) {
    final[[i]] = final[[i]]*100/sum(final[[i]])
  }
  return(final)
}

#função que dá a porcentagem para apenas uma variável. retorna um dataframe com o resultado
uma_pergunta = function (bd, pergunta) {
  bd$resultado = "resultado"
  return(cruza_respostas(bd,pergunta,"resultado"))
}

#função que calcula a variável para todos os dados e todos os recortes possíveis
#ATENÇÃO: nome da variável deve ser a data da pesquisa
calcula_tudo = function (final) {
  recortes = c("sexo","idade","instrucao","renda_familiar","condicao_municipio","regiao","raca","religiao","vida_hoje","interesse_eleicoes","grau_mudanca","avaliacao_governo")
  perguntas = c("vida_hoje","interesse_eleicoes","intencao_espontanea","intencao_estimulada","avaliacao_governo","aprova_dilma","grau_mudanca")
  saida = data.frame(data=character(0),cat_dado=character(0),dado=character(0),cat_recorte=character(0),recorte=character(0),variavel=numeric(0))
  for (r in recortes) {
    if (r  %in% names(marco)) {
      for (p in perguntas) {
        if (p %in% names(marco)) {
          if (p != r) {
            teste = round(normaliza(cruza_respostas(final,p,r)),1)
            saida = rbind(saida,norm2(teste,p,r)) 
          }
        }
      } 
    }
  }
  saida$data = deparse(substitute(final))
  return(saida)
}

#função que reagrega as variáveis de renda, escolaridade e idade de acordo com o padrão do Ibope
reagrega = function(ibopemar14) {
  # Reagrega a variável escolaridade
  ibopemar14$escolaridade=factor(NA,levels=c('Fundamental1','Fundamental 2','Medio','Superior'))
  escolaridade=sort(unique(ibopemar14$inst))
  ibopemar14$escolaridade[ibopemar14$inst==escolaridade[1]]='Fundamental 1'
  ibopemar14$escolaridade[ibopemar14$inst==escolaridade[2]]='Fundamental 1'
  ibopemar14$escolaridade[ibopemar14$inst==escolaridade[3]]='Fundamental 1'
  ibopemar14$escolaridade[ibopemar14$inst==escolaridade[4]]='Fundamental 1'
  ibopemar14$escolaridade[ibopemar14$inst==escolaridade[5]]='Fundamental 2'
  ibopemar14$escolaridade[ibopemar14$inst==escolaridade[6]]='Fundamental 2'
  ibopemar14$escolaridade[ibopemar14$inst==escolaridade[7]]='Medio'
  ibopemar14$escolaridade[ibopemar14$inst==escolaridade[8]]='Medio'
  ibopemar14$escolaridade[ibopemar14$inst==escolaridade[9]]='Superior'
  ibopemar14$escolaridade[ibopemar14$inst==escolaridade[10]]='Superior'
  # Reagrega a variável de renda
  ibopemar14$renda=factor(NA,levels=c('Mais de 5','2 a 5','1 a 2','Ate 1'))
  renda=sort(unique(ibopemar14$rend2))
  ibopemar14$renda[ibopemar14$rend2==renda[1]]='Mais de 5'
  ibopemar14$renda[ibopemar14$rend2==renda[2]]='Mais de 5'
  ibopemar14$renda[ibopemar14$rend2==renda[3]]='Mais de 5'
  ibopemar14$renda[ibopemar14$rend2==renda[4]]='2 a 5'
  ibopemar14$renda[ibopemar14$rend2==renda[5]]='1 a 2'
  ibopemar14$renda[ibopemar14$rend2==renda[6]]='Ate 1'
  ibopemar14$renda[ibopemar14$rend2==renda[7]]='Ate 1'
  # Reagrega a variável idade
  ibopemar14$idad2=factor(NA,levels=c('16 a 24','25 a 34','35 a 44','45 a 54','55 ou mais'))
  idad2=sort(unique(ibopemar14$idad))
  ibopemar14$idad2[ibopemar14$idad==idad2[1]]='16 a 24'
  ibopemar14$idad2[ibopemar14$idad==idad2[2]]='16 a 24'
  ibopemar14$idad2[ibopemar14$idad==idad2[3]]='25 a 34'
  ibopemar14$idad2[ibopemar14$idad==idad2[4]]='35 a 44'
  ibopemar14$idad2[ibopemar14$idad==idad2[5]]='45 a 54'
  ibopemar14$idad2[ibopemar14$idad==idad2[6]]='55 ou mais'
  ibopemar14$idad2[ibopemar14$idad==idad2[7]]='55 ou mais'
  # Reagrega a variável religiao
  ibopeabr2014$religiao=factor(NA,levels=c('Catolica','Evangelica','Outras'))
  religiao=sort(unique(ibopeabr2014$reli))
  ibopeabr2014$religiao[ibopeabr2014$reli==religiao[1]]='Catolica'
  ibopeabr2014$religiao[ibopeabr2014$reli==religiao[2]]='Evangelica'
  ibopeabr2014$religiao[ibopeabr2014$reli==religiao[3]]='Evangelica'
  ibopeabr2014$religiao[ibopeabr2014$reli==religiao[4]]='Evangelica'
  ibopeabr2014$religiao[ibopeabr2014$reli==religiao[5]]='Evangelica'
  ibopeabr2014$religiao[ibopeabr2014$reli==religiao[6]]='Evangelica'
  ibopeabr2014$religiao[ibopeabr2014$reli==religiao[7]]='Evangelica'
  ibopeabr2014$religiao[ibopeabr2014$reli==religiao[8]]='Evangelica'
  ibopeabr2014$religiao[ibopeabr2014$reli==religiao[9]]='Evangelica'
  ibopeabr2014$religiao[ibopeabr2014$reli==religiao[10]]='Evangelica'
  ibopeabr2014$religiao[ibopeabr2014$reli==religiao[11]]='Evangelica'
  ibopeabr2014$religiao[ibopeabr2014$reli==religiao[12]]='Outras'
  ibopeabr2014$religiao[ibopeabr2014$reli==religiao[13]]='Outras'
  ibopeabr2014$religiao[ibopeabr2014$reli==religiao[14]]='Outras'
  ibopeabr2014$religiao[ibopeabr2014$reli==religiao[15]]='Outras'
  ibopeabr2014$religiao[ibopeabr2014$reli==religiao[16]]='Outras'
  ibopeabr2014$religiao[ibopeabr2014$reli==religiao[17]]='Outras'
  ibopeabr2014$religiao[ibopeabr2014$reli==religiao[18]]='Outras'
  ibopeabr2014$religiao[ibopeabr2014$reli==religiao[19]]='Outras'
  ibopeabr2014$religiao[ibopeabr2014$reli==religiao[20]]='Outras'
  ibopeabr2014$religiao[ibopeabr2014$reli==religiao[21]]='Outras'
  
  return(ibopemar14)
}

#função normalizadora auxiliar para ser usada no processo calcula_tudo()
norm2 = function (final,p,r) {
  saida = data.frame(data=character(0),cat_dado=character(0),dado=character(0),cat_recorte=character(0),recorte=character(0),variavel=numeric(0))
  for (i in names(final)) {
    for (j in row.names(final)) {
      nova_linha = data.frame(data="",cat_dado=p,dado=j,cat_recorte=r,recorte=i,variavel=final[j,i])
      saida = rbind(saida,nova_linha)
    }
  }
  return(saida)
}

#função para criar aquivo a partir do .sav do Ibope
#ATENÇÃO: deve ser criado um vector - c() - com o nome das perguntas do .sav (só as p1,p2,etc, sem as variáveis já com nome) e outro com a tradução deles
#O nome do arquivo resultado dessa função deve ser o mês ou algum outro indicador da pesquisa - isso será necessário para a função calcula_tudo
cria_arquivo = function(arquivo,perg,trad) {
  library("memisc", lib.loc="/Library/Frameworks/R.framework/Versions/3.0/Resources/library")
  data <- as.data.frame(as.data.set(spss.system.file(arquivo)))
  saida = reagrega(saida)
  pergs = append(c("sexo", "escolaridade","renda","idad2","inst","raca","reli","cond","reg"),perg)
  trads = append(c("sexo","escolaridade","renda_familiar","idade","instrucao","raca","religiao","condicao_municipio","regiao"),trad)
  saida = data[,pergs]
  names(saida) = trads
  return(saida)
}