options(stringsAsFactors = FALSE)
library(memisc)
library(plyr)

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
calcula_tudo = function (final,data_pesquisa) {
  final$total = "total"
  recortes = c("sexo","idade","escolaridade","renda_familiar","condicao_municipio","regiao","cor","religiao","vida_hoje","interesse","desejo_mudanca","avaliacao_governo2","total","intencao_estimulada","favorito","nota_recorte","poder_compra","saude","emprego","educacao","partido","bolsa_familia","bolsa","porte","aecio","dilma","marina")
  perguntas = c("vida_hoje","interesse","intencao_espontanea","intencao_estimulada","avaliacao_governo","aprova_dilma","desejo_mudanca","rejeicao","2turno_aecio","2turno_campos","favorito","nota","poder_compra","saude","emprego","educacao","2turno_marina","aecio","marina","dilma","estimulada_validos","2marina_validos","2aecio_validos","espontanea_validos")
  saida = data.frame(data=character(0),cat_pergunta=character(0),dado=character(0),cat_recorte=character(0),recorte=character(0),valor=numeric(0))
  for (r in recortes) {
    if (r  %in% names(final)) {
      for (p in perguntas) {
        if (p %in% names(final)) {
          if (p != r) {
            if (!(p %in% c("rejeicao","nota"))) {
              temp = round(normaliza(cruza_respostas(final,p,r)),0)
              saida = rbind(saida,norm2(temp,p,r))
            } else if (p == "rejeicao") {
              if (final$rejeicao[1] == TRUE) {
                temp = calcula_rejeicao(final,r)
                saida = rbind(saida,norm2(temp,"rejeicao",r))  
              }
            } else if (p == "nota") {
              #tira os NAs pra nota
              temp = final[complete.cases(final$nota),]
              #faz a agregação da nota pela variável r (recorte)
              eval(parse(text = paste0("temp2 = ddply(temp,~",r,",summarise,nota=mean(nota))")))
              #se houver NA no recorte, tiramos
              temp2 = na.omit(temp2)
              #arrumamos os nomes e retiramos a coluna com eles
              names(temp2)[1] = r
              row.names(temp2) = temp2[[r]]
              temp2[[r]] = NULL
              #arredondamos, fazemos o transpose e mandamos para o norm2
              temp2 = round(temp2,1)
              temp2 = as.data.frame(t(temp2))
              saida = rbind(saida,norm2(temp2,"nota",r))
            }
          }
        }
      } 
    }
  }
  
  saida$data = data_pesquisa
  
  
  #agora vamos consertar alguns campos que não precisamos ou que têm de ser mostrados de maneira diferente
  if ("intencao_estimulada" %in% names(final)){
    saida[saida$cat_recorte == "intencao_estimulada" & saida$recorte == "Pastor Everaldo",] = NA
  }
  saida = na.omit(saida)
  
  if ("nota_recorte" %in% names(final)) {
    saida[saida$cat_recorte == "nota_recorte",][["cat_recorte"]] = "nota"
  }
  
  if ("avaliacao_governo2" %in% names(final)) {
    saida[saida$cat_recorte == "avaliacao_governo2",][["cat_recorte"]] = "avaliacao_governo"
    #soma as possibilidades de avaliação de governo
    saida = arruma_avaliacao(saida)
    }
  
  saida = na.omit(saida)
    
    return(saida)
}

arruma_avaliacao = function(arquivo) {
  #pega só as avaliações em um novo objeto
  aval = arquivo[arquivo$cat_pergunta == "avaliacao_governo",]
  dia = unique(aval$data)  
  #para cada recorte, soma otima e boa e ruim e pésisma e adiciona novas linhas com esses valores
  for (cat_recorte in unique(aval$cat_recorte)) {
    temp2 = aval[aval$cat_recorte == cat_recorte,]
    for (recorte in unique(temp2$recorte)){
      temp = temp2[temp2$recorte == recorte,]
      otima = temp[temp$dado == "Ótima",][["valor"]]
      boa = temp[temp$dado == "Boa",][["valor"]]
      ruim = temp[temp$dado == "Ruim",][["valor"]]
      pessima = temp[temp$dado == "Péssima",][["valor"]]
      #se ninguém achar que é ótima
      if (length(otima) == 0) { otima = 0 }
      nova_linha = data.frame(data=dia,cat_pergunta="avaliacao_governo",dado="Ótimo e bom",cat_recorte=cat_recorte,recorte=recorte,valor=otima+boa)
      aval = rbind(aval,nova_linha)
      nova_linha = data.frame(data=dia,cat_pergunta="avaliacao_governo",dado="Ruim e péssimo",cat_recorte=cat_recorte,recorte=recorte,valor=ruim+pessima)
      aval = rbind(aval,nova_linha)
    }    
  }
  
  #retira os valores de ótima, boa, ruim e péssima do nosso arquivo temporário
  aval[aval$dado == "Ótima",][["valor"]] = NA
  aval[aval$dado == "Boa",][["valor"]] = NA
  aval[aval$dado == "Ruim",][["valor"]] = NA
  aval[aval$dado == "Péssima",][["valor"]] = NA
  aval = na.omit(aval)
  
  #retira as avaliações de governo antigas e adiciona as da variável "aval"
  arquivo[arquivo$cat_pergunta == "avaliacao_governo",][["valor"]] = NA
  arquivo = na.omit(arquivo)
  arquivo = rbind(arquivo,aval)
  
  return(arquivo)
}


#função que reagrega as variáveis de renda, escolaridade e idade de acordo com o padrão do Ibope
reagrega_nomes = function(arquivo) {
  # Reagrega a variável escolaridade
  arquivo$escolaridade=factor(NA,levels=c('Fundamental 1','Fundamental 2','Medio','Superior'))
  escolaridade=c("ANALF.","SABE LER/ ESCR. MAS NÃO CURSOU ESCOLA","PRIM. INC.","PRIM. COMP.","GINÁS. INC.","GINÁS. COMP.","COLÉG. INC.","COLÉG. COMP.","SUP. INC.","SUP. COMP.")
  arquivo$escolaridade[arquivo$inst==escolaridade[1]]='Fundamental 1'
  arquivo$escolaridade[arquivo$inst==escolaridade[2]]='Fundamental 1'
  arquivo$escolaridade[arquivo$inst==escolaridade[3]]='Fundamental 1'
  arquivo$escolaridade[arquivo$inst==escolaridade[4]]='Fundamental 1'
  arquivo$escolaridade[arquivo$inst==escolaridade[5]]='Fundamental 2'
  arquivo$escolaridade[arquivo$inst==escolaridade[6]]='Fundamental 2'
  arquivo$escolaridade[arquivo$inst==escolaridade[7]]='Medio'
  arquivo$escolaridade[arquivo$inst==escolaridade[8]]='Medio'
  arquivo$escolaridade[arquivo$inst==escolaridade[9]]='Superior'
  arquivo$escolaridade[arquivo$inst==escolaridade[10]]='Superior'
  # Reagrega a variável de renda
  arquivo$renda=factor(NA,levels=c('Mais de 5','2 a 5','1 a 2','Ate 1'))
  renda=c("MAIS DE 20","MAIS DE 10 A 20","MAIS DE 5 A 10","MAIS DE 2 A 5","MAIS DE 1 A 2","ATÉ 1","NÃO TEM RENDIMENTO PESSOAL")
  arquivo$renda[arquivo$rend2==renda[1]]='Mais de 5'
  arquivo$renda[arquivo$rend2==renda[2]]='Mais de 5'
  arquivo$renda[arquivo$rend2==renda[3]]='Mais de 5'
  arquivo$renda[arquivo$rend2==renda[4]]='2 a 5'
  arquivo$renda[arquivo$rend2==renda[5]]='1 a 2'
  arquivo$renda[arquivo$rend2==renda[6]]='Ate 1'
  arquivo$renda[arquivo$rend2==renda[7]]='Ate 1'
  # Reagrega a variável idade
  arquivo$idad2=factor(NA,levels=c('16 a 24','25 a 34','35 a 44','45 a 54','55 ou mais'))
  idad2=c('16 E 17',"18 A 24",'25 A 34','35 A 44','45 A 54','55 A 64',"65 E MAIS")
  arquivo$idad2[arquivo$idad==idad2[1]]='16 a 24'
  arquivo$idad2[arquivo$idad==idad2[2]]='16 a 24'
  arquivo$idad2[arquivo$idad==idad2[3]]='25 a 34'
  arquivo$idad2[arquivo$idad==idad2[4]]='35 a 44'
  arquivo$idad2[arquivo$idad==idad2[5]]='45 a 54'
  arquivo$idad2[arquivo$idad==idad2[6]]='55 ou mais'
  arquivo$idad2[arquivo$idad==idad2[7]]='55 ou mais'
  # Reagrega a variável religiao
  arquivo$religiao=factor(NA,levels=c('Catolica','Evangelica','Outras'))
  religiao=c("Católica Apostólica Romana","Assembléia de Deus","Batista/ Metodista/ Presbiteriana","Universal do Reino de Deus","Deus é Amor","Evangelho Quadrangular","Igreja Internacional da Graça","Renascer em Cristo","Outras Evangélicas específicas","Evangélica - Não sabe especificar","Adventista","Testemunha de Jeová","Espírita/ Kardecista","Afro-Brasileiras (Umbanda, Candomblé, etc)","Orientais (Budismo, Islamismo, etc)","Outras religiões","É religioso mas não segue nenhuma/ Agnóstico","Ateu, não tem religião","Não respondeu")
  arquivo$religiao[arquivo$reli==religiao[1]]='Catolica'
  arquivo$religiao[arquivo$reli==religiao[2]]='Evangelica'
  arquivo$religiao[arquivo$reli==religiao[3]]='Evangelica'
  arquivo$religiao[arquivo$reli==religiao[4]]='Evangelica'
  arquivo$religiao[arquivo$reli==religiao[5]]='Evangelica'
  arquivo$religiao[arquivo$reli==religiao[6]]='Evangelica'
  arquivo$religiao[arquivo$reli==religiao[7]]='Evangelica'
  arquivo$religiao[arquivo$reli==religiao[8]]='Evangelica'
  arquivo$religiao[arquivo$reli==religiao[9]]='Evangelica'
  arquivo$religiao[arquivo$reli==religiao[10]]='Evangelica'
  arquivo$religiao[arquivo$reli==religiao[11]]='Evangelica'
  arquivo$religiao[arquivo$reli==religiao[12]]='Outras'
  arquivo$religiao[arquivo$reli==religiao[13]]='Outras'
  arquivo$religiao[arquivo$reli==religiao[14]]='Outras'
  arquivo$religiao[arquivo$reli==religiao[15]]='Outras'
  arquivo$religiao[arquivo$reli==religiao[16]]='Outras'
  arquivo$religiao[arquivo$reli==religiao[17]]='Outras'
  arquivo$religiao[arquivo$reli==religiao[18]]='Outras'
  arquivo$religiao[arquivo$reli==religiao[19]]='Outras'
  arquivo$religiao[arquivo$reli==religiao[20]]='Outras'
  arquivo$religiao[arquivo$reli==religiao[21]]='Outras'
  # Reagrega a variável cor
  arquivo$cor=factor(NA,levels=c('Branca','Preta ou parda','Outras'))
  cor=c("Branca","Preta","Parda","Amarela","Indígena")
  arquivo$cor[arquivo$raca==cor[1]]='Branca'
  arquivo$cor[arquivo$raca==cor[2]]='Preta ou parda'
  arquivo$cor[arquivo$raca==cor[3]]='Preta ou parda'
  arquivo$cor[arquivo$raca==cor[4]]='Outras'
  arquivo$cor[arquivo$raca==cor[5]]='Outras'
  arquivo$cor[arquivo$raca==cor[6]]='Outras'

  if ("porte" %in% names(arquivo)) {
    arquivo$porte[arquivo$porte == "ATÉ 5.000"] = "Cidades com até 20 mil"
    arquivo$porte[arquivo$porte == "DE 5.001 A 10.000"] = "Cidades com até 20 mil"
    arquivo$porte[arquivo$porte == "DE 10.001 A 20.000"] = "Cidades com até 20 mil"
    arquivo$porte[arquivo$porte == "DE 20.001 A 50.000"] = "Cidades de 20 a 100 mil"
    arquivo$porte[arquivo$porte == "DE 50.001 A 100.000"] = "Cidades de 20 a 100 mil"
    arquivo$porte[arquivo$porte == "DE 100.001 A 500.000"] = "Cidades com mais de 100 mil"
    arquivo$porte[arquivo$porte == "MAIS DE 500.000"] = "Cidades com mais de 100 mil"    
  }
  
  return(arquivo)
}

#função que reagrega a resposta das perguntas - interesse, vida hoje, etc
reagrega_perguntas = function(arquivo) {
  # Reagrega a variável satisfacao com a vida (satisf)
  if ("vida_hoje" %in% names(arquivo)) {
    arquivo$temp=factor(NA,levels=c('Satisfeito','Insatisfeito','NS/NR*'))
    satisf=c("Muito satisfeito",
             "Satisfeito",
             "Insatisfeito",
             "Muito insatisfeito")
    arquivo$temp[arquivo$vida_hoje==satisf[1]]='Satisfeito'
    arquivo$temp[arquivo$vida_hoje==satisf[2]]='Satisfeito'
    arquivo$temp[arquivo$vida_hoje==satisf[3]]='Insatisfeito'
    arquivo$temp[arquivo$vida_hoje==satisf[4]]='Insatisfeito'
    arquivo$temp[arquivo$vida_hoje =="Não sabe/ Não respondeu"]='NS/NR*'
    arquivo$vida_hoje = NULL
    names(arquivo)[names(arquivo)=="temp"] = "vida_hoje"
  }
  
  # Reagrega a variável mudanca
  if ("desejo_mudanca" %in% names(arquivo)) {
    arquivo$temp=factor(NA,levels=c('Quer mudança','Quer continuidade','NS/NR*'))
    mudanca=c("Mudasse totalmente o governo do país",
              "Mantivesse só alguns programas, mas mudasse muita coisa",
              "Fizesse poucas mudanças e desse continuidade para muita coisa",
              "Desse total continuidade ao governo atual")
    arquivo$temp[arquivo$desejo_mudanca==mudanca[1]]='Quer mudança'
    arquivo$temp[arquivo$desejo_mudanca==mudanca[2]]='Quer mudança'
    arquivo$temp[arquivo$desejo_mudanca==mudanca[3]]='Quer continuidade'
    arquivo$temp[arquivo$desejo_mudanca==mudanca[4]]='Quer continuidade'
    arquivo$temp[arquivo$desejo_mudanca =="Não sabe/ Não respondeu"]='NS/NR*'
    arquivo$desejo_mudanca = NULL
    names(arquivo)[names(arquivo)=="temp"] = "desejo_mudanca"
  }
  
  # Reagrega a variável interesse
  if ("interesse" %in% names(arquivo)) {
    arquivo$temp=factor(NA,levels=c('Tem muito interesse','Tem médio interesse','Não tem interesse','NS/NR*'))
    interesse=c("Muito interesse",
                "Interesse médio",
                "Pouco interesse",
                "Nenhum interesse")
    interesse2=c("Nenhum interesse nas eleições que ocorrerão em outubro","Não sabe","Não respondeu")
    arquivo$temp[arquivo$interesse==interesse[1]]='Tem muito interesse'
    arquivo$temp[arquivo$interesse==interesse[2]]='Tem médio interesse'
    arquivo$temp[arquivo$interesse==interesse[3]]='Não tem interesse'    
    arquivo$temp[arquivo$interesse==interesse[4]]='Não tem interesse'
    arquivo$temp[arquivo$interesse==interesse2[1]]='Não tem interesse'
    arquivo$temp[arquivo$interesse =="Não sabe/ Não respondeu"]='NS/NR*'
    arquivo$temp[arquivo$interesse ==interesse2[2]]='NS/NR*'
    arquivo$temp[arquivo$interesse ==interesse2[3]]='NS/NR*'
    arquivo$interesse = NULL
    names(arquivo)[names(arquivo)=="temp"] = "interesse"
  }
  # Reagrega a variável avaliação do governo
  if ("avaliacao_governo" %in% names(arquivo)) {
    #cria uma segunda coluna só por causa do recorte de avaliação
    arquivo$avaliacao_governo2 = "a"
    aval=c("Ótimo",'Bom','Regular','Ruim',"Péssimo",'Não sabe/ Não respondeu')
    aval2=c("Ótima",'Boa','Regular','Ruim',"Péssima",'Não sabe/ Não respondeu')
    arquivo$avaliacao_governo[arquivo$avaliacao_governo==aval[1]]='Ótima'
    arquivo$avaliacao_governo2[arquivo$avaliacao_governo==aval[1]]='Ótimo e bom'
    arquivo$avaliacao_governo2[arquivo$avaliacao_governo==aval2[1]]='Ótimo e bom'
    arquivo$avaliacao_governo[arquivo$avaliacao_governo==aval[2]]='Boa'
    arquivo$avaliacao_governo2[arquivo$avaliacao_governo==aval[2]]='Ótimo e bom'
    arquivo$avaliacao_governo2[arquivo$avaliacao_governo==aval2[2]]='Ótimo e bom'
    arquivo$avaliacao_governo[arquivo$avaliacao_governo==aval[3]]='Regular'
    arquivo$avaliacao_governo2[arquivo$avaliacao_governo==aval[3]]='Regular'    
    arquivo$avaliacao_governo[arquivo$avaliacao_governo==aval[4]]='Ruim'
    arquivo$avaliacao_governo2[arquivo$avaliacao_governo==aval[4]]='Ruim e péssimo'
    arquivo$avaliacao_governo[arquivo$avaliacao_governo==aval[5]]='Péssima'
    arquivo$avaliacao_governo2[arquivo$avaliacao_governo==aval[5]]='Ruim e péssimo'
    arquivo$avaliacao_governo2[arquivo$avaliacao_governo==aval2[5]]='Ruim e péssimo'
    arquivo$avaliacao_governo[arquivo$avaliacao_governo==aval[6]]='NS/NR*'
    arquivo$avaliacao_governo2[arquivo$avaliacao_governo==aval[6]]='NS/NR*'
    arquivo$avaliacao_governo2[arquivo$avaliacao_governo2 =="Não sabe"]='NS/NR*'
    arquivo$avaliacao_governo2[arquivo$avaliacao_governo2 =="Não respondeu"]='NS/NR*'
    arquivo$avaliacao_governo[arquivo$avaliacao_governo =="Não sabe"]='NS/NR*'
    arquivo$avaliacao_governo[arquivo$avaliacao_governo =="Não respondeu"]='NS/NR*'
  }
  
  # Reagrega os nanicos nas votações espontaneas e cria coluna de votos apenas válidos
  if ("intencao_espontanea" %in% names(arquivo)) {
    candidatos = c('Aécio Neves','Dilma Rousseff','Eduardo Campos','Marina Silva','Lula','Pastor Everaldo','Branco e Nulo','NS/NR*','Outros')
    arquivo$intencao_espontanea <- as.character(arquivo$intencao_espontanea)
    arquivo$intencao_espontanea[arquivo$intencao_espontanea =="Não sabe/ Não respondeu"]='NS/NR*'
    arquivo$intencao_espontanea[arquivo$intencao_espontanea =="Não sabe"]='NS/NR*'
    arquivo$intencao_espontanea[arquivo$intencao_espontanea =="Não respondeu"]='NS/NR*'
    arquivo$intencao_espontanea[arquivo$intencao_espontanea =="Branco/ Nulo"]='Branco e Nulo'
    arquivo$intencao_espontanea[!(arquivo$intencao_espontanea %in% candidatos)]='Outros'
    
    arquivo$espontanea_validos = arquivo$intencao_espontanea
    arquivo$espontanea_validos[arquivo$espontanea_validos == "NS/NR*"] = NA
    arquivo$espontanea_validos[arquivo$espontanea_validos == "Branco e Nulo"] = NA
  }
  
  # Reagrega os nanicos nas votações estimuladas e cria a coluna de votos apenas válidos
  if ("intencao_estimulada" %in% names(arquivo)) {
    arquivo$intencao_estimulada <- as.character(arquivo$intencao_estimulada)
    arquivo$intencao_estimulada[arquivo$intencao_estimulada =="Não sabe/ Não respondeu"]='NS/NR*'
    arquivo$intencao_estimulada[arquivo$intencao_estimulada =="Não respondeu"]='NS/NR*'
    arquivo$intencao_estimulada[arquivo$intencao_estimulada =="Não sabe"]='NS/NR*'
    arquivo$intencao_estimulada[arquivo$intencao_estimulada =="Branco/ Nulo"]='Branco e Nulo'
    arquivo$intencao_estimulada[!(arquivo$intencao_estimulada %in% candidatos)]='Outros'
    
    arquivo$estimulada_validos = arquivo$intencao_estimulada
    arquivo$estimulada_validos[arquivo$estimulada_validos == "NS/NR*"] = NA
    arquivo$estimulada_validos[arquivo$estimulada_validos == "Branco e Nulo"] = NA
  }
  
  # Reagrega os nanicos no favoritismo
  if ("favorito" %in% names(arquivo)) {
    candidatos = c('Aécio Neves','Dilma Rousseff','Eduardo Campos','Marina Silva','NS/NR*')
    arquivo$favorito <- as.character(arquivo$favorito)
    arquivo$favorito[arquivo$favorito =="Não sabe/ Não respondeu"]='NS/NR*'
    arquivo$favorito[!(arquivo$favorito %in% candidatos)]='Outros'
  }
  
  # Muda o nome do 2turno_aecio e cria coluna de válidos
  if ("2turno_aecio" %in% names(arquivo)) {
    arquivo[["2turno_aecio"]] = as.character(arquivo[["2turno_aecio"]])
    arquivo[["2turno_aecio"]][arquivo[["2turno_aecio"]] =="Não sabe/ Não respondeu"]='NS/NR*'
    arquivo[["2turno_aecio"]][arquivo[["2turno_aecio"]] =="Não sabe"]='NS/NR*'
    arquivo[["2turno_aecio"]][arquivo[["2turno_aecio"]] =="Não respondeu"]='NS/NR*'
    arquivo[["2turno_aecio"]][arquivo[["2turno_aecio"]] =="Branco/ Nulo"]='Branco e Nulo'  
    
    arquivo[["2aecio_validos"]] = arquivo[["2turno_aecio"]]
    arquivo[["2aecio_validos"]][arquivo[["2aecio_validos"]] == "NS/NR*"] = NA
    arquivo[["2aecio_validos"]][arquivo[["2aecio_validos"]] == "Branco e Nulo"] = NA
  }
  
  # Muda o nome do 2turno_campos
  if ("2turno_campos" %in% names(arquivo)) {
    arquivo[["2turno_campos"]] <- as.character(arquivo[["2turno_campos"]])
    arquivo[["2turno_campos"]][arquivo[["2turno_campos"]] =="Não sabe/ Não respondeu"]='NS/NR*'
    arquivo[["2turno_campos"]][arquivo[["2turno_campos"]] =="Branco/ Nulo"]='Branco e Nulo'
  }
  
  # Muda o nome do 2turno_marina e cria coluna de válidos
  if ("2turno_marina" %in% names(arquivo)) {
    arquivo[["2turno_marina"]] <- as.character(arquivo[["2turno_marina"]])
    arquivo[["2turno_marina"]][arquivo[["2turno_marina"]] =="Não sabe/ Não respondeu"]='NS/NR*'
    arquivo[["2turno_marina"]][arquivo[["2turno_marina"]] =="Não sabe"]='NS/NR*'
    arquivo[["2turno_marina"]][arquivo[["2turno_marina"]] =="Não respondeu"]='NS/NR*'
    arquivo[["2turno_marina"]][arquivo[["2turno_marina"]] =="Branco/ Nulo"]='Branco e Nulo'
    
    arquivo[["2marina_validos"]] = arquivo[["2turno_marina"]]
    arquivo[["2marina_validos"]][arquivo[["2marina_validos"]] == "NS/NR*"] = NA
    arquivo[["2marina_validos"]][arquivo[["2marina_validos"]] == "Branco e Nulo"] = NA
  }
  
  # Muda o nome do avalia_dilma
  if ("aprova_dilma" %in% names(arquivo)) {
    arquivo$aprova_dilma <- as.character(arquivo$aprova_dilma)
    arquivo$aprova_dilma[arquivo$aprova_dilma =="Não sabe/ Não respondeu"]='NS/NR*'
    arquivo$aprova_dilma[arquivo$aprova_dilma =="Não sabe"]='NS/NR*'
    arquivo$aprova_dilma[arquivo$aprova_dilma =="Não respondeu"]='NS/NR*'
    
  }
  # Tira / do Norte/Centro-Oeste
  if ("regiao" %in% names(arquivo)) {
    arquivo$regiao <- as.character(arquivo$regiao)
    arquivo$regiao[arquivo$regiao =="NORTE/ CENTRO OESTE"]='NORTE-CENTRO-OESTE'
  }
  
  #transforma as notas em inteiros e cria coluna para nota_recorte
  if ("nota" %in% names(arquivo)) {
    arquivo$nota = as.integer(gsub("^Nota ","",arquivo$nota))
    arquivo = within(arquivo, { nota_recorte = ifelse(nota < 4, "3 ou menos", ifelse(nota <7,"Entre 4 e 6","7 ou mais")) } )
      
  }
  
  # Reagrega as variáveis de melhora ou piora
  if ("poder_compra" %in% names(arquivo)) {
    arquivo[arquivo$poder_compra == "Melhorou muito",][["poder_compra"]] = "Melhorou"
    arquivo[arquivo$poder_compra == "Melhorou um pouco",][["poder_compra"]] = "Melhorou"
    arquivo[arquivo$poder_compra == "Ficou igual",][["poder_compra"]] = "Igual"
    arquivo[arquivo$poder_compra == "Piorou um pouco",][["poder_compra"]] = "Piorou"
    arquivo[arquivo$poder_compra == "Piorou muito",][["poder_compra"]] = "Piorou"
    arquivo[arquivo$poder_compra == "Não sabe/ Não respondeu",][["poder_compra"]] = "NS/NR*"
  }
  if ("saude" %in% names(arquivo)) {
    arquivo[arquivo$saude == "Melhorou muito",][["saude"]] = "Melhorou"
    arquivo[arquivo$saude == "Melhorou um pouco",][["saude"]] = "Melhorou"
    arquivo[arquivo$saude == "Ficou igual",][["saude"]] = "Igual"
    arquivo[arquivo$saude == "Piorou um pouco",][["saude"]] = "Piorou"
    arquivo[arquivo$saude == "Piorou muito",][["saude"]] = "Piorou"
    arquivo[arquivo$saude == "Não sabe/ Não respondeu",][["saude"]] = "NS/NR*"
  }
  if ("emprego" %in% names(arquivo)) {
    arquivo[arquivo$emprego == "Melhorou muito",][["emprego"]] = "Melhorou"
    arquivo[arquivo$emprego == "Melhorou um pouco",][["emprego"]] = "Melhorou"
    arquivo[arquivo$emprego == "Ficou igual",][["emprego"]] = "Igual"
    arquivo[arquivo$emprego == "Piorou um pouco",][["emprego"]] = "Piorou"
    arquivo[arquivo$emprego == "Piorou muito",][["emprego"]] = "Piorou"
    arquivo[arquivo$emprego == "Não sabe/ Não respondeu",][["emprego"]] = "NS/NR*"
  }
  if ("educacao" %in% names(arquivo)) {
    arquivo[arquivo$educacao == "Melhorou muito",][["educacao"]] = "Melhorou"
    arquivo[arquivo$educacao == "Melhorou um pouco",][["educacao"]] = "Melhorou"
    arquivo[arquivo$educacao == "Ficou igual",][["educacao"]] = "Igual"
    arquivo[arquivo$educacao == "Piorou um pouco",][["educacao"]] = "Piorou"
    arquivo[arquivo$educacao == "Piorou muito",][["educacao"]] = "Piorou"
    arquivo[arquivo$educacao == "Não sabe/ Não respondeu",][["educacao"]] = "NS/NR*"
  }
  
  #Partido favorito
  if ("partido" %in% names(arquivo)) {
    arquivo[arquivo$partido == "Nenhum/ Não tem preferência",][["partido"]] = "Nenhum"
  }
  
  #Tem bolsa
  if ("bolsa1" %in% names(arquivo) & "bolsa2" %in% names(arquivo)) {
    arquivo = within(arquivo, {bolsa = ifelse(bolsa1 == "Não participa de nenhum" & bolsa2 == "Ninguém da sua família", "Não","Sim")})    
  }  
  
  #ringue
  if ("aecio" %in% names(arquivo)) {
    arquivo[arquivo$aecio == "Com certeza votaria nele para Presidente da República",][["aecio"]] = "Com certeza votaria nela para Presidente da República"
    arquivo[arquivo$aecio == "Poderia votar nele para Presidente da República",][["aecio"]] = "Poderia votar nela para Presidente da República"
    arquivo[arquivo$aecio == "Não votaria nele de jeito nenhum para Presidente da República",][["aecio"]] = "Não votaria nela de jeito nenhum para Presidente da República"
    arquivo[arquivo$aecio == "Não o conhece o suficiente para opinar",][["aecio"]] = "Não sabe/Não conhece"
    arquivo[arquivo$aecio == "Não sabe/ Não respondeu",][["aecio"]] = "Não sabe/Não conhece"
  }
  
  if ("dilma" %in% names(arquivo)) {
    arquivo[arquivo$dilma == "Não a conhece o suficiente para opinar",][["dilma"]] = "Não sabe/Não conhece"
    arquivo[arquivo$dilma == "Não sabe/ Não respondeu",][["dilma"]] = "Não sabe/Não conhece"
  }
  
  if ("marina" %in% names(arquivo)) {
    arquivo[arquivo$marina == "Não a conhece o suficiente para opinar",][["marina"]] = "Não sabe/Não conhece"
    arquivo[arquivo$marina == "Não sabe/ Não respondeu",][["marina"]] = "Não sabe/Não conhece"
  }
  return(arquivo)
}

#função normalizadora auxiliar para ser usada no processo calcula_tudo()
norm2 = function (final,p,r) {
  saida = data.frame(data=character(0),cat_pergunta=character(0),dado=character(0),cat_recorte=character(0),recorte=character(0),valor=numeric(0))
  for (i in names(final)) {
    for (j in row.names(final)) {
      nova_linha = data.frame(data="",cat_pergunta=p,dado=j,cat_recorte=r,recorte=i,valor=final[j,i])
      saida = rbind(saida,nova_linha)
    }
  }
  return(saida)
}

#função para criar aquivo a partir do .sav do Ibope
#ATENÇÃO: deve ser criado um vector - c() - com o nome das perguntas do .sav (só as p1,p2,etc, sem as variáveis já com nome) e outro com a tradução deles
#O nome do arquivo resultado dessa função deve ser o mês ou algum outro indicador da pesquisa - isso será necessário para a função calcula_tudo
cria_arquivo = function(arquivo,perg,trad) {
  data <- as.data.frame(as.data.set(spss.system.file(arquivo)))
  
  #retira fatores e coloca tudo em minúscula
  data <- data.frame(lapply(data, as.character), stringsAsFactors=FALSE)
  names(data) = tolower(names(data))
  
  #arruma nome da Dilma se for o caso
  data[data == "Dilma"] = "Dilma Rousseff"

  #arruma nome das colunas
  names(data) = tolower(names(data))
  data = reagrega_nomes(data)
  perg_rejeicao = "não há"
  
  #cria rejeicao
  if ("rejeicao" %in% trad) {
    #acha o index da rejeição na tradução e pega qual é a pergunta correspondente no arquivo
    i = grep("rejeicao",trad)
    perg_rejeicao = perg[i]
    data = cria_rejeicao(data,perg_rejeicao)  
    pergs = append(c("sexo", "escolaridade","renda","idad2","cor","religiao","cond","reg","rejeicaoDilma","rejeicaoAecio","rejeicaoMarina","rejeicaoPastor","rejeicaoCampos"),perg)
    trads = append(c("sexo","escolaridade","renda_familiar","idade","cor","religiao","condicao_municipio","regiao","rejeicaoDilma","rejeicaoAecio","rejeicaoMarina","rejeicaoPastor","rejeicaoCampos"),trad)
  } else {
    pergs = append(c("sexo", "escolaridade","cor","renda","idad2","religiao","cond","reg"),perg)
    trads = append(c("sexo","escolaridade","cor","renda_familiar","idade","religiao","condicao_municipio","regiao"),trad)
  }
  
  #cria bolsa família
  if ("bolsa1" %in% trad) {
    j = grep("bolsa1",trad)
    perg_bolsa = perg[j]
    perg_bolsa = substr(perg_bolsa,1,3)
    data = cria_bolsa(data,perg_bolsa)
    pergs = append(pergs,"bolsa_familia")
    trads = append(trads,"bolsa_familia")    
  }
  
  #retira as perguntas desnecessárias e coloca o nome certo
  pergs = pergs[pergs != perg_rejeicao]
  trads = trads[trads != "rejeicao"]
  saida = data[,pergs]
  names(saida) = trads
  saida = reagrega_perguntas(saida)
  
  #cria variável para cálculo da rejeição
  if ("rejeicao" %in% trad) {
    saida$rejeicao = TRUE
  } else {
    saida$rejeicao = FALSE
  }
  
  #retira a rejeição que não existe (Campos ou Marina)
  if ("Marina Silva" %in% unique(saida$intencao_estimulada)) {
    saida$rejeicaoCampos = NULL
  } else if ("Eduardo Campos" %in% unique(saida$intencao_estimulada)) {
    saida$rejeicaoMarina = NULL
  }
  
  return(saida)
}

#função para ver se há alguma bolsa ou Bolsa família
cria_bolsa = function(arquivo, perg_bolsa) {
  arquivo = within(arquivo, {
    bolsa_familia = ifelse(eval(parse(text=paste(perg_bolsa,"a01",sep=""))) == "Bolsa Família" |
                             eval(parse(text=paste(perg_bolsa,"a02",sep=""))) == "Bolsa Família" |
                             eval(parse(text=paste(perg_bolsa,"a03",sep=""))) == "Bolsa Família" |
                             eval(parse(text=paste(perg_bolsa,"a04",sep=""))) == "Bolsa Família" |
                             eval(parse(text=paste(perg_bolsa,"a05",sep=""))) == "Bolsa Família" |
                             eval(parse(text=paste(perg_bolsa,"a06",sep=""))) == "Bolsa Família" |
                             eval(parse(text=paste(perg_bolsa,"a07",sep=""))) == "Bolsa Família" |
                             eval(parse(text=paste(perg_bolsa,"a08",sep=""))) == "Bolsa Família" |
                             eval(parse(text=paste(perg_bolsa,"a09",sep=""))) == "Bolsa Família" |
                            eval(parse(text=paste(perg_bolsa,"b01",sep=""))) == "Bolsa Família" |
                            eval(parse(text=paste(perg_bolsa,"b02",sep=""))) == "Bolsa Família" |
                            eval(parse(text=paste(perg_bolsa,"b03",sep=""))) == "Bolsa Família" |
                            eval(parse(text=paste(perg_bolsa,"b04",sep=""))) == "Bolsa Família" |
                            eval(parse(text=paste(perg_bolsa,"b05",sep=""))) == "Bolsa Família" |
                            eval(parse(text=paste(perg_bolsa,"b06",sep=""))) == "Bolsa Família" |
                            eval(parse(text=paste(perg_bolsa,"b07",sep=""))) == "Bolsa Família" |
                            eval(parse(text=paste(perg_bolsa,"b08",sep=""))) == "Bolsa Família" |
                            eval(parse(text=paste(perg_bolsa,"b09",sep=""))) == "Bolsa Família" |
                            eval(parse(text=paste(perg_bolsa,"b10",sep=""))) == "Bolsa Família","Sim", "Não")})
  
  
  return(arquivo)
}

#função para calcular a rejeição a cada um dos 4 principais candidatos
cria_rejeicao = function(arquivo,perg_rejeicao) {
  arquivo = within(arquivo, {
    rejeicaoDilma = ifelse(eval(parse(text=paste(perg_rejeicao,"01",sep=""))) == "Dilma Rousseff" |
                             eval(parse(text=paste(perg_rejeicao,"02",sep=""))) == "Dilma Rousseff" |
                             eval(parse(text=paste(perg_rejeicao,"03",sep=""))) == "Dilma Rousseff" |
                             eval(parse(text=paste(perg_rejeicao,"04",sep=""))) == "Dilma Rousseff" |
                             eval(parse(text=paste(perg_rejeicao,"05",sep=""))) == "Dilma Rousseff" |
                             eval(parse(text=paste(perg_rejeicao,"06",sep=""))) == "Dilma Rousseff" |
                             eval(parse(text=paste(perg_rejeicao,"07",sep=""))) == "Dilma Rousseff" |
                             eval(parse(text=paste(perg_rejeicao,"08",sep=""))) == "Dilma Rousseff" |
                             eval(parse(text=paste(perg_rejeicao,"09",sep=""))) == "Dilma Rousseff" |
                             eval(parse(text=paste(perg_rejeicao,"10",sep=""))) == "Dilma Rousseff" |
                             eval(parse(text=paste(perg_rejeicao,"11",sep=""))) == "Dilma Rousseff", "Sim", "Não")
    rejeicaoAecio = ifelse(eval(parse(text=paste(perg_rejeicao,"01",sep=""))) == "Aécio Neves" |
                             eval(parse(text=paste(perg_rejeicao,"02",sep=""))) == "Aécio Neves" |
                             eval(parse(text=paste(perg_rejeicao,"03",sep=""))) == "Aécio Neves" |
                             eval(parse(text=paste(perg_rejeicao,"04",sep=""))) == "Aécio Neves" |
                             eval(parse(text=paste(perg_rejeicao,"05",sep=""))) == "Aécio Neves" |
                             eval(parse(text=paste(perg_rejeicao,"06",sep=""))) == "Aécio Neves" |
                             eval(parse(text=paste(perg_rejeicao,"07",sep=""))) == "Aécio Neves" |
                             eval(parse(text=paste(perg_rejeicao,"08",sep=""))) == "Aécio Neves" |
                             eval(parse(text=paste(perg_rejeicao,"09",sep=""))) == "Aécio Neves" |
                             eval(parse(text=paste(perg_rejeicao,"10",sep=""))) == "Aécio Neves" |
                             eval(parse(text=paste(perg_rejeicao,"11",sep=""))) == "Aécio Neves", "Sim","Não")
    rejeicaoPastor = ifelse(eval(parse(text=paste(perg_rejeicao,"01",sep=""))) == "Pastor Everaldo" |
                              eval(parse(text=paste(perg_rejeicao,"02",sep=""))) == "Pastor Everaldo" |
                              eval(parse(text=paste(perg_rejeicao,"03",sep=""))) == "Pastor Everaldo" |
                              eval(parse(text=paste(perg_rejeicao,"04",sep=""))) == "Pastor Everaldo" |
                              eval(parse(text=paste(perg_rejeicao,"05",sep=""))) == "Pastor Everaldo" |
                              eval(parse(text=paste(perg_rejeicao,"06",sep=""))) == "Pastor Everaldo" |
                              eval(parse(text=paste(perg_rejeicao,"07",sep=""))) == "Pastor Everaldo" |
                              eval(parse(text=paste(perg_rejeicao,"08",sep=""))) == "Pastor Everaldo" |
                              eval(parse(text=paste(perg_rejeicao,"09",sep=""))) == "Pastor Everaldo" |
                              eval(parse(text=paste(perg_rejeicao,"10",sep=""))) == "Pastor Everaldo" |
                              eval(parse(text=paste(perg_rejeicao,"11",sep=""))) == "Pastor Everaldo", "Sim","Não")
    rejeicaoCampos = ifelse(eval(parse(text=paste(perg_rejeicao,"01",sep=""))) == "Eduardo Campos" |
                              eval(parse(text=paste(perg_rejeicao,"02",sep=""))) == "Eduardo Campos" |
                              eval(parse(text=paste(perg_rejeicao,"03",sep=""))) == "Eduardo Campos" |
                              eval(parse(text=paste(perg_rejeicao,"04",sep=""))) == "Eduardo Campos" |
                              eval(parse(text=paste(perg_rejeicao,"05",sep=""))) == "Eduardo Campos" |
                              eval(parse(text=paste(perg_rejeicao,"06",sep=""))) == "Eduardo Campos" |
                              eval(parse(text=paste(perg_rejeicao,"07",sep=""))) == "Eduardo Campos" |
                              eval(parse(text=paste(perg_rejeicao,"08",sep=""))) == "Eduardo Campos" |
                              eval(parse(text=paste(perg_rejeicao,"09",sep=""))) == "Eduardo Campos" |
                              eval(parse(text=paste(perg_rejeicao,"10",sep=""))) == "Eduardo Campos" |
                              eval(parse(text=paste(perg_rejeicao,"11",sep=""))) == "Eduardo Campos", "Sim","Não")
  rejeicaoMarina = ifelse(eval(parse(text=paste(perg_rejeicao,"01",sep=""))) == "Marina Silva" |
                            eval(parse(text=paste(perg_rejeicao,"02",sep=""))) == "Marina Silva" |
                            eval(parse(text=paste(perg_rejeicao,"03",sep=""))) == "Marina Silva" |
                            eval(parse(text=paste(perg_rejeicao,"04",sep=""))) == "Marina Silva" |
                            eval(parse(text=paste(perg_rejeicao,"05",sep=""))) == "Marina Silva" |
                            eval(parse(text=paste(perg_rejeicao,"06",sep=""))) == "Marina Silva" |
                            eval(parse(text=paste(perg_rejeicao,"07",sep=""))) == "Marina Silva" |
                            eval(parse(text=paste(perg_rejeicao,"08",sep=""))) == "Marina Silva" |
                            eval(parse(text=paste(perg_rejeicao,"09",sep=""))) == "Marina Silva" |
                            eval(parse(text=paste(perg_rejeicao,"10",sep=""))) == "Marina Silva" |
                            eval(parse(text=paste(perg_rejeicao,"11",sep=""))) == "Marina Silva", "Sim","Não")})
  
  arquivo[is.na(arquivo)] = "Não"
  return(arquivo)
}

#funcao para calcular a rejeicao para colocar no dataframe derretido final
calcula_rejeicao = function (arquivo,recorte) {
  #calcula a rejeicao para cada um dos 4 candidatos
  temp = round(normaliza(cruza_respostas(arquivo,"rejeicaoDilma",recorte)),0)
  rownames(temp)[2] = "Dilma Rousseff"
  saida = temp
  temp = round(normaliza(cruza_respostas(arquivo,"rejeicaoAecio",recorte)),0)
  rownames(temp)[2] = "Aécio Neves"
  saida = rbind(saida,temp)
  temp = round(normaliza(cruza_respostas(arquivo,"rejeicaoPastor",recorte)),0)
  rownames(temp)[2] = "Pastor Everaldo"
  saida = rbind(saida,temp)
  #if para descobrir se há rejeição do Campos ou da Marina
  if ("rejeicaoCampos" %in% names(arquivo)) {
    temp = round(normaliza(cruza_respostas(arquivo,"rejeicaoCampos",recorte)),0)
    rownames(temp)[2] = "Eduardo Campos"
    saida = rbind(saida,temp)    
  } 
  if ("rejeicaoMarina" %in% names(arquivo)) {
    temp = round(normaliza(cruza_respostas(arquivo,"rejeicaoMarina",recorte)),0)
    rownames(temp)[2] = "Marina Silva"
    saida = rbind(saida,temp)    
  }  
  
  #retira as linhas que não dizem nada
  deixar = c("Dilma Rousseff","Aécio Neves","Pastor Everaldo","Eduardo Campos","Marina Silva")
  saida$fake = 1
  saida = saida[rownames(saida) %in% deixar,]
  saida$fake = NULL
  return(saida)
}

#cruza respostas e normaliza
cruza <- function (arquivo,perg1,perg2) {
  return(round(normaliza(cruza_respostas(arquivo,perg1,perg2)),0))
}

#analisa uma amostra e dá o perfil do entrevistado
analise_amostra <- function (arquivo) {
 # recortes = c("sexo","idade","renda_familiar","escolaridade","regiao","condicao_municipio","religiao","cor","rejeicaoDilma","rejeicaoAecio","2turno_aecio",
#               "favorito","avaliacao_governo","aprova_dilma","partido","vida_hoje","desejo_mudanca","interesse")
  recortes = c("idade","renda_familiar","escolaridade","regiao","condicao_municipio","religiao","cor","interesse","vida_hoje","avaliacao_governo","desejo_mudanca","2turno_aecio","sexo","intencao_espontanea","bolsa1","bolsa2")
  saida = list()
  
  #calcula os recortes tradicionais
  for (r in recortes) {
    if (r %in% names(arquivo)) {
      d = round(normaliza(uma_pergunta(arquivo,r)),0)
      names(d) = r
      saida[[r]] = d
    }
  }  
  
  #calcula as médias
  idade = saida$idade["16 a 24",] * 20 + saida$idade["25 a 34",] * 29.5 + saida$idade["35 a 44",] * 39.5 + saida$idade["45 a 54",] * 49.5 + saida$idade["55 ou mais",] *60
  idade_media = as.data.frame(round(idade/100,1))
  names(idade_media) = "idade_media"
  row.names(idade_media) = "idade_media"
  saida$idade_media = idade_media
  renda = saida$renda_familiar["Ate 1",] * 362 + saida$renda_familiar["1 a 2",] * 1086 + saida$renda_familiar["2 a 5",] * 2534 + saida$renda_familiar["Mais de 5",] * 4334
  renda_media = as.data.frame(round(renda/100,1))
  names(renda_media) = "renda_media"
  row.names(renda_media) = "renda_media"
  saida$renda_media = renda_media
  anos_estudo = saida$escolaridade["Fundamental 1",]*5 + saida$escolaridade["Fundamental 2",]*9 + saida$escolaridade["Medio",]*13 + saida$escolaridade["Superior",]*17
  anos_estudo = as.data.frame(round(anos_estudo/100,1))
  names(anos_estudo) = "anos_estudo"
  row.names(anos_estudo) = "anos_estudo"
  saida$anos_estudo = anos_estudo
  
  #retira itens que não precisamos
  if ("avaliacao_governo" %in% names(saida)) {
    aval = as.data.frame(saida$avaliacao_governo[!(rownames(saida$avaliacao_governo) == "NS/NR*"),,drop=FALSE])
    saida$avaliacao_governo = aval
  }
  if ("partido" %in% names(saida)) {
    nomes_linha = c("PT","PSDB","Nenhum")
    partido = as.data.frame(saida$partido[(rownames(saida$partido) %in% nomes_linha),,drop=FALSE])
    saida$partido = partido 
  }
  return(saida)
}

#analise comparativa de dois recortes
analise_comparativa_dois_recortes = function(recorte01, recorte02) {
  library(ggplot2)
  library(reshape2)

  for (perg in names(recorte01)) {
    d1 = recorte01[[perg]]
    d2 = recorte02[[perg]]
    names(d1) = paste(names(recorte01[[perg]]),deparse(substitute(abril)),sep='_')
    names(d2) = paste(names(recorte02[[perg]]),deparse(substitute(maio)),sep='_')
    imprimir = cbind(d1,d2)
    print(imprimir)
    tab = t(as.matrix(imprimir))
    barX = barplot(tab, beside=TRUE, axis.lty=1, col=colors(2))
    text(cex=.5,x=barX,y=tab+par("cxy")[2]/2, round(tab,2), xpd=TRUE)
    plot.new()
    legend("center", "groups", colnames(imprimir), fill=colors(2))
  }
}

#analise comparativa de vários recortes
analise_comparativa_lista_recortes = function(lista_de_recortes) {
  #Assume-se que todos os recortes possuem a mesma quantidade de 'itens de perfil'
  library(ggplot2)
  library(reshape2)
  quantidade_recortes = length(lista_de_recortes)
  variaveis = names(lista_de_recortes[[1]])

  for (perg in variaveis) {
    lista = list()
    i = 1
    for (recorte in names(lista_de_recortes)) {
      df = lista_de_recortes[[recorte]]
      d1 = df[[perg]]
      names(d1) = paste(perg,recorte,sep='_')
      lista[[i]] = d1
      i = i + 1
    }
    imprimir = lista[[1]]
    for (df in lista) {
      if (names(df) != names(imprimir)) {
        imprimir = cbind(imprimir,df)
      }
    }
    print(imprimir)
    tab = t(as.matrix(imprimir))
    barX = barplot(tab, beside=TRUE, axis.lty=1, col=colors(7))
    text(cex=.5,x=barX,y=tab+par("cxy")[2]/2, round(tab,2), xpd=TRUE)
    plot.new()
    legend("center", "groups", colnames(imprimir), fill=colors(7))
  }
}

perfil_candidatos = function(total) {
  library(plyr)
  dilma = total[total$intencao_estimulada == "Dilma Rousseff",]
  marina = total[total$intencao_estimulada == "Marina Silva",]  
  aecio = total[total$intencao_estimulada == "Aécio Neves",]
  pastor = total[total$intencao_estimulada == "Pastor Everaldo",]
  outros = total[total$intencao_estimulada == "Outros",]
  branco = total[total$intencao_estimulada == "Branco e Nulo",]
  indeciso = total[total$intencao_estimulada == "NS/NR*",]
  
  a = analise_amostra(total)
  b = analise_amostra(dilma)
  c = analise_amostra(aecio)
  d = analise_amostra(marina)
  e = analise_amostra(pastor)
  f = analise_amostra(outros)
  g = analise_amostra(indeciso)
  i = analise_amostra(branco)
  h = list()
  h$total = a
  h$dilma = b
  h$aecio = c
  h$marina = d
  h$pastor = e
  h$outros = f
  h$indeciso = g
  h$branco = i
  
  saida = data.frame(data=character(0),cat_pergunta=character(0),dado=character(0),cat_recorte=character(0),recorte=character(0),valor=numeric(0))
  for (i in names(h)) {  
    for (j in h[[i]]) {
      saida = rbind(saida,norm2(j,i,i))
    }
  }
  
  #arruma arquivo de saída
  names(saida) = c("data","cat_recorte","dado","recorte","cat_pergunta","valor")
  saida = arruma_avaliacao(saida)
  names(saida) = c("data","candidato","dado","cat_recorte","recorte","valor")
  saida$data = NULL
  saida$cat_recorte = NULL
  saida[saida == "SUDESTE"] = "Sudeste"
  saida[saida == "NORDESTE"] = "Nordeste"
  saida[saida == "NORTE-CENTRO-OESTE"] = "Norte/CO"
  saida[saida == "SUL"] = "Sul"
  saida[saida == "CAPITAL"] = "Capital"
  saida[saida == "INTERIOR"] = "Interior"
  saida[saida == "PERIFERIA"] = "Periferia"
  saida[saida == "Fundamental 1"] = "Fund. 1"
  saida[saida == "Fundamental 2"] = "Fund. 2"
  saida[saida == "Médio"] = "Medio"
  saida[saida == "MAS"] = "Homens"
  saida[saida == "FEM"] = "Mulheres"
  saida[saida == "NS/NR*"] = NA
  saida = na.omit(saida)
  return(saida)
}
