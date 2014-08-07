options(stringsAsFactors = FALSE) 

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

  if(pergunta == "desejo_mudanca" & pergunta2 =="cor") {
    print(final)
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
calcula_tudo = function (final,data_pesquisa) {
  final$total = "total"
  recortes = c("sexo","idade","escolaridade","renda_familiar","condicao_municipio","regiao","cor","religiao","vida_hoje","interesse","desejo_mudanca","avaliacao_governo","total","intencao_estimulada","favorito","nota_recorte")
  perguntas = c("vida_hoje","interesse","intencao_espontanea","intencao_estimulada","avaliacao_governo","aprova_dilma","desejo_mudanca","rejeicao","2turno_aecio","2turno_campos","favorito","nota")
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
  saida[saida$cat_recorte == "intencao_estimulada" & saida$recorte == "Pastor Everaldo",] = NA
  saida = na.omit(saida)
  if ("nota" %in% names(final)) {
    print(saida[saida$dado == "nota" & saida$cat_recorte == "nota_recorte",])
    saida[saida$dado == "nota" & saida$cat_recorte == "nota_recorte",] = NA    
  } 
  
  saida = na.omit(saida)
  return(saida)
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
  
  # Reagrega a variável avaliacao do governo (aval)
  if("avaliacao_governo" %in% names(arquivo)) {
  arquivo$temp=factor(NA,levels=c('Ótimo e bom','Regular','Ruim e péssimo','NS/NR*'))
  aval=c("Ótima",
         "Boa",
         "Regular",
         "Ruim",
         "Péssima")
  arquivo$temp[arquivo$avaliacao_governo==aval[1]]='Ótimo e bom'
  arquivo$temp[arquivo$avaliacao_governo==aval[2]]='Ótimo e bom'
  arquivo$temp[arquivo$avaliacao_governo==aval[3]]='Regular'
  arquivo$temp[arquivo$avaliacao_governo==aval[4]]='Ruim e péssimo'
  arquivo$temp[arquivo$avaliacao_governo==aval[5]]='Ruim e péssimo'
  arquivo$temp[arquivo$avaliacao_governo =="Não sabe/ Não respondeu"]='NS/NR*'
  arquivo$avaliacao_governo = NULL
  names(arquivo)[names(arquivo)=="temp"] = "avaliacao_governo"
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
    arquivo$temp[arquivo$interesse==interesse[1]]='Tem muito interesse'
    arquivo$temp[arquivo$interesse==interesse[2]]='Tem médio interesse'
    arquivo$temp[arquivo$interesse==interesse[3]]='Não tem interesse'
    arquivo$temp[arquivo$interesse==interesse[4]]='Não tem interesse'
    arquivo$temp[arquivo$interesse =="Não sabe/ Não respondeu"]='NS/NR*'
    arquivo$interesse = NULL
    names(arquivo)[names(arquivo)=="temp"] = "interesse"
  }
  
  # Reagrega os nanicos nas votações espontaneas
  if ("intencao_espontanea" %in% names(arquivo)) {
    candidatos = c('Aécio Neves','Dilma Rousseff','Eduardo Campos','Marina Silva','Lula','Pastor Everaldo','Branco e Nulo','NS/NR*','Outros')
    arquivo$intencao_espontanea <- as.character(arquivo$intencao_espontanea)
    arquivo$intencao_espontanea[arquivo$intencao_espontanea =="Não sabe/ Não respondeu"]='NS/NR*'
    arquivo$intencao_espontanea[arquivo$intencao_espontanea =="Branco/ Nulo"]='Branco e Nulo'
    arquivo$intencao_espontanea[!(arquivo$intencao_espontanea %in% candidatos)]='Outros'
  }
  
  # Reagrega os nanicos nas votações estimuladas
  if ("intencao_estimulada" %in% names(arquivo)) {
    arquivo$intencao_estimulada <- as.character(arquivo$intencao_estimulada)
    arquivo$intencao_estimulada[arquivo$intencao_estimulada =="Não sabe/ Não respondeu"]='NS/NR*'
    arquivo$intencao_estimulada[arquivo$intencao_estimulada =="Branco/ Nulo"]='Branco e Nulo'
    arquivo$intencao_estimulada[!(arquivo$intencao_estimulada %in% candidatos)]='Outros'
  }
  
  # Reagrega os nanicos no favoritismo
  if ("favorito" %in% names(arquivo)) {
    candidatos = c('Aécio Neves','Dilma Rousseff','Eduardo Campos','NS/NR*')
    arquivo$favorito <- as.character(arquivo$favorito)
    arquivo$favorito[arquivo$favorito =="Não sabe/ Não respondeu"]='NS/NR*'
    arquivo$favorito[!(arquivo$favorito %in% candidatos)]='Outros'
  }
  
  # Muda o nome do 2turno_aecio
  if ("2turno_aecio" %in% names(arquivo)) {
    arquivo[["2turno_aecio"]] = as.character(arquivo[["2turno_aecio"]])
    arquivo[["2turno_aecio"]][arquivo[["2turno_aecio"]] =="Não sabe/ Não respondeu"]='NS/NR*'
    arquivo[["2turno_aecio"]][arquivo[["2turno_aecio"]] =="Branco/ Nulo"]='Branco e Nulo'  }
  
  # Muda o nome do 2turno_campos
  if ("2turno_campos" %in% names(arquivo)) {
    arquivo[["2turno_campos"]] <- as.character(arquivo[["2turno_campos"]])
    arquivo[["2turno_campos"]][arquivo[["2turno_campos"]] =="Não sabe/ Não respondeu"]='NS/NR*'
    arquivo[["2turno_campos"]][arquivo[["2turno_campos"]] =="Branco/ Nulo"]='Branco e Nulo'
  }
  
  # Muda o nome do avalia_dilma
  if ("aprova_dilma" %in% names(arquivo)) {
    arquivo$aprova_dilma <- as.character(arquivo$aprova_dilma)
    arquivo$aprova_dilma[arquivo$aprova_dilma =="Não sabe/ Não respondeu"]='NS/NR*'
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
#O último argumento é se há pergunta sobre rejeicao (boolean)
cria_arquivo = function(arquivo,perg,trad,rejeicao) {
  library("memisc", lib.loc="/Library/Frameworks/R.framework/Versions/3.0/Resources/library")
  data <- as.data.frame(as.data.set(spss.system.file(arquivo)))
  
  #retira fatores
  data <- data.frame(lapply(data, as.character), stringsAsFactors=FALSE)
  
  #arruma nome da Dilma se for o caso
  data[data == "Dilma"] = "Dilma Rousseff"

  #arruma nome das colunas
  names(data) = tolower(names(data))
  data = reagrega_nomes(data)
  
  #cria rejeicao
  if (rejeicao) {
    data = cria_rejeicao(data)  
    pergs = append(c("sexo", "escolaridade","renda","idad2","cor","religiao","cond","reg","rejeicaoDilma","rejeicaoAecio","rejeicaoPastor","rejeicaoCampos"),perg)
    trads = append(c("sexo","escolaridade","renda_familiar","idade","cor","religiao","condicao_municipio","regiao","rejeicaoDilma","rejeicaoAecio","rejeicaoPastor","rejeicaoCampos"),trad)
  } else {
    pergs = append(c("sexo", "escolaridade","cor","renda","idad2","religiao","cond","reg"),perg)
    trads = append(c("sexo","escolaridade","cor","renda_familiar","idade","religiao","condicao_municipio","regiao"),trad)
  }
  
  #retira as perguntas desnecessárias e coloca o nome certo
  saida = data[,pergs]
  names(saida) = trads
  saida = reagrega_perguntas(saida)
  saida$rejeicao = rejeicao
  
  return(saida)
}

#função para calcular a rejeição a cada um dos 4 principais candidatos
cria_rejeicao = function(arquivo) {
  #acha a pergunta da rejeição
  for (n in names(arquivo)) { 
    nomes = names(arquivo)
    nomes = nomes [! nomes %in% c(n)]
    comeco = substring(n,0,nchar(n)-2)
    if (paste(comeco,"02",sep="") %in% nomes &
          paste(comeco,"03",sep="") %in% nomes &
          paste(comeco,"04",sep="") %in% nomes &
          paste(comeco,"05",sep="") %in% nomes &
          paste(comeco,"06",sep="") %in% nomes &
          paste(comeco,"07",sep="") %in% nomes &
          paste(comeco,"08",sep="") %in% nomes &
          paste(comeco,"09",sep="") %in% nomes &
          paste(comeco,"10",sep="") %in% nomes &
          paste(comeco,"11",sep="") %in% nomes) {
            perg_rejeicao = comeco
      }
  }

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
                             eval(parse(text=paste(perg_rejeicao,"11",sep=""))) == "Dilma", "Sim", "Não")
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
                              eval(parse(text=paste(perg_rejeicao,"11",sep=""))) == "Eduardo Campos", "Sim","Não")})
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
  temp = round(normaliza(cruza_respostas(arquivo,"rejeicaoCampos",recorte)),0)
  rownames(temp)[2] = "Eduardo Campos"
  saida = rbind(saida,temp)
  
  #retira as linhas que não dizem nada
  deixar = c("Dilma Rousseff","Aécio Neves","Pastor Everaldo","Eduardo Campos")
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
  recortes = c("sexo","idade","renda_familiar","escolaridade","regiao","condicao_municipio","religiao","cor")
  saida = list()
  
  #calcula os recortes tradicionais
  for (r in recortes) {
    if (r %in% names(arquivo)) {
      d = round(normaliza(uma_pergunta(arquivo,r)),1)
      names(d) = r
      saida[[r]] = d
    }
  }  
  
  #calcula as médias
  idade = saida$idade["16 a 24",] * 20 + saida$idade["25 a 34",] * 29.5 + saida$idade["35 a 44",] * 39.5 + saida$idade["45 a 54",] * 49.5 + saida$idade["55 ou mais",] *60
  saida$idade_media = round(idade/100,1)
  renda = saida$renda_familiar["Ate 1",] * 362 + saida$renda_familiar["1 a 2",] * 1086 + saida$renda_familiar["2 a 5",] * 2534 + saida$renda_familiar["Mais de 5",] * 4334
  saida$renda_media = round(renda/100,1)
  anos_estudo = saida$escolaridade["Fundamental 1",]*5 + saida$escolaridade["Fundamental 2",]*9 + saida$escolaridade["Medio",]*13 + saida$escolaridade["Superior",]*17
  saida$anos_estudo = round(anos_estudo/100,1)
  return(saida)
}