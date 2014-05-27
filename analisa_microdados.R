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
  recortes = c("sexo","idade","escolaridade","renda_familiar","condicao_municipio","regiao","raca","religiao","vida_hoje","interesse","desejo_mudanca","avaliacao_governo")
  perguntas = c("vida_hoje","interesse","intencao_espontanea","intencao_estimulada","avaliacao_governo","aprova_dilma","desejo_mudanca","rejeicao","2turno_aecio","2turno_campos")
  saida = data.frame(data=character(0),cat_dado=character(0),dado=character(0),cat_recorte=character(0),recorte=character(0),variavel=numeric(0))
  for (r in recortes) {
    if (r  %in% names(final)) {
      for (p in perguntas) {
        if (p %in% names(final)) {
          if (p != r) {
            if (p != "rejeicao") {
              temp = round(normaliza(cruza_respostas(final,p,r)),1)
              saida = rbind(saida,norm2(temp,p,r))  
            } else {
              if (final$rejeicao[1] == TRUE) {
                temp = calcula_rejeicao(final,r)
                saida = rbind(saida,norm2(temp,"rejeicao",r))  
              }
            }
          }
        }
      } 
    }
  }
  saida$data = deparse(substitute(final))
  return(saida)
}

#função que reagrega as variáveis de renda, escolaridade e idade de acordo com o padrão do Ibope
reagrega = function(arquivo) {
  # Reagrega a variável escolaridade
  arquivo$escolaridade=factor(NA,levels=c('Fundamental 1','Fundamental 2','Medio','Superior'))
  escolaridade=sort(unique(arquivo$inst))
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
  renda=sort(unique(arquivo$rend2))
  arquivo$renda[arquivo$rend2==renda[1]]='Mais de 5'
  arquivo$renda[arquivo$rend2==renda[2]]='Mais de 5'
  arquivo$renda[arquivo$rend2==renda[3]]='Mais de 5'
  arquivo$renda[arquivo$rend2==renda[4]]='2 a 5'
  arquivo$renda[arquivo$rend2==renda[5]]='1 a 2'
  arquivo$renda[arquivo$rend2==renda[6]]='Ate 1'
  arquivo$renda[arquivo$rend2==renda[7]]='Ate 1'
  # Reagrega a variável idade
  arquivo$idad2=factor(NA,levels=c('16 a 24','25 a 34','35 a 44','45 a 54','55 ou mais'))
  idad2=sort(unique(arquivo$idad))
  arquivo$idad2[arquivo$idad==idad2[1]]='16 a 24'
  arquivo$idad2[arquivo$idad==idad2[2]]='16 a 24'
  arquivo$idad2[arquivo$idad==idad2[3]]='25 a 34'
  arquivo$idad2[arquivo$idad==idad2[4]]='35 a 44'
  arquivo$idad2[arquivo$idad==idad2[5]]='45 a 54'
  arquivo$idad2[arquivo$idad==idad2[6]]='55 ou mais'
  arquivo$idad2[arquivo$idad==idad2[7]]='55 ou mais'
  # Reagrega a variável religiao
  arquivo$religiao=factor(NA,levels=c('Catolica','Evangelica','Outras'))
  religiao=sort(unique(arquivo$reli))
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
  
  return(arquivo)
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
#O último argumento é se há pergunta sobre rejeicao (boolean)
cria_arquivo = function(arquivo,perg,trad,rejeicao) {
  library("memisc", lib.loc="/Library/Frameworks/R.framework/Versions/3.0/Resources/library")
  data <- as.data.frame(as.data.set(spss.system.file(arquivo)))
  data = reagrega(data)
  if (rejeicao) {
    data = cria_rejeicao(data)  
    pergs = append(c("sexo", "escolaridade","renda","idad2","raca","religiao","cond","reg","rejeicaoDilma","rejeicaoAecio","rejeicaoPastor","rejeicaoCampos"),perg)
    trads = append(c("sexo","escolaridade","renda_familiar","idade","raca","religiao","condicao_municipio","regiao","rejeicaoDilma","rejeicaoAecio","rejeicaoPastor","rejeicaoCampos"),trad)
  } else {
    pergs = append(c("sexo", "escolaridade","raca","renda","idad2","religiao","cond","reg"),perg)
    trads = append(c("sexo","escolaridade","raca","renda_familiar","idade","religiao","condicao_municipio","regiao"),trad)
  }

  saida = data[,pergs]
  names(saida) = trads
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
                              eval(parse(text=paste(perg_rejeicao,"11",sep=""))) == "Eduardo Campos", "Sim","Não")})
  arquivo[is.na(arquivo)] = "Não"
  return(arquivo)
}

#funcao para calcular a rejeicao para colocar no dataframe derretido final
calcula_rejeicao = function (arquivo,recorte) {
  temp = round(normaliza(cruza_respostas(arquivo,"rejeicaoDilma",recorte)),1)
  rownames(temp)[2] = "Dilma Rousseff"
  temp = temp[-1,]
  saida = temp
  temp = round(normaliza(cruza_respostas(arquivo,"rejeicaoAecio",recorte)),1)
  rownames(temp)[2] = "Aécio Neves"
  temp = temp[-1,]
  saida = rbind(saida,temp)
    temp = round(normaliza(cruza_respostas(arquivo,"rejeicaoPastor",recorte)),1)
  rownames(temp)[2] = "Pastor Everaldo"
  temp = temp[-1,]
  saida = rbind(saida,temp)
  temp = round(normaliza(cruza_respostas(arquivo,"rejeicaoCampos",recorte)),1)
  rownames(temp)[2] = "Eduardo Campos"
  temp = temp[-1,]
  saida = rbind(saida,temp)
  return(saida)
}
