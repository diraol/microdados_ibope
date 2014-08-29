import json
import os
import csv
from pandas import DataFrame, read_csv


path = os.path.dirname(os.path.abspath(__file__))

saida = {}
base = {
            "Escolaridade": {
                "Fundamental": ["Com certeza votaria nela para Presidente da República","Poderia votar nela para Presidente da República","Não votaria nela de jeito nenhum para Presidente da República","Não sabe/Não conhece"],

                "5ª a 8ª Série": ["Com certeza votaria nela para Presidente da República","Poderia votar nela para Presidente da República","Não votaria nela de jeito nenhum para Presidente da República","Não sabe/Não conhece"],

                "Ensino Médio": ["Com certeza votaria nela para Presidente da República","Poderia votar nela para Presidente da República","Não votaria nela de jeito nenhum para Presidente da República","Não sabe/Não conhece"],

                "Ensino Superior":["Com certeza votaria nela para Presidente da República","Poderia votar nela para Presidente da República","Não votaria nela de jeito nenhum para Presidente da República","Não sabe/Não conhece"] },

            "Região": {
                "Brasil": ["Com certeza votaria nela para Presidente da República","Poderia votar nela para Presidente da República","Não votaria nela de jeito nenhum para Presidente da República","Não sabe/Não conhece"],

                "Nordeste": ["Com certeza votaria nela para Presidente da República","Poderia votar nela para Presidente da República","Não votaria nela de jeito nenhum para Presidente da República","Não sabe/Não conhece"],

                "Norte e Centro-Oeste": ["Com certeza votaria nela para Presidente da República","Poderia votar nela para Presidente da República","Não votaria nela de jeito nenhum para Presidente da República","Não sabe/Não conhece"],

                "Sudeste": ["Com certeza votaria nela para Presidente da República","Poderia votar nela para Presidente da República","Não votaria nela de jeito nenhum para Presidente da República","Não sabe/Não conhece"],

                "Sul":["Com certeza votaria nela para Presidente da República","Poderia votar nela para Presidente da República","Não votaria nela de jeito nenhum para Presidente da República","Não sabe/Não conhece"] },

            "Porte do Município (Habitantes)": {
                "Cidades com até 20 mil": ["Com certeza votaria nela para Presidente da República","Poderia votar nela para Presidente da República","Não votaria nela de jeito nenhum para Presidente da República","Não sabe/Não conhece"],

                "Cidades de 20 a 100 mil": ["Com certeza votaria nela para Presidente da República","Poderia votar nela para Presidente da República","Não votaria nela de jeito nenhum para Presidente da República","Não sabe/Não conhece"],

                "Cidades com mais de 100 mil":["Com certeza votaria nela para Presidente da República","Poderia votar nela para Presidente da República","Não votaria nela de jeito nenhum para Presidente da República","Não sabe/Não conhece"] },

            "Renda (SM)": {
                "Até 1": ["Com certeza votaria nela para Presidente da República","Poderia votar nela para Presidente da República","Não votaria nela de jeito nenhum para Presidente da República","Não sabe/Não conhece"],

                "Mais de 1 a 2": ["Com certeza votaria nela para Presidente da República","Poderia votar nela para Presidente da República","Não votaria nela de jeito nenhum para Presidente da República","Não sabe/Não conhece"],

                "Mais de 2 a 5": ["Com certeza votaria nela para Presidente da República","Poderia votar nela para Presidente da República","Não votaria nela de jeito nenhum para Presidente da República","Não sabe/Não conhece"],

                "Mais de 5": ["Com certeza votaria nela para Presidente da República","Poderia votar nela para Presidente da República","Não votaria nela de jeito nenhum para Presidente da República","Não sabe/Não conhece"]
            }
    }
    
saida["Aécio Neves"] = base
saida["Dilma Rousseff"] = {
            "Escolaridade": {
                "Fundamental": ["Com certeza votaria nela para Presidente da República","Poderia votar nela para Presidente da República","Não votaria nela de jeito nenhum para Presidente da República","Não sabe/Não conhece"],

                "5ª a 8ª Série": ["Com certeza votaria nela para Presidente da República","Poderia votar nela para Presidente da República","Não votaria nela de jeito nenhum para Presidente da República","Não sabe/Não conhece"],

                "Ensino Médio": ["Com certeza votaria nela para Presidente da República","Poderia votar nela para Presidente da República","Não votaria nela de jeito nenhum para Presidente da República","Não sabe/Não conhece"],

                "Ensino Superior":["Com certeza votaria nela para Presidente da República","Poderia votar nela para Presidente da República","Não votaria nela de jeito nenhum para Presidente da República","Não sabe/Não conhece"] },

            "Região": {
                "Brasil": ["Com certeza votaria nela para Presidente da República","Poderia votar nela para Presidente da República","Não votaria nela de jeito nenhum para Presidente da República","Não sabe/Não conhece"],

                "Nordeste": ["Com certeza votaria nela para Presidente da República","Poderia votar nela para Presidente da República","Não votaria nela de jeito nenhum para Presidente da República","Não sabe/Não conhece"],

                "Norte e Centro-Oeste": ["Com certeza votaria nela para Presidente da República","Poderia votar nela para Presidente da República","Não votaria nela de jeito nenhum para Presidente da República","Não sabe/Não conhece"],

                "Sudeste": ["Com certeza votaria nela para Presidente da República","Poderia votar nela para Presidente da República","Não votaria nela de jeito nenhum para Presidente da República","Não sabe/Não conhece"],

                "Sul":["Com certeza votaria nela para Presidente da República","Poderia votar nela para Presidente da República","Não votaria nela de jeito nenhum para Presidente da República","Não sabe/Não conhece"] },

            "Porte do Município (Habitantes)": {
                "Cidades com até 20 mil": ["Com certeza votaria nela para Presidente da República","Poderia votar nela para Presidente da República","Não votaria nela de jeito nenhum para Presidente da República","Não sabe/Não conhece"],

                "Cidades de 20 a 100 mil": ["Com certeza votaria nela para Presidente da República","Poderia votar nela para Presidente da República","Não votaria nela de jeito nenhum para Presidente da República","Não sabe/Não conhece"],

                "Cidades com mais de 100 mil":["Com certeza votaria nela para Presidente da República","Poderia votar nela para Presidente da República","Não votaria nela de jeito nenhum para Presidente da República","Não sabe/Não conhece"] },

            "Renda (SM)": {
                "Até 1": ["Com certeza votaria nela para Presidente da República","Poderia votar nela para Presidente da República","Não votaria nela de jeito nenhum para Presidente da República","Não sabe/Não conhece"],

                "Mais de 1 a 2": ["Com certeza votaria nela para Presidente da República","Poderia votar nela para Presidente da República","Não votaria nela de jeito nenhum para Presidente da República","Não sabe/Não conhece"],

                "Mais de 2 a 5": ["Com certeza votaria nela para Presidente da República","Poderia votar nela para Presidente da República","Não votaria nela de jeito nenhum para Presidente da República","Não sabe/Não conhece"],

                "Mais de 5": ["Com certeza votaria nela para Presidente da República","Poderia votar nela para Presidente da República","Não votaria nela de jeito nenhum para Presidente da República","Não sabe/Não conhece"]
            }
    }
saida["Marina Silva"] = {
            "Escolaridade": {
                "Fundamental": ["Com certeza votaria nela para Presidente da República","Poderia votar nela para Presidente da República","Não votaria nela de jeito nenhum para Presidente da República","Não sabe/Não conhece"],

                "5ª a 8ª Série": ["Com certeza votaria nela para Presidente da República","Poderia votar nela para Presidente da República","Não votaria nela de jeito nenhum para Presidente da República","Não sabe/Não conhece"],

                "Ensino Médio": ["Com certeza votaria nela para Presidente da República","Poderia votar nela para Presidente da República","Não votaria nela de jeito nenhum para Presidente da República","Não sabe/Não conhece"],

                "Ensino Superior":["Com certeza votaria nela para Presidente da República","Poderia votar nela para Presidente da República","Não votaria nela de jeito nenhum para Presidente da República","Não sabe/Não conhece"] },

            "Região": {
                "Brasil": ["Com certeza votaria nela para Presidente da República","Poderia votar nela para Presidente da República","Não votaria nela de jeito nenhum para Presidente da República","Não sabe/Não conhece"],

                "Nordeste": ["Com certeza votaria nela para Presidente da República","Poderia votar nela para Presidente da República","Não votaria nela de jeito nenhum para Presidente da República","Não sabe/Não conhece"],

                "Norte e Centro-Oeste": ["Com certeza votaria nela para Presidente da República","Poderia votar nela para Presidente da República","Não votaria nela de jeito nenhum para Presidente da República","Não sabe/Não conhece"],

                "Sudeste": ["Com certeza votaria nela para Presidente da República","Poderia votar nela para Presidente da República","Não votaria nela de jeito nenhum para Presidente da República","Não sabe/Não conhece"],

                "Sul":["Com certeza votaria nela para Presidente da República","Poderia votar nela para Presidente da República","Não votaria nela de jeito nenhum para Presidente da República","Não sabe/Não conhece"] },

            "Porte do Município (Habitantes)": {
                "Cidades com até 20 mil": ["Com certeza votaria nela para Presidente da República","Poderia votar nela para Presidente da República","Não votaria nela de jeito nenhum para Presidente da República","Não sabe/Não conhece"],

                "Cidades de 20 a 100 mil": ["Com certeza votaria nela para Presidente da República","Poderia votar nela para Presidente da República","Não votaria nela de jeito nenhum para Presidente da República","Não sabe/Não conhece"],

                "Cidades com mais de 100 mil":["Com certeza votaria nela para Presidente da República","Poderia votar nela para Presidente da República","Não votaria nela de jeito nenhum para Presidente da República","Não sabe/Não conhece"] },

            "Renda (SM)": {
                "Até 1": ["Com certeza votaria nela para Presidente da República","Poderia votar nela para Presidente da República","Não votaria nela de jeito nenhum para Presidente da República","Não sabe/Não conhece"],

                "Mais de 1 a 2": ["Com certeza votaria nela para Presidente da República","Poderia votar nela para Presidente da República","Não votaria nela de jeito nenhum para Presidente da República","Não sabe/Não conhece"],

                "Mais de 2 a 5": ["Com certeza votaria nela para Presidente da República","Poderia votar nela para Presidente da República","Não votaria nela de jeito nenhum para Presidente da República","Não sabe/Não conhece"],

                "Mais de 5": ["Com certeza votaria nela para Presidente da República","Poderia votar nela para Presidente da República","Não votaria nela de jeito nenhum para Presidente da República","Não sabe/Não conhece"]
            }
    }

trad = {
    'aecio': "Aécio Neves",
    'dilma': "Dilma Rousseff",
    'marina': "Marina Silva",
    'total': "Brasil",
    'renda_familiar': "Renda (SM)",
     'escolaridade': "Escolaridade",
     'porte': "Porte do Município (Habitantes)",
     'regiao': "Região",
    '2 a 5': "Mais de 2 a 5",
     'Medio': "Ensino Médio",
     'SUDESTE': "Sudeste",
     'Superior': "Ensino Superior",
     'NORTE-CENTRO-OESTE': "Norte e Centro-Oeste",
     'Mais de 5': "Mais de 5",
     'SUL': "Sul",
     'NORDESTE': "Nordeste",
     'Fundamental 2': "5ª a 8ª Série",
     'Fundamental 1': "Fundamental",
     '1 a 2': "Mais de 1 a 2",
     'Ate 1': "Até 1",
     'Cidades de 20 a 100 mil': 'Cidades de 20 a 100 mil',
     'Cidades com até 20 mil': 'Cidades com até 20 mil',
      "Cidades com mais de 100 mil": "Cidades com mais de 100 mil"     
     }


with open(path+"/arquivos_originais/ringue.csv","r") as file:
    arquivo = csv.reader(file)
    next(arquivo, None)  # ignora o cabeçalho
    for row in arquivo:
        if row[2] in trad and row[3] in trad:
            candidato = trad[row[0]]
            categoria = trad[row[2]] if row[2] != "total" else "Região"
            recorte = trad[row[3]]
            indice = saida[candidato][categoria][recorte].index(row[1])
            saida[candidato][categoria][recorte][indice] = row[4]

with open("ringue.json","w",encoding='utf8') as file:
    file.write(json.dumps(saida,ensure_ascii=False))
