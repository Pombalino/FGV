#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Aug  3 06:55:40 2018
@author: C339182
"""

import openpyxl

path_to_file = "/home/gustavo/Documents/LPC & Co/SEC_CULT/EDITAIS/EDITAIS GERAIS Nº 2015 OFICIAL - Bárbara.xlsx"
texto_pivo = "NÚMERO DO EDITAL" #coluna 1, linha 5 do cabeçalho
sheet = "Plan1"

book = openpyxl.load_workbook(path_to_file)

planilha = book[sheet]

# No of written Rows in sheet
r = planilha.max_row

# No of written Columns in sheet
c = planilha.max_column

row_list = []

for linha in range(1, r+1):
    if planilha.cell(row=linha,column=1).value == "NÚMERO DO EDITAL":
        row_list.append(planilha.cell(row=linha,column=1).row)
        
# start = n_ed + 6
# end = n_ed[+1] - 6

col = 1
nomes = []

while col <= c+1:
    nomes.append(planilha.cell(row=7,column = col).value)
    print(nomes[-1])
    col += 3

nomes = nomes[1:-3]

Val = {}
temp = [] # triplas
i=1

for trip in nomes:
    Val[trip] = []
# divide and conquer, criar uma coluna gigante com todos os valores, sum(Val[trip][start-1:end-1])
while i <= r + 1:
    for trip in nomes:
        j = 1
        temp = []
        while j <= 3:
            coluna = j + 3*(nomes.index(trip)+1)
            if planilha.cell(row=i,column=coluna).value != None:
                temp.append(planilha.cell(row = i,column=coluna).value)
            else:
                temp.append(0)
            j += 1 # working
        Val[trip].append(temp) #working
    print("row")
    i +=1

def soma(valor, valor1, valor2, valor3):
    a=0
    try:
        a = valor + valor1 + valor2 + valor3
    except TypeError:
        a = eval(valor) + eval(valor1) + eval(valor2) + eval(valor3)
    return a # falta tirar o = do começo das strings

valu = []
for l in row_list:
    start = l + 5  # pega o começo do projeto
    try:
        ind = row_list.index(l)
        end = row_list[ind+1]-6 # pega o fim
    except IndexError:
        end = 131
    for trip in nomes:
        som = 0
        values = Val[trip][start:end] # slice os falores dos projetos
        print(values)
        for valor in values:
            som = soma(som,*valor) # not working
            valu.append(som)
            