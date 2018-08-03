# -*- coding: utf-8 -*-
"""
Created on Fri Aug  3 06:55:40 2018

@author: C339182
"""

import openpyxl

path_to_file = "C:/Users/C339182/Downloads/EDITAIS GERAIS Nº 2015 OFICIAL - Bárbara.xlsx"
texto_pivo = "NÚMERO DO EDITAL" #coluna 1, linha 5 do cabeçalho
sheet = "Plan1"

book = openpyxl.load_workbook(path_to_file, read_only=True)

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
temp = []
i=18


# divide and conquer, criar uma coluna gigante com todos os valores, sum(Val[trip][start-1:end-1])
for trip in nomes:
    j = 1
    temp = []
    Val[trip] = []
    while j <= 3:
        coluna = j + 3*(nomes.index(trip)+1)
        if planilha.cell(row=18,column=coluna).value != None:
            temp.append(planilha.cell(row = 18,column=coluna).value)
        j += 1 # working
    Val[trip].append(sum(temp)) #working
    
# tentativa 2: merge as colunas, mas só se não for read_only

print(planilha.merged_cells.ranges) # AttributeError: 'ReadOnlyWorksheet' object has no attribute 'merged_cells'


