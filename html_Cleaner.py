# -*- coding: utf-8 -*-
"""
Created on Tue Apr  9 08:46:16 2019

@author: Pohlmann
"""

import os
from bs4 import BeautifulSoup as bs

files = os.listdir('C:/Users/Pohlmann/Documents/FGV/6º Semestre/DesafioOracle/bdmepshrjporestao')

for f in files:
    with open('C:/Users/Pohlmann/Documents/FGV/6º Semestre/DesafioOracle/bdmepshrjporestao/'+f,"r") as html:
        soup = bs(html,'html.parser')
        txt = soup.find('a')
        txt = txt.next_sibling[22:]
        with open('C:/Users/Pohlmann/Documents/FGV/6º Semestre/DesafioOracle/bdmepshrjporestao/'+f[:-5]+".txt","w") as file:
            file.write(txt)