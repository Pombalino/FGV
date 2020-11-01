library(tidyr)
library(dplyr)

#IMPORTAÇÃO DE BASES
#Adaptação necessária por conta de tipo de arquivo .asc
account <- tbl_df(read.csv("account.asc", stringsAsFactors = FALSE, sep = ";"))
card <- tbl_df(read.csv("card.asc", stringsAsFactors = FALSE, sep = ";"))
client <- tbl_df(read.csv("client.asc", stringsAsFactors = FALSE, sep = ";"))
disposition <- tbl_df(read.csv("disp.asc", stringsAsFactors = FALSE, sep = ";"))
district <- tbl_df(read.csv("district.asc", stringsAsFactors = FALSE, sep = ";"))
loan <- tbl_df(read.csv("loan.asc", stringsAsFactors = FALSE, sep = ";"))
order <- tbl_df(read.csv("order.asc", stringsAsFactors = FALSE, sep = ";"))
transactions <- tbl_df(read.csv("trans.asc", stringsAsFactors = FALSE, sep = ";"))

#TRATAMENTO CLIENT
#Diferenciação de sexo de acordo com regra
client <- mutate(client, sex = ifelse(substr(client$birth_number, 3,3) == 5 | substr(client$birth_number, 3,3) == 6, "FEM", "MASC"))
#Definição de ano de nascimento de acordo com tipo de data
client <- mutate(client, birth_year = as.numeric(substr(client$birth_number, 1,2)))
#Marcação de sexo
client <- mutate(client, sex_dummy = ifelse(client$sex == "MASC", as.double(1),as.double(0)))
#Definição de idade a fazer no ano presente (1998) de acordo com ano de nascimento
client <- mutate(client, age = as.double(1998 - (1900 + client$birth_year)))

#TRATAMENTO DISTRICT
#Nomes de colunas
colnames(district) <- c("dist_code", "dist_name", "region", "inhab", "mh_less_499", "mh500_1999", "mh2000_9999", "mh_greater_10000", "num_cities", "ratio_urban_inhab", "avg_salary_district", "unemploy_rate_95", "unemploy_rate_96", "empreendedor_1000_inhab","num_crimes_95", "num_crimes_96")
#Transformação para double
district<- mutate(district, unemploy_rate_95 = as.double(unemploy_rate_95))
district<- mutate(district, num_crimes_95 = as.double(num_crimes_95))
#Filtro de duplicidade
district<- mutate(district, ratio_urban_inhab = ratio_urban_inhab)
#Ajuste de percentual
district<- mutate(district, ratio_rural_inhab = as.double(100 - ratio_urban_inhab))

#Valor médio consolidado de desemprego
district_avg<- district %>%
  group_by(dist_code)%>%
  summarise(avg_unemploy = as.double(sum(unemploy_rate_95,unemploy_rate_96)/2),
            avg_num_crimes = as.double(sum(num_crimes_95,num_crimes_96)/2))

district<- left_join(district,district_avg, by = c("dist_code"="dist_code"))
district_avg <- NULL

#TRATAMENTO LOAN
#Nomenclatura de Status
loan <- mutate(loan, status = ifelse(loan$status == "A", "A - Ended and OK",
                                     ifelse(loan$status == "B", "B - Endended not OK",
                                            ifelse(loan$status == "C", "C - Going and OK", 
                                                   ifelse(loan$status == "D", "D - Going not OK", "N/A")))))

#Uso de nomenclatura para classificação binária entre adimplente (1) e inadimplente (0)
loan <-mutate(loan, status_loan_dummy = ifelse(loan$status == "A - A - Ended and OK", 1,
                                              ifelse(loan$status == "C - Going and OK", 1,
                                                     ifelse(loan$status == "B - B - Endended not OK", 0,
                                                            ifelse(loan$status == "D - Going not OK", 0, "N/A")))))

loan <-mutate(loan, status_loan_dummy = as.double(loan$status_loan_dummy))

#Troca de nomes de colunas
colnames(loan) <- c("loan_id","account_id", "date_loan", "amount_loan", 
                    "duration_loan", "payments_loan", "status_loan", "status_loan_dummy")

loan <-mutate(loan, payments_loan = as.double(loan$payments_loan))

#TRATAMENTO CARD
#Nomenclatura
colnames(card) <- c("card_id", "disp_id", "type_card", "issued_card")

#Conversão de tipo de dados
card$year <- c(1900 + as.numeric(substr(card$issued_card, 1,2)))
card$month <- c(as.numeric(substr(card$issued_card, 3,4)))
card$day <- c(as.numeric(substr(card$issued_card, 5,6)))
card$date_issued_card <- as.Date(with(card, paste(year, month, day, sep = "-")),"%Y-%m-%d")
card$year <- NULL
card$month <- NULL
card$day <- NULL
card$issued_card <- NULL

#Dummys
card <- mutate(card, card_classic = ifelse(type_card == "classic", 1,0))
card <- mutate(card, card_gold = ifelse(type_card == "gold", 1,0))
card <- mutate(card, card_junior = ifelse(type_card == "junior", 1,0))

#TRATMENTO ACCOUNT
#Nomenclatura
colnames(account) <- c("account_id","district_id","frequency_account", "date_account")

#Ajuste das classificações
account <- mutate(account, frequency_account =
                    ifelse(account$frequency_account == "POPLATEK MESICNE", "Monthly",
                           ifelse(account$frequency_account == "POPLATEK TYDNE", "Weekly",
                                  ifelse(account$frequency_account == "POPLATEK PO OBRATU","Trans", "N/A"))))

#Ajuste de datas
account$year <- c(1900 + as.numeric(substr(account$date_account, 1,2)))
account$month <- c(as.numeric(substr(account$date_account, 3,4)))
account$day <- c(as.numeric(substr(account$date_account, 5,6)))
account$date_account <- as.Date(with(account, paste(year, month, day, sep = "-")),"%Y-%m-%d")

#Considerando ano presente de 1998
account<-mutate(account, account_years = (1998 - year))
account$year<- NULL
account$month <- NULL
account$day <- NULL

colnames(account) <- c("account_id","district_id_account","frequency_account", "date_account", "account_years")

#TRATAMENTO ORDER
#Ajuste das classificações
order <- mutate(order, k_symbol_new = ifelse(order$k_symbol == "POJISTNE","Ensur",
                                             ifelse(order$k_symbol == "SIPO","Ensur_house",
                                                    ifelse(order$k_symbol == "LEASING","Leasing",
                                                           ifelse(order$k_symbol == "UVER","Loan","N/A")))))

#TRATAMENTO TRANSACTIONS
#Ajuste das classificações
transactions <- mutate(transactions, operation = ifelse(transactions$operation == "VYBER KARTOU", "-CreditCardDraw",
                                                        ifelse(transactions$operation == "VKLAD", "+MoneyDeposit",
                                                               ifelse(transactions$operation == "PREVOD Z UCTU", "+Transfer",
                                                                      ifelse(transactions$operation == "VYBER", "-MoneyDraw",
                                                                             ifelse(transactions$operation == "PREVOD NA UCET", "-TransferOther", "N/A"))))))

#Ajuste das classificações
transactions <- mutate(transactions, k_symbol = ifelse(transactions$k_symbol == "POJISTNE","-SafePay",
                                                       ifelse(transactions$k_symbol == "SLUZBY","-Charge",
                                                              ifelse(transactions$k_symbol == "UROK", "+Interest",
                                                                     ifelse(transactions$k_symbol == "SANKC. UROK", "-Interest",
                                                                            ifelse(transactions$k_symbol == "SIPO", "-ImobLoan",
                                                                                   ifelse(transactions$k_symbol == "DUCHOD","+Pension",
                                                                                          ifelse(transactions$k_symbol == "UVER", "-LoanPay","N/A"))))))))

#Ajuste das classificações
transactions <- mutate(transactions, type = ifelse(transactions$type == "PRIJEM", "Credit",
                                                  ifelse(transactions$type == "VYDAJ", "Debit", "N/A")))

#Consolidação em nova base com chave por conta
trans_balance <- transactions %>%
  dplyr::select(account_id, balance)%>%
  group_by(account_id)%>%
  summarise(mean_balance = mean(balance))

#CONSOLIDAÇÃO EM BASE ÚNICA

#Agregação e ajustes de classificações
z.A <- left_join(client, disposition, by=c("client_id" = "client_id"))
z.B <- left_join(z.A, district, by=c("district_id"="dist_code"))
z.C <- left_join(z.B, loan, by=c("account_id" = "account_id"))
z.C <- mutate(z.C, possui_emprestimo = ifelse(is.na(z.C$loan_id), "N", "Y"))
z.D <- left_join(z.C, card, by=c("disp_id"="disp_id"))
z.D <- mutate(z.D, possui_cartao_cred = ifelse(is.na(z.D$card_id), "N", "Y"))
z.E <- left_join(z.D, account, by = c("account_id"="account_id"))

#Separação de clientes para poder fazer a regressão
#Nesse caso é obrigatório ter emprestimo, ser titular e empregado
z.E_Possui_Emp <- z.E %>%
  filter(possui_emprestimo == "Y") %>%
  filter(unemploy_rate_95 != "NA") %>%
  filter(type == "OWNER")%>%
  dplyr::select(status_loan_dummy,
         district_id,
         account_id,
         sex_dummy,
         age,
         inhab,
         mh_less_499,
         mh500_1999,
         mh2000_9999,
         mh_greater_10000,
         num_cities, 
         ratio_urban_inhab, 
         avg_salary_district, 
         unemploy_rate_95,
         unemploy_rate_96,
         empreendedor_1000_inhab,
         num_crimes_95,
         num_crimes_96,
         amount_loan,
         duration_loan,
         payments_loan,
         ratio_rural_inhab,
         avg_unemploy,
         avg_num_crimes,
         account_years,
         card_classic,
         card_gold,
         card_junior)

#Trazendo saldo médio em cc
z.E_Possui_Emp2 <- left_join(z.E_Possui_Emp, trans_balance, by = c("account_id" = "account_id"))

#Trazendo cartão por tipos separados em dummys
z.E_possui_cartao <- z.E %>%
  filter(type == "OWNER")%>%
  dplyr::select(account_id, possui_cartao_cred)

z.E_possui_cartao <- mutate(z.E_possui_cartao, possui_cartao_cred = ifelse(possui_cartao_cred == "Y", 1,0))

z.E_Possui_Emp_Cartao <- left_join(z.E_Possui_Emp2,z.E_possui_cartao, by = c("account_id" = "account_id"))

z.E_Possui_Emp_Cartao <-mutate(z.E_Possui_Emp_Cartao, card_classic = ifelse(is.na(card_classic),0,card_classic))

z.E_Possui_Emp_Cartao <-mutate(z.E_Possui_Emp_Cartao, card_gold = ifelse(is.na(card_gold),0,card_gold))

z.E_Possui_Emp_Cartao <-mutate(z.E_Possui_Emp_Cartao, card_junior = ifelse(is.na(card_junior),0,card_junior))

#REGRSSÃO LOGÍSTICA
#Treinamento
z.E_Regr_Possui_Empr <- glm(status_loan_dummy ~ 
                              num_cities + 
                              empreendedor_1000_inhab +
                              account_years +
                              mean_balance,
                            family = "binomial", data = z.E_Possui_Emp_Cartao)
#Resultado
summary(z.E_Regr_Possui_Empr)

#Análises do resultado
z.E.predict <- predict(z.E_Regr_Possui_Empr, z.E_Possui_Emp_Cartao, type = "response")
z.E.model_pred_direction = rep(0, 674)
z.E.model_pred_direction[z.E.predict>0.5] = 1
table(z.E.model_pred_direction,z.E_Possui_Emp2$status_loan_dummy)
mean(z.E.model_pred_direction == z.E_Possui_Emp2$status_loan_dummy)
mean(z.E.model_pred_direction != z.E_Possui_Emp2$status_loan_dummy)

#APLICAÇÃO DO MODELO ENCONTRADO
#Nova tabela apenas com variáveis existentes no modelo
z.F_ident_account <- account
z.F_ident_account <- left_join(z.F_ident_account, district, by = c("district_id_account"="dist_code"))
z.F_ident_account$district_id_account <- NULL
z.F_ident_account$frequency_account <- NULL
z.F_ident_account$date_account <- NULL
z.F_ident_account$dist_name <- NULL
z.F_ident_account$region <- NULL
z.F_ident_account$inhab <- NULL
z.F_ident_account$mh_menor_499 <- NULL
z.F_ident_account$mh500_1999 <- NULL 
z.F_ident_account$mh2000_9999 <- NULL   
z.F_ident_account$mh_maior_10000 <- NULL
z.F_ident_account$ratio_urban_inhab <- NULL    
z.F_ident_account$avg_salary_district <- NULL    
z.F_ident_account$unemploy_rate_95 <- NULL 
z.F_ident_account$unemploy_rate_96 <- NULL 
z.F_ident_account$num_crimes_95 <- NULL 
z.F_ident_account$num_crimes_96 <- NULL 
z.F_ident_account$ratio_rural_inhab <- NULL 
z.F_ident_account$avg_unemploy <- NULL 
z.F_ident_account$avg_num_crimes <- NULL 

#Saldo em cc
z.F_ident_account <-left_join(z.F_ident_account,trans_balance, by=c("account_id"="account_id"))

#Valores de probabilidade com base no modelo
z.F_ident_account <- mutate(z.F_ident_account, 
                            prob = as.double(1/(1+exp(-(-2.001e+00+
                                                          num_cities*1.159e-01+
                                                          empreendedor_1000_inhab*1.596e-02+
                                                          account_years*-2.915e-01+mean_balance*6.094e-05)))))

#Classificação de risco em 3 níveis propostos
z.F_ident_account <- mutate(z.F_ident_account, result = ifelse(prob<=0.5,"High Risk", ifelse(prob>0.5 & prob<0.9, "Medium Risk", "Low Risk")))

#Tabela final de resultados consolidados
z.F_ident_account_resumo <- z.F_ident_account %>%
  dplyr::select(result)%>%
  group_by(result)%>%
  summarise(count_result = n())

#plotagem de gráfico de barras com classificação resultado
library(ggplot2)

ggplot(data = z.F_ident_account_resumo, aes(x = reorder(z.F_ident_account_resumo$result,-z.F_ident_account_resumo$count_result),
                                            y = z.F_ident_account_resumo$count_result))+
  geom_bar(stat = "identity", color = "black")+
  ylab("Total Clients")+
  xlab("Categories")

#KNN
#Importando library com a função
library(class)

# é necessário normalizar os valores?
var(z.E_Possui_Emp_Cartao$age) # 161.5969
var(z.E_Possui_Emp_Cartao$amount_loan) # 12665897748
# sim, as variâncias são muito distoantes. vamos usar o Min Max Scaler
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) }
z.E_normaliz <- as.data.frame(lapply(z.E_Possui_Emp_Cartao[,2:29], normalize))

# Dividindo em sets Teste e Treino, labels separados
set.seed(42)
dat.d <- sample(1:nrow(z.E_normaliz),size=nrow(z.E_normaliz)*0.7,replace = FALSE) # selecionando aleatoriamente 70%

train <- z.E_normaliz[dat.d,] # treino, 70%
test <- z.E_normaliz[-dat.d,] # teste
train.labels <- z.E_Possui_Emp_Cartao[dat.d,1] # target é se o cliente paga ou não
test.labels <- z.E_Possui_Emp_Cartao[-dat.d,1]

train.labels[is.na(train.labels)]<-0
test.labels[is.na(test.labels)]<-0

#encontrando o valor de k
sqrt(NROW(train.labels)) #21.70253, arredondando para neighbourds
knn.21 <- knn(train=train, test=test, cl=train.labels$status_loan_dummy, k=21)
# accuracy
ACC.21 <- 100 * sum(test.labels$status_loan_dummy == knn.21)/NROW(test.labels)
ACC.21 # 80.29557
knn.relabeled <- mutate(as.data.frame(knn.21), knn = ifelse(as.data.frame(knn.21)$knn.21==0,"Mal pagador", "Bom pagador"))
test.relabeled <- mutate(test.labels, truth = ifelse(test.labels$status_loan_dummy==0,"Mal pagador", "Bom pagador"))
#confusion matrix
library(caret)
confusionMatrix(table(knn.relabeled$knn, test.relabeled$truth, dnn=c('knn','truth'))) # p-valor 1.767e-11
