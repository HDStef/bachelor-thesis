library(rpart) 
library(rpart.plot)


load('data_training.RData')
load('data_test.RData')

attach(train)
attach(test)

set.seed(123456)

tree1 <- rpart(is_fraud~., data=train , method="class", cp = 0.001)  # albero di classificazione che include tutte le covariate 

rpart.plot(tree1)
print(tree1) 


predict(tree1,type="class") # classe di appartenenza prevista per le osservazioni

predict(tree1,type="prob") # probabilit? di appartenenza alle due classi prevista per le osservazioni


test$score.albero=predict(tree1, newdata=test) # stimo le probabilit? essere bad e good per il test set (score.albero) e aggiungo le colonnne al dataframe test 

head(test) # come si pu? vedere vengono create due nuove colonne: score.albero.bad e score.albero.good

test$score.albero[,2] # seconda colonna di score.albero (prob di essere good)

test$aff.prev.albero <- ifelse(test$score.albero[,2]>=0.5,"si","no") # gruppo di appartenenza stimato secondo l'albero

head(test) # vedi ultima colonna!

t.albero=table(test$is_fraud,test$aff.prev.albero) # tabella di classificazione: calcolare gli indicatori di performance
t.albero

# precisione
1251 / (1251 + 625) * 100

# specificità
4123 / (4123 + 625) * 100

# sensitività
1251 / (1251 + 625) * 100



CC=sum(diag(t.albero))/sum(t.albero)  # calcola il tasso di corretta classificazione (tasso assoluto secondo il metodo di risostituzione)
CC * 100

EC=1-CC # tasso di errata classificazione
EC * 100


test$score.albero=predict(tree1, newdata=test) 
test$aff.prev.albero <- ifelse(test$score.albero[,2]>=0.5,"si","no") # gruppo di appartenenza stimato secondo l'albero
tab2 <-table(oss=test$is_fraud, prev=test$aff.prev.albero)
tab2



#PRUNING

plotcp(tree1)  # grafico dell'errore relativo di crossvalidation rispetto ai parametri di complessit?

tree2 <- prune(tree1, cp=0.006) # pruning dell'albero (6 foglie)

rpart.plot(tree2)
print(tree2)

test$score.albero=predict(tree2, newdata=test) 
test$aff.prev.albero <- ifelse(test$score.albero[,2]>=0.5,"si","no") # gruppo di appartenenza stimato secondo l'albero
tab2 <-table(oss=test$is_fraud, prev=test$aff.prev.albero)
tab2


CC2 <- sum(diag(tab2))/sum(tab2)  # calcola il tasso di corretta classificazione (tasso assoluto secondo il metodo di risostituzione)
CC2 * 100

EC <- 1-CC2 # tasso di errata classificazione
EC * 100

# precisione
1218 / (1218 + 223) * 100

# specificità
4150 / (4150 + 658) * 100

# sensitività
1218 / (1218 + 658) * 100

print(tree2)



predict(tree2,type="class") # classe di appartenenza prevista per le osservazioni

predict(tree2,type="prob") # probabilit? di appartenenza alle due classi prevista per le osservazioni


# # creo una nuova unit? (ipotizzo valori/modalit? per le variabili contenute in tree1 di cui tree3 ? la versione pruned)
# newdata<-data.frame(saldo_conto_bin="pos",durata_prestito=12,pagamenti_precedenti_ricod="non regolari",scopo_ricod="casa",ammontare_prestito=2000,risparmi_bin="<100 o non disponibili",anzianita_lavorativa_bin="1",maggiori_investimenti_ricod="assenti",eta=35,ulteriori_prestiti_bin="no")
# 
# predict(tree2,newdata) # previsione per il nuovo caso


# CURVA ROC
library(ROCR)

pred.albero <- prediction(test$score.albero[,2],test$is_fraud) # calcola tutte le stime che servono per la curva ROC

pred.albero

perf.albero.roc <- performance(pred.albero,"tpr","fpr") # calcola tutte le misure di performance che gli servono per la curva ROC
plot(perf.albero.roc, main="Curva ROC albero pruned", colorize=T) # aggiunge colore a seconda del cut-off

auc.tree <- performance(pred.albero, measure = "auc")
auc.tree <- auc.tree@y.values[[1]]
auc.tree # 0.80


