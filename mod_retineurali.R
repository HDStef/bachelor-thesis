library(nnet)
library(NeuralNetTools)
library(ROCR)

load('data_training.RData')
load('data_test.RData')

Ms <- c(1,2,3,4,5)    # provo diversi M (non più di 5 altrimenti ci sono troppi parametri da stimare)
lambdas <- c(0.0001,0.001,0.01) # provo diversi lambda

size <- rep(Ms, length(lambdas))
size

lambda <- rep(lambdas,length(Ms))
lambda

sl<-cbind(size,lambda) # combino size e lambda
sl


out<-matrix(NA,nrow(sl),1)  # creo una vettore di dimensione size*lambda in cui mettere la stima dell'errore per ogni soluzione

out

# vado a valutare la combinazione di size e lambda che produce l'errore di previsione minore allenando la rete sul training set (train)
# e poi stimando l'errore di classificazione sul test set (test). 

set.seed(123456)
for (j in 1:nrow(sl)){ # ciclo per le righe di sl =15
  cat("size=", sl[j,1], "decay=", sl[j,2],"\n") #concatena e stampa (prova j=1)
  best <- Inf
  for (h in 1:10){   # multiple starting 
    nnh<-nnet(is_fraud ~.,train,trace=FALSE, size=sl[j,1],decay=sl[j,2],
              maxit=10000)
    if(nnh$value < best) {best <- nnh$value; nn <- nnh} # ?nnet vedi: value 
  }
  pred <- predict(nn, newdata = test)
  pred <- as.numeric(pred>0.5)	# y previste = good nel test set
  truth <- as.numeric(test$is_fraud == "si") # y osservate = good nel test set
  er <- 1-mean(truth == pred)		        # tasso di errata classificazione
  print(paste("er=", round(er,3)))
  out[j] <- er
}

out.cv <- cbind(sl, out)
out.cv  # stampa la matrice sl con una terza colonna contenente l'errore

out.cv[which.min(out.cv[,3]),1:2]  

## ? Si sceglie la soluzione con size = M = 1 e decay = lambda = 0.0001  che ? associata al tasso di errore pi? basso 

set.seed(42534)
best <- Inf
for(h in 1:10){ # multiple starting
  nnh <-  nnet(is_fraud ~.,data= train, size=4, decay=0.001, maxit=10000, trace=FALSE) # size e decay da cambiare secondo la scelta sopra
  print(paste("h, valore:", h, round(nnh$value,4), "convergence", nnh$convergence)) 
  if(nnh$value < best) {best <- nnh$value; nn <- nnh} # la rete migliore viene salvata in nn!
}

summary(nn)

#plot della rete (ci serve anche per capire il summary sopra, vedi come ricodifica variabili categoriali con più di 2 modalità)

plotnet(nn, cex_val =0.6)  # nn ? il nome dell'oggetto creato da nnet; Cex_val: numeric value indicating size of text labels, default 1

# PREVISIONI BASATE SULLA RETE NEURALE SUL TEST SET

test$score.nn=predict(nn, newdata=test) # stimo la probabilit? di essere good per il test set (score.nn) e aggiungo la colonnna al dataframe test 

head(test) # viene creata un'ulteriore colonna score.nn

test$aff.prev.nn<-ifelse(test$score.nn>=0.5,"si","no") # gruppo di appartenenza stimato secondo la rete neurale nn

head(test) # vedi ultima colonna!

t.nn=table(test$is_fraud ,test$aff.prev.nn) # tabella di classificazione: calcolare  le misure di performance
t.nn

# precisione 
1201 / (1201 + 752) * 100

# specificità 
3621 / (3621 + 752) * 100

# sensitività 
1201 / (1201 + 675) * 100

EC <- 1-sum(diag(t.nn))/sum(t.nn)	
EC * 100
CC <- 1-EC
CC * 100


# CURVA ROC
pred.nn <-prediction(test$score.nn,test$is_fraud) # calcola tutte le stime che servono per la curva ROC
pred.nn

perf.nn.roc<-performance(pred.nn,"tpr","fpr") # calcola tutte le misure di performance che gli servono per la curva ROC
plot(perf.nn.roc, main="Curva ROC rete neurale", colorize=T) # aggiunge colore a seconda del cut-off

auc.nn<- performance(pred.nn, measure = "auc")
auc.nn<- auc.nn@y.values[[1]]
auc.nn  # 0,81



plot(perf.nn.roc, main="Curva ROC Albero di classificazione vs Rete neurale", col="blue") 
plot(perf.albero.roc, add=TRUE, col="red") 
legend("bottomright", legend=c("Rete Neurale", "Albero di classificazione"), col=c("blue", "red"), lty=1)



