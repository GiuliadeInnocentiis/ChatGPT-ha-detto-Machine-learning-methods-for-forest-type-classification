#progetto Faccanoni, de Innocentiis: Forest cover type 
library(corrplot)
library(ggplot2)
library(gridExtra)
library(heplots)
library(MASS)
library(tidyverse)
library(car)
library(class)
library(caret)
library(grid)
library( ROCR)
library(mice)
library(randomForest)
library(dbscan)
library(dbplyr)
library(clValid)
library(e1071)
library(mlrMBO)
library(EMCluster)
library(farff)
library(DiceKriging)

foreste<-read.csv("C:/Users/giuli/OneDrive/Desktop/Università/4anno/Machine Learning/Progetto/FORESTE.csv")
str(foreste)
#variabile target 
levels(as.factor(foreste$Cover_Type))
table(foreste$Cover_Type) #classi bilanciate

#controllo dei livelli della variabile wildness area
levels(as.factor(foreste$Wilderness_Area1))
levels(as.factor(foreste$Wilderness_Area2))
levels(as.factor(foreste$Wilderness_Area3))
levels(as.factor(foreste$Wilderness_Area4))

#non ci sono valori mancanti
anyNA(foreste)

#-----------------------------------------------------
#divido il dataset in traning e test set 
set.seed(123)
ixs<-createDataPartition(foreste$Cover_Type, times = 1, p=0.9)
test<- foreste[-ixs$Resample1, ]
train<- foreste[ixs$Resample1, ]

#costruzione di un dataset partendo da foreste in cui si crea una variabile categoriale "wild" 
#conenente i 4 tipi di aree selvagge e una variabile categoriale "soil" contenente tutti 
#i 40 tipi di suolo
#dataset con soil e wild:
prova<-train %>%
  gather(`Soil_Type1`, `Soil_Type2`, `Soil_Type3`, `Soil_Type4`,
         `Soil_Type5`, `Soil_Type6`, `Soil_Type7`, `Soil_Type8`,
         `Soil_Type9`, `Soil_Type10`, `Soil_Type11`, `Soil_Type12`,
         `Soil_Type13`, `Soil_Type14`, `Soil_Type15`, `Soil_Type16`,
         `Soil_Type17`, `Soil_Type18`, `Soil_Type19`, `Soil_Type20`,
         `Soil_Type21`, `Soil_Type22`, `Soil_Type23`, `Soil_Type24`,
         `Soil_Type25`, `Soil_Type26`, `Soil_Type27`, `Soil_Type28`,
         `Soil_Type29`, `Soil_Type30`, `Soil_Type31`, `Soil_Type32`,
         `Soil_Type33`, `Soil_Type34`, `Soil_Type35`, `Soil_Type36`,
         `Soil_Type37`, `Soil_Type38`, `Soil_Type39`, `Soil_Type40`,
         key = "soil", value = "cases")

pp<-prova%>%
  filter(cases==1 )
pp1<-pp[, -18]
pp2<-pp1%>%
  gather(`Wilderness_Area1`, `Wilderness_Area2`, `Wilderness_Area3`,
         `Wilderness_Area4`,
         key = "wild", value = "cc")
pp3<-pp2%>%
  filter(cc==1 )
pp4<-pp3[,-15]

train_unito<-pp4
train_unito$soil<-as.factor(train_unito$soil)
train_unito$wild<-as.factor(train_unito$wild)
str(train_unito)

#test
prova<-test %>%
  gather(`Soil_Type1`, `Soil_Type2`, `Soil_Type3`, `Soil_Type4`,
         `Soil_Type5`, `Soil_Type6`, `Soil_Type7`, `Soil_Type8`,
         `Soil_Type9`, `Soil_Type10`, `Soil_Type11`, `Soil_Type12`,
         `Soil_Type13`, `Soil_Type14`, `Soil_Type15`, `Soil_Type16`,
         `Soil_Type17`, `Soil_Type18`, `Soil_Type19`, `Soil_Type20`,
         `Soil_Type21`, `Soil_Type22`, `Soil_Type23`, `Soil_Type24`,
         `Soil_Type25`, `Soil_Type26`, `Soil_Type27`, `Soil_Type28`,
         `Soil_Type29`, `Soil_Type30`, `Soil_Type31`, `Soil_Type32`,
         `Soil_Type33`, `Soil_Type34`, `Soil_Type35`, `Soil_Type36`,
         `Soil_Type37`, `Soil_Type38`, `Soil_Type39`, `Soil_Type40`,
         key = "soil", value = "cases")

pp<-prova%>%
  filter(cases==1 )
pp1<-pp[, -18]
pp2<-pp1%>%
  gather(`Wilderness_Area1`, `Wilderness_Area2`, `Wilderness_Area3`,
         `Wilderness_Area4`,
         key = "wild", value = "cc")
pp3<-pp2%>%
  filter(cc==1 )
pp4<-pp3[,-15]
test_unito<-pp4
test_unito$soil<-as.factor(test_unito$soil)
test_unito$wild<-as.factor(test_unito$wild)
#-----------------------------------------------------------------------------

#Riutilizzo il dataset originale foreste 
#controllo dei livelli delle variabili soil
livelli<- vector()
for (i in 15:55) {
  livelli[i-15]<- sum(foreste[,i])
}
which(livelli==0) #la variabile Soil_7 e Soil_15 hanno tutti valori pari a zero
#elimino le variabili Id, Soil_7 e Soil_15
foreste<-foreste[, -c(1,22,30 )]

#divisione del dataset in traning e test set
set.seed(123)
ixs<-createDataPartition(foreste$Cover_Type, times = 1, p=0.9)
test<- foreste[-ixs$Resample1, ]
train<- foreste[ixs$Resample1, ]
#le classi rimangono comunque bilanciate sia nel train che nel test
table(test$Cover_Type)
table(train$Cover_Type)

#impongo come factor la variabile target: Cover_type
foreste$Cover_Type<- as.factor((foreste$Cover_Type))
train$Cover_Type<- as.factor(train$Cover_Type)
test$Cover_Type<- as.factor(test$Cover_Type)

#la correlazione tra le variabili numeriche non è molto elevata, quindi non viene eliminata nessuna variabile 
var_factor<- c(11:53)
correlazione<-cor(train[, -var_factor])
round(correlazione,2)
col <- colorRampPalette(c("#8B008B", "#FF1493", "#FFFFFF", "#00CED1", "#104E8B"))
par(mfrow=c(1,1))
corrplot(correlazione, method="color", col=col(200),  
         type="upper", order="original", title = "correlazione tra variabili",
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         # hide correlation coefficient on the principal diagonal
         diag=FALSE
)


#controllo outlier 
boxplot(train[, -var_factor])
boxplot(train$Horizontal_Distance_To_Roadways)
boxplot((train$Horizontal_Distance_To_Fire_Points))
#controllo outlier sulle variabili standardizzate 
boxplot(scale(train[, - var_factor]))

#grafici della densità per le diverse variabili 
#variabile elevation 
ggplot(train, mapping=aes(x=Elevation, y=..density.., color=Cover_Type )) +
  geom_density( alpha=0.5,lwd=1)+
  theme_minimal()+
  scale_color_discrete(name = "Tipo di foresta", labels = c("Spruce/Fir",
                                                            "Lodgepole Pine",
                                                            "Ponderosa Pine",
                                                            "Cottonwood/Willow",
                                                           "Aspen "
                                                            ,"Douglas-Fir",
                                                            "Krummholz"))
#variabile slope
ggplot(train, mapping=aes(x=Slope, y=..density.., color=Cover_Type )) +
  geom_density(lwd=1)+
  theme_minimal()+
  scale_color_discrete(name = "Tipo di foresta", labels = c("Spruce/Fir",
                                                            "Lodgepole Pine",
                                                            "Ponderosa Pine",
                                                            "Cottonwood/Willow",
                                                            "Aspen "
                                                            ,"Douglas-Fir",
                                                            "Krummholz"))
#variabile horizontal distance to hydrology
ggplot(train, mapping=aes(x=Horizontal_Distance_To_Hydrology, y=..density.., color=as.factor(Cover_Type) )) +
  geom_density(lwd=1)+
  theme_minimal()+
  scale_color_discrete(name = "Tipo di foresta", labels = c("Spruce/Fir",
                                                            "Lodgepole Pine",
                                                            "Ponderosa Pine",
                                                            "Cottonwood/Willow",
                                                            "Aspen "
                                                            ,"Douglas-Fir",
                                                            "Krummholz"))
#nella classe 4 ho la maggior parte dei dati che hanno la distanza pari a zero

#variabile Vertical_Distance_To_Hydrology
ggplot(train, mapping=aes(x=Vertical_Distance_To_Hydrology, y=..density.., color=Cover_Type )) +
  geom_density(lwd=1)+
  theme_minimal()+
  scale_color_discrete(name = "Tipo di foresta", labels = c("Spruce/Fir",
                                                            "Lodgepole Pine",
                                                            "Ponderosa Pine",
                                                            "Cottonwood/Willow",
                                                            "Aspen "
                                                            ,"Douglas-Fir",
                                                            "Krummholz"))

#variabile Horizontal_Distance_To_Roadways
ggplot(train, mapping=aes(x=Horizontal_Distance_To_Roadways, y=..density.., color=Cover_Type )) +
  geom_density(lwd=1)+
  theme_minimal()+
  scale_color_discrete(name = "Tipo di foresta", labels = c("Spruce/Fir",
                                                            "Lodgepole Pine",
                                                            "Ponderosa Pine",
                                                            "Cottonwood/Willow",
                                                            "Aspen "
                                                            ,"Douglas-Fir",
                                                            "Krummholz"))


#Boxplot divisi per classi 
ggplot(train, mapping=aes(x=Elevation,  color=Cover_Type)) +
  geom_boxplot()+
  theme_minimal()+
  scale_color_discrete(name = "Tipo di foresta", labels = c("Spruce/Fir",
                                                            "Lodgepole Pine",
                                                            "Ponderosa Pine",
                                                            "Cottonwood/Willow",
                                                            "Aspen "
                                                            ,"Douglas-Fir",
                                                            "Krummholz"))

ggplot(train, mapping=aes(x=Slope, color=Cover_Type )) +
  geom_boxplot()+
  theme_minimal()+
  scale_color_discrete(name = "Tipo di foresta", labels = c("Spruce/Fir",
                                                            "Lodgepole Pine",
                                                            "Ponderosa Pine",
                                                            "Cottonwood/Willow",
                                                            "Aspen "
                                                            ,"Douglas-Fir",
                                                            "Krummholz"))

ggplot(train, mapping=aes(x=Horizontal_Distance_To_Roadways, color=as.factor(Cover_Type ))) +
  geom_boxplot()+
  theme_minimal()+
  scale_color_discrete(name = "Tipo di foresta", labels = c("Spruce/Fir",
                                                            "Lodgepole Pine",
                                                            "Ponderosa Pine",
                                                            "Cottonwood/Willow",
                                                            "Aspen "
                                                            ,"Douglas-Fir",
                                                            "Krummholz"))

ggplot(train, mapping=aes(x=Vertical_Distance_To_Hydrology,color=Cover_Type )) +
  geom_boxplot()+
  theme_minimal()+
  scale_color_discrete(name = "Tipo di foresta", labels = c("Spruce/Fir",
                                                            "Lodgepole Pine",
                                                            "Ponderosa Pine",
                                                            "Cottonwood/Willow",
                                                            "Aspen "
                                                            ,"Douglas-Fir",
                                                            "Krummholz"))

ggplot(train, mapping=aes(x=Hillshade_9am, color=Cover_Type )) +
  geom_boxplot()+
  theme_minimal()+
  scale_color_discrete(name = "Tipo di foresta", labels = c("Spruce/Fir",
                                                            "Lodgepole Pine",
                                                            "Ponderosa Pine",
                                                            "Cottonwood/Willow",
                                                            "Aspen "
                                                            ,"Douglas-Fir",
                                                            "Krummholz"))
train[train$Hillshade_9am<10,] #valore sbagliato per la variabile hillshade alla riga 2242, probabilmente non registrato

ggplot(train, mapping=aes(x=Hillshade_3pm, color=Cover_Type )) +
  geom_boxplot()+
  theme_minimal()+
  scale_color_discrete(name = "Tipo di foresta", labels = c("Spruce/Fir",
                                                            "Lodgepole Pine",
                                                            "Ponderosa Pine",
                                                            "Cottonwood/Willow",
                                                            "Aspen "
                                                            ,"Douglas-Fir",
                                                            "Krummholz"))

ggplot(train, mapping=aes(x=Hillshade_Noon, color=Cover_Type )) +
  geom_boxplot()+
  theme_minimal()+
  scale_color_discrete(name = "Tipo di foresta", labels = c("Spruce/Fir",
                                                            "Lodgepole Pine",
                                                            "Ponderosa Pine",
                                                            "Cottonwood/Willow",
                                                            "Aspen "
                                                            ,"Douglas-Fir",
                                                            "Krummholz"))


#scatterplot per variabili
ggplot(train, mapping=aes( x=Elevation, y=Horizontal_Distance_To_Roadways, color=Cover_Type, palette="rainbow_r")) +
  geom_point(pch=19)+
  theme_minimal()+
  scale_color_discrete(name = "Tipo di foresta", labels = c("Spruce/Fir",
                                                            "Lodgepole Pine",
                                                            "Ponderosa Pine",
                                                            "Cottonwood/Willow",
                                                            "Aspen "
                                                            ,"Douglas-Fir",
                                                            "Krummholz"))

ggplot(train, mapping=aes( x=Aspect, y=Hillshade_3pm, color=Cover_Type)) +
  geom_point()+
  theme_minimal()+
  scale_color_discrete(name = "Tipo di foresta", labels = c("Spruce/Fir",
                                                            "Lodgepole Pine",
                                                            "Ponderosa Pine",
                                                            "Cottonwood/Willow",
                                                            "Aspen "
                                                            ,"Douglas-Fir",
                                                            "Krummholz"))

ggplot(train, mapping=aes( x=Hillshade_3pm, y=Hillshade_Noon, color=Cover_Type)) +
  geom_point()+
  scale_color_discrete(name = "Tipo di foresta", labels = c("Spruce/Fir",
                                                            "Lodgepole Pine",
                                                            "Ponderosa Pine",
                                                            "Cottonwood/Willow",
                                                            "Aspen "
                                                            ,"Douglas-Fir",
                                                            "Krummholz")) + scale_fill_brewer(palette = "Spectral")

ggplot(train_unito, mapping=aes( x=wild,fill=as.factor(Cover_Type))) +
  geom_bar()

#------------------------------------------------------------------------
###########################################
#CLASSIFICATION

#RANDOM FOREST
#utilizzo il dataset con le variabili trasformate per soil e wild 
train_unito$Cover_Type<- as.factor(train_unito$Cover_Type)
train_unito_id<- train_unito
train_unito<- train_unito_id[,-1]

rf <- randomForest(Cover_Type~., data=train_unito,importance=TRUE, ntree=500)
p1 <- predict(rf, train_unito)
confusionMatrix(p1, train_unito$Cover_Type) #ACCURACY PARI A 1

#grafico per indicare importanza delle variabili
imp <- importance(rf)
options(repr.plot.width = 14, repr.plot.height = 10)
varImpPlot(rf, main="Variable Importance")
df_imp<- data.frame(variable=row.names(imp),
                    Importanza= imp[,'MeanDecreaseGini'],
                    accuracy= imp[,'MeanDecreaseAccuracy'])
noquote("Variable Importance Dataframe")
df_imp

#visualizzazione delle tabelle di importanza
options(repr.plot.width = 14, repr.plot.height = 10)
ggplot(df_imp, aes(x=reorder(variable,-Importanza),y=Importanza, #usa come importanza la gini decrease
                   fill=Importanza))+
  geom_col(position="dodge")+
  labs(x="Predittori", y ="Importanza")+
  theme_minimal()+
  theme(plot.title=element_text(face="bold",hjust=0.5),
        legend.position = "right", axis.text.x = element_text(angle=45, hjust=1),
        text=element_text(size=10))+
  scale_fill_continuous(type="viridis")

#nel test non ho soil : soil type 8,9, 25, 28,36
indici<-train_unito_id[train_unito_id$soil=="Soil_Type9",1] [1]
indici<-c(indici,train_unito_id[train_unito_id$soil=="Soil_Type25",1] [1] )
indici<-c(indici,train_unito_id[train_unito_id$soil=="Soil_Type8",1] [1] )
indici<-c(indici,train_unito_id[train_unito_id$soil=="Soil_Type36",1] [1] )
indici<-c(indici,train_unito_id[train_unito_id$soil=="Soil_Type28",1] [1] )

osservazioni<- rbind( train_unito_id[train_unito_id$Id==indici[1],],
                      train_unito_id[train_unito_id$Id==indici[2],],
                      train_unito_id[train_unito_id$Id==indici[3],],
                      train_unito_id[train_unito_id$Id==indici[4],],
                      train_unito_id[train_unito_id$Id==indici[5],])
osservazioni<- osservazioni

test_p<-rbind(test_unito, osservazioni)

p2<-predict(rf, test_p)
conf<-confusionMatrix(p2, as.factor(test_p$Cover_Type))
conf

#accuracy 0.7864
precision<-conf$byClass[,"Specificity"]
recall<-conf$byClass[,"Sensitivity"]
f1score<-(2*precision*recall)/(precision+recall)
round(f1score, 4)

#sbaglia 324 osservazioni su 1512
sum(p2!=test_p$Cover_Type)
#scopriamole:
index<- which(p2!=test_p$Cover_Type)
(test_p[index,11])
p2<- as.numeric(p2)

#Analisi delle osservazioni previste come classe 1 ma in realta appartengono alla classe 2  
#e viceversa con grafico
prova<-cbind(test_p, p2)
#grafico
c12<-prova%>%
  filter(Cover_Type==2 & p2==1)%>%
  mutate(cl=3)
c21<-prova%>%
  filter(Cover_Type==1 & p2==2)%>%
  mutate(cl=4)
ccsbag<-rbind(c12,c21)

c11<-prova%>%
  filter(Cover_Type==1 & p2==1) %>%
  mutate(cl=1)
c22<-prova%>%
  filter(Cover_Type==2 & p2==2)%>%
  mutate(cl=2)
ccgius<-rbind(c11,c22)
cc<-rbind(ccsbag, ccgius)

#grafico delle osservazioni misclassificate
ggplot(cc, mapping=aes( x=Elevation, y=Horizontal_Distance_To_Roadways, color=as.factor(cl))) +
  geom_point(pch=19, size=2)+
  theme_minimal()+
  scale_color_manual( name = "Tipo di foresta", 
                        labels = c("1"="Spruce/Fir", "2"="Lodgepole Pine", 
                                  "3"= "Lodgepole Pine ma classificate Spruce", 
                                   "4"="Spruce ma classificate Lodgepole Pine")
                        ,values = c("1"="lightgreen", "2"="yellow",
                                    "3"="blue2", 
                                    "4"="magenta3") )


#range di variazione
#ELEVATION
#classificati come classe 1 ma che appartengono alla classe 2 
range(c12$Elevation)    #2913 3305
range(train[train$Cover_Type==1,]$Elevation) # 2525 3666
range(train[train$Cover_Type==2,]$Elevation) #2169 3413
median(c12$Elevation) #3110
median(train[train$Cover_Type==1,]$Elevation) #3141
median(train[train$Cover_Type==2,]$Elevation) #2932

#classificati come classe 2 ma che appartengono alla classe 1 
range(c21$Elevation)    #2613 3215
median(c21$Elevation)   #2978

#VERTICAL DISTANCE TO HYDROLOGY
#classificati come classe 1 ma che appartengono alla classe 2 
range(c12$Vertical_Distance_To_Hydrology) #-54 132
range(train[train$Cover_Type==1,]$Vertical_Distance_To_Hydrology) #-97 411
range(train[train$Cover_Type==2,]$Vertical_Distance_To_Hydrology) # -146  554
median(c12$Vertical_Distance_To_Hydrology) #22
median(train[train$Cover_Type==1,]$Vertical_Distance_To_Hydrology) #23
median(train[train$Cover_Type==2,]$Vertical_Distance_To_Hydrology) #32
#classificati come classe 2 ma che appartengono alla classe 1
range(c21$Vertical_Distance_To_Hydrology)    #-40 256
median(c21$Vertical_Distance_To_Hydrology)  #42

#---------------------------------------------------

#SUPPORT VECTOR MACHINE
#utilizzo dataset foreste originario, con le variabili soil e wild binarie  

#AUTO-ML
par.set<- makeParamSet(
  makeDiscreteParam("kernel", values = c( "radial", "polynomial")),   #scegliere tra kernel polinomiale o radiale
  makeNumericParam("cost", lower=-2, upper = 2, trafo = function(x) 10^x),
  makeNumericParam("gamma", lower = -2, upper = 2, trafo = function(x) 10^x),
  makeNumericParam("coef0", lower=-2, upper=2, trafo=function(x) 10^x,  requires = quote(kernel=="polynomial")),
  makeDiscreteParam("degree", values = c(5:7),  requires = quote(kernel=="polynomial"))
)

#controllo di default
#stabilire un numero massimo di configurazioni
ctrl<- makeMBOControl()
ctrl<- setMBOControlTermination(ctrl, iter=20)
#altro tipo di controllo
ctrl<- setMBOControlInfill(ctrl, crit=makeMBOInfillCritEI())
tune.ctrl<- makeTuneControlMBO(mbo.control = ctrl)  
task<- makeClassifTask(data=train, target="Cover_Type") 

run<- tuneParams( makeLearner("classif.svm"), task, cv3, #cv3=funzione di cross-validazione
                  measures = acc, par.set = par.set, 
                  control=tune.ctrl, show.info = T) 
#parametri ottimi trovati 
# kernel=radial; cost=63.9; gamma=0.121
#accuracy=0.8256908

#provo questi paramentri stimando il modello
#scalo i dati
train_s<- as.data.frame(cbind(scale(train[, -53]), Cover_Type=train$Cover_Type))
train_s$Cover_Type<- as.factor(train_s$Cover_Type)

set.seed(123)
ixs<- createFolds(y=train_s$Cover_Type, k=10, list=T)  

#salvo accuratezza del training e del validation set
trnAcc <- valAcc<- numeric(length(ixs))
for (k in 1:length(ixs)){
  trnFold<- train_s[-ixs[[k]], ]
  valFold<- train_s[ixs[[k]], ]
  
  # training the model (SVM)
  model<- svm(Cover_Type~., data=trnFold, scale=F,
              type="C-classification", cost=63.9, kernel="radial",  gamma=0.121)
  #accuratezza
  cat("* confusion matrix on the training fold:\n")
  print(table(trnFold$Cover_Type, model$fitted))
  trnAcc[k]<- mean(trnFold$Cover_Type==model$fitted)
  
  # validation del modello 
  preds<- predict(model, newdata=valFold)
  #accuratezza
  cat("+ confusion matrix on the validation fold:\n")
  print(table(valFold$Cover_Type, preds))
  valAcc[k]<- mean(valFold$Cover_Type==preds)
}

mean(valAcc)
mean(trnAcc)


plot(trnAcc, type="o", pch=19, lwd=2, col="green", ylab="Accuracy", xlab="fold",
     ylim=range(c(1, trnAcc, valAcc)))
lines(valAcc, type="o", pch=19, lwd=2, col="orange") 
abline(h=1, col="blue", lty=2, lwd=2) 
legend("topright", legend=c("training", "validation"), col=c("green", "orange"),
       lwd=2, pch=19, cex=1.3)

#unisco tutto il train e provo il modello
modello_train_completo<- model<- svm(Cover_Type~., data=train, scale=T,
                                     type="C-classification", cost=63.9, kernel="radial",  gamma=0.121)
#accuracy media sul train completo
fullDsAcc<- mean(train$Cover_Type==modello_train_completo$fitted) #0.95995

cat(">Empirical error:", round(1-fullDsAcc, 4))
cat(">Generalization Errror:", (1-mean(valAcc)))


#applico il modello al Test set
pred_test<-predict(modello_train_completo, newdata=test)
table(test$Cover_Type, pred_test)
conf<-confusionMatrix(test$Cover_Type, pred_test)
acc_test<- mean(test$Cover_Type==pred_test)
acc_test

#f1 score
precision<-conf$byClass[,"Specificity"]
recall<-conf$byClass[,"Sensitivity"]
f1score<-(2*precision*recall)/(precision+recall)

round(f1score,4)
#sbaglia soprattutto a classificare la classe 6 e 3, le confonde: 3 - Ponderosa Pine e 6 - Douglas-fir
#e la classe 1 e 2 :1 - Spruce/Fir e 2 - Lodgepole Pine

#numero di support vector
modello_train_completo$tot.nSV #ho poche support vector rispetto alle istanze

#-----------------------------------------------------------------

###RETI NEURALI
library(keras)
install_keras()

#utilizzo dataset con sole variabili quantitative
#test_unito<-test_unito[,-1]
train_nn<-train_unito[,-c(12,13)]
test_nn<-test_unito[,-c(12,13)]

x_train <- as.matrix(scale(train_nn[,-11]))
y_train <-as.numeric(train_nn[,11])-1
x_test <- as.matrix(scale(test_nn[,-11]))
y_test <- as.numeric(test_nn[,11])-1

y_train <- to_categorical(y_train, 7)
y_test <- to_categorical(y_test, 7)

model <- keras_model_sequential() %>%
  layer_dense(units = 50, activation = 'relu', input_shape = c(10)) %>%
  layer_dropout(rate = 0.4) %>% #learning rate
  layer_dense(units = 50, activation = 'relu')%>%
  layer_dropout(rate = 0.4) %>%
  layer_dense(units = 7, activation = 'softmax')

# Compila il modello
model %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = "adam",
  metrics = c('accuracy')
)
history <- model %>% fit(
  x_train, y_train,
  batch_size = 30,
  epochs = 100,
  validation_split = 0.2
)

#con epoch=100, 2 layer ognuno da 50, dropout=0.4 
#accuracy e loss training: loss: 0.7238 - accuracy: 0.7009 
#accuracy e loss validation: val_loss: 0.8376 - val_accuracy: 0.5889

score <- model %>% evaluate(x_test, y_test) #loss: 0.6690 - accuracy: 0.7044

predictions <- as.array(model %>% predict(x_test)%>% k_argmax())
#pred<-round(predict(model, x_test), 7)
actual_classes <- apply(y_test, 1, which.max) - 1
conf<-confusionMatrix(as.factor(predictions), as.factor(actual_classes)) # test accuracy del 0.7044
#f1 score
precision<-conf$byClass[,"Specificity"]
recall<-conf$byClass[,"Sensitivity"]
f1score<-(2*precision*recall)/(precision+recall)
f1score
round(f1score,4)

#diagnostica reti neurali
weights <- model %>% get_weights()
print(weights) #non c'è nessuno zero, non ho neuroni morti
summary(model)

#----------------------------------------
############################################

#CLUSTERING
#DATASET COMPLETO: utilizzo il dataset con 581012 osservazioni, preso da UCI repositery
dati_comp<-read.table(gzfile("C:/Users/giuli/OneDrive/Desktop/covtype.data.gz" ), sep=",")
#ricarico foreste perche cosi mi corrispondono le colonne
foreste<-read.csv("C:/Users/giuli/OneDrive/Desktop/Università/4anno/Machine Learning/Progetto/FORESTE.csv")
colnames(dati_comp)<-colnames(foreste[,-1]) 
dati_comp$Cover_Type<-as.factor(dati_comp$Cover_Type)
table(dati_comp$Cover_Type)
#ricreo il dataset con le variabili categoriali "wild" contenente le 4 aree selvagge
#e la variabile "soil" con 40 livelli
prova<-dati_comp %>%
  gather(`Soil_Type1`, `Soil_Type2`, `Soil_Type3`, `Soil_Type4`,
         `Soil_Type5`, `Soil_Type6`, `Soil_Type7`, `Soil_Type8`,
         `Soil_Type9`, `Soil_Type10`, `Soil_Type11`, `Soil_Type12`,
         `Soil_Type13`, `Soil_Type14`, `Soil_Type15`, `Soil_Type16`,
         `Soil_Type17`, `Soil_Type18`, `Soil_Type19`, `Soil_Type20`,
         `Soil_Type21`, `Soil_Type22`, `Soil_Type23`, `Soil_Type24`,
         `Soil_Type25`, `Soil_Type26`, `Soil_Type27`, `Soil_Type28`,
         `Soil_Type29`, `Soil_Type30`, `Soil_Type31`, `Soil_Type32`,
         `Soil_Type33`, `Soil_Type34`, `Soil_Type35`, `Soil_Type36`,
         `Soil_Type37`, `Soil_Type38`, `Soil_Type39`, `Soil_Type40`,
         key = "soil", value = "cases")

pp<-prova%>%
  filter(cases==1 )
pp1<-pp[, -18]
pp2<-pp1%>%
  gather(`Wilderness_Area1`, `Wilderness_Area2`, `Wilderness_Area3`,
         `Wilderness_Area4`,
         key = "wild", value = "cc")
pp3<-pp2%>%
  filter(cc==1 )
pp4<-pp3[,-15]

comp_unito<-pp4
comp_unito$soil<-as.factor(comp_unito$soil)
comp_unito$wild<-as.factor(comp_unito$wild)
str(comp_unito)

table(comp_unito$Cover_Type)
table(comp_unito$soil)
table(comp_unito$wild)

#creo datatset bilanciato per wild
wild1<-comp_unito%>%
  filter(wild=="Wilderness_Area1")
wild2<-comp_unito%>%
  filter(wild=="Wilderness_Area2")
wild3<-comp_unito%>%
  filter(wild=="Wilderness_Area3")
wild4<-comp_unito%>%
  filter(wild=="Wilderness_Area4")
ind1<-wild1[sample(1:nrow(wild1),size=2000),]
ind2<-wild2[sample(1:nrow(wild2),size=2000),]
ind3<-wild3[sample(1:nrow(wild3),size=2000),]
ind4<-wild4[sample(1:nrow(wild4),size=2000),]
wild_b<-rbind(ind1,ind2,ind3,ind4)

#calcolo le coordinate in formato UTM per ogni punto sul dataset bilanciato rispetto alle wildness area:wild_b
#tolgo la variabile cases da wild_b
wild_b<- wild_b[, -13]

#coordinate UTM di ogni wild area
longitudine<- c(415520.446, 428858.895,440347.390,462673.755)
latitudine<-c(4525288.676, 4486171.888, 4504378.511, 4504657.994)

dati<- as.data.frame(cbind(longitudine, latitudine), nrow = 4, ncol = 2 )
colnames(dati)<- c("longitudine", "latitudine")
rownames(dati)<- c("Rawah", "Neota", "Comanche Peak", "Cache la Poudre")

sloper<- vector()
aspenr<- vector()
#calcolo in radianti si slope e aspen
for ( i in 1:dim(wild_b)[1]){
  sloper[i]<- wild_b$Slope[i] * (pi/180)
  aspenr[i]<- wild_b$Aspect[i]*(pi/180)
}
sloper<- as.matrix(sloper)
aspenr<- as.matrix(aspenr)
wild_b2<- cbind(wild_b, sloper,aspenr)

#calcolo coordinate utilizzando come punto iniziale le coordinate delle varie aree wild
#divido il dataset per le varie aree
wild1<- wild_b2[wild_b2$wild=="Wilderness_Area1", ]
wild2<- wild_b2[wild_b2$wild=="Wilderness_Area2", ]
wild3<- wild_b2[wild_b2$wild=="Wilderness_Area3", ]
wild4<- wild_b2[wild_b2$wild=="Wilderness_Area4", ]

#per ogni dataset diviso calcolo le coordinate di ogni punto
#wild 1 area Rawah
dist_oriz<- vector()
nord<- vector()
est<-vector()
latitud<- vector()
longi<- vector()
for(i in 1:dim(wild1)[1]){
  dist_oriz[i]<- wild1$Elevation[i]*tan(wild1$sloper[i])
  nord[i]<- dist_oriz[i] * cos(wild1$aspenr[i])
  est[i]<-dist_oriz[i] * sin(wild1$aspenr[i])
  latitud[i]<- dati[1,2] + nord[i]
  longi[i]<- dati[1,1] + est[i]
}

latitud<- as.matrix(latitud)
longi<- as.matrix(longi)
wild11<- cbind(wild1, latitud, longi)

#wild2
dist_oriz<- vector()
nord<- vector()
est<-vector()
latitud<- vector()
longi<- vector()
for(i in 1:dim(wild2)[1]){
  dist_oriz[i]<- wild2$Elevation[i] * tan(wild2$sloper[i])
  nord[i]<- dist_oriz[i] * cos(wild2$aspenr[i])
  est[i]<- dist_oriz[i] * sin(wild2$aspenr[i])
  latitud[i]<- dati[2,2] + nord[i]
  longi[i]<- dati[2,1] + est[i]
}

latitud<- as.matrix(latitud)
longi<- as.matrix(longi)
wild22<- cbind(wild2, latitud, longi)

#wild3
dist_oriz<- vector()
nord<- vector()
est<-vector()
latitud<- vector()
longi<- vector()
for(i in 1:dim(wild3)[1]){
  dist_oriz[i]<- wild3$Elevation[i] * tan(wild3$sloper[i])
  nord[i]<- dist_oriz[i] * cos(wild3$aspenr[i])
  est[i]<- dist_oriz[i] * sin(wild3$aspenr[i])
  latitud[i]<- dati[3,2] + nord[i]
  longi[i]<- dati[3,1] + est[i]
}

latitud<- as.matrix(latitud)
longi<- as.matrix(longi)
wild33<- cbind(wild3, latitud, longi)

#wild4
dist_oriz<- vector()
nord<- vector()
est<-vector()
latitud<- vector()
longi<- vector()
for(i in 1:dim(wild4)[1]){
  dist_oriz[i]<- wild4$Elevation[i] *tan(wild4$sloper[i])
  nord[i]<- dist_oriz[i] * cos(wild4$aspenr[i])
  est[i]<- dist_oriz[i] * sin(wild4$aspenr[i])
  latitud[i]<- dati[4,2] + nord[i]
  longi[i]<- dati[4,1] + est[i]
}

latitud<- as.matrix(latitud)
longi<- as.matrix(longi)
wild44<- cbind(wild4, latitud, longi)

#unicsco il dataset
wild<- rbind(wild11, wild22, wild33, wild44)

#controllo la correlazione tra le variabili numeriche di wild
str(wild)
num<- wild[, -c(7,8,9,11,12,13)]
correlazione<-cor(num)
round(correlazione,2)

#CLUSTERING
#clustering considerando le aree wild, tenendo conto delle coordinate geografiche in forma UTM
#tolgo le variabili slope e aspect, anche quelle calcolate in radianti, visto che sono le meno significative
num1<- num[, -c(2,3,8,9)]

#KMEANS
set.seed(1) #scalo i dati 
num1_scale<- as.data.frame(scale(num1)) 
km.res<- kmeans(num1_scale, centers = 4,iter.max = 100)
table(km.res$cluster)

prova<- cbind(wild, km.res$cluster)
prova %>%
  ggplot(mapping=(aes(x=km.res$cluster, fill=wild )))+
  geom_bar(stat="count")+
  theme_minimal()+
  scale_fill_manual(values = c("Wilderness_Area1"="red2", "Wilderness_Area2"="blue3", "Wilderness_Area3"="green3", "Wilderness_Area4"="orange") ,
                    labels=c("Wilderness_Area1"="Rawah", "Wilderness_Area2"="Neota", "Wilderness_Area3"="Comanche Peak", "Wilderness_Area4"="Cauche la Poudre"))+
  labs(x="Cluster", y="Frequenze assolute", fill="Aree selvagge")

D<- dist(num1_scale)^2
sil<-silhouette(km.res$cluster, dist=D)
summary(sil)$avg.width # 0.4854
plot(sil)
plot(num1_scale$longi, num1_scale$latitud, col=km.res$cluster, pch=19)

#controllo la distribuzione delle varibili 
u<-cbind(num1_scale, wild$wild)
ggplot(u, mapping=aes(x=Horizontal_Distance_To_Roadways, y=..density.., color=wild$wild )) +
  geom_density( alpha=0.5,lwd=1)+
  theme_minimal()+
  scale_color_discrete(name = "Aree selvagge", labels = c("Rawah",
                                                          "Neota",
                                                          "Comanche Peak",
                                                          "Cauche la Poudre"))

ggplot(u, mapping=aes(x=Elevation, y=..density.., color=wild$wild )) +
  geom_density( alpha=0.5,lwd=1)+
  theme_minimal()+
  scale_color_discrete(name = "Aree selvagge", labels = c("Rawah",
                                                          "Neota",
                                                          "Comanche Peak",
                                                          "Cauche la Poudre"))

ggplot(u, mapping=aes(x=Horizontal_Distance_To_Fire_Points, y=..density.., color=wild$wild )) +
  geom_density( alpha=0.5,lwd=1)+
  theme_minimal()+
  scale_color_discrete(name = "Aree selvagge", labels = c("Rawah",
                                                          "Neota",
                                                          "Comanche Peak",
                                                          "Cauche la Poudre"))
ggplot(u, mapping=aes(x=longi, y=..density.., color=wild$wild )) +
  geom_density( alpha=0.5,lwd=1)+
  theme_minimal()+
  scale_color_discrete(name = "Aree selvagge", labels = c("Rawah",
                                                          "Neota",
                                                          "Comanche Peak",
                                                          "Cauche la Poudre"))

ggplot(u, mapping=aes(x=latitud, y=..density.., color=wild$wild )) +
  geom_density( alpha=0.5,lwd=1)+
  theme_minimal()+
  scale_color_discrete(name = "Aree selvagge", labels = c("Rawah",
                                                          "Neota",
                                                          "Comanche Peak",
                                                          "Cauche la Poudre"))

#----------------------------------------------------------------------------
#EM
set.seed(123)
emobj<- init.EM(num1_scale, nclass=4)
em.res<- emcluster(num1_scale, emobj , assign.class = T)
em.res

prova<- cbind(wild, em.res$class)

prova %>% ggplot(mapping=(aes(x=em.res$class, fill=wild )))+
  geom_bar(stat="count")+ theme_minimal()+ scale_fill_manual(values =
                                                               c("Wilderness_Area1"="red2",
                                                                 "Wilderness_Area2"="blue3",
                                                                 "Wilderness_Area3"="green3",
                                                                 "Wilderness_Area4"="orange") ,
                                                             labels=c("Wilderness_Area1"="Rawah",
                                                                      "Wilderness_Area2"="Neota",
                                                                      
                                                                      "Wilderness_Area3"="Comanche peak",
                                                                      
                                                                      "Wilderness_Area4"="Cauche la Poudre")) + labs(x="Cluster",
                                                                                                                     y="Frequenze assolute", fill= " Aree selvagge")
#silhouette
D<- dist(num1_scale)^2
sil<-silhouette(em.res$class, dist=D)
summary(sil)$avg.width  #0.2657284
plot(sil)
par(mfrow=c(1,1))
plot(num1_scale$longi, num1_scale$latitud, col=em.res$class)
plot(num1_scale$Vertical_Distance_To_Hydrology, num1_scale$Horizontal_Distance_To_Hydrology, col=em.res$class)
plot(num1_scale$Vertical_Distance_To_Hydrology, num1_scale$Horizontal_Distance_To_Hydrology, col=prova$wild)

prob<-(e.step( num1_scale, em.res))$Gamma
prob
zi_j<- round(prob)+0.0001
EN<- - sum(zi_j*log(zi_j))
EN/(nrow(num1_scale)*log(3)) 

#-----------------------------------------------------------------
#DBSCAN 
kNNdistplot(num1_scale, k=4)
dbs.res<- dbscan(num1_scale, eps=0.9, minPts = 14 )
str(wild)
prova<- cbind(wild, cluster=dbs.res$cluster)
#GRAFICI DBSCAN
prova %>% ggplot(mapping=(aes(x=dbs.res$cluster, fill=wild )))+
  geom_bar(stat="count")+
   theme_minimal()+
  scale_fill_manual(values = c("Wilderness_Area1"="red2", "Wilderness_Area2"="blue3",
                               "Wilderness_Area3"="green3", "Wilderness_Area4"="orange") , 
                    labels=c("Wilderness_Area1"="Rawah", "Wilderness_Area2"="Neota", 
                             "Wilderness_Area3"="Comanche peak", 
                             "Wilderness_Area4"="Cauche la Poudre"))+
  labs(x="Cluster", y="Frequenze assolute", fill="Aree selvagge")

colori<- as.factor(dbs.res$cluster)
prova$cluster<-as.factor(prova$cluster)
levels(prova$cluster)<-c("5", "1", "2" ,"3" ,"4")
ggplot(data=prova, mapping=aes(x=longi, y=latitud, color=cluster))+
  geom_point(size=2, shape=20)+
  theme_minimal()+
  scale_color_manual(values = c( "5"="black","1"="red2", "2"="blue3",
                               "3"="green3", "4"="orange") , 
                    labels=c( "1"="Rawah", "2"="Neota", 
                             "3"="Comanche peak", "4"="Cauche la Poudre", "5"="Noise Points"))+
  
  labs(x="Longitudine", y="Latitudine", color="Aree selvagge")

##FINE CLUSTERING
###################################
#RISOLUZIONE ERRORI

#RANDOM FOREST
#CLASSIFICATION con solo le classi 3 e 6
train_unito<-train_unito[,-1]
cl3<-train_unito%>%
  filter(Cover_Type==3)%>%
  mutate(classe=3)%>%
  select(-Cover_Type)

cl6<-train_unito%>%
  filter(Cover_Type==6)%>%
  mutate(classe=6)%>%
  select(-Cover_Type)

cl3_6<-rbind(cl3, cl6)
cl3_6$classe<-as.factor(cl3_6$classe)

rf<-randomForest(classe~., data=cl3_6, importance=T, ntree=500)
p1 <- predict(rf, cl3_6)
confusionMatrix(p1, cl3_6$classe)

#prova sul test
test_unito<-test_p[,-1]
cl3t<-test_p%>%
  filter(Cover_Type==3)%>%
  mutate(classe=3)%>%
  select(-Cover_Type)

cl6t<-test_p%>%
  filter(Cover_Type==6)%>%
  mutate(classe=6)%>%
  select(-Cover_Type)

cl3_6t<-rbind(cl3t, cl6t)
cl3_6t$classe<-as.factor(cl3_6t$classe)
p2 <- predict(rf, cl3_6t)
confusionMatrix(p2, cl3_6t$classe)
as.numeric(p2)

ind<-which(p==TRUE)
p<-as.numeric(p2)!=as.numeric(cl3_6t$classe)
p<-as.vector(p)
p

#CLASSIFICATION con solo le classi 1 e 2
library(tidyverse)
cl1<-train_unito%>%
  filter(Cover_Type==1)%>%
  mutate(classe=1)

cl2<-train_unito%>%
  filter(Cover_Type==2)%>%
  mutate(classe=2)%>%
  select()

cl1_2<-rbind(cl1, cl2)
cl1_2$classe<-as.factor(cl1_2$classe)
library(randomForest)
rf<-randomForest(classe~., data=cl1_2, importance=T, ntree=500)
p1 <- predict(rf, cl1_2)
library(caret)
confusionMatrix(p1, cl1_2$classe)

#prova sul test

cl1t<-test_p%>%
  filter(Cover_Type==1)%>%
  mutate(classe=1)%>%
  select(-Cover_Type)

cl2t<-test_p%>%
  filter(Cover_Type==2)%>%
  mutate(classe=2)%>%
  select(-Cover_Type)

cl1_2t<-rbind(cl1t, cl2t)
cl1_2t$classe<-as.factor(cl1_2t$classe)
p2 <- predict(rf, cl1_2t)
confusionMatrix(p2, cl1_2t$classe)
#sul test ho accuracy di 0.7926


#SUPPORT VECTOR MACHINE
#CLASSI 3 E 6
cl3_6_num<-cl3_6%>% select(-soil, -wild)
str(cl3_6_num)
#AUTO-ML
par.set<- makeParamSet(
  makeDiscreteParam("kernel", values = c( "radial", "polynomial")),   #scegliere tra kernel lineare o radiale
  makeNumericParam("cost", lower=-2, upper = 1, trafo = function(x) 10^x), #parametro C di regolarizzazione (continuo), va da 0 a 100, ma qui impongo il logaritmo
  makeNumericParam("gamma", lower = -2, upper = 1, trafo = function(x) 10^x),
  makeNumericParam("coef0", lower=-2, upper=2, trafo=function(x) 10^x,  requires = quote(kernel=="polynomial")),
  makeDiscreteParam("degree", values = c(1:7),  requires = quote(kernel=="polynomial"))
)


#stabilire
#controllo di default
#stabilire un numero massimo di configurazioni
ctrl<- makeMBOControl()
ctrl<- setMBOControlTermination(ctrl, iter=20) #massimo numero di iterazioni per ottimizzare=15
#posso cambiare il controllo
ctrl<- setMBOControlInfill(ctrl, crit=makeMBOInfillCritEI())


#controllo per l'esperimento interno, algoritmo+dataset
tune.ctrl<- makeTuneControlMBO(mbo.control = ctrl)  
task<- makeClassifTask(data=cl3_6_num, target="classe") #quello che voglio classificare

#fa 15 volte l'algoritmo per trovare la migliore SVM
run<- tuneParams( makeLearner("classif.svm"), task, cv3, #cv3=funzione di cross-validazione
                  measures = acc, par.set = par.set, control=tune.ctrl, show.info = T) #


#parametri ottimi PER CLASSI 3 E 6 
#kernel=radial; cost=47.4, gamma=0.46 
#acc.test.mean=0.8410494

#provo questi paramentri stimando il modello

ixs<- createFolds(y=cl3_6_num$classe, k=10, list=T) #k=numero di fold che voglio,

#salvo accuratezza di train e set, avrò k=10 accuracy
trnAcc <- valAcc<- numeric(length(ixs))
for (k in 1:length(ixs)){
  trnFold<- cl3_6_num[-ixs[[k]], ]
  valFold<- cl3_6_num[ixs[[k]], ]
  
  # training the model (SVM)
  model<- svm(classe~., data=trnFold, scale=T,
              type="C-classification", cost=47.4, gamma=0.46, kernel="radial")  #iperparametro:C = cost=1 (defalt)
  #accuratezza
  cat("* confusion matrix on the training fold:\n")
  print(table(trnFold$classe, model$fitted))
  trnAcc[k]<- mean(trnFold$classe==model$fitted)
  
  # validating the model
  preds<- predict(model, newdata=valFold)
  #accuratezza
  cat("+ confusion matrix on the validation fold:\n")
  print(table(valFold$classe, preds))
  valAcc[k]<- mean(valFold$classe==preds)
}


plot(trnAcc, type="o", pch=19, lwd=2, col="green", ylab="Accuracy", xlab="fold",
     ylim=range(c(1, trnAcc, valAcc)))
lines(valAcc, type="o", pch=19, lwd=2, col="orange") #valAcc è quasi sempre minore di trnAcc
abline(h=1, col="blue", lty=2, lwd=2) #valore massimo dell'accuratezza
legend("topright", legend=c("training", "validation"), col=c("green", "orange"),
       lwd=2, pch=19, cex=1.3)
model<- svm(classe~., data=cl3_6_num, scale=T,
            type="C-classification", kernel="radial", cost=47.4, gamma=0.46)
model$tot.nSV #metà delle oss

# kernel="radial", cost=47.4, gamma=0.46 con accuracy=0.84
#paramteri tirati fuori da autoML anche se non sono i mgiliori perche
#mi overfittavano con una cost troppo bassa dato che mi faceva 
#3500 support vector su 3800

#applico il modello al Test set
pred_test<-predict(model, newdata=cl3_6t[,-1])
table(cl3_6t$classe, pred_test)
acc_test<- mean(cl3_6t$classe==pred_test)

#ACCURACY SUL TEST 0.86 per classe 3 e 6

#--CLASSE 1 E 2 SVM

cl1_2_num<-cl1_2%>% select(-soil, -wild)
str(cl1_2_num)

#parametri ottimi PER CLASSI 1 e 2 uso quelli ottimi per classe 3 e 6
#cost=47.4, gamma=0.46, kernel="radial"

ixs<- createFolds(y=cl1_2_num$classe, k=10, list=T) #k=numero di fold che voglio,
library(svm)
#salvo accuratezza di train e set, avrò k=10 accuracy
trnAcc <- valAcc<- numeric(length(ixs))
for (k in 1:length(ixs)){
  trnFold<- cl1_2_num[-ixs[[k]], ]
  valFold<- cl1_2_num[ixs[[k]], ]
  
  # training the model (SVM)
  model<- svm(classe~., data=trnFold, scale=T,
              type="C-classification", cost=47.4, gamma=0.46, kernel="radial")  #iperparametro:C = cost=1 (defalt)
  #accuratezza
  cat("* confusion matrix on the training fold:\n")
  print(table(trnFold$classe, model$fitted))
  trnAcc[k]<- mean(trnFold$classe==model$fitted)
  
  # validating the model
  preds<- predict(model, newdata=valFold)
  #accuratezza
  cat("+ confusion matrix on the validation fold:\n")
  print(table(valFold$classe, preds))
  valAcc[k]<- mean(valFold$classe==preds)
}


plot(trnAcc, type="o", pch=19, lwd=2, col="green", ylab="Accuracy", xlab="fold",
     ylim=range(c(1, trnAcc, valAcc)))

lines(valAcc, type="o", pch=19, lwd=2, col="orange") #valAcc è quasi sempre minore di trnAcc
abline(h=1, col="blue", lty=2, lwd=2) #valore massimo dell'accuratezza
legend("topright", legend=c("training", "validation"), col=c("green", "orange"),
       lwd=2, pch=19, cex=1.3)
model<- svm(classe~., data=cl1_2_num, scale=T,
            type="C-classification", kernel="radial", cost=47.4, gamma=0.46)
model$tot.nSV #2080 su 3888


#applico il modello al Test set
pred_test<-predict(model, newdata=cl1_2t)
table(cl1_2t$classe, pred_test)
acc_test<- mean(cl1_2t$classe==pred_test)
#0.7626728


