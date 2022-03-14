rm(list=ls());gc()

library(caret)
library(ggplot2)
# library(GetoptLong)
# library(randomForest)
library(plyr)

# functions_path<-
#   "C:/Users/rsuarez/Dropbox/R_Proyects/Functions/"

# source(qq("@{functions_path}pro_correlation.R"))

df_ss<-
  read.csv(file= "C:/Users/rsuarez/Downloads/BD_Sueldos_Automotriz.csv",
           stringsAsFactors = F)

table(complete.cases(df_ss))

df_ss[!complete.cases(df_ss),]

df_ss<-
  df_ss[complete.cases(df_ss),]


# pro_correlation(df_ss,T)
# 
# table_correlations[table_correlations$Var1%in%"U",]
# table_correlations[table_correlations$Var2%in%"U",]
# 
# table_correlations[table_correlations$Var1%in%"costo_unitario_obreros",]
# table_correlations[table_correlations$Var2%in%"costo_unitario_obreros",]

# a kind of strange, we shoul classify on binary

xtabs(~round(U)+year,df_ss)

df_U_qtl<-
  ddply(.data = df_ss,
        .variables = ~year,
        .fun = function(x){
          quantile(x[,"U"],.5)
        })

names(df_U_qtl)<-
  c("year","qtl")

df_ss<-
  merge(df_ss,df_U_qtl,by = "year")

df_ss$U_class<-
  factor((df_ss$U>df_ss[,"qtl"])*1)


# fitsalary<- lm(wo ~ yt - u + t, na.action=na.exclude)
# summary (fitsalary)

formula<-
  U_class~
  prod_real+
  productividad_horas+
  salario_real_manager+
  salario_real_obreros+
  costo_unitario_manager+
  costo_unitario_obreros+
  productividad_personal+
  prod_real:productividad_horas+
  prod_real:salario_real_manager+
  prod_real:salario_real_obreros+
  costo_unitario_manager:prod_real+
  costo_unitario_obreros:prod_real+
  productividad_horas:salario_real_manager+
  # salario_real_obreros:salario_real_manager+
  costo_unitario_manager:productividad_horas+
  costo_unitario_obreros:productividad_horas+
  productividad_horas:productividad_personal+
  costo_unitario_manager:salario_real_manager+
  costo_unitario_manager:salario_real_obreros+
  costo_unitario_obreros:salario_real_manager+
  costo_unitario_obreros:salario_real_obreros+
  productividad_personal:salario_real_manager+
  # productividad_personal:salario_real_obreros+
  costo_unitario_manager:productividad_personal+
  costo_unitario_obreros:costo_unitario_manager+
  costo_unitario_obreros:productividad_personal+
  prod_real:productividad_horas:productividad_personal+
  # prod_real:productividad_personal:salario_real_obreros+
  costo_unitario_manager:prod_real:productividad_personal+
  costo_unitario_manager:productividad_horas:salario_real_manager+
  costo_unitario_obreros:productividad_horas:salario_real_obreros+
  # productividad_horas:productividad_personal:salario_real_obreros+
  costo_unitario_manager:productividad_horas:productividad_personal+
  costo_unitario_obreros:costo_unitario_manager:productividad_horas+
  costo_unitario_obreros:productividad_horas:productividad_personal+
  costo_unitario_obreros:costo_unitario_manager:salario_real_obreros+
  costo_unitario_obreros:productividad_personal:salario_real_manager


df_TRAIN<-
  df_ss[df_ss$year<2014,]

df_TEST<-
  df_ss[df_ss$year>2009,]


trainGrid <-  expand.grid(interaction.depth = c(2),
                          n.trees = 35,
                          shrinkage = 0.35,
                          n.minobsinnode = 15
)


train_Cont <- trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 1)

set.seed(911)

m_ss <- train(formula,
             data = df_TRAIN, 
             # method = "multinom",
             method = "gbm",
             trControl = train_Cont,
             tuneGrid = trainGrid )



# HOW DOES MODEL LOOKS ####

df_TRAIN$U_class<-
  factor(df_TRAIN$U_class, levels = c("1","0"))


df_TRAIN$ScoredProbability<-
  predict(m_ss,df_TRAIN,"prob",na.action = na.pass)[,1]

ggplot(df_TRAIN, aes(x=ScoredProbability)) + 
  geom_density(aes(group=U_class, 
                   colour=U_class, 
                   fill=U_class), 
               alpha=0.3)+ 
  scale_fill_manual(values=c("#4876FF", "#B9D3EE"))


df_TRAIN$deciles<-
  cut(x = df_TRAIN$ScoredProbability,
      breaks = 
        quantile(df_TRAIN$ScoredProbability,
                 seq(0,1,.2),na.rm = T),
      labels = 1:5,include.lowest = T)

table_weekly<-
  xtabs(~deciles+U_class,data = df_TRAIN)

table_weekly<-
  round(prop.table(table_weekly,1)*100,2)

table_weekly

ggplot(as.data.frame(table_weekly), 
       aes(x=deciles, y = Freq, fill=U_class)) + 
  geom_bar(stat="identity") + 
  scale_fill_manual(values=c("#4876FF", "#B9D3EE")) + 
  geom_hline(yintercept =(prop.table(table(df_TRAIN$U_class))*100)[1],
             col="#FF8C00")




# TEST ####

df_TEST$U_class<-
  factor(df_TEST$U_class, levels = c("1","0"))


df_TEST$ScoredProbability<-
  predict(m_ss,df_TEST,"prob",na.action = na.pass)[,1]

ggplot(df_TEST, aes(x=ScoredProbability)) + 
  geom_density(aes(group=U_class, 
                   colour=U_class, 
                   fill=U_class), 
               alpha=0.3)+ 
  scale_fill_manual(values=c("#4876FF", "#B9D3EE"))


df_TEST$deciles<-
  cut(x = df_TEST$ScoredProbability,
      breaks = 
        quantile(df_TEST$ScoredProbability,
                 seq(0,1,.20),na.rm = T),
      labels = 1:5,include.lowest = T)

table_weekly<-
  xtabs(~deciles+U_class,data = df_TEST)

table_weekly<-
  round(prop.table(table_weekly,1)*100,2)

table_weekly

ggplot(as.data.frame(table_weekly), 
       aes(x=deciles, y = Freq, fill=U_class)) + 
  geom_bar(stat="identity") + 
  scale_fill_manual(values=c("#4876FF", "#B9D3EE")) + 
  geom_hline(yintercept =(prop.table(table(df_TEST$U_class))*100)[1],
             col="#FF8C00")

# The method has change a liitle bit, now we are saying:
# P[x E Q(i)]=P(i)[Q[5]]

range_TRAIN<-
  as.data.frame(
    ddply(df_TRAIN,
          ~year,
          function(x) quantile(x[,"U"],seq(0,1,.2))
          )
    )


range_TEST<-
  as.data.frame(
    ddply(df_TEST,
          ~year,
          function(x) quantile(x[,"U"],seq(0,1,.2))
    )
  )

names(range_TRAIN)<-
  c("year","min","f1","f2","f3","f4","max")

names(range_TEST)<-
  c("year","min","f1","f2","f3","f4","max")


df_TEST$U_q<-
  cut(df_TEST$U,range_TEST[,2:7],1:5,include.lowest = T)


round(prop.table(xtabs(~deciles+U_q,df_TEST),1)*100,2)
