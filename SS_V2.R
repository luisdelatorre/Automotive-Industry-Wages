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

df_ss<-
  df_ss[!df_ss$city%in%"00 Total Nacional",]

df_ss$tecnologia<-
  factor(df_ss$tecnologia)

df_ss$city[df_ss$city%in%"005 Apaseo el grande"]<-
  "005 Apaseo el Grande"

df_ss$city[df_ss$city%in%c("028 San luis potosí",
                           "24 San Luis Potosí",
                           "25 San Luis Potosí")]<-
  "028 San Luis Potosí"

df_ss$city[df_ss$city%in%"21 Puebla"]<-
  "114 Puebla"

df_ss$city[df_ss$city%in%"22 Querétaro"]<-
  "23 Querétaro"

df_ss$city[df_ss$city%in%"01 Aguascalientes"]<-
  "001 Aguascalientes"

xtabs(~city+U,df_ss)
  
df_ss$city<-
  factor(df_ss$city)

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
          quantile(unique(x[,"U"]),seq(0,1,.2))
          # quantile(x[,"U"],seq(0,1,.2))
        })

names(df_U_qtl)<-
  c("year","min","f1","f2","f3","f4","max")


df_ss$U_class<-
  apply(df_ss,
        1,
        function(x){
          cut(as.numeric(x["U"]),
              breaks = df_U_qtl[df_U_qtl[,1]%in%as.numeric(x["year"]),2:7],
              1:5,
              include.lowest = T)
        })


# fitsalary<- lm(wo ~ yt - u + t, na.action=na.exclude)
# summary (fitsalary)

formula<-
  U_class~
  prod_real*
  salario_real_obreros*
  salario_real_manager*
  productividad_horas*
  productividad_personal*
  costo_unitario_obreros*
  costo_unitario_manager*
  tecnologia#*
  # city
  

df_TRAIN<-
  df_ss[df_ss$year<2014,]

df_TEST<-
  df_ss[df_ss$year>2009,]


trainGrid <-  expand.grid(interaction.depth = c(2),
                          n.trees = 15,
                          shrinkage = 0.35,
                          n.minobsinnode = 15
)


train_Cont <- trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 1)

set.seed(1504)

m_ss <- train(formula,
             data = df_TRAIN, 
             # method = "multinom",
             method = "gbm",
             trControl = train_Cont,
             tuneGrid = trainGrid )

summary(m_ss)

# df_formula<-
#   as.data.frame(summary(m_ss),row.names = NULL)
# 
# View(df_formula[df_formula$rel.inf>0,])

# HOW DOES MODEL LOOKS ####

# df_TRAIN$U_class<-
#   factor(df_TRAIN$U_class, levels = c("1","0"))


df_TRAIN$ScoredClass<-
  # predict(m_ss,df_TRAIN,"prob",na.action = na.pass)
  predict(m_ss,df_TRAIN,"raw",na.action = na.pass)

xtabs(~ScoredClass+U_class,df_TRAIN)

# ggplot(df_TRAIN, aes(x=ScoredProbability)) + 
#   geom_density(aes(group=U_class, 
#                    colour=U_class, 
#                    fill=U_class), 
#                alpha=0.3)+ 
#   scale_fill_manual(values=c("#4876FF", "#B9D3EE"))


# df_TRAIN$deciles<-
#   cut(x = df_TRAIN$ScoredProbability,
#       breaks = 
#         quantile(df_TRAIN$ScoredProbability,
#                  seq(0,1,.2),na.rm = T),
#       labels = 1:5,include.lowest = T)

table_weekly<-
  xtabs(~ScoredClass+U_class,data = df_TRAIN)

table_weekly<-
  round(prop.table(table_weekly,1)*100,2)

table_weekly

ggplot(as.data.frame(table_weekly), 
       aes(x=ScoredClass, y = Freq, fill=U_class)) + 
  geom_bar(stat="identity")
  # geom_bar(stat="identity") + 
  # scale_fill_manual(values=c("#4876FF", "#B9D3EE")) + 
  # geom_hline(yintercept =(prop.table(table(df_TRAIN$U_class))*100)[1],
  #            col="#FF8C00")




# TEST ####

# df_TEST$U_class<-
#   factor(df_TEST$U_class, levels = c("1","0"))


df_TEST$ScoredClass<-
  predict(m_ss,df_TEST,"raw",na.action = na.pass)

# ggplot(df_TEST, aes(x=ScoredProbability)) + 
#   geom_density(aes(group=U_class, 
#                    colour=U_class, 
#                    fill=U_class), 
#                alpha=0.3)+ 
#   scale_fill_manual(values=c("#4876FF", "#B9D3EE"))


# df_TEST$deciles<-
#   cut(x = df_TEST$ScoredProbability,
#       breaks = 
#         quantile(df_TEST$ScoredProbability,
#                  seq(0,1,.20),na.rm = T),
#       labels = 1:5,include.lowest = T)

table_weekly<-
  xtabs(~ScoredClass+U_class,data = df_TEST)

table_weekly

table_weekly<-
  round(prop.table(table_weekly,1)*100,2)

table_weekly

ggplot(as.data.frame(table_weekly), 
       aes(x=ScoredClass, y = Freq, fill=U_class)) + 
  geom_bar(stat="identity")
  # geom_bar(stat="identity") + 
  # scale_fill_manual(values=c("#4876FF", "#B9D3EE")) + 
  # geom_hline(yintercept =(prop.table(table(df_TEST$U_class))*100)[1],
  #            col="#FF8C00")

# The method has change a liitle bit, now we are saying:
# P[x E Q(i)]=P(i)[Q[5]]


# We have to try  