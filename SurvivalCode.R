##Загрузка пакетов
library('dplyr')
library('readxl')
library('survival')
library('ggplot2')
library('dplyr')
library('ggfortify')
library('survminer')
library('tableone')
library('stargazer')
library('corrplot')
library('reshape2')
library('ggforce')

##############Моделирование#############################
Dataset <- read.csv('Dataset.csv')
########################################################

###Преобразования переменных и первичная работа с данными

Dataset <- Dataset[Dataset$t0 >= 30, ]
Dataset$cigarretes0[Dataset$smoking0 == 0] <- 0
Dataset$alcoholism0[Dataset$alco0 == 0] <- 7
Dataset$income0 <- Dataset$income0/1000
Dataset$income1 <- Dataset$income1/1000
Dataset$age_group <- cut(Dataset$t0, breaks = c(0, 49, 64, 200),
                      labels = c("1", "2", "3"))
Dataset$job0[Dataset$job0 < 5] <- 1
Dataset$job0[Dataset$job0 == 5] <- 0
Dataset$job1[Dataset$job1 < 5] <- 1
Dataset$job1[Dataset$job1 == 5] <- 0
Dataset$alone[Dataset$members > 1] <- 0
Dataset$alone[Dataset$members == 1] <- 1
Dataset$children_old <- Dataset$children_total - Dataset$children_young
Dataset <- Dataset[Dataset$children_old >= 0, ]

#Удаляем выбросы по доходу
par(mfrow=c(1,2)) 
hist(Dataset$income0, 
     main = 'До',
     xlab= 'Доход, тыс. руб. в ценах 2020',
     ylab = 'Частота',
     col="grey",
     border="black")
Dataset <- Dataset[Dataset$income0 < mean(Dataset$income0, na.rm = TRUE) + 3*sd(Dataset$income0, na.rm = TRUE),]
hist(Dataset$income0, 
     main = 'После',
     xlab= 'Доход, тыс. руб. в ценах 2020',
     ylab = 'Частота',
     col="grey",
     border="black")
par(mfrow=c(1,1)) 

#Бинаризуем физкультуру:
Dataset$phys0[Dataset$phys0 == 99999995] <- 5
hist(Dataset$phys0, 
     main = 'Распределение по уровню физической активности',
     xlab= '',
     ylab = 'Частота',
     col="grey",
     border="black")
Dataset$phys0[Dataset$phys0 != 5] <- 1
Dataset$phys0[Dataset$phys0 == 5] <- 0

#Создаём events

Dataset$event_health <- NA
Dataset$event_health[Dataset$health0 <= 3 & Dataset$health1 > 3] <- 1
Dataset$event_health[Dataset$health0 <= 3 & Dataset$health1 <= 3] <- 0

###Описательные статистики общие###

#Всего респондентов
observations <- length(na.omit(Dataset$ID))
observations #Всего наблюдений в панели
people <- length(unique(na.omit(Dataset$ID)))
people #Всего человек в панели

#Распределение по брачному статусу:
V_m = c('marst')  
factors_m <- c('marst')  
cov_bal <- CreateTableOne(vars= V_m, strata = "male0", data=Dataset, 
                          test=FALSE, includeNA = FALSE, factorVars = factors_m)
print(cov_bal, showAllLevels = TRUE, quote = TRUE)

#Распределение по здоровью:
V_m = c('health0')  
factors_m <- c('health0')  
cov_bal_male <- CreateTableOne(vars= V_m, strata = "age_group", data=Dataset[Dataset$male0 == 1, ], 
                               test=FALSE, includeNA = FALSE, factorVars = factors_m)
cov_bal_female <- CreateTableOne(vars= V_m, strata = "age_group", data=Dataset[Dataset$male0 == 0, ], 
                                 test=FALSE, includeNA = FALSE, factorVars = factors_m)
print(cov_bal_male, showAllLevels = TRUE, quote = TRUE)
print(cov_bal_female, showAllLevels = TRUE, quote = TRUE)

#Баланс ковариатов по брачному статусу:
V = c("t0", "income0", "educ0", "job0", 
      "status", "optimism0", "alone", "children_old", "children_young",
      "cigarretes0", "alco0", "phys0", 
      "health0", "born")  
factors <- c("educ0", "status")  
cov_bal_1 <- CreateTableOne(vars= V, strata = "marst", data=Dataset[Dataset$male0 == 1, ], 
                            test=FALSE, includeNA = FALSE, factorVars = factors)
cov_bal_2 <- CreateTableOne(vars= V, strata = "marst", data=Dataset[Dataset$male0 == 0, ], 
                            test=FALSE, includeNA = FALSE, factorVars = factors)
print(cov_bal_1, showAllLevels = TRUE, quote = TRUE)  #Мужчины 30-85
print(cov_bal_2, showAllLevels = TRUE, quote = TRUE) #Женщины 30-85

#Корреляционная матрица

V_cont <- c("t0", "income0", "job0", "cigarretes0", "health0", "alco0", "phys0", 'alone', 'children_old', 'children_young') 
V_cat <- c("status", "marst", 'educ0') 
data_male <- Dataset[Dataset$male0 == 1, ]
data_male <- data_male[, V_cont]
for (el in V_cat){
  data <- data.frame('outcome' = seq(1,length(data_male$t0)), el = Dataset[Dataset$male0 == 1, el])
  data <- dcast(data = data, outcome ~ noquote(el), length)
  data <- data[ , -which(names(data) %in% c("NA"))]
  data_male <- cbind(data_male, data[, -1])
}
colnames(data_male) <- c("age", "income", "job", "cigarettes", "health", "alcohol", "phys",
                         "alone", 'children_old', 'children_young',
                         'status = 1', 'status = 2', 'status = 3', 'status = 4',
                         'marst = 1', 'marst = 2', 'marst = 3', 'marst = 4', 'marst = 5', 'marst = 6',
                         'educ = 0', 'educ = 1', 'educ = 2'
                         ) 
M = cor(data_male,  use = "complete.obs")
corrplot(M, method = 'color', addCoef.col = 'black', number.cex= 0.5, number.digits = 1) 
data_female <- Dataset[Dataset$male0 == 0, ]
data_female <- data_female[, V_cont]
for (el in V_cat){
  data <- data.frame('outcome' = seq(1,length(data_female$t0)), el = Dataset[Dataset$male0 == 0, el])
  data <- dcast(data = data, outcome ~ noquote(el), length)
  data <- data[ , -which(names(data) %in% c("NA"))]
  data_female <- cbind(data_female, data[, -1])
}
colnames(data_female) <- c("age", "income", "job", "cigarettes", "health", "alcohol", "phys",
                           "alone", 'children_old', 'children_young',
                           'status = 1', 'status = 2', 'status = 3', 'status = 4',
                           'marst = 1', 'marst = 2', 'marst = 3', 'marst = 4', 'marst = 5', 'marst = 6',
                           'educ = 0', 'educ = 1', 'educ = 2') 
Fem = cor(data_female,  use = "complete.obs")
corrplot(Fem, method = 'color', addCoef.col = 'black', number.cex= 0.5, number.digits = 1)

#Матрица переходов

h <- na.omit(Dataset[Dataset$event_health == 1, 'health0'])
o <- na.omit(Dataset[Dataset$event_health == 1, 'optimism0'])
for (i in 1:5){
  l_h <- length(h[h == i])
  l_o <- length(o[o == i])
  vect <- c(i, l_h, l_o)
  if (i == 1){
    matrix <- data.frame('i' = i, 'l_h' = l_h, 'l_o' = l_o)
  }
  else {
    matrix <- rbind(matrix, vect)
  }
}
matrix$l_h <- round(matrix$l_h/sum(matrix$l_h)*100, 1)
matrix$l_o <- round(matrix$l_o/sum(matrix$l_o)*100, 1)
matrix
#######################Самооценка здоровья#######################
data_male <- Dataset[Dataset$male0 == 1, ]
data_female <- Dataset[Dataset$male0 == 0, ]
levels_marst <- c('2','1', '3', '4', '5', '6')
levels_alcohol <- c('7', '1', '2', '3', '4', '5', '6')
par(mfrow=c(1,2)) 
#Создаём survival-объект на основе датафрейма
km_1_m <- with(data_male, 
               Surv(time = t0, time2 = t1, event = event_health))
km_1_f <- with(data_female, 
               Surv(time = t0, time2 = t1, event = event_health))
#Оцениваем модель Кокса

cox_1_m_1 <- coxph(km_1_m ~ factor(marst, levels = levels_marst):factor(age_group)
                   
                   + factor(Wave)
                   + born
                   + factor(age_group)
                   
                   + factor(educ0):factor(age_group)  
                   + income0:factor(age_group)   
                   + job0:factor(age_group)
                   
                   ,data = data_male, id = ID)

cox_1_f_1 <- coxph(km_1_f ~ factor(marst, levels = levels_marst):factor(age_group)
                   
                   + strata(Wave)
                   + born
                   + factor(age_group)
                   
                   + factor(educ0):factor(age_group)  
                   + income0:factor(age_group)   
                   + job0:factor(age_group) 
                   
                 ,data = data_female, id = ID)

cox_1_m_2 <- coxph(km_1_m ~ factor(marst, levels = levels_marst):factor(age_group)
                   
                   + factor(Wave)
                   + born
                   + factor(age_group)
                   
                   + factor(educ0):factor(age_group)  
                   + income0:factor(age_group)   
                   + job0:factor(age_group)
                   
                   + factor(status):factor(age_group) 
                   + alone:factor(age_group) 
                   + children_old:factor(age_group) 
                   + children_young:factor(age_group) 
                   
                   ,data = data_male, id = ID)

cox_1_f_2 <- coxph(km_1_f ~ factor(marst, levels = levels_marst):factor(age_group) 
                   
                   + strata(Wave)
                   + born
                   + factor(age_group)
                   
                   + factor(educ0):factor(age_group)  
                   + income0:factor(age_group)   
                   + job0:factor(age_group) 
                   
                   + strata(status) 
                   + alone:factor(age_group) 
                   + children_old:factor(age_group) 
                   + children_young:factor(age_group) 
                   
                   ,data = data_female, id = ID)
cox_1_m_3 <- coxph(km_1_m ~ factor(marst, levels = levels_marst):factor(age_group)
                   
                   + factor(Wave)
                   + born
                   + factor(age_group)
                   
                   + factor(educ0):factor(age_group)  
                   + income0:factor(age_group)   
                   + job0:factor(age_group)
                   
                   + factor(status):factor(age_group) 
                   + alone:factor(age_group) 
                   + children_old:factor(age_group) 
                   + children_young:factor(age_group) 

                   + cigarretes0:factor(age_group)  
                   + alco0:factor(age_group)  
                   + phys0:factor(age_group)
                   
                   ,data = data_male, id = ID)
summary(cox_1_m_3)
cox_1_f_3 <- coxph(km_1_f ~ factor(marst, levels = levels_marst):factor(age_group)
                   
                   + strata(Wave)
                   + born
                   + factor(age_group)
                   
                   + factor(educ0):factor(age_group)  
                   + income0:factor(age_group)   
                   + job0:factor(age_group) 
                   
                   + strata(status) 
                   + alone:factor(age_group) 
                   + children_old:factor(age_group) 
                   + children_young:factor(age_group) 
                   
                   + cigarretes0:factor(age_group)  
                   + alco0:factor(age_group)  
                   + phys0:factor(age_group)
                   
                   ,data = data_female, id = ID)
summary(cox_1_f_3)
stargazer(cox_1_m_1, cox_1_m_2, cox_1_m_3, cox_1_f_1, cox_1_f_2, cox_1_f_3, type = 'html',
          dep.var.labels = c('Male', 'Female'), digits = 2, dep.var.caption = '',
          add.lines = list(c('Events', cox_1_m_1$nevent, cox_1_m_2$nevent, cox_1_m_3$nevent, 
          cox_1_f_1$nevent, cox_1_f_2$nevent, cox_1_f_3$nevent), 
          c('Concordance', round(cox_1_m_1$concordance[6], 3), round(cox_1_m_2$concordance[6], 3), round(cox_1_m_3$concordance[6], 3), round(cox_1_f_1$concordance[6], 3), round(cox_1_f_2$concordance[6], 3), round(cox_1_f_3$concordance[6], 3))), out = 'table1.doc')

#Проверка пропорциональности:
prop_1_m <- cox.zph(cox_1_m_3)
print(prop_1_m, quote = TRUE)
prop_1_f <- cox.zph(cox_1_f_3)
print(prop_1_f, quote = TRUE)
#Проверка линейности:
plot(predict(cox_1_m_3), residuals(cox_1_m_3, type="martingale"), xlab = 'Fitted values',
     ylab = 'Martingale Residuals', las = 1, main = 'Мужчины 30+')
abline(h=0)
lines(smooth.spline(predict(cox_1_m_3), residuals(cox_1_m_3, type="martingale")), col = 'red')

plot(predict(cox_1_f_3), residuals(cox_1_f_3, type="martingale"), xlab = 'Fitted values',
     ylab = 'Martingale Residuals', las = 1, main = 'Женщины 30+')
abline(h=0)
lines(smooth.spline(predict(cox_1_f_3), residuals(cox_1_f_3, type="martingale")), col = 'red')


###################################
########Анализ устойчивости########
###################################

#Sankey plot
health_dyn <- na.omit(Dataset[, c('health0', 'health1', 'age_group', 'male0')])
colnames(health_dyn) <- c('health_0', 'health_1', 'age_group', 'male')
health_dyn$health_0 <- as.factor(health_dyn$health_0)
health_dyn$health_1 <- as.factor(health_dyn$health_1)
health_dyn$age_group <- as.factor(health_dyn$age_group)
health_dyn$male <- as.factor(health_dyn$male)
health_dyn <- health_dyn %>% 
  group_by(male, age_group, health_0, health_1) %>% 
  summarise(value=n())


health_dyn <- gather_set_data(health_dyn, c(2, 3,4))

sankey <- ggplot(health_dyn, aes(x, id=id, split=y, value=value))+
  geom_parallel_sets(aes(fill=male), 
                     alpha=0.5,
                     axis.width = 0.1)+
  geom_parallel_sets_axes(axis.width=0.1)+
  geom_parallel_sets_labels(angle = 0, colour = 'white')+
  theme_minimal()+
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank() 
  )
sankey

#Анализ переменной здоровья:
df <- read.csv('df.csv')
df <- df[df$age >= 30, ]
df$age_group[ 30 <= df$age & df$age <= 49] <- 1
df$age_group[ 50 <= df$age & df$age <= 64] <- 2
df$age_group[ 65 <= df$age] <- 3
df$male[df$sex == 1] <- 1
df$male[df$sex == 2] <- 0
df$health_bad[df$srh <= 3] <- 0
df$health_bad[df$srh > 3] <- 1
df$heart <- -df$heart + 2
df$diab <- -df$diab + 2
df$hyp <- -df$hyp + 2
df$cancer <- -df$cancer + 2
df$health_bad_with3[df$srh < 3] <- 0
df$health_bad_with3[df$srh >= 3] <- 1

correlations <- data.frame()
  for (sex in 0:1){
    frame = df[df$male == sex, ]
    vect <- c(sex, length(frame$male), 
              round(cor(frame$heart, frame$srh, use = 'complete.obs'), 2), 
              round(cor(frame$heart, frame$health_bad, use = 'complete.obs'), 2),
              round(cor(frame$heart, frame$health_bad_with3, use = 'complete.obs'), 2),
              round(cor(frame$diab, frame$srh, use = 'complete.obs'), 2),
              round(cor(frame$diab, frame$health_bad, use = 'complete.obs'), 2),
              round(cor(frame$diab, frame$health_bad_with3, use = 'complete.obs'), 2),
              round(cor(frame$hyp, frame$srh, use = 'complete.obs'), 2),
              round(cor(frame$hyp, frame$health_bad, use = 'complete.obs'), 2),
              round(cor(frame$hyp, frame$health_bad_with3, use = 'complete.obs'), 2),
              round(cor(frame$cancer, frame$srh, use = 'complete.obs'), 2),
              round(cor(frame$cancer, frame$health_bad, use = 'complete.obs'), 2),
              round(cor(frame$cancer, frame$health_bad_with3, use = 'complete.obs'), 2))
    correlations <- rbind(correlations, vect)
    print(CreateTableOne(vars= c('heart', 'diab', 'hyp', 'cancer'), strata = "srh", data=frame, 
                   test=FALSE, includeNA = FALSE, factorVars = c('heart', 'diab', 'hyp', 'cancer')))
  }
colnames(correlations) <- c('male', 'obs',
                            'srh-heart',
                            'dummy-heart',
                            'dummy3-heart',
                            'srh-diab',
                            'dummy-diab',
                            'dummy3-diab',
                            'srh-hyp',
                            'dummy-hyp',
                            'dummy3-hyp',
                            'srh-cancer',
                            'dummy-cancer',
                            'dummy3-cancer')
write.csv(correlations, 'correlations.csv')
