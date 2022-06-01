##Загрузка пакетов
library('haven')
library('dplyr')
library('openxlsx')
library('readxl')
###Загрузка данных

#Данные по числу членов д/х - вопросник по домохозяйствам
houses <- read_sav("C:/Users/Рафаэль/Downloads/houses.sav")
members_data <- data.frame('wave' = houses$ID_W, 'house' = houses$ID_H, 'members' = houses$NFM)
#write.csv(members_data, 'members_data.csv')
#members_data <- read.csv('members_data.csv')

#Данные из индивидуального вопросника 
w13<- read_sav("C:/Users/Рафаэль/Downloads/w13.sav")
d13<- data.frame(w13$idind, w13$iid_h, w13$region, w13$i_marst, w13$im71, w13$im75, w13$im3, w13$im80, w13$im81, w13$ij65, 
                 w13$ij60, w13$ih5, w13$i_age, w13$ij1, w13$i_diplom, w13$i_origsm, w13$status, w13$iid_h, w13$im114, w13$ih6, 
                 w13$ij72.171, w13$ij72.172, w13$ij72.173)
marst <- d13
w14<- read_sav("C:/Users/Рафаэль/Downloads/w14.sav")
d14<- data.frame(w14$idind, w14$jid_h, w14$region, w14$j_marst, w14$jm71, w14$jm75, w14$jm3, w14$jm80, w14$jm81, w14$jj65, 
                 w14$jj60, w14$jh5, w14$j_age, w14$jj1, w14$j_diplom, w14$j_origsm, w14$status, w14$jid_h, w14$jm114, w14$jh6,
                 w14$jj72.171, w14$jj72.172, w14$jj72.173)
marst <- full_join(marst, d14, by = c("w13.idind" = "w14.idind"))
w15<- read_sav("C:/Users/Рафаэль/Downloads/w15.sav")
d15<- data.frame(w15$idind, w15$kid_h, w15$region, w15$k_marst, w15$km71, w15$km75, w15$km3, w15$km80, w15$km81, w15$kj65, 
                 w15$kj60, w15$kh5, w15$k_age, w15$kj1, w15$k_diplom, w15$k_origsm, w15$status, w15$kid_h, w15$km114, w15$kh6,
                 w15$kj72.171, w15$kj72.172, w15$kj72.173)
marst <- full_join(marst, d15, by = c("w13.idind" = "w15.idind"))
w16<- read_sav("C:/Users/Рафаэль/Downloads/w16.sav")
d16<- data.frame(w16$idind, w16$lid_h, w16$region, w16$l_marst, w16$lm71, w16$lm75, w16$lm3, w16$lm80, w16$lm81, w16$lj65, 
                 w16$lj60, w16$lh5, w16$l_age, w16$lj1, w16$l_diplom, w16$l_origsm, w16$status, w16$lid_h, w16$lh6,
                 w16$lj72.171, w16$lj72.172, w16$lj72.173)
marst <- full_join(marst, d16, by = c("w13.idind" = "w16.idind"))
w17<- read_sav("C:/Users/Рафаэль/Downloads/w17.sav")
d17<- data.frame(w17$idind, w17$mid_h, w17$region, w17$m_marst, w17$mm71, w17$mm75, w17$mm3, w17$mm80, w17$mm81, w17$mj65, 
                 w17$mj60, w17$mh5, w17$m_age, w17$mj1, w17$m_diplom, w17$m_origsm, w17$status, w17$mid_h, w17$mm114, w17$mh6,
                 w17$mj72.171, w17$mj72.172, w17$mj72.173)
marst <- full_join(marst, d17, by = c("w13.idind" = "w17.idind"))
w18<- read_sav("C:/Users/Рафаэль/Downloads/w18.sav")
d18<- data.frame(w18$idind, w18$nid_h, w18$region, w18$n_marst, w18$nm71, w18$nm75, w18$nm3, w18$nm80, w18$nm81, w18$nj65, 
                 w18$nj60, w18$nh5, w18$n_age, w18$nj1, w18$n_diplom, w18$n_origsm, w18$status, w18$nid_h, w18$nm114, w18$nh6,
                 w18$nj72.171, w18$nj72.172, w18$nj72.173)
marst <- full_join(marst, d18, by = c("w13.idind" = "w18.idind"))
w19<- read_sav("C:/Users/Рафаэль/Downloads/w19.sav")
d19<- data.frame(w19$idind, w19$oid_h, w19$region, w19$o_marst, w19$om71, w19$om75, w19$om3, w19$om80, w19$om81, w19$oj65, 
                 w19$oj60, w19$oh5, w19$o_age, w19$oj1, w19$o_diplom, w19$o_origsm, w19$status, w19$oid_h, w19$om114, w19$oh6,
                 w19$oj72.171, w19$oj72.172, w19$oj72.173)
marst <- full_join(marst, d19, by = c("w13.idind" = "w19.idind"))
w20<- read_sav("C:/Users/Рафаэль/Downloads/w20.sav")
d20<- data.frame(w20$idind, w20$pid_h, w20$region, w20$p_marst, w20$pm71, w20$pm75, w20$pm3, w20$pm80, w20$pm81, w20$pj65, 
                 w20$pj60, w20$ph5, w20$p_age, w20$pj1, w20$p_diplom, w20$p_origsm, w20$status, w20$pid_h, w20$pm114, w20$ph6,
                 w20$pj72.171, w20$pj72.172, w20$pj72.173)
marst <- full_join(marst, d20, by = c("w13.idind" = "w20.idind"))
w21<- read_sav("C:/Users/Рафаэль/Downloads/w21.sav")
d21<- data.frame(w21$idind, w21$qid_h, w21$region, w21$q_marst, w21$qm71, w21$qm75, w21$qm3, w21$qm80, w21$qm81, w21$qj65, 
                 w21$qj60, w21$qh5, w21$q_age, w21$qj1, w21$q_diplom, w21$q_origsm, w21$status, w21$qid_h, w21$qm114, w21$qh6,
                 w21$qj72.171, w21$qj72.172, w21$qj72.173)
marst <- full_join(marst, d21, by = c("w13.idind" = "w21.idind"))
w22<- read_sav("C:/Users/Рафаэль/Downloads/w22.sav")
d22<- data.frame(w22$idind, w22$rid_h, w22$region, w22$r_marst, w22$rm71, w22$rm75, w22$rm3, w22$rm80, w22$rm81, w22$rj65, 
                 w22$rj60, w22$rh5, w22$r_age, w22$rj1, w22$r_diplom, w22$r_origsm, w22$status, w22$rid_h, w22$rm114, w22$rh6,
                 w22$rj72.171, w22$rj72.172, w22$rj72.173)
marst <- full_join(marst, d22, by = c("w13.idind" = "w22.idind"))
w23<- read_sav("C:/Users/Рафаэль/Downloads/w23.sav")
d23<- data.frame(w23$idind, w23$sid_h, w23$region, w23$s_marst, w23$sm71, w23$sm75, w23$sm3, w23$sm80, w23$sm81, w23$sj65, 
                 w23$sj60, w23$sh5, w23$s_age, w23$sj1, w23$s_diplom, w23$s_origsm, w23$status, w23$sid_h, w23$sm114, w23$sh6,
                 w23$sj72.171, w23$sj72.172, w23$sj72.173)
marst <- full_join(marst, d23, by = c("w13.idind" = "w23.idind"))
w24<- read_sav("C:/Users/Рафаэль/Downloads/w24.sav")
d24<- data.frame(w24$idind, w24$tid_h, w24$region, w24$t_marst, w24$tm71, w24$tm75, w24$tm3, w24$tm80, w24$tm81, w24$tj65, 
                 w24$tj60, w24$th5, w24$t_age, w24$tj1, w24$t_diplom, w24$t_origsm, w24$status, w24$tid_h, w24$tm114, w24$th6,
                 w24$tj72.171, w24$tj72.172, w24$tj72.173)
marst <- full_join(marst, d24, by = c("w13.idind" = "w24.idind"))
w25<- read_sav("C:/Users/Рафаэль/Downloads/w25.sav")
d25<- data.frame(w25$idind, w25$uid_h, w25$region, w25$u_marst, w25$um71, w25$um75, w25$um3, w25$um80, w25$um81, w25$uj65, 
                 w25$uj60, w25$uh5, w25$u_age, w25$uj1, w25$u_diplom, w25$u_origsm, w25$status, w25$uid_h, w25$um114, w25$uh6,
                 w25$uj72.171, w25$uj72.172, w25$uj72.173)
marst <- full_join(marst, d25, by = c("w13.idind" = "w25.idind"))
w26<- read_sav("C:/Users/Рафаэль/Downloads/w26.sav")
d26<- data.frame(w26$idind, w26$vid_h, w26$region, w26$v_marst, w26$vm71, w26$vm75, w26$vm3, w26$vm80, w26$vm81, w26$vj65, 
                 w26$vj60, w26$vh5, w26$v_age, w26$vj1, w26$v_diplom, w26$v_origsm, w26$status, w26$vid_h, w26$vm114, w26$vh6,
                 w26$vj72.171, w26$vj72.172, w26$vj72.173)
marst <- full_join(marst, d26, by = c("w13.idind" = "w26.idind"))
w27<- read_sav("C:/Users/Рафаэль/Downloads/w27.sav")
d27<- data.frame(w27$idind, w27$wid_h, w27$region, w27$w_marst, w27$wm71, w27$wm75, w27$wm3, w27$wm80, w27$wm81, w27$wj65, 
                 w27$wj60, w27$wh5, w27$w_age, w27$wj1, w27$w_diplom, w27$w_origsm, w27$status, w27$wid_h, w27$wm114, w27$wh6,
                 w27$wj72.171, w27$wj72.172, w27$wj72.173)
marst <- full_join(marst, d27, by = c("w13.idind" = "w27.idind"))
w28<- read_sav("C:/Users/Рафаэль/Downloads/w28.sav")
d28<- data.frame(w28$idind, w28$xid_h, w28$region, w28$x_marst, w28$xm71, w28$xm75, w28$xm3, w28$xm80, w28$xj65, w28$xj60, 
                 w28$xh5, w28$x_age, w28$xj1, w28$x_diplom, w28$x_origsm, w28$status, w28$xid_h, w28$xm114, w28$xh6,
                 w28$xj72.171, w28$xj72.172, w28$xj72.173)
marst <- full_join(marst, d28, by = c("w13.idind" = "w28.idind"))
marst[marst == 99999997 | marst == 99999998 | marst == 99999999] <- NA
#write.csv(marst, 'marst.csv')
############################################################
############################################################
#Автособираемая выборка#####################################
############################################################
############################################################
#marst <- read.csv('marst.csv')
prices <- read_excel('price_corr.xlsx')

#13 волна
Wave <- marst[complete.cases(marst[,which(colnames(marst)=="w13.i_marst" )]),]
houses <- members_data[members_data$wave == 13, c('house','members')]
Viborka <- data.frame('ID' = Wave$w13.idind, 'house' = Wave$w13.iid_h, 't0' = Wave$w13.i_age)
Viborka$t1 <- Viborka$t0 + 1
Viborka$marst <- Wave$w13.i_marst
Viborka$region0 <-  Wave$w13.region
Viborka$smoking0 <- -1*Wave$w13.im71 + 2
Viborka$smoking1 <-  -1*Wave$w14.jm71 + 2
Viborka$cigarretes0 <-  Wave$w13.im75
Viborka$cigarretes1 <-  Wave$w14.jm75
Viborka$health0 <- Wave$w13.im3 
Viborka$health1 <- Wave$w14.jm3 
Viborka$alco0 <-  -1*Wave$w13.im80 + 2
Viborka$alco1 <-  -1*Wave$w14.jm80 +2
Viborka$alcoholism0 <-  Wave$w13.im81
Viborka$alcoholism1 <-  Wave$w14.jm81
Viborka$optimism0 <-  Wave$w13.ij65
Viborka$optimism1 <-  Wave$w14.jj65
Viborka$income0 <- Wave$w13.ij60/prices$Coeff[5]
Viborka$income1 <- Wave$w14.jj60
Viborka$male0 <- -1*Wave$w13.ih5 + 2
Viborka$job0 <- Wave$w13.ij1
Viborka$job1 <- Wave$w14.jj1
Viborka$educ0[Wave$w13.i_diplom <= 3] <- 0
Viborka$educ0[Wave$w13.i_diplom == 4 | Wave$w13.i_diplom == 5] <- 1
Viborka$educ0[Wave$w13.i_diplom == 6] <- 2
Viborka$status <- Wave$w13.status
Viborka$Wave <- 13
Viborka$phys0 <- Wave$w13.im114
Viborka$born <- Wave$w13.ih6

Viborka$children <- Wave$w13.ij72.171
Viborka$children_total <- Wave$w13.ij72.172
Viborka$children_young <- Wave$w13.ij72.173
Viborka$children_total[Viborka$children == 2] <- 0
Viborka$children_young[Viborka$children == 2] <- 0

Viborka <- merge(x = Viborka, y = houses, by = "house", all.x = TRUE)
Dataset <- Viborka


#14 волна
Wave <- marst[complete.cases(marst[,which(colnames(marst)=="w14.j_marst" )]),]
houses <- members_data[members_data$wave == 14, c('house','members')]
Viborka <- data.frame('ID' = Wave$w13.idind, 'house' = Wave$w14.jid_h, 't0' = Wave$w14.j_age)
Viborka$t1 <- Viborka$t0 + 1
Viborka$marst <- Wave$w14.j_marst
Viborka$region0 <-  Wave$w14.region
Viborka$smoking0 <- -1*Wave$w14.jm71 + 2
Viborka$smoking1 <-  -1*Wave$w15.km71 + 2
Viborka$cigarretes0 <-  Wave$w14.jm75
Viborka$cigarretes1 <-  Wave$w15.km75
Viborka$health0 <- Wave$w14.jm3 
Viborka$health1 <- Wave$w15.km3 
Viborka$alco0 <-  -1*Wave$w14.jm80 + 2
Viborka$alco1 <-  -1*Wave$w15.km80 +2
Viborka$alcoholism0 <-  Wave$w14.jm81
Viborka$alcoholism1 <-  Wave$w15.km81
Viborka$optimism0 <-  Wave$w14.jj65
Viborka$optimism1 <-  Wave$w15.kj65
Viborka$income0 <- Wave$w14.jj60/prices$Coeff[6]
Viborka$income1 <- Wave$w15.kj60
Viborka$male0 <- -1*Wave$w14.jh5 + 2
Viborka$job0 <- Wave$w14.jj1
Viborka$job1 <- Wave$w15.kj1
Viborka$educ0[Wave$w14.j_diplom <= 3] <- 0
Viborka$educ0[Wave$w14.j_diplom == 4 | Wave$w14.j_diplom == 5] <- 1
Viborka$educ0[Wave$w14.j_diplom == 6] <- 2
Viborka$status <- Wave$w14.status
Viborka$Wave <- 14
Viborka$phys0 <- Wave$w14.jm114
Viborka$born <- Wave$w14.jh6

Viborka$children <- Wave$w14.jj72.171
Viborka$children_total <- Wave$w14.jj72.172
Viborka$children_young <- Wave$w14.jj72.173
Viborka$children_total[Viborka$children == 2] <- 0
Viborka$children_young[Viborka$children == 2] <- 0

Viborka <- merge(x = Viborka, y = houses, by = "house", all.x = TRUE)
Dataset <- rbind(Viborka, Dataset)


#15 волна
Wave <- marst[complete.cases(marst[,which(colnames(marst)=="w15.k_marst" )]),]
houses <- members_data[members_data$wave == 15, c('house','members')]
Viborka <- data.frame('ID' = Wave$w13.idind, 'house' = Wave$w15.kid_h, 't0' = Wave$w15.k_age)
Viborka$t1 <- Viborka$t0 + 1
Viborka$marst <- Wave$w15.k_marst
Viborka$region0 <-  Wave$w15.region
Viborka$smoking0 <- -1*Wave$w15.km71 + 2
Viborka$smoking1 <-  -1*Wave$w16.lm71 + 2
Viborka$cigarretes0 <-  Wave$w15.km75
Viborka$cigarretes1 <-  Wave$w16.lm75
Viborka$health0 <- Wave$w15.km3 
Viborka$health1 <- Wave$w16.lm3 
Viborka$alco0 <-  -1*Wave$w15.km80 + 2
Viborka$alco1 <-  -1*Wave$w16.lm80 +2
Viborka$alcoholism0 <-  Wave$w15.km81
Viborka$alcoholism1 <-  Wave$w16.lm81
Viborka$optimism0 <-  Wave$w15.kj65
Viborka$optimism1 <-  Wave$w16.lj65
Viborka$income0 <- Wave$w15.kj60/prices$Coeff[7]
Viborka$income1 <- Wave$w16.lj60
Viborka$male0 <- -1*Wave$w15.kh5 + 2
Viborka$job0 <- Wave$w15.kj1
Viborka$job1 <- Wave$w16.lj1
Viborka$educ0[Wave$w15.k_diplom <= 3] <- 0
Viborka$educ0[Wave$w15.k_diplom == 4 | Wave$w15.k_diplom == 5] <- 1
Viborka$educ0[Wave$w15.k_diplom == 6] <- 2
Viborka$status <- Wave$w15.status
Viborka$Wave <- 15
Viborka$phys0 <- Wave$w15.km114
Viborka$born <- Wave$w15.kh6

Viborka$children <- Wave$w15.kj72.171
Viborka$children_total <- Wave$w15.kj72.172
Viborka$children_young <- Wave$w15.kj72.173
Viborka$children_total[Viborka$children == 2] <- 0
Viborka$children_young[Viborka$children == 2] <- 0

Viborka <- merge(x = Viborka, y = houses, by = "house", all.x = TRUE)
Dataset <- rbind(Viborka, Dataset)


#16 волна
Wave <- marst[complete.cases(marst[,which(colnames(marst)=="w16.l_marst" )]),]
houses <- members_data[members_data$wave == 16, c('house','members')]
Viborka <- data.frame('ID' = Wave$w13.idind, 'house' = Wave$w16.lid_h, 't0' = Wave$w16.l_age)
Viborka$t1 <- Viborka$t0 + 1
Viborka$marst <- Wave$w16.l_marst
Viborka$region0 <-  Wave$w16.region
Viborka$smoking0 <- -1*Wave$w16.lm71 + 2
Viborka$smoking1 <-  -1*Wave$w17.mm71 + 2
Viborka$cigarretes0 <-  Wave$w16.lm75
Viborka$cigarretes1 <-  Wave$w17.mm75
Viborka$health0 <- Wave$w16.lm3 
Viborka$health1 <- Wave$w17.mm3 
Viborka$alco0 <-  -1*Wave$w16.lm80 + 2
Viborka$alco1 <-  -1*Wave$w17.mm80 +2
Viborka$alcoholism0 <-  Wave$w16.lm81
Viborka$alcoholism1 <-  Wave$w17.mm81
Viborka$optimism0 <-  Wave$w16.lj65
Viborka$optimism1 <-  Wave$w17.mj65
Viborka$income0 <- Wave$w16.lj60/prices$Coeff[8]
Viborka$income1 <- Wave$w17.mj60
Viborka$male0 <- -1*Wave$w16.lh5 + 2
Viborka$job0 <- Wave$w16.lj1
Viborka$job1 <- Wave$w17.mj1
Viborka$educ0[Wave$w16.l_diplom <= 3] <- 0
Viborka$educ0[Wave$w16.l_diplom == 4 | Wave$w16.l_diplom == 5] <- 1
Viborka$educ0[Wave$w16.l_diplom == 6] <- 2
Viborka$status <- Wave$w16.status
Viborka$Wave <- 16
Viborka$phys0 <- NA
Viborka$born <- Wave$w16.lh6

Viborka$children <- Wave$w16.lj72.171
Viborka$children_total <- Wave$w16.lj72.172
Viborka$children_young <- Wave$w16.lj72.173
Viborka$children_total[Viborka$children == 2] <- 0
Viborka$children_young[Viborka$children == 2] <- 0

Viborka <- merge(x = Viborka, y = houses, by = "house", all.x = TRUE)
Dataset <- rbind(Viborka, Dataset)


#17 волна
Wave <- marst[complete.cases(marst[,which(colnames(marst)=="w17.m_marst" )]),]
houses <- members_data[members_data$wave == 17, c('house','members')]
Viborka <- data.frame('ID' = Wave$w13.idind, 'house' = Wave$w17.mid_h, 't0' = Wave$w17.m_age)
Viborka$t1 <- Viborka$t0 + 1
Viborka$marst <- Wave$w17.m_marst
Viborka$region0 <-  Wave$w17.region
Viborka$smoking0 <- -1*Wave$w17.mm71 + 2
Viborka$smoking1 <-  -1*Wave$w18.nm71 + 2
Viborka$cigarretes0 <-  Wave$w17.mm75
Viborka$cigarretes1 <-  Wave$w18.nm75
Viborka$health0 <- Wave$w17.mm3 
Viborka$health1 <- Wave$w18.nm3 
Viborka$alco0 <-  -1*Wave$w17.mm80 + 2
Viborka$alco1 <-  -1*Wave$w18.nm80 +2
Viborka$alcoholism0 <-  Wave$w17.mm81
Viborka$alcoholism1 <-  Wave$w18.nm81
Viborka$optimism0 <-  Wave$w17.mj65
Viborka$optimism1 <-  Wave$w18.nj65
Viborka$income0 <- Wave$w17.mj60/prices$Coeff[9]
Viborka$income1 <- Wave$w18.nj60
Viborka$male0 <- -1*Wave$w17.mh5 + 2
Viborka$job0 <- Wave$w17.mj1
Viborka$job1 <- Wave$w18.nj1
Viborka$educ0[Wave$w17.m_diplom <= 3] <- 0
Viborka$educ0[Wave$w17.m_diplom == 4 | Wave$w17.m_diplom == 5] <- 1
Viborka$educ0[Wave$w17.m_diplom == 6] <- 2
Viborka$status <- Wave$w17.status
Viborka$Wave <- 17
Viborka$phys0 <- Wave$w17.mm114
Viborka$born <- Wave$w17.mh6

Viborka$children <- Wave$w13.ij72.171
Viborka$children_total <- Wave$w13.ij72.172
Viborka$children_young <- Wave$w13.ij72.173
Viborka$children_total[Viborka$children == 2] <- 0
Viborka$children_young[Viborka$children == 2] <- 0

Viborka$children <- Wave$w17.mj72.171
Viborka$children_total <- Wave$w17.mj72.172
Viborka$children_young <- Wave$w17.mj72.173
Viborka$children_total[Viborka$children == 2] <- 0
Viborka$children_young[Viborka$children == 2] <- 0

Viborka <- merge(x = Viborka, y = houses, by = "house", all.x = TRUE)
Dataset <- rbind(Viborka, Dataset)


#18 волна
Wave <- marst[complete.cases(marst[,which(colnames(marst)=="w18.n_marst" )]),]
houses <- members_data[members_data$wave == 18, c('house','members')]
Viborka <- data.frame('ID' = Wave$w13.idind, 'house' = Wave$w18.nid_h, 't0' = Wave$w18.n_age)
Viborka$t1 <- Viborka$t0 + 1
Viborka$marst <- Wave$w18.n_marst
Viborka$region0 <-  Wave$w18.region
Viborka$smoking0 <- -1*Wave$w18.nm71 + 2
Viborka$smoking1 <-  -1*Wave$w19.om71 + 2
Viborka$cigarretes0 <-  Wave$w18.nm75
Viborka$cigarretes1 <-  Wave$w19.om75
Viborka$health0 <- Wave$w18.nm3 
Viborka$health1 <- Wave$w19.om3 
Viborka$alco0 <-  -1*Wave$w18.nm80 + 2
Viborka$alco1 <-  -1*Wave$w19.om80 +2
Viborka$alcoholism0 <-  Wave$w18.nm81
Viborka$alcoholism1 <-  Wave$w19.om81
Viborka$optimism0 <-  Wave$w18.nj65
Viborka$optimism1 <-  Wave$w19.oj65
Viborka$income0 <- Wave$w18.nj60/prices$Coeff[10]
Viborka$income1 <- Wave$w19.oj60
Viborka$male0 <- -1*Wave$w18.nh5 + 2
Viborka$job0 <- Wave$w18.nj1
Viborka$job1 <- Wave$w19.oj1
Viborka$educ0[Wave$w18.n_diplom <= 3] <- 0
Viborka$educ0[Wave$w18.n_diplom == 4 | Wave$w18.n_diplom == 5] <- 1
Viborka$educ0[Wave$w18.n_diplom == 6] <- 2
Viborka$status <- Wave$w18.status
Viborka$Wave <- 18
Viborka$phys0 <- Wave$w18.nm114
Viborka$born <- Wave$w18.nh6

Viborka$children <- Wave$w18.nj72.171
Viborka$children_total <- Wave$w18.nj72.172
Viborka$children_young <- Wave$w18.nj72.173
Viborka$children_total[Viborka$children == 2] <- 0
Viborka$children_young[Viborka$children == 2] <- 0

Viborka <- merge(x = Viborka, y = houses, by = "house", all.x = TRUE)
Dataset <- rbind(Viborka, Dataset)


#19 волна
Wave <- marst[complete.cases(marst[,which(colnames(marst)=="w19.o_marst" )]),]
houses <- members_data[members_data$wave == 19, c('house','members')]
Viborka <- data.frame('ID' = Wave$w13.idind, 'house' = Wave$w19.oid_h, 't0' = Wave$w19.o_age)
Viborka$t1 <- Viborka$t0 + 1
Viborka$marst <- Wave$w19.o_marst
Viborka$region0 <-  Wave$w19.region
Viborka$smoking0 <- -1*Wave$w19.om71 + 2
Viborka$smoking1 <-  -1*Wave$w20.pm71 + 2
Viborka$cigarretes0 <-  Wave$w19.om75
Viborka$cigarretes1 <-  Wave$w20.pm75
Viborka$health0 <- Wave$w19.om3 
Viborka$health1 <- Wave$w20.pm3 
Viborka$alco0 <-  -1*Wave$w19.om80 + 2
Viborka$alco1 <-  -1*Wave$w20.pm80 +2
Viborka$alcoholism0 <-  Wave$w19.om81
Viborka$alcoholism1 <-  Wave$w20.pm81
Viborka$optimism0 <-  Wave$w19.oj65
Viborka$optimism1 <-  Wave$w20.pj65
Viborka$income0 <- Wave$w19.oj60/prices$Coeff[11]
Viborka$income1 <- Wave$w20.pj60
Viborka$male0 <- -1*Wave$w19.oh5 + 2
Viborka$job0 <- Wave$w19.oj1
Viborka$job1 <- Wave$w20.pj1
Viborka$educ0[Wave$w19.o_diplom <= 3] <- 0
Viborka$educ0[Wave$w19.o_diplom == 4 | Wave$w19.o_diplom == 5] <- 1
Viborka$educ0[Wave$w19.o_diplom == 6] <- 2
Viborka$status <- Wave$w19.status
Viborka$Wave <- 19
Viborka$phys0 <- Wave$w19.om114
Viborka$born <- Wave$w19.oh6

Viborka$children <- Wave$w19.oj72.171
Viborka$children_total <- Wave$w19.oj72.172
Viborka$children_young <- Wave$w19.oj72.173
Viborka$children_total[Viborka$children == 2] <- 0
Viborka$children_young[Viborka$children == 2] <- 0

Viborka <- merge(x = Viborka, y = houses, by = "house", all.x = TRUE)
Dataset <- rbind(Viborka, Dataset)


#20 волна
Wave <- marst[complete.cases(marst[,which(colnames(marst)=="w20.p_marst" )]),]
houses <- members_data[members_data$wave == 20, c('house','members')]
Viborka <- data.frame('ID' = Wave$w13.idind, 'house' = Wave$w20.pid_h, 't0' = Wave$w20.p_age)
Viborka$t1 <- Viborka$t0 + 1
Viborka$marst <- Wave$w20.p_marst
Viborka$region0 <-  Wave$w20.region
Viborka$smoking0 <- -1*Wave$w20.pm71 + 2
Viborka$smoking1 <-  -1*Wave$w21.qm71 + 2
Viborka$cigarretes0 <-  Wave$w20.pm75
Viborka$cigarretes1 <-  Wave$w21.qm75
Viborka$health0 <- Wave$w20.pm3 
Viborka$health1 <- Wave$w21.qm3 
Viborka$alco0 <-  -1*Wave$w20.pm80 + 2
Viborka$alco1 <-  -1*Wave$w21.qm80 +2
Viborka$alcoholism0 <-  Wave$w20.pm81
Viborka$alcoholism1 <-  Wave$w21.qm81
Viborka$optimism0 <-  Wave$w20.pj65
Viborka$optimism1 <-  Wave$w21.qj65
Viborka$income0 <- Wave$w20.pj60/prices$Coeff[12]
Viborka$income1 <- Wave$w21.qj60
Viborka$male0 <- -1*Wave$w20.ph5 + 2
Viborka$job0 <- Wave$w20.pj1
Viborka$job1 <- Wave$w21.qj1
Viborka$educ0[Wave$w20.p_diplom <= 3] <- 0
Viborka$educ0[Wave$w20.p_diplom == 4 | Wave$w20.p_diplom == 5] <- 1
Viborka$educ0[Wave$w20.p_diplom == 6] <- 2
Viborka$status <- Wave$w20.status
Viborka$Wave <- 20
Viborka$phys0 <- Wave$w20.pm114
Viborka$born <- Wave$w20.ph6

Viborka$children <- Wave$w20.pj72.171
Viborka$children_total <- Wave$w20.pj72.172
Viborka$children_young <- Wave$w20.pj72.173
Viborka$children_total[Viborka$children == 2] <- 0
Viborka$children_young[Viborka$children == 2] <- 0

Viborka <- merge(x = Viborka, y = houses, by = "house", all.x = TRUE)
Dataset <- rbind(Viborka, Dataset)


#21 волна
Wave <- marst[complete.cases(marst[,which(colnames(marst)=="w21.q_marst" )]),]
houses <- members_data[members_data$wave == 21, c('house','members')]
Viborka <- data.frame('ID' = Wave$w13.idind, 'house' = Wave$w21.qid_h, 't0' = Wave$w21.q_age)
Viborka$t1 <- Viborka$t0 + 1
Viborka$marst <- Wave$w21.q_marst
Viborka$region0 <-  Wave$w21.region
Viborka$smoking0 <- -1*Wave$w21.qm71 + 2
Viborka$smoking1 <-  -1*Wave$w22.rm71 + 2
Viborka$cigarretes0 <-  Wave$w21.qm75
Viborka$cigarretes1 <-  Wave$w22.rm75
Viborka$health0 <- Wave$w21.qm3 
Viborka$health1 <- Wave$w22.rm3 
Viborka$alco0 <-  -1*Wave$w21.qm80 + 2
Viborka$alco1 <-  -1*Wave$w22.rm80 +2
Viborka$alcoholism0 <-  Wave$w21.qm81
Viborka$alcoholism1 <-  Wave$w22.rm81
Viborka$optimism0 <-  Wave$w21.qj65
Viborka$optimism1 <-  Wave$w22.rj65
Viborka$income0 <- Wave$w21.qj60/prices$Coeff[13]
Viborka$income1 <- Wave$w22.rj60
Viborka$male0 <- -1*Wave$w21.qh5 + 2
Viborka$job0 <- Wave$w21.qj1
Viborka$job1 <- Wave$w22.rj1
Viborka$educ0[Wave$w21.q_diplom <= 3] <- 0
Viborka$educ0[Wave$w21.q_diplom == 4 | Wave$w21.q_diplom == 5] <- 1
Viborka$educ0[Wave$w21.q_diplom == 6] <- 2
Viborka$status <- Wave$w21.status
Viborka$Wave <- 21
Viborka$phys0 <- Wave$w21.qm114
Viborka$born <- Wave$w21.qh6

Viborka$children <- Wave$w21.qj72.171
Viborka$children_total <- Wave$w21.qj72.172
Viborka$children_young <- Wave$w21.qj72.173
Viborka$children_total[Viborka$children == 2] <- 0
Viborka$children_young[Viborka$children == 2] <- 0

Viborka <- merge(x = Viborka, y = houses, by = "house", all.x = TRUE)
Dataset <- rbind(Viborka, Dataset)


#22 волна
Wave <- marst[complete.cases(marst[,which(colnames(marst)=="w22.r_marst" )]),]
houses <- members_data[members_data$wave == 22, c('house','members')]
Viborka <- data.frame('ID' = Wave$w13.idind, 'house' = Wave$w22.rid_h, 't0' = Wave$w22.r_age)
Viborka$t1 <- Viborka$t0 + 1
Viborka$marst <- Wave$w22.r_marst
Viborka$region0 <-  Wave$w22.region
Viborka$smoking0 <- -1*Wave$w22.rm71 + 2
Viborka$smoking1 <-  -1*Wave$w23.sm71 + 2
Viborka$cigarretes0 <-  Wave$w22.rm75
Viborka$cigarretes1 <-  Wave$w23.sm75
Viborka$health0 <- Wave$w22.rm3 
Viborka$health1 <- Wave$w23.sm3 
Viborka$alco0 <-  -1*Wave$w22.rm80 + 2
Viborka$alco1 <-  -1*Wave$w23.sm80 +2
Viborka$alcoholism0 <-  Wave$w22.rm81
Viborka$alcoholism1 <-  Wave$w23.sm81
Viborka$optimism0 <-  Wave$w22.rj65
Viborka$optimism1 <-  Wave$w23.sj65
Viborka$income0 <- Wave$w22.rj60/prices$Coeff[14]
Viborka$income1 <- Wave$w23.sj60
Viborka$male0 <- -1*Wave$w22.rh5 + 2
Viborka$job0 <- Wave$w22.rj1
Viborka$job1 <- Wave$w23.sj1
Viborka$educ0[Wave$w22.r_diplom <= 3] <- 0
Viborka$educ0[Wave$w22.r_diplom == 4 | Wave$w22.r_diplom == 5] <- 1
Viborka$educ0[Wave$w22.r_diplom == 6] <- 2
Viborka$status <- Wave$w22.status
Viborka$Wave <- 22
Viborka$phys0 <- Wave$w22.rm114
Viborka$born <- Wave$w22.rh6

Viborka$children <- Wave$w22.rj72.171
Viborka$children_total <- Wave$w22.rj72.172
Viborka$children_young <- Wave$w22.rj72.173
Viborka$children_total[Viborka$children == 2] <- 0
Viborka$children_young[Viborka$children == 2] <- 0

Viborka <- merge(x = Viborka, y = houses, by = "house", all.x = TRUE)
Dataset <- rbind(Viborka, Dataset)


#23 волна
Wave <- marst[complete.cases(marst[,which(colnames(marst)=="w23.s_marst" )]),]
houses <- members_data[members_data$wave == 23, c('house','members')]
Viborka <- data.frame('ID' = Wave$w13.idind, 'house' = Wave$w23.sid_h, 't0' = Wave$w23.s_age)
Viborka$t1 <- Viborka$t0 + 1
Viborka$marst <- Wave$w23.s_marst
Viborka$region0 <-  Wave$w23.region
Viborka$smoking0 <- -1*Wave$w23.sm71 + 2
Viborka$smoking1 <-  -1*Wave$w24.tm71 + 2
Viborka$cigarretes0 <-  Wave$w23.sm75
Viborka$cigarretes1 <-  Wave$w24.tm75
Viborka$health0 <- Wave$w23.sm3 
Viborka$health1 <- Wave$w24.tm3 
Viborka$alco0 <-  -1*Wave$w23.sm80 + 2
Viborka$alco1 <-  -1*Wave$w24.tm80 +2
Viborka$alcoholism0 <-  Wave$w23.sm81
Viborka$alcoholism1 <-  Wave$w24.tm81
Viborka$optimism0 <-  Wave$w23.sj65
Viborka$optimism1 <-  Wave$w24.tj65
Viborka$income0 <- Wave$w23.sj60/prices$Coeff[15]
Viborka$income1 <- Wave$w24.tj60
Viborka$male0 <- -1*Wave$w23.sh5 + 2
Viborka$job0 <- Wave$w23.sj1
Viborka$job1 <- Wave$w24.tj1
Viborka$educ0[Wave$w23.s_diplom <= 3] <- 0
Viborka$educ0[Wave$w23.s_diplom == 4 | Wave$w23.s_diplom == 5] <- 1
Viborka$educ0[Wave$w23.s_diplom == 6] <- 2
Viborka$status <- Wave$w23.status
Viborka$Wave <- 23
Viborka$phys0 <- Wave$w23.sm114
Viborka$born <- Wave$w23.sh6

Viborka$children <- Wave$w23.sj72.171
Viborka$children_total <- Wave$w23.sj72.172
Viborka$children_young <- Wave$w23.sj72.173
Viborka$children_total[Viborka$children == 2] <- 0
Viborka$children_young[Viborka$children == 2] <- 0

Viborka <- merge(x = Viborka, y = houses, by = "house", all.x = TRUE)
Dataset <- rbind(Viborka, Dataset)


#24 волна
Wave <- marst[complete.cases(marst[,which(colnames(marst)=="w24.t_marst" )]),]
houses <- members_data[members_data$wave == 24, c('house','members')]
Viborka <- data.frame('ID' = Wave$w13.idind, 'house' = Wave$w24.tid_h, 't0' = Wave$w24.t_age)
Viborka$t1 <- Viborka$t0 + 1
Viborka$marst <- Wave$w24.t_marst
Viborka$region0 <-  Wave$w24.region
Viborka$smoking0 <- -1*Wave$w24.tm71 + 2
Viborka$smoking1 <-  -1*Wave$w25.um71 + 2
Viborka$cigarretes0 <-  Wave$w24.tm75
Viborka$cigarretes1 <-  Wave$w25.um75
Viborka$health0 <- Wave$w24.tm3 
Viborka$health1 <- Wave$w25.um3 
Viborka$alco0 <-  -1*Wave$w24.tm80 + 2
Viborka$alco1 <-  -1*Wave$w25.um80 +2
Viborka$alcoholism0 <-  Wave$w24.tm81
Viborka$alcoholism1 <-  Wave$w25.um81
Viborka$optimism0 <-  Wave$w24.tj65
Viborka$optimism1 <-  Wave$w25.uj65
Viborka$income0 <- Wave$w24.tj60/prices$Coeff[16]
Viborka$income1 <- Wave$w25.uj60
Viborka$male0 <- -1*Wave$w24.th5 + 2
Viborka$job0 <- Wave$w24.tj1
Viborka$job1 <- Wave$w25.uj1
Viborka$educ0[Wave$w24.t_diplom <= 3] <- 0
Viborka$educ0[Wave$w24.t_diplom == 4 | Wave$w24.t_diplom == 5] <- 1
Viborka$educ0[Wave$w24.t_diplom == 6] <- 2
Viborka$status <- Wave$w24.status
Viborka$Wave <- 24
Viborka$phys0 <- Wave$w24.tm114
Viborka$born <- Wave$w24.th6

Viborka$children <- Wave$w24.tj72.171
Viborka$children_total <- Wave$w24.tj72.172
Viborka$children_young <- Wave$w24.tj72.173
Viborka$children_total[Viborka$children == 2] <- 0
Viborka$children_young[Viborka$children == 2] <- 0

Viborka <- merge(x = Viborka, y = houses, by = "house", all.x = TRUE)
Dataset <- rbind(Viborka, Dataset)


#25 волна
Wave <- marst[complete.cases(marst[,which(colnames(marst)=="w25.u_marst" )]),]
houses <- members_data[members_data$wave == 25, c('house','members')]
Viborka <- data.frame('ID' = Wave$w13.idind, 'house' = Wave$w25.uid_h, 't0' = Wave$w25.u_age)
Viborka$t1 <- Viborka$t0 + 1
Viborka$marst <- Wave$w25.u_marst
Viborka$region0 <-  Wave$w25.region
Viborka$smoking0 <- -1*Wave$w25.um71 + 2
Viborka$smoking1 <-  -1*Wave$w26.vm71 + 2
Viborka$cigarretes0 <-  Wave$w25.um75
Viborka$cigarretes1 <-  Wave$w26.vm75
Viborka$health0 <- Wave$w25.um3 
Viborka$health1 <- Wave$w26.vm3 
Viborka$alco0 <-  -1*Wave$w25.um80 + 2
Viborka$alco1 <-  -1*Wave$w26.vm80 +2
Viborka$alcoholism0 <-  Wave$w25.um81
Viborka$alcoholism1 <-  Wave$w26.vm81
Viborka$optimism0 <-  Wave$w25.uj65
Viborka$optimism1 <-  Wave$w26.vj65
Viborka$income0 <- Wave$w25.uj60/prices$Coeff[17]
Viborka$income1 <- Wave$w26.vj60
Viborka$male0 <- -1*Wave$w25.uh5 + 2
Viborka$job0 <- Wave$w25.uj1
Viborka$job1 <- Wave$w26.vj1
Viborka$educ0[Wave$w25.u_diplom <= 3] <- 0
Viborka$educ0[Wave$w25.u_diplom == 4 | Wave$w25.u_diplom == 5] <- 1
Viborka$educ0[Wave$w25.u_diplom == 6] <- 2
Viborka$status <- Wave$w25.status
Viborka$Wave <- 25
Viborka$phys0 <- Wave$w25.um114
Viborka$born <- Wave$w25.uh6

Viborka$children <- Wave$w25.uj72.171
Viborka$children_total <- Wave$w25.uj72.172
Viborka$children_young <- Wave$w25.uj72.173
Viborka$children_total[Viborka$children == 2] <- 0
Viborka$children_young[Viborka$children == 2] <- 0

Viborka <- merge(x = Viborka, y = houses, by = "house", all.x = TRUE)
Dataset <- rbind(Viborka, Dataset)


#26 волна
Wave <- marst[complete.cases(marst[,which(colnames(marst)=="w26.v_marst" )]),]
houses <- members_data[members_data$wave == 26, c('house','members')]
Viborka <- data.frame('ID' = Wave$w13.idind, 'house' = Wave$w26.vid_h, 't0' = Wave$w26.v_age)
Viborka$t1 <- Viborka$t0 + 1
Viborka$marst <- Wave$w26.v_marst
Viborka$region0 <-  Wave$w26.region
Viborka$smoking0 <- -1*Wave$w26.vm71 + 2
Viborka$smoking1 <-  -1*Wave$w27.wm71 + 2
Viborka$cigarretes0 <-  Wave$w26.vm75
Viborka$cigarretes1 <-  Wave$w27.wm75
Viborka$health0 <- Wave$w26.vm3 
Viborka$health1 <- Wave$w27.wm3 
Viborka$alco0 <-  -1*Wave$w26.vm80 + 2
Viborka$alco1 <-  -1*Wave$w27.wm80 +2
Viborka$alcoholism0 <-  Wave$w26.vm81
Viborka$alcoholism1 <-  Wave$w27.wm81
Viborka$optimism0 <-  Wave$w26.vj65
Viborka$optimism1 <-  Wave$w27.wj65
Viborka$income0 <- Wave$w26.vj60/prices$Coeff[18]
Viborka$income1 <- Wave$w27.wj60
Viborka$male0 <- -1*Wave$w26.vh5 + 2
Viborka$job0 <- Wave$w26.vj1
Viborka$job1 <- Wave$w27.wj1
Viborka$educ0[Wave$w26.v_diplom <= 3] <- 0
Viborka$educ0[Wave$w26.v_diplom == 4 | Wave$w26.v_diplom == 5] <- 1
Viborka$educ0[Wave$w26.v_diplom == 6] <- 2
Viborka$status <- Wave$w26.status
Viborka$Wave <- 26
Viborka$phys0 <- Wave$w26.vm114
Viborka$born <- Wave$w26.vh6

Viborka$children <- Wave$w26.vj72.171
Viborka$children_total <- Wave$w26.vj72.172
Viborka$children_young <- Wave$w26.vj72.173
Viborka$children_total[Viborka$children == 2] <- 0
Viborka$children_young[Viborka$children == 2] <- 0

Viborka <- merge(x = Viborka, y = houses, by = "house", all.x = TRUE)
Dataset <- rbind(Viborka, Dataset)


#27 волна
Wave <- marst[complete.cases(marst[,which(colnames(marst)=="w27.w_marst" )]),]
houses <- members_data[members_data$wave == 27, c('house','members')]
Viborka <- data.frame('ID' = Wave$w13.idind, 'house' = Wave$w27.wid_h, 't0' = Wave$w27.w_age)
Viborka$t1 <- Viborka$t0 + 1
Viborka$marst <- Wave$w27.w_marst
Viborka$region0 <-  Wave$w27.region
Viborka$smoking0 <- -1*Wave$w27.wm71 + 2
Viborka$smoking1 <-  -1*Wave$w28.xm71 + 2
Viborka$cigarretes0 <-  Wave$w27.wm75
Viborka$cigarretes1 <-  Wave$w28.xm75
Viborka$health0 <- Wave$w27.wm3 
Viborka$health1 <- Wave$w28.xm3 
Viborka$alco0 <-  -1*Wave$w27.wm80 + 2
Viborka$alco1 <-  -1*Wave$w28.xm80 +2
Viborka$alcoholism0 <-  Wave$w27.wm81
Viborka$alcoholism1 <-  NA
Viborka$optimism0 <-  Wave$w27.wj65
Viborka$optimism1 <-  Wave$w28.xj65
Viborka$income0 <- Wave$w27.wj60/prices$Coeff[19]
Viborka$income1 <- Wave$w28.xj60
Viborka$male0 <- -1*Wave$w27.wh5 + 2
Viborka$job0 <- Wave$w27.wj1
Viborka$job1 <- Wave$w28.xj1
Viborka$educ0[Wave$w27.w_diplom <= 3] <- 0
Viborka$educ0[Wave$w27.w_diplom == 4 | Wave$w27.w_diplom == 5] <- 1
Viborka$educ0[Wave$w27.w_diplom == 6] <- 2
Viborka$status <- Wave$w27.status
Viborka$Wave <- 27
Viborka$phys0 <- Wave$w27.wm114
Viborka$born <- Wave$w27.wh6

Viborka$children <- Wave$w27.wj72.171
Viborka$children_total <- Wave$w27.wj72.172
Viborka$children_young <- Wave$w27.wj72.173
Viborka$children_total[Viborka$children == 2] <- 0
Viborka$children_young[Viborka$children == 2] <- 0

Viborka <- merge(x = Viborka, y = houses, by = "house", all.x = TRUE)
Dataset <- rbind(Viborka, Dataset)
#Записываем данные
write.csv(Dataset, 'Dataset.csv')


#Данные по индексам здоровья:

df21 <- data.frame(w21$qm20.61, w21$qm20.620, w21$qm20.69, w21$qm20.618, w21$qm3, w21$qh5, w21$q_age)
colnames(df21) <- c('heart', 'diab', 'hyp', 'cancer', 'srh', 'sex', 'age')
df <- df21
df22 <- data.frame(w22$rm20.61, w22$rm20.620, w22$rm20.69, w22$rm20.618, w22$rm3, w22$rh5, w22$r_age)
colnames(df22) <- c('heart', 'diab', 'hyp', 'cancer', 'srh', 'sex', 'age')
df <- rbind(df, df22)
df23 <- data.frame(w23$sm20.61, w23$sm20.620, w23$sm20.69, w23$sm20.618, w23$sm3, w23$sh5, w23$s_age)
colnames(df23) <- c('heart', 'diab', 'hyp', 'cancer', 'srh', 'sex', 'age')
df <- rbind(df, df23)
df24 <- data.frame(w24$tm20.61, w24$tm20.620, w24$tm20.69, w24$tm20.618, w24$tm3, w24$th5, w24$t_age)
colnames(df24) <- c('heart', 'diab', 'hyp', 'cancer', 'srh', 'sex', 'age')
df <- rbind(df, df24)
df25 <- data.frame(w25$um20.61, w25$um20.620, w25$um20.69, w25$um20.618, w25$um3, w25$uh5, w25$u_age)
colnames(df25) <- c('heart', 'diab', 'hyp', 'cancer', 'srh', 'sex', 'age')
df <- rbind(df, df25)
df26 <- data.frame(w26$vm20.61, w26$vm20.620, w26$vm20.69, w26$vm20.618, w26$vm3, w26$vh5, w26$v_age)
colnames(df26) <- c('heart', 'diab', 'hyp', 'cancer', 'srh', 'sex', 'age')
df <- rbind(df, df26)
df27 <- data.frame(w27$wm20.61, w27$wm20.620, w27$wm20.69, w27$wm20.618, w27$wm3, w27$wh5, w27$w_age)
colnames(df27) <- c('heart', 'diab', 'hyp', 'cancer', 'srh', 'sex', 'age')
df <- rbind(df, df27)
df28 <- data.frame(w28$xm20.61, w28$xm20.620, w28$xm20.69, w28$xm20.618, w28$xm3, w28$xh5, w28$x_age)
colnames(df28) <- c('heart', 'diab', 'hyp', 'cancer', 'srh', 'sex', 'age')
df <- rbind(df, df28)

df[df == 99999997 | df == 99999998 | df == 99999999] <- NA
write.csv(df, 'df.csv')
