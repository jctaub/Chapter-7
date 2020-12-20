#syn_dcap_soga is the name of the GA synthetic dataset and ori_data2 
# is the name of the original dataset

vars<-c('parish', 'sex', 'mar_stat', 'agegroup', 'employ')


orig<- ori_data2[,vars]

orig$mar_stat<- as.factor(orig$mar_stat)
orig$parish<-as.factor(orig$parish)
orig$sex<-as.factor(orig$sex)
orig$employ<- as.factor(orig$employ)
orig$agegroup<- as.factor(orig$agegroup)


#here I create a CART and parametric synthetic dataset to compare to the 
# ga dataset
library(synthpop)

cart<- syn(orig)
parametric<- syn(orig, method= "parametric")




#labelling the datasets as A, B, C, D so they can be combined together 
# as one dataset
orig$Data<-'A'

syn_dcap_soga$Data<-'B'
cartsyn<- cart$syn
parasyn<- parametric$syn

cartsyn$Data<-'C'
parasyn$Data<- 'D'

orig$mar_stat<- ifelse(orig$mar_stat==0, 3, orig$mar_stat)
syn_dcap_soga$mar_stat<- ifelse(syn$mar_stat==0, 3, syn$mar_stat)
cartsyn$mar_stat<- ifelse(cartsyn$mar_stat==0, 3, cartsyn$mar_stat)
parasyn$mar_stat<- ifelse(parasyn$mar_stat==0, 3, parasyn$mar_stat)

combo1<-rbind(syn_dcap_soga, orig)
combo2<- rbind(combo1, cartsyn)
combo3<- rbind(combo2, parasyn)

combo3$mar_stat<- as.factor(combo3$mar_stat)
combo3$parish<-as.factor(combo3$parish)
combo3$sex<-as.factor(combo3$sex)
combo3$employ<- as.factor(combo3$employ)
combo3$agegroup<- as.factor(combo3$agegroup)


#using the combined dataset, I creaded barcharts of all the variables
library(ggplot2)
ggplot(combo3, aes(x=mar_stat, color=Data, fill=Data)) + 
  geom_bar(position = position_dodge())+ xlab('Marital Status')+ ylab('Count')+ 
  ggtitle('Counts of Marital Status')+ 
  scale_fill_discrete(name = "Data", labels = c("Original", "GA", "CART", "Parametric"))

ggplot(combo3, aes(x=mar_stat, color=Data, fill=Data)) + 
  geom_bar(position = position_dodge())+ xlab('Marital Status')+ ylab('Count')+ 
  ggtitle('Counts of Marital Status')+  theme(legend.position = "none")

ggplot(combo3, aes(x=sex, color=Data, fill=Data)) + 
  geom_bar(position = position_dodge())+ xlab('Sex')+ ylab('Count')+
  ggtitle('Counts of Sex')+  theme(legend.position = "none")

ggplot(combo3, aes(x=agegroup, color=Data, fill=Data)) + 
  geom_bar(position = position_dodge())+ xlab('Age Group')+ ylab('Count')+
  ggtitle('Counts of Age Group')+ theme(legend.position = "none")

ggplot(combo3, aes(x=parish, color=Data, fill=Data)) + 
  geom_bar(position = position_dodge())+ xlab('Parish')+ ylab('Count')+
  ggtitle('Counts of Parish Residence')+  theme(legend.position = "none")

ggplot(combo3, aes(x=employ, color=Data, fill=Data)) + 
  geom_bar(position = position_dodge())+ xlab('Employment Status')+ ylab('Count')+
  ggtitle('Counts of Employment Status')+  theme(legend.position = "none")




#Here using the combined datasets I created Chi Square tables to determine if the
# data synthetis method effected the the variable distribution

#mar_stat
mtab<-table(combo$mar_stat, combo$Data)
mchi<-chisq.test(mtab, correct = FALSE)
mchi

#sex
stab<-table(combo$sex, combo$Data)
schi<-chisq.test(stab, correct = FALSE)
schi


#age
atab<-table(combo$agegroup, combo$Data)
achi<-chisq.test(atab, correct = FALSE)
achi

#parish
ptab<-table(combo$parish, combo$Data)
pchi<-chisq.test(ptab, correct = FALSE)
pchi

#employ
etab<-table(combo$employ, combo$Data)
echi<-chisq.test(etab, correct = FALSE)
echi


#here I created the propensity scores

#here the code is written so that any of the dataset can be labelled as
# syn and run through the same analysis
#syn<-syn_dcap_soga[,vars]
#syn<- cart$syn
#syn<- parametric$syn

combo<-rbind(orig, syn)



combo$databin<-combo$Data

combo$databin[combo$Data=='Original']<-0
combo$databin[combo$Data!= 'Original']<-1

combo$databin<- as.numeric(combo$databin)

log_prop<- glm(databin~factor(parish)+ factor(sex)+factor(mar_stat)+factor(employ)+ 
                 factor(agegroup), data= combo, family = binomial(link = "logit"))

library(broom)

summary(log_prop)

log_prop_sum<-tidy(log_prop)


vector<-log_prop$fitted.values

mean(vector)

prop_score<-sum((vector-0.5)^2)/nrow(combo)

epmse<- ((nrow(log_prop_sum)-1)*0.5^3)/nrow(combo)
stdev_pmse<- (sqrt(2*(nrow(log_prop_sum)-1))*0.5^3)/nrow(combo)
standpmse<-(prop_score-epmse)/stdev_pmse

pmseratio<- prop_score/epmse


#Here I am calculating the Ratio of Estimates
roe<- function(original, synthetic, var1, var2){
  oritab<-as.data.frame(ftable(original[[var1]], original[[var2]]))
  syntab<-as.data.frame(ftable(synthetic[[var1]], synthetic[[var2]]))
  combo<- merge(oritab, syntab, by= c('Var1', 'Var2'))
  combo$max<- pmax(combo$Freq.x, combo$Freq.y)
  combo$min<- pmin(combo$Freq.x, combo$Freq.y)
  combo$roe<- combo$min/combo$max
  combo$roe[is.na(combo$roe)]<-1
  return(mean(combo$roe))
}

roe(orig, syn, 'mar_stat', 'sex')
roe(orig, syn, 'mar_stat', 'agegroup')
roe(orig, syn, 'mar_stat', 'parish')
roe(orig, syn, 'mar_stat', 'employ')

roe(orig, syn, 'sex', 'agegroup')
roe(orig, syn, 'sex', 'parish')
roe(orig, syn, 'sex', 'employ')

roe(orig, syn, 'agegroup', 'parish')
roe(orig, syn, 'agegroup', 'employ')

roe(orig, syn, 'parish', 'employ')


#Here I am calculating the CAP score for the statistical uniques for the CART
# and parametric datasets- the cap score for the GA dataset has already been built 
#into the set
cap4_unique<- function(dataset, syndata1 ,key1, key2, key3, key4, target){
  table1<-as.data.frame(ftable(dataset[[key1]], dataset[[key2]], dataset[[key3]], dataset[[key4]]))
  
  
  table1<- subset(table1, table1$Freq==1)
  table2<-as.data.frame(ftable(dataset[[key1]], dataset[[key2]], dataset[[key3]], dataset[[key4]],
                               dataset[[target]]))
  table3<-merge(table1, table2, by= c('Var1', 'Var2', 'Var3', 'Var4'))
  table3<-subset(table3, table3$Freq.y!=0)
  vars<- c('Var1', 'Var2', 'Var3', 'Var4', 'Var5')
  table3<- table3[, vars]
  
  
  #pop 1
  syn1tab1<-as.data.frame(ftable(syndata1[[key1]], syndata1[[key2]], syndata1[[key3]], 
                                 syndata1[[key4]], syndata1[[target]]))
  syn1tab2<-as.data.frame(ftable(syndata1[[key1]], syndata1[[key2]], syndata1[[key3]],
                                 syndata1[[key4]]))
  combined<-merge(table3, syn1tab1, by= c('Var1', 'Var2', 'Var3', 'Var4', 'Var5'), all.x=TRUE)
  combined<- merge(combined, syn1tab2, by= c('Var1','Var2', 'Var3', 'Var4'), all.x=TRUE)
  combined$freq1x<- combined$Freq.x
  combined$freq1y<-combined$Freq.y
  combined$Freq.x<- NULL
  combined$Freq.y<- NULL
  
  
  combined$paa<- combined$freq1x/combined$freq1y
  
  combined$paa[is.na(combined$paa)]<-0
  
  
  return(mean(combined$paa))
}


cap4_unique(ori_data2, cart$syn, 'parish', 'sex', 'mar_stat', 'agegroup', 'employ')
cap4_unique(ori_data2, parametric$syn, 'parish', 'sex', 'mar_stat', 'agegroup', 'employ')








