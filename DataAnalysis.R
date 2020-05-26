
library(dplyr)
library(readr)
data104 <- read_csv("C:/Users/Gabriel/Desktop/104.csv")

data107 <- read_csv("C:/Users/Gabriel/Desktop/107.csv")


data104$`大學薪資`<-as.numeric(gsub("—|…",NA,data104$`大學薪資`))
data107$`大學薪資`<-as.numeric(gsub("—|…",NA,data107$`大學薪資`))
datacompare<-inner_join(data104,data107,"大職業別")

salary<-select(datacompare,大職業別,大學薪資.x,大學薪資.y)
salary$rate<-salary$大學薪資.y/salary$大學薪資.x
raise<-filter(salary,rate>1)
salaryrank<-arrange(salary,desc(rate))
View(head(salaryrank,10))
highsalary<-filter(salary,rate>1.05)
highsalaryrank<-arrange(highsalary,desc(rate))
frequency<-table(sapply (strsplit (highsalary$大職業別,"-") , "[" ,  1)) 
  View(frequency)
 
  
data104$`大學女/男`<-as.numeric(gsub("—|…",NA,data104$`大學女/男`))
boy_girl_104<-select(data104,大職業別,`大學女/男`)
boy_girl_104dif<-filter(boy_girl_104,`大學女/男`<100)
boy_girl_104rank<-arrange(boy_girl_104dif,desc(`大學女/男`))


#107年
data107$`大學女/男`<-as.numeric(gsub("—|…",NA,data107$`大學女/男`))
boy_girl_107<-select(data107,大職業別,`大學女/男`)
boy_girl_107dif<-filter(boy_girl_107,`大學女/男`<100)
boy_girl_107rank<-arrange(boy_girl_107dif,desc(`大學女/男`))

data107$研究所薪資<-as.numeric(gsub("—|…",NA,data107$研究所薪資))
studio107<-select(data107,"大職業別","研究所薪資","大學薪資")
studio107<-studio107[complete.cases(studio107),]
studio107$rate<-studio107$研究所薪資/studio107$大學薪資
studio107rank<-arrange(studio107,desc(rate))

my_favorite<-data.frame(職業=studio107rank$大職業別,
                        大學薪資=studio107rank$大學薪資,
                        研究所薪資=studio107rank$研究所薪資)
head(my_favorite[grep('金融',my_favorite$職業),],10)
my_favorite$比率<-my_favorite$研究所薪資/my_favorite$大學薪資
