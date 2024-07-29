#import data_q
str(data_q)
library(tidyverse)
library(rstatix)
library(ggpubr)
#caffeine
#tukey location:light
tk_ll<- tukey_Caffeine[c(10,15,19),]#seleccionamos las filas a utilizar segun el grupo que desemos comparar
tk_ll$term= "light"#cambiamos el termino de comparacion x:Y a el nombre del factor y
tk_ll$group1= "-" #cambiamos el termino de comparacion x:Y a solo el termino de la variable y perteneciente a ese grupo
tk_ll$group2= "+"
tk_ll$location = c("A","B","C") # agregagamos el una columna con el correspondiente termino del factor x
ggboxplot(data_q,x = "location", y = "caffeine", color="light") #creamos un box plor
 #revisamos la pocicion donde queremos que esten los corchetes 
tk_ll$y.position = c(45, 50, 45) #posición de los corchetes
tk_ll$xmin =  c(0.8,1.8,2.8)# posición minima del corchete 
tk_ll$xmax = c(1.2,2.2,3.2) #pocicon maxima del corchete

p1<- ggboxplot(data_q,x = "location", y = "caffeine", color="light", 
               palette = c ("#B8860B","#030303"))+ #creamos el grafico definitivo
  stat_pvalue_manual(tk_ll,label = "p.adj.signif" ) #adjuntamos el pvalue y los corchetes
  
p1


#tukey location:age
tk_la<- tukey_Caffeine[c(25,28,46,33,36,51,40,43,55),]#seleccionamos las filas a utilizar segun el grupo que desemos comparar
tk_la$term= "age"#cambiamos el termino de comparacion x:Y a el nombre del factor y
tk_la$group1= c("T0","T0","T1","T0","T0","T1", "T0","T0","T1") #cambiamos el termino de comparacion x:Y a solo el termino de la variable y perteneciente a ese grupo
tk_la$group2= c("T1","T2","T2","T1","T2","T2","T1","T2","T2")
tk_la$location = c("A","A","A","B","B","B","C","C","C") # agregagamos el una columna con el correspondiente termino del factor x
ggboxplot(data_q,x = "location", y = "caffeine", color="age")#creamos un box plor
#revisamos la pocicion donde queremos que esten los corchetes 
tk_la$y.position = c(32,45,42,42,53,50,43,48,45)
tk_la$xmin = c(0.75,0.75,1,1.75,1.75,2,2.75,2.75,3)
tk_la$xmax =c(1,1.25,1.25,2,2.25,2.25,3,3.25,3.25) 


p2 <- ggboxplot(data_q,x = "location", y = "caffeine", color="age",
                palette = c("#C0FF3E","#9ACD32", "#6B8E23"))+#creamos el grafico definitivo
  stat_pvalue_manual(tk_la,label = "p.adj.signif" ) #adjuntamos el pvalue y los corchetes
p2

#tukey age:light
tk_al<- tukey_Caffeine[c(59,68,73),]#seleccionamos las filas a utilizar segun el grupo que desemos comparar
tk_al$term= "light"#cambiamos el termino de comparacion x:Y a el nombre del factor y
tk_al$group1= "-" #cambiamos el termino de comparacion x:Y a solo el termino de la variable y perteneciente a ese grupo
tk_al$group2= "+"
tk_al$age = c("T0","T1","T2") # agregagamos el una columna con el correspondiente termino del factor x
ggboxplot(data_q,x = "age", y = "caffeine", color="light") #creamos un box plor
#revisamos la pocicion donde queremos que esten los corchetes 
tk_al$y.position = c(40, 45, 50) #posición de los corchetes
tk_al$xmin =  c(0.8,1.8,2.8)# posición minima del corchete 
tk_al$xmax = c(1.2,2.2,3.2) #pocicon maxima del corchete

p3<- ggboxplot(data_q,x = "age", y = "caffeine", color="light",
               palette = c ("#B8860B","#030303"))+ #creamos el grafico definitivo
  stat_pvalue_manual(tk_al,label = "p.adj.signif" ) #adjuntamos el pvalue y los corchetes
p3


#theobromine

#tukey location:light
tk_ll_t<- tukey_theobromine[c(10,15,19),]#seleccionamos las filas a utilizar segun el grupo que desemos comparar
tk_ll_t$term= "light"#cambiamos el termino de comparacion x:Y a el nombre del factor y
tk_ll_t$group1= "-" #cambiamos el termino de comparacion x:Y a solo el termino de la variable y perteneciente a ese grupo
tk_ll_t$group2= "+"
tk_ll_t$location = c("A","B","C") # agregagamos el una columna con el correspondiente termino del factor x
ggboxplot(data_q,x = "location", y = "theobromine", color="light") #creamos un box plor
#revisamos la pocicion donde queremos que esten los corchetes 
tk_ll_t$y.position = c(0.40, 0.75, 0.70) #posición de los corchetes
tk_ll_t$xmin =  c(0.8,1.8,2.8)# posición minima del corchete 
tk_ll_t$xmax = c(1.2,2.2,3.2) #pocicon maxima del corchete

p4<- ggboxplot(data_q,x = "location", y = "theobromine", color="light", 
               palette = c ("#B8860B","#030303"))+ #creamos el grafico definitivo
  stat_pvalue_manual(tk_ll_t,label = "p.adj.signif" ) #adjuntamos el pvalue y los corchetes
p4


#tukey location:age
tk_la_t<- tukey_theobromine[c(25,28,46,33,36,51,40,43,55),]#seleccionamos las filas a utilizar segun el grupo que desemos comparar
tk_la_t$term= "age"#cambiamos el termino de comparacion x:Y a el nombre del factor y
tk_la_t$group1= c("T0","T0","T1","T0","T0","T1", "T0","T0","T1") #cambiamos el termino de comparacion x:Y a solo el termino de la variable y perteneciente a ese grupo
tk_la_t$group2= c("T1","T2","T2","T1","T2","T2","T1","T2","T2")
tk_la_t$location = c("A","A","A","B","B","B","C","C","C") # agregagamos el una columna con el correspondiente termino del factor x
ggboxplot(data_q,x = "location", y = "theobromine", color="age")#creamos un box plor
#revisamos la pocicion donde queremos que esten los corchetes 
tk_la_t$y.position = c(0.25,0.50,0.40,0.72,1.10,1,0.60,0.70,0.45)
tk_la_t$xmin = c(0.75,0.75,1,1.75,1.75,2,2.75,2.75,3)
tk_la_t$xmax =c(1,1.25,1.25,2,2.25,2.25,3,3.25,3.25) 


p5 <- ggboxplot(data_q,x = "location", y = "theobromine", color="age",
                palette = c("#C0FF3E","#9ACD32", "#6B8E23"))+#creamos el grafico definitivo
  stat_pvalue_manual(tk_la_t,label = "p.adj.signif" ) #adjuntamos el pvalue y los corchetes
p5

#tukey age:light
tk_al_t<- tukey_theobromine[c(59,68,73),]#seleccionamos las filas a utilizar segun el grupo que desemos comparar
tk_al_t$term= "light"#cambiamos el termino de comparacion x:Y a el nombre del factor y
tk_al_t$group1= "-" #cambiamos el termino de comparacion x:Y a solo el termino de la variable y perteneciente a ese grupo
tk_al_tl$group2= "+"
tk_al_t$age = c("T0","T1","T2") # agregagamos el una columna con el correspondiente termino del factor x
ggboxplot(data_q,x = "age", y = "theobromine", color="light") #creamos un box plor
#revisamos la pocicion donde queremos que esten los corchetes 
tk_al_t$y.position = c(0.60, 0.70, 1) #posición de los corchetes
tk_al_t$xmin =  c(0.8,1.8,2.8)# posición minima del corchete 
tk_al_t$xmax = c(1.2,2.2,3.2) #pocicon maxima del corchete

p6<- ggboxplot(data_q,x = "age", y = "theobromine", color="light",
               palette = c ("#B8860B","#030303"))+ #creamos el grafico definitivo
  stat_pvalue_manual(tk_al_t,label = "p.adj.signif" ) #adjuntamos el pvalue y los corchetes
p6
F2<- ggpubr::ggarrange(p1,p4,p3,p6,p2,p5, 
                       labels = c("A", "B", "C","D","E","F"),
                       ncol = 2, nrow = 3)
F2
#jpeg
ggsave(filename = "C:/Users/Mateo/Documents/Plot R studio/JPEG/F2.jpeg", plot = F2,
       width = 15, height = 13, units = "cm", dpi = 300, scale = 2)
#PDF
ggsave(filename = "C:/Users/Mateo/Documents/Plot R studio/PDF/F2.pdf", plot = F2,
       width = 14, height = 12, units = "cm", dpi = 300, scale = 2)
