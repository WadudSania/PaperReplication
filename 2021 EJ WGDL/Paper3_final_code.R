
source("~/PHD Research Analysis/Comovement/data_gathering.R")

# pre-process data

y<- dret_all_p3
T<-dim(y)[1]
DT<-NULL
for(i in 1:length(colName))
{
  x<-dret_all_p3
  
  z<- x %>% as.data.frame() %>% dplyr::mutate(Date=as.Date(index(x),"%d-%m-%y")) %>% 
    dplyr::select(everything()) %>%
    gather(key = "series", value = "value",-Date)
  
  dt<-data.table(t=1:T,date=z$Date,x=z$value,
                 series=colName[i])
  DT<-rbind(DT,dt)  
}  

#Putting on the same scale

#DTnorm<-DT[,.(t,date, x=(x-mean(x,na.rm=TRUE))/sd(x,na.rm=TRUE) ),by=.(series)]

#Compute TPT

#TPT<-NULL
#tau.values<-c(22, 126, 252,756)

#for(i in 1:length(tau.values))
#{
 # temp<-DTnorm[,.(U=myoperator(x=t,y=x,tau=tau.values[i],operator=max),
                  #L=myoperator(x=t,y=x,tau=tau.values[i],operator=min),
                  #t,date),by=.(series)]
#  temp$thickness<-tau.values[i]
 # TPT<-rbind(TPT,temp)
#}

#TPT$L[is.infinite(TPT$L)]<-NA
#TPT$U[is.infinite(TPT$U)]<-NA

#temp<-TPT
#temp[,`:=`(thicknessfac=paste0("thickness=",thickness))]
#temp[,`:=`(thicknessfac=reorder(thicknessfac,thickness))]

#plotting (Figure 6 of the current  article)

#all_tpt<-ggplot(temp,aes(x=date,y=L,col=series))+geom_line()
##all_tpt<- all_tpt + geom_line(aes(x=date,y=U,col=series ))+ facet_wrap(~thicknessfac, nrow = 2)
#all_tpt<- all_tpt+theme_bw()+scale_y_continuous(name="TPT")



##### manual calculation 

y<- dret_all_p3 %>% as.data.frame() %>% dplyr::mutate(Date=as.Date(index(dret_all_p3))) %>% dplyr::select(everything())

y$Date<-as.Date(y$Date,order.by=y$Date)

T<-dim(y)[1]
m1<-y[,1]
m2<-y[,2]
m3<-y[,3]
m4<-y[,4]
m5<-y[,5]
m6<-y[,6]
m7<-y[,7]
m8<-y[,8]
m9<-y[365:6425,9]
m10<-y[,10]
m11<-y[,11]
m12<-y[,12]
m13<-y[,13]
m14<-y[,14]
m15<-y[,15]
m16<-y[,16]
m17<-y[,17]
m18<-y[,18]
m19<-y[,19]
m20<-y[,20]
m21<-y[,21]
m22<-y[,22]
m23<-y[,23]
m24<-y[365:6425,1] # to have same date range as MW

DT12<-NULL
dt12<-data.table(t=1:T,date=y$Date,x=m1,series=paste0("series",1))
DT12<-rbind(DT12,dt12)  
dt12<-data.table(t=1:T,date=y$Date,x=m2,series=paste0("series",2))
DT12<-rbind(DT12,dt12)


#needed to put on the same scale!
DTnorm12<-DT12[,.(t,date,x=(x-mean(x,na.rm=TRUE))/sd(x,na.rm=TRUE) ),by=.(series)]

#compute TPT
TPT12<-NULL
tau.values<-c(22,126,252,756)

for(i in 1:length(tau.values))
{
  temp12<-DTnorm12[,.(U=myoperator(x=t,y=x,tau=tau.values[i],operator=max),
                    L=myoperator(x=t,y=x,tau=tau.values[i],operator=min),
                    t,date=as.Date(date)),by=.(series)]
  temp12$thickness<-tau.values[i]
  TPT12<-rbind(TPT12,temp12)
}

TPT12$L[is.infinite(TPT12$L)]<-NA
TPT12$U[is.infinite(TPT12$U)]<-NA

# compute MTTPMA, all possible combinations, use 'setkey'

setkey(TPT12,series,thickness)

tau.values.matrix<-t(combn(tau.values,m=2))
K<-dim(tau.values.matrix)[2]
temp12<-tau.values.matrix[,K:1] #reverse the combinations!
tau.values.matrix<-rbind(tau.values.matrix,temp12)
tau.values.matrix<-rbind(tau.values.matrix,cbind(tau.values,tau.values) )

MTTPMA12<-NULL
for(i in 1:dim(tau.values.matrix)[1])#as many tau vectors as the number of rows
{    
  temp12<-TPT12[.(paste0("series",1:K),tau.values.matrix[i,]),
                .(MTTPMA=(min(U)-max(L))/(max(U)-min(L)),
                  thickness=toString(c(1:K,tau.values.matrix[i,]) ),
                  thickness1=tau.values.matrix[i,1],
                  thickness2=tau.values.matrix[i,2]),by=.(date)]
  MTTPMA12<-rbind(MTTPMA12,temp12)
}

#replace '-1' by NA
MTTPMA12$MTTPMA[MTTPMA12$MTTPMA==-1]<-NA

#plotting (Figure 5 of the current article)
temp12<-MTTPMA12

temp12[,`:=`(thickness1fac=paste0("thickness1=",thickness1),
             thickness2fac=paste0("thickness2=",thickness2))]

temp12[,`:=`(thickness1fac=reorder(thickness1fac,thickness1))]
temp12[,`:=`(thickness2fac=reorder(thickness2fac,thickness2))]


m_sp_cw<-ggplot(temp12,aes(x=date,y=MTTPMA,color=MTTPMA))+geom_line()+xlab("Date") +
  scale_x_date(date_breaks = "4 years", date_labels = "%Y", limits = c(min_1,max_1))
m_sp_cw<-m_sp_cw+facet_grid(thickness1fac~thickness2fac)+scale_color_gradient2(low="2547f2",mid = "#25edf2", high="#FC4E07")
m_sp_cw<-m_sp_cw+coord_cartesian(ylim=c(0,1))+ggtitle("S&P500 and Chicago Wheat") +theme(text=element_text(family="serif"))
m_sp_cw<-m_sp_cw + theme(legend.position="bottom",axis.text.x = element_text(color="black", size=8, angle=20))



# S&P500 KC Wheat

DT13<-NULL
dt13<-data.table(t=1:T,date=y$Date,x=m1,series=paste0("series",1))
DT13<-rbind(DT13,dt13)  
dt13<-data.table(t=1:T,date=y$Date,x=m3,series=paste0("series",2))
DT13<-rbind(DT13,dt13)


#needed to put on the same scale!
DTnorm13<-DT13[,.(t,date,x=(x-mean(x,na.rm=TRUE))/sd(x,na.rm=TRUE) ),by=.(series)]

#compute TPT
TPT13<-NULL
tau.values<-c(22,126,252,756)

for(i in 1:length(tau.values))
{
  temp13<-DTnorm13[,.(U=myoperator(x=t,y=x,tau=tau.values[i],operator=max),
                      L=myoperator(x=t,y=x,tau=tau.values[i],operator=min),
                      t,date=as.Date(date)),by=.(series)]
  temp13$thickness<-tau.values[i]
  TPT13<-rbind(TPT13,temp13)
}

TPT13$L[is.infinite(TPT13$L)]<-NA
TPT13$U[is.infinite(TPT13$U)]<-NA

# compute MTTPMA, all possible combinations, use 'setkey'


setkey(TPT13,series,thickness)

tau.values.matrix<-t(combn(tau.values,m=2))
K<-dim(tau.values.matrix)[2]
temp13<-tau.values.matrix[,K:1] #reverse the combinations!
tau.values.matrix<-rbind(tau.values.matrix,temp13)
tau.values.matrix<-rbind(tau.values.matrix,cbind(tau.values,tau.values) )

MTTPMA13<-NULL
for(i in 1:dim(tau.values.matrix)[1])#as many tau vectors as the number of rows
{    
  temp13<-TPT13[.(paste0("series",1:K),tau.values.matrix[i,]),
                .(MTTPMA=(min(U)-max(L))/(max(U)-min(L)),
                  thickness=toString(c(1:K,tau.values.matrix[i,]) ),
                  thickness1=tau.values.matrix[i,1],
                  thickness2=tau.values.matrix[i,2]),by=.(date)]
  MTTPMA13<-rbind(MTTPMA13,temp13)
}

#replace '-1' by NA
MTTPMA13$MTTPMA[MTTPMA13$MTTPMA==-1]<-NA

#plotting 
temp13<-MTTPMA13

temp13[,`:=`(thickness1fac=paste0("thickness1=",thickness1),
             thickness2fac=paste0("thickness2=",thickness2))]

temp13[,`:=`(thickness1fac=reorder(thickness1fac,thickness1))]
temp13[,`:=`(thickness2fac=reorder(thickness2fac,thickness2))]


m_sp_kw<-ggplot(temp13,aes(x=date,y=MTTPMA,color=MTTPMA))+geom_line()+xlab("Date") +
  scale_x_date(date_breaks = "4 years", date_labels = "%Y", limits = c(min_1,max_1))
m_sp_kw<-m_sp_kw+facet_grid(thickness1fac~thickness2fac)+scale_color_gradient2(low="2547f2",mid = "#25edf2", high="#FC4E07")
m_sp_kw<-m_sp_kw+coord_cartesian(ylim=c(0,1))+ggtitle("S&P500 and KC Wheat")+theme(text=element_text(family="serif"))


####### S&P500 Corn

DT14<-NULL
dt14<-data.table(t=1:T,date=y$Date,x=m1,series=paste0("series",1))
DT14<-rbind(DT14,dt14)  
dt14<-data.table(t=1:T,date=y$Date,x=m4,series=paste0("series",2))
DT14<-rbind(DT14,dt14)


#needed to put on the same scale!
DTnorm14<-DT14[,.(t,date,x=(x-mean(x,na.rm=TRUE))/sd(x,na.rm=TRUE) ),by=.(series)]

#compute TPT
TPT14<-NULL
tau.values<-c(22,126,252,756)

for(i in 1:length(tau.values))
{
  temp14<-DTnorm14[,.(U=myoperator(x=t,y=x,tau=tau.values[i],operator=max),
                      L=myoperator(x=t,y=x,tau=tau.values[i],operator=min),
                      t,date=as.Date(date)),by=.(series)]
  temp14$thickness<-tau.values[i]
  TPT14<-rbind(TPT14,temp14)
}

TPT14$L[is.infinite(TPT14$L)]<-NA
TPT14$U[is.infinite(TPT14$U)]<-NA

# compute MTTPMA, all possible combinations, use 'setkey'


setkey(TPT14,series,thickness)

tau.values.matrix<-t(combn(tau.values,m=2))
K<-dim(tau.values.matrix)[2]
temp14<-tau.values.matrix[,K:1] #reverse the combinations!
tau.values.matrix<-rbind(tau.values.matrix,temp14)
tau.values.matrix<-rbind(tau.values.matrix,cbind(tau.values,tau.values) )

MTTPMA14<-NULL
for(i in 1:dim(tau.values.matrix)[1])#as many tau vectors as the number of rows
{    
  temp14<-TPT14[.(paste0("series",1:K),tau.values.matrix[i,]),
                .(MTTPMA=(min(U)-max(L))/(max(U)-min(L)),
                  thickness=toString(c(1:K,tau.values.matrix[i,]) ),
                  thickness1=tau.values.matrix[i,1],
                  thickness2=tau.values.matrix[i,2]),by=.(date)]
  MTTPMA14<-rbind(MTTPMA14,temp14)
}

#replace '-1' by NA
MTTPMA14$MTTPMA[MTTPMA14$MTTPMA==-1]<-NA

#plotting (Figure 5 of the current article)
temp14<-MTTPMA14

temp14[,`:=`(thickness1fac=paste0("thickness1=",thickness1),
             thickness2fac=paste0("thickness2=",thickness2))]

temp14[,`:=`(thickness1fac=reorder(thickness1fac,thickness1))]
temp14[,`:=`(thickness2fac=reorder(thickness2fac,thickness2))]


m_sp_c<-ggplot(temp14,aes(x=date,y=MTTPMA,color=MTTPMA))+geom_line()+xlab("Date") +
  scale_x_date(date_breaks = "4 years", date_labels = "%Y", limits = c(min_1,max_1))
m_sp_c<-m_sp_c+facet_grid(thickness1fac~thickness2fac)+scale_color_gradient2(low="2547f2",mid = "#25edf2", high="#FC4E07")
m_sp_c<-m_sp_c+coord_cartesian(ylim=c(0,1))+ggtitle("S&P500 and Corn")+theme(text=element_text(family="serif"))



####### S&P500 Soybean

DT15<-NULL
dt15<-data.table(t=1:T,date=y$Date,x=m1,series=paste0("series",1))
DT15<-rbind(DT15,dt15)  
dt15<-data.table(t=1:T,date=y$Date,x=m5,series=paste0("series",2))
DT15<-rbind(DT15,dt15)


#needed to put on the same scale!
DTnorm15<-DT15[,.(t,date,x=(x-mean(x,na.rm=TRUE))/sd(x,na.rm=TRUE) ),by=.(series)]

#compute TPT
TPT15<-NULL
tau.values<-c(22,126,252,756)

for(i in 1:length(tau.values))
{
  temp15<-DTnorm15[,.(U=myoperator(x=t,y=x,tau=tau.values[i],operator=max),
                      L=myoperator(x=t,y=x,tau=tau.values[i],operator=min),
                      t,date=as.Date(date)),by=.(series)]
  temp15$thickness<-tau.values[i]
  TPT15<-rbind(TPT15,temp15)
}

TPT15$L[is.infinite(TPT15$L)]<-NA
TPT15$U[is.infinite(TPT15$U)]<-NA

# compute MTTPMA, all possible combinations, use 'setkey'


setkey(TPT15,series,thickness)

tau.values.matrix<-t(combn(tau.values,m=2))
K<-dim(tau.values.matrix)[2]
temp15<-tau.values.matrix[,K:1] #reverse the combinations!
tau.values.matrix<-rbind(tau.values.matrix,temp15)
tau.values.matrix<-rbind(tau.values.matrix,cbind(tau.values,tau.values) )

MTTPMA15<-NULL
for(i in 1:dim(tau.values.matrix)[1])#as many tau vectors as the number of rows
{    
  temp15<-TPT15[.(paste0("series",1:K),tau.values.matrix[i,]),
                .(MTTPMA=(min(U)-max(L))/(max(U)-min(L)),
                  thickness=toString(c(1:K,tau.values.matrix[i,]) ),
                  thickness1=tau.values.matrix[i,1],
                  thickness2=tau.values.matrix[i,2]),by=.(date)]
  MTTPMA15<-rbind(MTTPMA15,temp15)
}

#replace '-1' by NA
MTTPMA15$MTTPMA[MTTPMA15$MTTPMA==-1]<-NA

#plotting (Figure 5 of the current article)
temp15<-MTTPMA15

temp15[,`:=`(thickness1fac=paste0("thickness1=",thickness1),
             thickness2fac=paste0("thickness2=",thickness2))]

temp15[,`:=`(thickness1fac=reorder(thickness1fac,thickness1))]
temp15[,`:=`(thickness2fac=reorder(thickness2fac,thickness2))]

m_sp_s<-ggplot(temp15,aes(x=date,y=MTTPMA,color=MTTPMA))+geom_line()+xlab("Date") +
  scale_x_date(date_breaks = "4 years", date_labels = "%Y", limits = c(min_1,max_1))
m_sp_s<-m_sp_s+facet_grid(thickness1fac~thickness2fac)+scale_color_gradient2(low="2547f2",mid = "#25edf2", high="#FC4E07")
m_sp_s<-m_sp_s+coord_cartesian(ylim=c(0,1))+ggtitle("S&P500 and Soybeans")


####### S&P500 Soybean Meal

DT16<-NULL
dt16<-data.table(t=1:T,date=y$Date,x=m1,series=paste0("series",1))
DT16<-rbind(DT16,dt16)  
dt16<-data.table(t=1:T,date=y$Date,x=m6,series=paste0("series",2))
DT16<-rbind(DT16,dt16)


#needed to put on the same scale!
DTnorm16<-DT16[,.(t,date,x=(x-mean(x,na.rm=TRUE))/sd(x,na.rm=TRUE) ),by=.(series)]

#compute TPT
TPT16<-NULL
tau.values<-c(22,126,252,756)

for(i in 1:length(tau.values))
{
  temp16<-DTnorm16[,.(U=myoperator(x=t,y=x,tau=tau.values[i],operator=max),
                      L=myoperator(x=t,y=x,tau=tau.values[i],operator=min),
                      t,date=as.Date(date)),by=.(series)]
  temp16$thickness<-tau.values[i]
  TPT16<-rbind(TPT16,temp16)
}

TPT16$L[is.infinite(TPT16$L)]<-NA
TPT16$U[is.infinite(TPT16$U)]<-NA

# compute MTTPMA, all possible combinations, use 'setkey'


setkey(TPT16,series,thickness)

tau.values.matrix<-t(combn(tau.values,m=2))
K<-dim(tau.values.matrix)[2]
temp16<-tau.values.matrix[,K:1] #reverse the combinations!
tau.values.matrix<-rbind(tau.values.matrix,temp16)
tau.values.matrix<-rbind(tau.values.matrix,cbind(tau.values,tau.values) )

MTTPMA16<-NULL
for(i in 1:dim(tau.values.matrix)[1])#as many tau vectors as the number of rows
{    
  temp16<-TPT16[.(paste0("series",1:K),tau.values.matrix[i,]),
                .(MTTPMA=(min(U)-max(L))/(max(U)-min(L)),
                  thickness=toString(c(1:K,tau.values.matrix[i,]) ),
                  thickness1=tau.values.matrix[i,1],
                  thickness2=tau.values.matrix[i,2]),by=.(date)]
  MTTPMA16<-rbind(MTTPMA16,temp16)
}

#replace '-1' by NA
MTTPMA16$MTTPMA[MTTPMA16$MTTPMA==-1]<-NA

#plotting (Figure 5 of the current article)
temp16<-MTTPMA16

temp16[,`:=`(thickness1fac=paste0("thickness1=",thickness1),
             thickness2fac=paste0("thickness2=",thickness2))]

temp16[,`:=`(thickness1fac=reorder(thickness1fac,thickness1))]
temp16[,`:=`(thickness2fac=reorder(thickness2fac,thickness2))]


m_sp_sm<-ggplot(temp16,aes(x=date,y=MTTPMA,color=MTTPMA))+geom_line()+xlab("Date") +
  scale_x_date(date_breaks = "4 years", date_labels = "%Y", limits = c(min_1,max_1))
m_sp_sm<-m_sp_sm+facet_grid(thickness1fac~thickness2fac)+scale_color_gradient2(low="2547f2",mid = "#25edf2", high="#FC4E07")
m_sp_sm<-m_sp_sm+coord_cartesian(ylim=c(0,1))+ggtitle("S&P500 and Soybean Meal")+theme(text=element_text(family="serif"))




####### S&P500 Soybean oil

DT17<-NULL
dt17<-data.table(t=1:T,date=y$Date,x=m1,series=paste0("series",1))
DT17<-rbind(DT17,dt17)  
dt17<-data.table(t=1:T,date=y$Date,x=m7,series=paste0("series",2))
DT17<-rbind(DT17,dt17)


#needed to put on the same scale!
DTnorm17<-DT17[,.(t,date,x=(x-mean(x,na.rm=TRUE))/sd(x,na.rm=TRUE) ),by=.(series)]

#compute TPT
TPT17<-NULL
tau.values<-c(22,126,252,756)

for(i in 1:length(tau.values))
{
  temp17<-DTnorm17[,.(U=myoperator(x=t,y=x,tau=tau.values[i],operator=max),
                      L=myoperator(x=t,y=x,tau=tau.values[i],operator=min),
                      t,date=as.Date(date)),by=.(series)]
  temp17$thickness<-tau.values[i]
  TPT17<-rbind(TPT17,temp17)
}

TPT17$L[is.infinite(TPT17$L)]<-NA
TPT17$U[is.infinite(TPT17$U)]<-NA

# compute MTTPMA, all possible combinations, use 'setkey'


setkey(TPT17,series,thickness)

tau.values.matrix<-t(combn(tau.values,m=2))
K<-dim(tau.values.matrix)[2]
temp17<-tau.values.matrix[,K:1] #reverse the combinations!
tau.values.matrix<-rbind(tau.values.matrix,temp17)
tau.values.matrix<-rbind(tau.values.matrix,cbind(tau.values,tau.values) )

MTTPMA17<-NULL
for(i in 1:dim(tau.values.matrix)[1])#as many tau vectors as the number of rows
{    
  temp17<-TPT17[.(paste0("series",1:K),tau.values.matrix[i,]),
                .(MTTPMA=(min(U)-max(L))/(max(U)-min(L)),
                  thickness=toString(c(1:K,tau.values.matrix[i,]) ),
                  thickness1=tau.values.matrix[i,1],
                  thickness2=tau.values.matrix[i,2]),by=.(date)]
  MTTPMA17<-rbind(MTTPMA17,temp17)
}

#replace '-1' by NA
MTTPMA17$MTTPMA[MTTPMA17$MTTPMA==-1]<-NA

#plotting (Figure 5 of the current article)
temp17<-MTTPMA17

temp17[,`:=`(thickness1fac=paste0("thickness1=",thickness1),
             thickness2fac=paste0("thickness2=",thickness2))]

temp17[,`:=`(thickness1fac=reorder(thickness1fac,thickness1))]
temp17[,`:=`(thickness2fac=reorder(thickness2fac,thickness2))]

m_sp_bo<-ggplot(temp17,aes(x=date,y=MTTPMA,color=MTTPMA))+geom_line()+xlab("Date") +
  scale_x_date(date_breaks = "4 years", date_labels = "%Y", limits = c(min_1,max_1))
m_sp_bo<-m_sp_bo+facet_grid(thickness1fac~thickness2fac)+scale_color_gradient2(low="2547f2",mid = "#25edf2", high="#FC4E07")
m_sp_bo<-m_sp_bo+coord_cartesian(ylim=c(0,1))+ggtitle("S&P500 and Soybean Oil")+theme(text=element_text(family="serif"))


####### S&P500 Oats

DT18<-NULL
dt18<-data.table(t=1:T,date=y$Date,x=m1,series=paste0("series",1))
DT18<-rbind(DT18,dt18)  
dt18<-data.table(t=1:T,date=y$Date,x=m8,series=paste0("series",2))
DT18<-rbind(DT18,dt18)


#needed to put on the same scale!
DTnorm18<-DT18[,.(t,date,x=(x-mean(x,na.rm=TRUE))/sd(x,na.rm=TRUE) ),by=.(series)]

#compute TPT
TPT18<-NULL
tau.values<-c(22,126,252,756)

for(i in 1:length(tau.values))
{
  temp18<-DTnorm18[,.(U=myoperator(x=t,y=x,tau=tau.values[i],operator=max),
                      L=myoperator(x=t,y=x,tau=tau.values[i],operator=min),
                      t,date=as.Date(date)),by=.(series)]
  temp18$thickness<-tau.values[i]
  TPT18<-rbind(TPT18,temp18)
}

TPT18$L[is.infinite(TPT18$L)]<-NA
TPT18$U[is.infinite(TPT18$U)]<-NA

# compute MTTPMA, all possible combinations, use 'setkey'


setkey(TPT18,series,thickness)

tau.values.matrix<-t(combn(tau.values,m=2))
K<-dim(tau.values.matrix)[2]
temp18<-tau.values.matrix[,K:1] #reverse the combinations!
tau.values.matrix<-rbind(tau.values.matrix,temp18)
tau.values.matrix<-rbind(tau.values.matrix,cbind(tau.values,tau.values) )

MTTPMA18<-NULL
for(i in 1:dim(tau.values.matrix)[1])#as many tau vectors as the number of rows
{    
  temp18<-TPT18[.(paste0("series",1:K),tau.values.matrix[i,]),
                .(MTTPMA=(min(U)-max(L))/(max(U)-min(L)),
                  thickness=toString(c(1:K,tau.values.matrix[i,]) ),
                  thickness1=tau.values.matrix[i,1],
                  thickness2=tau.values.matrix[i,2]),by=.(date)]
  MTTPMA18<-rbind(MTTPMA18,temp18)
}

#replace '-1' by NA
MTTPMA18$MTTPMA[MTTPMA18$MTTPMA==-1]<-NA

#plotting (Figure 5 of the current article)
temp18<-MTTPMA18

temp18[,`:=`(thickness1fac=paste0("thickness1=",thickness1),
             thickness2fac=paste0("thickness2=",thickness2))]

temp18[,`:=`(thickness1fac=reorder(thickness1fac,thickness1))]
temp18[,`:=`(thickness2fac=reorder(thickness2fac,thickness2))]

m_sp_o<-ggplot(temp18,aes(x=date,y=MTTPMA,color=MTTPMA))+geom_line()+xlab("Date") +
  scale_x_date(date_breaks = "4 years", date_labels = "%Y", limits = c(min_1,max_1))
m_sp_o<-m_sp_o+facet_grid(thickness1fac~thickness2fac)+scale_color_gradient2(low="2547f2",mid = "#25edf2", high="#FC4E07")
m_sp_o<-m_sp_o+coord_cartesian(ylim=c(0,1))+ggtitle("S&P500 and Oats")+theme(text=element_text(family="serif"))
m_sp_o<- m_sp_o + theme(legend.position="bottom",axis.text.x = element_text(color="black", size=8, angle=20))
m_sp_o




####### S&P500 MPLS Wheat


T<-dim(y[365:6425,])[1]
DT19<-NULL
dt19<-data.table(t=1:T,date=y$Date[365:6425],x=m24,series=paste0("series",1))
DT19<-rbind(DT19,dt19)  
dt19<-data.table(t=1:T,date=y$Date[365:6425],x=m9,series=paste0("series",2))
DT19<-rbind(DT19,dt19)


#needed to put on the same scale!
DTnorm19<-DT19[,.(t,date,x=(x-mean(x,na.rm=TRUE))/sd(x,na.rm=TRUE) ),by=.(series)]

#compute TPT
TPT19<-NULL
tau.values<-c(22,126,252,756)

for(i in 1:length(tau.values))
{
  temp19<-DTnorm19[,.(U=myoperator(x=t,y=x,tau=tau.values[i],operator=max),
                      L=myoperator(x=t,y=x,tau=tau.values[i],operator=min),
                      t,date=as.Date(date)),by=.(series)]
  temp19$thickness<-tau.values[i]
  TPT19<-rbind(TPT19,temp19)
}

TPT19$L[is.infinite(TPT19$L)]<-NA
TPT19$U[is.infinite(TPT19$U)]<-NA

# compute MTTPMA, all possible combinations, use 'setkey'


setkey(TPT19,series,thickness)

tau.values.matrix<-t(combn(tau.values,m=2))
K<-dim(tau.values.matrix)[2]
temp19<-tau.values.matrix[,K:1] #reverse the combinations!
tau.values.matrix<-rbind(tau.values.matrix,temp19)
tau.values.matrix<-rbind(tau.values.matrix,cbind(tau.values,tau.values) )

MTTPMA19<-NULL
for(i in 1:dim(tau.values.matrix)[1])#as many tau vectors as the number of rows
{    
  temp19<-TPT19[.(paste0("series",1:K),tau.values.matrix[i,]),
                .(MTTPMA=(min(U)-max(L))/(max(U)-min(L)),
                  thickness=toString(c(1:K,tau.values.matrix[i,]) ),
                  thickness1=tau.values.matrix[i,1],
                  thickness2=tau.values.matrix[i,2]),by=.(date)]
  MTTPMA19<-rbind(MTTPMA19,temp19)
}

#replace '-1' by NA
MTTPMA19$MTTPMA[MTTPMA19$MTTPMA==-1]<-NA

#plotting (Figure 5 of the current article)
temp19<-MTTPMA19

temp19[,`:=`(thickness1fac=paste0("thickness1=",thickness1),
             thickness2fac=paste0("thickness2=",thickness2))]

temp19[,`:=`(thickness1fac=reorder(thickness1fac,thickness1))]
temp19[,`:=`(thickness2fac=reorder(thickness2fac,thickness2))]


m_sp_mw<-ggplot(temp19,aes(x=date,y=MTTPMA,color=MTTPMA))+geom_line()+xlab("Date") +
  scale_x_date(date_breaks = "4 years", date_labels = "%Y", limits = c(min_1,max_1))
m_sp_mw<-m_sp_mw+facet_grid(thickness1fac~thickness2fac)+scale_color_gradient2(low="2547f2",mid = "#25edf2", high="#FC4E07")
m_sp_mw<-m_sp_mw+coord_cartesian(ylim=c(0,1))+ggtitle("S&P500 and MPLS Wheat") +theme(text=element_text(family="serif"))



# S&P500 and Rough rice
T<-dim(y)[1]
DT110<-NULL
dt110<-data.table(t=1:T,date=y$Date,x=m1,series=paste0("series",1))
DT110<-rbind(DT110,dt110)  
dt110<-data.table(t=1:T,date=y$Date,x=m10,series=paste0("series",2))
DT110<-rbind(DT110,dt110)


#needed to put on the same scale!
DTnorm110<-DT110[,.(t,date,x=(x-mean(x,na.rm=TRUE))/sd(x,na.rm=TRUE) ),by=.(series)]

#compute TPT
TPT110<-NULL
tau.values<-c(22,126,252,756)

for(i in 1:length(tau.values))
{
  temp110<-DTnorm110[,.(U=myoperator(x=t,y=x,tau=tau.values[i],operator=max),
                      L=myoperator(x=t,y=x,tau=tau.values[i],operator=min),
                      t,date=as.Date(date)),by=.(series)]
  temp110$thickness<-tau.values[i]
  TPT110<-rbind(TPT110,temp110)
}

TPT110$L[is.infinite(TPT110$L)]<-NA
TPT110$U[is.infinite(TPT110$U)]<-NA

# compute MTTPMA, all possible combinations, use 'setkey'


setkey(TPT110,series,thickness)

tau.values.matrix<-t(combn(tau.values,m=2))
K<-dim(tau.values.matrix)[2]
temp110<-tau.values.matrix[,K:1] #reverse the combinations!
tau.values.matrix<-rbind(tau.values.matrix,temp110)
tau.values.matrix<-rbind(tau.values.matrix,cbind(tau.values,tau.values) )

MTTPMA110<-NULL
for(i in 1:dim(tau.values.matrix)[1])#as many tau vectors as the number of rows
{    
  temp110<-TPT110[.(paste0("series",1:K),tau.values.matrix[i,]),
                .(MTTPMA=(min(U)-max(L))/(max(U)-min(L)),
                  thickness=toString(c(1:K,tau.values.matrix[i,]) ),
                  thickness1=tau.values.matrix[i,1],
                  thickness2=tau.values.matrix[i,2]),by=.(date)]
  MTTPMA110<-rbind(MTTPMA110,temp110)
}

#replace '-1' by NA
MTTPMA110$MTTPMA[MTTPMA110$MTTPMA==-1]<-NA

#plotting (Figure 5 of the current article)
temp110<-MTTPMA110

temp110[,`:=`(thickness1fac=paste0("thickness1=",thickness1),
             thickness2fac=paste0("thickness2=",thickness2))]

temp110[,`:=`(thickness1fac=reorder(thickness1fac,thickness1))]
temp110[,`:=`(thickness2fac=reorder(thickness2fac,thickness2))]


m_sp_rr<-ggplot(temp110,aes(x=date,y=MTTPMA,color=MTTPMA))+geom_line()+xlab("Date") +
  scale_x_date(date_breaks = "4 years", date_labels = "%Y", limits = c(min_1,max_1))
m_sp_rr<-m_sp_rr+facet_grid(thickness1fac~thickness2fac)+scale_color_gradient2(low="2547f2",mid = "#25edf2", high="#FC4E07")
m_sp_rr<-m_sp_rr+coord_cartesian(ylim=c(0,1))+ggtitle("S&P500 and Rough Rice")+theme(text=element_text(family="serif"))



####### S&P500 coffee

DT111<-NULL
dt111<-data.table(t=1:T,date=y$Date,x=m1,series=paste0("series",1))
DT111<-rbind(DT111,dt111)  
dt111<-data.table(t=1:T,date=y$Date,x=m11,series=paste0("series",2))
DT111<-rbind(DT111,dt111)


#needed to put on the same scale!
DTnorm111<-DT111[,.(t,date,x=(x-mean(x,na.rm=TRUE))/sd(x,na.rm=TRUE) ),by=.(series)]

#compute TPT
TPT111<-NULL
tau.values<-c(22,126,252,756)

for(i in 1:length(tau.values))
{
  temp111<-DTnorm111[,.(U=myoperator(x=t,y=x,tau=tau.values[i],operator=max),
                      L=myoperator(x=t,y=x,tau=tau.values[i],operator=min),
                      t,date=as.Date(date)),by=.(series)]
  temp111$thickness<-tau.values[i]
  TPT111<-rbind(TPT111,temp111)
}

TPT111$L[is.infinite(TPT111$L)]<-NA
TPT111$U[is.infinite(TPT111$U)]<-NA

# compute MTTPMA, all possible combinations, use 'setkey'


setkey(TPT111,series,thickness)

tau.values.matrix<-t(combn(tau.values,m=2))
K<-dim(tau.values.matrix)[2]
temp111<-tau.values.matrix[,K:1] #reverse the combinations!
tau.values.matrix<-rbind(tau.values.matrix,temp111)
tau.values.matrix<-rbind(tau.values.matrix,cbind(tau.values,tau.values) )

MTTPMA111<-NULL
for(i in 1:dim(tau.values.matrix)[1])#as many tau vectors as the number of rows
{    
  temp111<-TPT111[.(paste0("series",1:K),tau.values.matrix[i,]),
                .(MTTPMA=(min(U)-max(L))/(max(U)-min(L)),
                  thickness=toString(c(1:K,tau.values.matrix[i,]) ),
                  thickness1=tau.values.matrix[i,1],
                  thickness2=tau.values.matrix[i,2]),by=.(date)]
  MTTPMA111<-rbind(MTTPMA111,temp111)
}

#replace '-1' by NA
MTTPMA111$MTTPMA[MTTPMA111$MTTPMA==-1]<-NA

#plotting (Figure 5 of the current article)
temp111<-MTTPMA111

temp111[,`:=`(thickness1fac=paste0("thickness1=",thickness1),
             thickness2fac=paste0("thickness2=",thickness2))]

temp111[,`:=`(thickness1fac=reorder(thickness1fac,thickness1))]
temp111[,`:=`(thickness2fac=reorder(thickness2fac,thickness2))]

m_sp_kc<-ggplot(temp111,aes(x=date,y=MTTPMA,color=MTTPMA))+geom_line()+xlab("Date") +
  scale_x_date(date_breaks = "4 years", date_labels = "%Y", limits = c(min_1,max_1))
m_sp_kc<-m_sp_kc+facet_grid(thickness1fac~thickness2fac)+scale_color_gradient2(low="2547f2",mid = "#25edf2", high="#FC4E07")
m_sp_kc<-m_sp_kc+coord_cartesian(ylim=c(0,1))+ggtitle("S&P500 and Coffee")+theme(text=element_text(family="serif"))



####### S&P500 Sugar

DT112<-NULL
dt112<-data.table(t=1:T,date=y$Date,x=m1,series=paste0("series",1))
DT112<-rbind(DT112,dt112)  
dt112<-data.table(t=1:T,date=y$Date,x=m12,series=paste0("series",2))
DT112<-rbind(DT112,dt112)


#needed to put on the same scale!
DTnorm112<-DT112[,.(t,date,x=(x-mean(x,na.rm=TRUE))/sd(x,na.rm=TRUE) ),by=.(series)]

#compute TPT
TPT112<-NULL
tau.values<-c(22,126,252,756)

for(i in 1:length(tau.values))
{
  temp112<-DTnorm112[,.(U=myoperator(x=t,y=x,tau=tau.values[i],operator=max),
                        L=myoperator(x=t,y=x,tau=tau.values[i],operator=min),
                        t,date=as.Date(date)),by=.(series)]
  temp112$thickness<-tau.values[i]
  TPT112<-rbind(TPT112,temp112)
}

TPT112$L[is.infinite(TPT112$L)]<-NA
TPT112$U[is.infinite(TPT112$U)]<-NA

# compute MTTPMA, all possible combinations, use 'setkey'


setkey(TPT112,series,thickness)

tau.values.matrix<-t(combn(tau.values,m=2))
K<-dim(tau.values.matrix)[2]
temp112<-tau.values.matrix[,K:1] #reverse the combinations!
tau.values.matrix<-rbind(tau.values.matrix,temp112)
tau.values.matrix<-rbind(tau.values.matrix,cbind(tau.values,tau.values) )

MTTPMA112<-NULL
for(i in 1:dim(tau.values.matrix)[1])#as many tau vectors as the number of rows
{    
  temp112<-TPT112[.(paste0("series",1:K),tau.values.matrix[i,]),
                  .(MTTPMA=(min(U)-max(L))/(max(U)-min(L)),
                    thickness=toString(c(1:K,tau.values.matrix[i,]) ),
                    thickness1=tau.values.matrix[i,1],
                    thickness2=tau.values.matrix[i,2]),by=.(date)]
  MTTPMA112<-rbind(MTTPMA112,temp112)
}

#replace '-1' by NA
MTTPMA112$MTTPMA[MTTPMA112$MTTPMA==-1]<-NA

#plotting (Figure 5 of the current article)
temp112<-MTTPMA112

temp112[,`:=`(thickness1fac=paste0("thickness1=",thickness1),
              thickness2fac=paste0("thickness2=",thickness2))]

temp112[,`:=`(thickness1fac=reorder(thickness1fac,thickness1))]
temp112[,`:=`(thickness2fac=reorder(thickness2fac,thickness2))]

m_sp_sb<-ggplot(temp112,aes(x=date,y=MTTPMA,color=MTTPMA))+geom_line()+xlab("Date") +
  scale_x_date(date_breaks = "4 years", date_labels = "%Y", limits = c(min_1,max_1))
m_sp_sb<-m_sp_sb+facet_grid(thickness1fac~thickness2fac)+scale_color_gradient2(low="2547f2",mid = "#25edf2", high="#FC4E07")
m_sp_sb<-m_sp_sb+coord_cartesian(ylim=c(0,1))+ggtitle("S&P500 and Sugar")+theme(text=element_text(family="serif"))


####### S&P500 Cocoa

DT113<-NULL
dt113<-data.table(t=1:T,date=y$Date,x=m1,series=paste0("series",1))
DT113<-rbind(DT113,dt113)  
dt113<-data.table(t=1:T,date=y$Date,x=m13,series=paste0("series",2))
DT113<-rbind(DT113,dt113)


#needed to put on the same scale!
DTnorm113<-DT113[,.(t,date,x=(x-mean(x,na.rm=TRUE))/sd(x,na.rm=TRUE) ),by=.(series)]

#compute TPT
TPT113<-NULL
tau.values<-c(22,126,252,756)

for(i in 1:length(tau.values))
{
  temp113<-DTnorm113[,.(U=myoperator(x=t,y=x,tau=tau.values[i],operator=max),
                        L=myoperator(x=t,y=x,tau=tau.values[i],operator=min),
                        t,date=as.Date(date)),by=.(series)]
  temp113$thickness<-tau.values[i]
  TPT113<-rbind(TPT113,temp113)
}

TPT113$L[is.infinite(TPT113$L)]<-NA
TPT113$U[is.infinite(TPT113$U)]<-NA

# compute MTTPMA, all possible combinations, use 'setkey'


setkey(TPT113,series,thickness)

tau.values.matrix<-t(combn(tau.values,m=2))
K<-dim(tau.values.matrix)[2]
temp113<-tau.values.matrix[,K:1] #reverse the combinations!
tau.values.matrix<-rbind(tau.values.matrix,temp113)
tau.values.matrix<-rbind(tau.values.matrix,cbind(tau.values,tau.values) )

MTTPMA113<-NULL
for(i in 1:dim(tau.values.matrix)[1])#as many tau vectors as the number of rows
{    
  temp113<-TPT113[.(paste0("series",1:K),tau.values.matrix[i,]),
                  .(MTTPMA=(min(U)-max(L))/(max(U)-min(L)),
                    thickness=toString(c(1:K,tau.values.matrix[i,]) ),
                    thickness1=tau.values.matrix[i,1],
                    thickness2=tau.values.matrix[i,2]),by=.(date)]
  MTTPMA113<-rbind(MTTPMA113,temp113)
}

#replace '-1' by NA
MTTPMA113$MTTPMA[MTTPMA113$MTTPMA==-1]<-NA

#plotting (Figure 5 of the current article)
temp113<-MTTPMA113

temp113[,`:=`(thickness1fac=paste0("thickness1=",thickness1),
              thickness2fac=paste0("thickness2=",thickness2))]

temp113[,`:=`(thickness1fac=reorder(thickness1fac,thickness1))]
temp113[,`:=`(thickness2fac=reorder(thickness2fac,thickness2))]


m_sp_cc<-ggplot(temp113,aes(x=date,y=MTTPMA,color=MTTPMA))+geom_line()+xlab("Date") +
  scale_x_date(date_breaks = "4 years", date_labels = "%Y", limits = c(min_1,max_1))
m_sp_cc<-m_sp_cc+facet_grid(thickness1fac~thickness2fac)+scale_color_gradient2(low="2547f2",mid = "#25edf2", high="#FC4E07")
m_sp_cc<-m_sp_cc+coord_cartesian(ylim=c(0,1))+ggtitle("S&P500 and Cocoa")+theme(text=element_text(family="serif"))




####### S&P500 Cotton

DT114<-NULL
dt114<-data.table(t=1:T,date=y$Date,x=m1,series=paste0("series",1))
DT114<-rbind(DT114,dt114)  
dt114<-data.table(t=1:T,date=y$Date,x=m14,series=paste0("series",2))
DT114<-rbind(DT114,dt114)


#needed to put on the same scale!
DTnorm114<-DT114[,.(t,date,x=(x-mean(x,na.rm=TRUE))/sd(x,na.rm=TRUE) ),by=.(series)]

#compute TPT
TPT114<-NULL
tau.values<-c(22,126,252,756)

for(i in 1:length(tau.values))
{
  temp114<-DTnorm114[,.(U=myoperator(x=t,y=x,tau=tau.values[i],operator=max),
                        L=myoperator(x=t,y=x,tau=tau.values[i],operator=min),
                        t,date=as.Date(date)),by=.(series)]
  temp114$thickness<-tau.values[i]
  TPT114<-rbind(TPT114,temp114)
}

TPT114$L[is.infinite(TPT114$L)]<-NA
TPT114$U[is.infinite(TPT114$U)]<-NA

# compute MTTPMA, all possible combinations, use 'setkey'


setkey(TPT114,series,thickness)

tau.values.matrix<-t(combn(tau.values,m=2))
K<-dim(tau.values.matrix)[2]
temp114<-tau.values.matrix[,K:1] #reverse the combinations!
tau.values.matrix<-rbind(tau.values.matrix,temp114)
tau.values.matrix<-rbind(tau.values.matrix,cbind(tau.values,tau.values) )

MTTPMA114<-NULL
for(i in 1:dim(tau.values.matrix)[1])#as many tau vectors as the number of rows
{    
  temp114<-TPT114[.(paste0("series",1:K),tau.values.matrix[i,]),
                  .(MTTPMA=(min(U)-max(L))/(max(U)-min(L)),
                    thickness=toString(c(1:K,tau.values.matrix[i,]) ),
                    thickness1=tau.values.matrix[i,1],
                    thickness2=tau.values.matrix[i,2]),by=.(date)]
  MTTPMA114<-rbind(MTTPMA114,temp114)
}

#replace '-1' by NA
MTTPMA114$MTTPMA[MTTPMA114$MTTPMA==-1]<-NA

#plotting (Figure 5 of the current article)
temp114<-MTTPMA114

temp114[,`:=`(thickness1fac=paste0("thickness1=",thickness1),
              thickness2fac=paste0("thickness2=",thickness2))]

temp114[,`:=`(thickness1fac=reorder(thickness1fac,thickness1))]
temp114[,`:=`(thickness2fac=reorder(thickness2fac,thickness2))]


m_sp_ct<-ggplot(temp114,aes(x=date,y=MTTPMA,color=MTTPMA))+geom_line()+xlab("Date") +
  scale_x_date(date_breaks = "4 years", date_labels = "%Y", limits = c(min_1,max_1))
m_sp_ct<-m_sp_ct+facet_grid(thickness1fac~thickness2fac)+scale_color_gradient2(low="2547f2",mid = "#25edf2", high="#FC4E07")
m_sp_ct<-m_sp_ct+coord_cartesian(ylim=c(0,1))+ggtitle("S&P500 and Cotton")+theme(text=element_text(family="serif"))


####### S&P500 Orange Juice

DT115<-NULL
dt115<-data.table(t=1:T,date=y$Date,x=m1,series=paste0("series",1))
DT115<-rbind(DT115,dt115)  
dt115<-data.table(t=1:T,date=y$Date,x=m15,series=paste0("series",2))
DT115<-rbind(DT115,dt115)


#needed to put on the same scale!
DTnorm115<-DT115[,.(t,date,x=(x-mean(x,na.rm=TRUE))/sd(x,na.rm=TRUE) ),by=.(series)]

#compute TPT
TPT115<-NULL
tau.values<-c(22,126,252,756)

for(i in 1:length(tau.values))
{
  temp115<-DTnorm115[,.(U=myoperator(x=t,y=x,tau=tau.values[i],operator=max),
                        L=myoperator(x=t,y=x,tau=tau.values[i],operator=min),
                        t,date=as.Date(date)),by=.(series)]
  temp115$thickness<-tau.values[i]
  TPT115<-rbind(TPT115,temp115)
}

TPT115$L[is.infinite(TPT115$L)]<-NA
TPT115$U[is.infinite(TPT115$U)]<-NA

# compute MTTPMA, all possible combinations, use 'setkey'


setkey(TPT115,series,thickness)

tau.values.matrix<-t(combn(tau.values,m=2))
K<-dim(tau.values.matrix)[2]
temp115<-tau.values.matrix[,K:1] #reverse the combinations!
tau.values.matrix<-rbind(tau.values.matrix,temp115)
tau.values.matrix<-rbind(tau.values.matrix,cbind(tau.values,tau.values) )

MTTPMA115<-NULL
for(i in 1:dim(tau.values.matrix)[1])#as many tau vectors as the number of rows
{    
  temp115<-TPT115[.(paste0("series",1:K),tau.values.matrix[i,]),
                  .(MTTPMA=(min(U)-max(L))/(max(U)-min(L)),
                    thickness=toString(c(1:K,tau.values.matrix[i,]) ),
                    thickness1=tau.values.matrix[i,1],
                    thickness2=tau.values.matrix[i,2]),by=.(date)]
  MTTPMA115<-rbind(MTTPMA115,temp115)
}

#replace '-1' by NA
MTTPMA115$MTTPMA[MTTPMA115$MTTPMA==-1]<-NA

#plotting (Figure 5 of the current article)
temp115<-MTTPMA115

temp115[,`:=`(thickness1fac=paste0("thickness1=",thickness1),
              thickness2fac=paste0("thickness2=",thickness2))]

temp115[,`:=`(thickness1fac=reorder(thickness1fac,thickness1))]
temp115[,`:=`(thickness2fac=reorder(thickness2fac,thickness2))]


m_sp_oj<-ggplot(temp115,aes(x=date,y=MTTPMA,color=MTTPMA))+geom_line()+xlab("Date") +
  scale_x_date(date_breaks = "4 years", date_labels = "%Y", limits = c(min_1,max_1))
m_sp_oj<-m_sp_oj+facet_grid(thickness1fac~thickness2fac)+scale_color_gradient2(low="2547f2",mid = "#25edf2", high="#FC4E07")
m_sp_oj<-m_sp_oj+coord_cartesian(ylim=c(0,1))+ggtitle("S&P500 and Orange Juice")+theme(text=element_text(family="serif"))




####### S&P500 Lumber
DT116<-NULL
dt116<-data.table(t=1:T,date=y$Date,x=m1,series=paste0("series",1))
DT116<-rbind(DT116,dt116)  
dt116<-data.table(t=1:T,date=y$Date,x=m16,series=paste0("series",2))
DT116<-rbind(DT116,dt116)


#needed to put on the same scale!
DTnorm116<-DT116[,.(t,date,x=(x-mean(x,na.rm=TRUE))/sd(x,na.rm=TRUE) ),by=.(series)]

#compute TPT
TPT116<-NULL
tau.values<-c(22,126,252,756)

for(i in 1:length(tau.values))
{
  temp116<-DTnorm116[,.(U=myoperator(x=t,y=x,tau=tau.values[i],operator=max),
                        L=myoperator(x=t,y=x,tau=tau.values[i],operator=min),
                        t,date=as.Date(date)),by=.(series)]
  temp116$thickness<-tau.values[i]
  TPT116<-rbind(TPT116,temp116)
}

TPT116$L[is.infinite(TPT116$L)]<-NA
TPT116$U[is.infinite(TPT116$U)]<-NA

# compute MTTPMA, all possible combinations, use 'setkey'


setkey(TPT116,series,thickness)

tau.values.matrix<-t(combn(tau.values,m=2))
K<-dim(tau.values.matrix)[2]
temp116<-tau.values.matrix[,K:1] #reverse the combinations!
tau.values.matrix<-rbind(tau.values.matrix,temp116)
tau.values.matrix<-rbind(tau.values.matrix,cbind(tau.values,tau.values) )

MTTPMA116<-NULL
for(i in 1:dim(tau.values.matrix)[1])#as many tau vectors as the number of rows
{    
  temp116<-TPT116[.(paste0("series",1:K),tau.values.matrix[i,]),
                  .(MTTPMA=(min(U)-max(L))/(max(U)-min(L)),
                    thickness=toString(c(1:K,tau.values.matrix[i,]) ),
                    thickness1=tau.values.matrix[i,1],
                    thickness2=tau.values.matrix[i,2]),by=.(date)]
  MTTPMA116<-rbind(MTTPMA116,temp116)
}

#replace '-1' by NA
MTTPMA116$MTTPMA[MTTPMA116$MTTPMA==-1]<-NA

#plotting (Figure 5 of the current article)
temp116<-MTTPMA116

temp116[,`:=`(thickness1fac=paste0("thickness1=",thickness1),
              thickness2fac=paste0("thickness2=",thickness2))]

temp116[,`:=`(thickness1fac=reorder(thickness1fac,thickness1))]
temp116[,`:=`(thickness2fac=reorder(thickness2fac,thickness2))]

m_sp_lb<-ggplot(temp116,aes(x=date,y=MTTPMA,color=MTTPMA))+geom_line()+xlab("Date") +
  scale_x_date(date_breaks = "4 years", date_labels = "%Y", limits = c(min_1,max_1))
m_sp_lb<-m_sp_lb+facet_grid(thickness1fac~thickness2fac)+scale_color_gradient2(low="2547f2",mid = "#25edf2", high="#FC4E07")
m_sp_lb<-m_sp_lb+coord_cartesian(ylim=c(0,1))+ggtitle("S&P500 and Lumber")+theme(text=element_text(family="serif"))




####### S&P500 Live cattle
DT117<-NULL
dt117<-data.table(t=1:T,date=y$Date,x=m1,series=paste0("series",1))
DT117<-rbind(DT117,dt117)  
dt117<-data.table(t=1:T,date=y$Date,x=m17,series=paste0("series",2))
DT117<-rbind(DT117,dt117)


#needed to put on the same scale!
DTnorm117<-DT117[,.(t,date,x=(x-mean(x,na.rm=TRUE))/sd(x,na.rm=TRUE) ),by=.(series)]

#compute TPT
TPT117<-NULL
tau.values<-c(22,126,252,756)

for(i in 1:length(tau.values))
{
  temp117<-DTnorm117[,.(U=myoperator(x=t,y=x,tau=tau.values[i],operator=max),
                        L=myoperator(x=t,y=x,tau=tau.values[i],operator=min),
                        t,date=as.Date(date)),by=.(series)]
  temp117$thickness<-tau.values[i]
  TPT117<-rbind(TPT117,temp117)
}

TPT117$L[is.infinite(TPT117$L)]<-NA
TPT117$U[is.infinite(TPT117$U)]<-NA

# compute MTTPMA, all possible combinations, use 'setkey'


setkey(TPT117,series,thickness)

tau.values.matrix<-t(combn(tau.values,m=2))
K<-dim(tau.values.matrix)[2]
temp117<-tau.values.matrix[,K:1] #reverse the combinations!
tau.values.matrix<-rbind(tau.values.matrix,temp117)
tau.values.matrix<-rbind(tau.values.matrix,cbind(tau.values,tau.values) )

MTTPMA117<-NULL
for(i in 1:dim(tau.values.matrix)[1])#as many tau vectors as the number of rows
{    
  temp117<-TPT117[.(paste0("series",1:K),tau.values.matrix[i,]),
                  .(MTTPMA=(min(U)-max(L))/(max(U)-min(L)),
                    thickness=toString(c(1:K,tau.values.matrix[i,]) ),
                    thickness1=tau.values.matrix[i,1],
                    thickness2=tau.values.matrix[i,2]),by=.(date)]
  MTTPMA117<-rbind(MTTPMA117,temp117)
}

#replace '-1' by NA
MTTPMA117$MTTPMA[MTTPMA117$MTTPMA==-1]<-NA

#plotting (Figure 5 of the current article)
temp117<-MTTPMA117

temp117[,`:=`(thickness1fac=paste0("thickness1=",thickness1),
              thickness2fac=paste0("thickness2=",thickness2))]

temp117[,`:=`(thickness1fac=reorder(thickness1fac,thickness1))]
temp117[,`:=`(thickness2fac=reorder(thickness2fac,thickness2))]


m_sp_lc<-ggplot(temp117,aes(x=date,y=MTTPMA,color=MTTPMA))+geom_line()+xlab("Date") +
  scale_x_date(date_breaks = "4 years", date_labels = "%Y", limits = c(min_1,max_1))
m_sp_lc<-m_sp_lc+facet_grid(thickness1fac~thickness2fac)+scale_color_gradient2(low="2547f2",mid = "#25edf2", high="#FC4E07")
m_sp_lc<-m_sp_lc+coord_cartesian(ylim=c(0,1))+ggtitle("S&P500 and Live Cattle")+theme(text=element_text(family="serif"))



####### S&P500 Feeder cattle
DT118<-NULL
dt118<-data.table(t=1:T,date=y$Date,x=m1,series=paste0("series",1))
DT118<-rbind(DT118,dt118)  
dt118<-data.table(t=1:T,date=y$Date,x=m18,series=paste0("series",2))
DT118<-rbind(DT118,dt118)


#needed to put on the same scale!
DTnorm118<-DT118[,.(t,date,x=(x-mean(x,na.rm=TRUE))/sd(x,na.rm=TRUE) ),by=.(series)]

#compute TPT
TPT118<-NULL
tau.values<-c(22,126,252,756)

for(i in 1:length(tau.values))
{
  temp118<-DTnorm118[,.(U=myoperator(x=t,y=x,tau=tau.values[i],operator=max),
                        L=myoperator(x=t,y=x,tau=tau.values[i],operator=min),
                        t,date=as.Date(date)),by=.(series)]
  temp118$thickness<-tau.values[i]
  TPT118<-rbind(TPT118,temp118)
}

TPT118$L[is.infinite(TPT118$L)]<-NA
TPT118$U[is.infinite(TPT118$U)]<-NA

# compute MTTPMA, all possible combinations, use 'setkey'


setkey(TPT118,series,thickness)

tau.values.matrix<-t(combn(tau.values,m=2))
K<-dim(tau.values.matrix)[2]
temp118<-tau.values.matrix[,K:1] #reverse the combinations!
tau.values.matrix<-rbind(tau.values.matrix,temp118)
tau.values.matrix<-rbind(tau.values.matrix,cbind(tau.values,tau.values) )

MTTPMA118<-NULL
for(i in 1:dim(tau.values.matrix)[1])#as many tau vectors as the number of rows
{    
  temp118<-TPT118[.(paste0("series",1:K),tau.values.matrix[i,]),
                  .(MTTPMA=(min(U)-max(L))/(max(U)-min(L)),
                    thickness=toString(c(1:K,tau.values.matrix[i,]) ),
                    thickness1=tau.values.matrix[i,1],
                    thickness2=tau.values.matrix[i,2]),by=.(date)]
  MTTPMA118<-rbind(MTTPMA118,temp118)
}

#replace '-1' by NA
MTTPMA118$MTTPMA[MTTPMA118$MTTPMA==-1]<-NA

#plotting (Figure 5 of the current article)
temp118<-MTTPMA118

temp118[,`:=`(thickness1fac=paste0("thickness1=",thickness1),
              thickness2fac=paste0("thickness2=",thickness2))]

temp118[,`:=`(thickness1fac=reorder(thickness1fac,thickness1))]
temp118[,`:=`(thickness2fac=reorder(thickness2fac,thickness2))]


m_sp_fc<-ggplot(temp118,aes(x=date,y=MTTPMA,color=MTTPMA))+geom_line()+xlab("Date") +
  scale_x_date(date_breaks = "4 years", date_labels = "%Y", limits = c(min_1,max_1))
m_sp_fc<-m_sp_fc+facet_grid(thickness1fac~thickness2fac)+scale_color_gradient2(low="2547f2",mid = "#25edf2", high="#FC4E07")
m_sp_fc<-m_sp_fc+coord_cartesian(ylim=c(0,1))+ggtitle("S&P500 and Feeder Cattle")+theme(text=element_text(family="serif"))



####### S&P500 Crude oil

DT119<-NULL
dt119<-data.table(t=1:T,date=y$Date,x=m1,series=paste0("series",1))
DT119<-rbind(DT119,dt119)  
dt119<-data.table(t=1:T,date=y$Date,x=m19,series=paste0("series",2))
DT119<-rbind(DT119,dt119)


#needed to put on the same scale!
DTnorm119<-DT119[,.(t,date,x=(x-mean(x,na.rm=TRUE))/sd(x,na.rm=TRUE) ),by=.(series)]

#compute TPT
TPT119<-NULL
tau.values<-c(22,126,252,756)

for(i in 1:length(tau.values))
{
  temp119<-DTnorm119[,.(U=myoperator(x=t,y=x,tau=tau.values[i],operator=max),
                        L=myoperator(x=t,y=x,tau=tau.values[i],operator=min),
                        t,date=as.Date(date)),by=.(series)]
  temp119$thickness<-tau.values[i]
  TPT119<-rbind(TPT119,temp119)
}

TPT119$L[is.infinite(TPT119$L)]<-NA
TPT119$U[is.infinite(TPT119$U)]<-NA

# compute MTTPMA, all possible combinations, use 'setkey'


setkey(TPT119,series,thickness)

tau.values.matrix<-t(combn(tau.values,m=2))
K<-dim(tau.values.matrix)[2]
temp119<-tau.values.matrix[,K:1] #reverse the combinations!
tau.values.matrix<-rbind(tau.values.matrix,temp119)
tau.values.matrix<-rbind(tau.values.matrix,cbind(tau.values,tau.values) )

MTTPMA119<-NULL
for(i in 1:dim(tau.values.matrix)[1])#as many tau vectors as the number of rows
{    
  temp119<-TPT119[.(paste0("series",1:K),tau.values.matrix[i,]),
                  .(MTTPMA=(min(U)-max(L))/(max(U)-min(L)),
                    thickness=toString(c(1:K,tau.values.matrix[i,]) ),
                    thickness1=tau.values.matrix[i,1],
                    thickness2=tau.values.matrix[i,2]),by=.(date)]
  MTTPMA119<-rbind(MTTPMA119,temp119)
}

#replace '-1' by NA
MTTPMA119$MTTPMA[MTTPMA119$MTTPMA==-1]<-NA

#plotting (Figure 5 of the current article)
temp119<-MTTPMA119

temp119[,`:=`(thickness1fac=paste0("thickness1=",thickness1),
              thickness2fac=paste0("thickness2=",thickness2))]

temp119[,`:=`(thickness1fac=reorder(thickness1fac,thickness1))]
temp119[,`:=`(thickness2fac=reorder(thickness2fac,thickness2))]


m_sp_cl<-ggplot(temp119,aes(x=date,y=MTTPMA,color=MTTPMA))+geom_line()+xlab("Date") +
  scale_x_date(date_breaks = "4 years", date_labels = "%Y", limits = c(min_1,max_1))
m_sp_cl<-m_sp_cl+facet_grid(thickness1fac~thickness2fac)+scale_color_gradient2(low="2547f2",mid = "#25edf2", high="#FC4E07")
m_sp_cl<-m_sp_cl+coord_cartesian(ylim=c(0,1))+ggtitle("S&P500 and Crude Oil")+theme(text=element_text(family="serif"),legend.position="bottom",axis.text.x = element_text(color="black", size=8, angle=20))




####### S&P500 Heating Oil

DT120<-NULL
dt120<-data.table(t=1:T,date=y$Date,x=m1,series=paste0("series",1))
DT120<-rbind(DT120,dt120)  
dt120<-data.table(t=1:T,date=y$Date,x=m20,series=paste0("series",2))
DT120<-rbind(DT120,dt120)


#needed to put on the same scale!
DTnorm120<-DT120[,.(t,date,x=(x-mean(x,na.rm=TRUE))/sd(x,na.rm=TRUE) ),by=.(series)]

#compute TPT
TPT120<-NULL
tau.values<-c(22,126,252,756)

for(i in 1:length(tau.values))
{
  temp120<-DTnorm120[,.(U=myoperator(x=t,y=x,tau=tau.values[i],operator=max),
                        L=myoperator(x=t,y=x,tau=tau.values[i],operator=min),
                        t,date=as.Date(date)),by=.(series)]
  temp120$thickness<-tau.values[i]
  TPT120<-rbind(TPT120,temp120)
}

TPT120$L[is.infinite(TPT120$L)]<-NA
TPT120$U[is.infinite(TPT120$U)]<-NA

# compute MTTPMA, all possible combinations, use 'setkey'


setkey(TPT120,series,thickness)

tau.values.matrix<-t(combn(tau.values,m=2))
K<-dim(tau.values.matrix)[2]
temp120<-tau.values.matrix[,K:1] #reverse the combinations!
tau.values.matrix<-rbind(tau.values.matrix,temp120)
tau.values.matrix<-rbind(tau.values.matrix,cbind(tau.values,tau.values) )

MTTPMA120<-NULL
for(i in 1:dim(tau.values.matrix)[1])#as many tau vectors as the number of rows
{    
  temp120<-TPT120[.(paste0("series",1:K),tau.values.matrix[i,]),
                  .(MTTPMA=(min(U)-max(L))/(max(U)-min(L)),
                    thickness=toString(c(1:K,tau.values.matrix[i,]) ),
                    thickness1=tau.values.matrix[i,1],
                    thickness2=tau.values.matrix[i,2]),by=.(date)]
  MTTPMA120<-rbind(MTTPMA120,temp120)
}

#replace '-1' by NA
MTTPMA120$MTTPMA[MTTPMA120$MTTPMA==-1]<-NA

#plotting (Figure 5 of the current article)
temp120<-MTTPMA120

temp120[,`:=`(thickness1fac=paste0("thickness1=",thickness1),
              thickness2fac=paste0("thickness2=",thickness2))]

temp120[,`:=`(thickness1fac=reorder(thickness1fac,thickness1))]
temp120[,`:=`(thickness2fac=reorder(thickness2fac,thickness2))]

m_sp_ho<-ggplot(temp120,aes(x=date,y=MTTPMA,color=MTTPMA))+geom_line()+xlab("Date") +
  scale_x_date(date_breaks = "4 years", date_labels = "%Y", limits = c(min_1,max_1))
m_sp_ho<-m_sp_ho+facet_grid(thickness1fac~thickness2fac)+scale_color_gradient2(low="2547f2",mid = "#25edf2", high="#FC4E07")
m_sp_ho<-m_sp_ho+coord_cartesian(ylim=c(0,1))+ggtitle("S&P500 and Heating Oil")+theme(text=element_text(family="serif"))




####### S&P500 Natural Gas
DT121<-NULL
dt121<-data.table(t=1:T,date=y$Date,x=m1,series=paste0("series",1))
DT121<-rbind(DT121,dt121)  
dt121<-data.table(t=1:T,date=y$Date,x=m21,series=paste0("series",2))
DT121<-rbind(DT121,dt121)


#needed to put on the same scale!
DTnorm121<-DT121[,.(t,date,x=(x-mean(x,na.rm=TRUE))/sd(x,na.rm=TRUE) ),by=.(series)]

#compute TPT
TPT121<-NULL
tau.values<-c(22,126,252,756)

for(i in 1:length(tau.values))
{
  temp121<-DTnorm121[,.(U=myoperator(x=t,y=x,tau=tau.values[i],operator=max),
                        L=myoperator(x=t,y=x,tau=tau.values[i],operator=min),
                        t,date=as.Date(date)),by=.(series)]
  temp121$thickness<-tau.values[i]
  TPT121<-rbind(TPT121,temp121)
}

TPT121$L[is.infinite(TPT121$L)]<-NA
TPT121$U[is.infinite(TPT121$U)]<-NA

# compute MTTPMA, all possible combinations, use 'setkey'


setkey(TPT121,series,thickness)

tau.values.matrix<-t(combn(tau.values,m=2))
K<-dim(tau.values.matrix)[2]
temp121<-tau.values.matrix[,K:1] #reverse the combinations!
tau.values.matrix<-rbind(tau.values.matrix,temp121)
tau.values.matrix<-rbind(tau.values.matrix,cbind(tau.values,tau.values) )

MTTPMA121<-NULL
for(i in 1:dim(tau.values.matrix)[1])#as many tau vectors as the number of rows
{    
  temp121<-TPT121[.(paste0("series",1:K),tau.values.matrix[i,]),
                  .(MTTPMA=(min(U)-max(L))/(max(U)-min(L)),
                    thickness=toString(c(1:K,tau.values.matrix[i,]) ),
                    thickness1=tau.values.matrix[i,1],
                    thickness2=tau.values.matrix[i,2]),by=.(date)]
  MTTPMA121<-rbind(MTTPMA121,temp121)
}

#replace '-1' by NA
MTTPMA121$MTTPMA[MTTPMA121$MTTPMA==-1]<-NA

#plotting (Figure 5 of the current article)
temp121<-MTTPMA121

temp121[,`:=`(thickness1fac=paste0("thickness1=",thickness1),
              thickness2fac=paste0("thickness2=",thickness2))]

temp121[,`:=`(thickness1fac=reorder(thickness1fac,thickness1))]
temp121[,`:=`(thickness2fac=reorder(thickness2fac,thickness2))]


m_sp_ng<-ggplot(temp121,aes(x=date,y=MTTPMA,color=MTTPMA))+geom_line()+xlab("Date") +
  scale_x_date(date_breaks = "4 years", date_labels = "%Y", limits = c(min_1,max_1))
m_sp_ng<-m_sp_ng+facet_grid(thickness1fac~thickness2fac)+scale_color_gradient2(low="2547f2",mid = "#25edf2", high="#FC4E07")
m_sp_ng<-m_sp_ng+coord_cartesian(ylim=c(0,1))+ggtitle("S&P500 and Natural Gas")+theme(text=element_text(family="serif"))




####### S&P500 Gold
DT122<-NULL
dt122<-data.table(t=1:T,date=y$Date,x=m1,series=paste0("series",1))
DT122<-rbind(DT122,dt122)  
dt122<-data.table(t=1:T,date=y$Date,x=m22,series=paste0("series",2))
DT122<-rbind(DT122,dt122)


#needed to put on the same scale!
DTnorm122<-DT122[,.(t,date,x=(x-mean(x,na.rm=TRUE))/sd(x,na.rm=TRUE) ),by=.(series)]

#compute TPT
TPT122<-NULL
tau.values<-c(22,126,252,756)

for(i in 1:length(tau.values))
{
  temp122<-DTnorm122[,.(U=myoperator(x=t,y=x,tau=tau.values[i],operator=max),
                        L=myoperator(x=t,y=x,tau=tau.values[i],operator=min),
                        t,date=as.Date(date)),by=.(series)]
  temp122$thickness<-tau.values[i]
  TPT122<-rbind(TPT122,temp122)
}

TPT122$L[is.infinite(TPT122$L)]<-NA
TPT122$U[is.infinite(TPT122$U)]<-NA

# compute MTTPMA, all possible combinations, use 'setkey'


setkey(TPT122,series,thickness)

tau.values.matrix<-t(combn(tau.values,m=2))
K<-dim(tau.values.matrix)[2]
temp122<-tau.values.matrix[,K:1] #reverse the combinations!
tau.values.matrix<-rbind(tau.values.matrix,temp122)
tau.values.matrix<-rbind(tau.values.matrix,cbind(tau.values,tau.values) )

MTTPMA122<-NULL
for(i in 1:dim(tau.values.matrix)[1])#as many tau vectors as the number of rows
{    
  temp122<-TPT122[.(paste0("series",1:K),tau.values.matrix[i,]),
                  .(MTTPMA=(min(U)-max(L))/(max(U)-min(L)),
                    thickness=toString(c(1:K,tau.values.matrix[i,]) ),
                    thickness1=tau.values.matrix[i,1],
                    thickness2=tau.values.matrix[i,2]),by=.(date)]
  MTTPMA122<-rbind(MTTPMA122,temp122)
}

#replace '-1' by NA
MTTPMA122$MTTPMA[MTTPMA122$MTTPMA==-1]<-NA

#plotting (Figure 5 of the current article)
temp122<-MTTPMA122

temp122[,`:=`(thickness1fac=paste0("thickness1=",thickness1),
              thickness2fac=paste0("thickness2=",thickness2))]

temp122[,`:=`(thickness1fac=reorder(thickness1fac,thickness1))]
temp122[,`:=`(thickness2fac=reorder(thickness2fac,thickness2))]

m_sp_gc<-ggplot(temp122,aes(x=date,y=MTTPMA,color=MTTPMA))+geom_line()+xlab("Date") +
  scale_x_date(date_breaks = "4 years", date_labels = "%Y", limits = c(min_1,max_1))
m_sp_gc<-m_sp_gc+facet_grid(thickness1fac~thickness2fac)+scale_color_gradient2(low="2547f2",mid = "#25edf2", high="#FC4E07")
m_sp_gc<-m_sp_gc+coord_cartesian(ylim=c(0,1))+ggtitle("S&P500 and Gold")+theme(text=element_text(family="serif"))




####### S&P500 Copper
DT123<-NULL
dt123<-data.table(t=1:T,date=y$Date,x=m1,series=paste0("series",1))
DT123<-rbind(DT123,dt123)  
dt123<-data.table(t=1:T,date=y$Date,x=m23,series=paste0("series",2))
DT123<-rbind(DT123,dt123)


#needed to put on the same scale!
DTnorm123<-DT123[,.(t,date,x=(x-mean(x,na.rm=TRUE))/sd(x,na.rm=TRUE) ),by=.(series)]

#compute TPT
TPT123<-NULL
tau.values<-c(22,126,252,756)

for(i in 1:length(tau.values))
{
  temp123<-DTnorm123[,.(U=myoperator(x=t,y=x,tau=tau.values[i],operator=max),
                        L=myoperator(x=t,y=x,tau=tau.values[i],operator=min),
                        t,date=as.Date(date)),by=.(series)]
  temp123$thickness<-tau.values[i]
  TPT123<-rbind(TPT123,temp123)
}

TPT123$L[is.infinite(TPT123$L)]<-NA
TPT123$U[is.infinite(TPT123$U)]<-NA

# compute MTTPMA, all possible combinations, use 'setkey'


setkey(TPT123,series,thickness)

tau.values.matrix<-t(combn(tau.values,m=2))
K<-dim(tau.values.matrix)[2]
temp123<-tau.values.matrix[,K:1] #reverse the combinations!
tau.values.matrix<-rbind(tau.values.matrix,temp123)
tau.values.matrix<-rbind(tau.values.matrix,cbind(tau.values,tau.values) )

MTTPMA123<-NULL
for(i in 1:dim(tau.values.matrix)[1])#as many tau vectors as the number of rows
{    
  temp123<-TPT123[.(paste0("series",1:K),tau.values.matrix[i,]),
                  .(MTTPMA=(min(U)-max(L))/(max(U)-min(L)),
                    thickness=toString(c(1:K,tau.values.matrix[i,]) ),
                    thickness1=tau.values.matrix[i,1],
                    thickness2=tau.values.matrix[i,2]),by=.(date)]
  MTTPMA123<-rbind(MTTPMA123,temp123)
}

#replace '-1' by NA
MTTPMA123$MTTPMA[MTTPMA123$MTTPMA==-1]<-NA

#plotting (Figure 5 of the current article)
temp123<-MTTPMA123

temp123[,`:=`(thickness1fac=paste0("thickness1=",thickness1),
              thickness2fac=paste0("thickness2=",thickness2))]

temp123[,`:=`(thickness1fac=reorder(thickness1fac,thickness1))]
temp123[,`:=`(thickness2fac=reorder(thickness2fac,thickness2))]

m_sp_hg<-ggplot(temp123,aes(x=date,y=MTTPMA,color=MTTPMA))+geom_line()+xlab("Date") +
  scale_x_date(date_breaks = "4 years", date_labels = "%Y", limits = c(min_1,max_1))
m_sp_hg<-m_sp_hg+facet_grid(thickness1fac~thickness2fac)+scale_color_gradient2(low="2547f2",mid = "#25edf2", high="#FC4E07")
m_sp_hg<-m_sp_hg+coord_cartesian(ylim=c(0,1))+ggtitle("S&P500 and Copper")+theme(text=element_text(family="serif"))






