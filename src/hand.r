#setwd("~/Desktop/Dropbox/research/flood_prediction/TWM_HaysTravisWilco_addresses_csv/")
setwd("C:/Users/yongli/Desktop/Dropbox/research/flood_prediction/TWM_HaysTravisWilco_addresses_csv/")
require(ggmap)
data<-read.csv(file="TWM_HaysTravisWilco_addresses-sorted.csv",header=TRUE,sep=",")
intervals<-200  #### grid size intervalsxintervals


sub<-data.frame(data$POINT_X[data$County=="Williamson"],data$POINT_Y[data$County=="Williamson"],data$HAND_ft[data$County=="Williamson"])
subset<-cbind(sub[,1],sub[,2],sub[,3])
names(subset)<-c("long","lat","hand")
xmin<-min(subset[,1])
xmax<-max(subset[,1])
ymin<-min(subset[,2])
ymax<-max(subset[,2])
hand_max<-0
pointx<-subset[,1]
pointy<-subset[,2]
x <- grid1( intervals, range = c(xmin,xmax))
y <- grid1( intervals, range = c(ymin,ymax))
xinterval<-(xmax-xmin)/(intervals-1)
yinterval<-(ymax-ymin)/(intervals-1)

xy <- grid2(x,y)
hand<-vector()

for(i in 1: (intervals*intervals)) { 
	X<-xy[i,1]
	Y<-xy[i,2]
  hand_tmp<-vector()
	for(m in 1:length(pointx)){
		if(pointx[m]<X-xinterval ||  pointy[m]<Y-yinterval || pointx[m]>X+xinterval || pointy[m]>Y+yinterval) next;
		hand_tmp<-c(hand_tmp,subset[m,3])
	}
	if(length(hand_tmp)==0) {
		hand<-c(hand,hand_max)
	}
	else{
		hand<-c(hand,100-mean(hand_tmp))
	}
	
} 
hand_final<-matrix(hand,ncol=length(x))
save.image()

# contour(x = x, y = y, z = hand_final, main="contour lines for hand value 25ft, grid 50x50",levels=75,drawlabels=FALSE)

pdf(file = "C:/Users/yongli/Desktop/Dropbox/research/flood_prediction/TWM_HaysTravisWilco_addresses_csv/100x100grid/hand value 35ft-grid100x100.pdf",onefile=TRUE,width=10, height=10)
cl<-contourLines(x = x, y = y, z = hand_final ,levels=65)
pol_list<-NULL
for (i in 1:length(cl)){
	pol_list<-rbind(pol_list,data.frame(cbind(cl[[i]]$x,cl[[i]]$y,rep(i,length(cl[[i]]$x)))))
}
names(pol_list)<-c("lon","lat","id")

points1<-cbind(pointx,pointy)
points1<-data.frame(points1)
names(points1)<-c("lon","lat")
require(ggmap)
# getting the map
Williamson <- get_map(location = c( lon = -97.675,lat =30.639), zoom = 10,
                    maptype = "terrain", scale = 2)

# plotting the map with some points on it
lw<-1 # line width
ggmap(Williamson)+
  geom_point(aes(x = lon, y = lat),data=points1,cex=0.01,color = "gray")+
  geom_polygon(aes(x = lon, y = lat,group=id),   data = pol_list,   color = "black", fill = NA,alpha = 0.5,size=lw) +
  labs(x = "Longitude",  y = "Latitude")+
  ggtitle("contour lines for hand value 35ft, grid 300x300")+
  theme(plot.title = element_text(size = 20, face = "bold",hjust = 0.5))

graphics.off()



