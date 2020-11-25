library(ggplot2)
library(ggExtra)    
library(tidyverse)
library(gridExtra)

# carga de datos y filtro por origen conocido y residencia en Cataluña
repres <- read.csv("represaliats.csv", na.strings="")
repres <- subset(repres, !(is.na(edat)) & !(is.na(sexe)) & edat>0)

cat <- subset(repres,provincia.res %in% c("Barcelona", "Girona", "Tarragona", "Lleida"))

cat2 <- cat %>%  group_by(ccaa.nai, provincia.res)%>% summarise(count=n())
cat2 <- subset(cat2, !(is.na(ccaa.nai)))


# fig 4 - heatmap de origenes
pdf("fig_04.pdf", width=5, height=10)
ggplot(cat2, aes(y=reorder(ccaa.nai, count), x=provincia.res, fill=count, label=count)) + geom_tile(col="white") + geom_text(colour='black') + scale_fill_distiller(name='# de causas',type='div',palette = 9)+theme_minimal() + theme(legend.position="none") + xlab("provincia de residencia") + ylab("ccaa de nacimiento")+ theme(axis.text.x = element_text(angle = 45, vjust=1, hjust=1), axis.ticks = element_blank(), panel.background = element_blank()) + coord_fixed()+theme(axis.text=element_text(size=14), axis.title=element_text(size=14, face="bold"))
dev.off()


# fig 5 - causas-ejecuciones por año 
totsAnys <- data.frame(any.final=1938:1983, mean=NA)
afusellats.mean <- afusellats %>% group_by(any.final) %>% summarise(mean=mean(edat))
afusellats.seq <- data.frame(any.final=totsAnys$any.final, mean=with(afusellats.mean, mean[match(totsAnys$any.final, any.final)]))
tots <- subset(repres, !(is.na(any.final)))
tots.mean <- tots %>% group_by(any.final) %>% summarise(mean=mean(edat))
tots.seq <- data.frame(any.final=totsAnys$any.final, mean=with(tots.mean, mean[match(totsAnys$any.final, any.final)]))

pdf("fig_05.pdf", width=10, height=4)    
g1 <- ggplot(tots, aes(x=any.final)) + geom_histogram(fill="skyblue3", alpha=0.5, col="skyblue3", binwidth=1) + theme_bw() + scale_x_continuous(breaks=seq(1940,1980,10), limits=c(1937,1980)) + xlab("año") + ylab("num. causas") +theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold")) + annotate("label", x = 1980, y = 30000, label = "causas abiertas en Catalunya\ndurante el franquismo", fill="skyblue3", col="white", size=5, hjust=1, vjust=1) 
g2 <- ggplot(afusellats, aes(x=any.final)) + geom_histogram(fill="#651C32", alpha=0.5, col="#651C32", binwidth=1) + theme_bw() + scale_x_continuous(breaks=seq(1940,1980,10), limits=c(1937,1980)) + xlab("año") + ylab("num. ejecutados") +theme(axis.text=element_text(size=12), axis.title=element_text(size=14, face="bold")) + annotate("label", x = 1980, y = 2500, label = "personas ejecutadas", fill="#651C32", col="white", size=5, hjust=1, vjust=1) 
grid.arrange(g1,g2, ncol=2)
dev.off()

# fig 6 - causas-ejecuciones por año (log)
pdf("fig_06.pdf", width=10, height=4)    
g1 <- ggplot(tots, aes(x=any.final)) + geom_histogram(fill="skyblue3", alpha=0.5, col="skyblue3", binwidth=1) + theme_bw() + scale_x_continuous(breaks=seq(1940,1980,10), limits=c(1937,1980)) + xlab("any") + ylab("num. causes") +theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold")) + annotate("label", x = 1980, y = 30000, label = "causas abiertas en Catalunya\ndurante el franquismo", fill="skyblue3", col="white", size=5, hjust=1, vjust=1) + scale_y_log10()
g2 <- ggplot(afusellats, aes(x=any.final)) + geom_histogram(fill="#651C32", alpha=0.5, col="#651C32", binwidth=1) + theme_bw() + scale_x_continuous(breaks=seq(1940,1980,10), limits=c(1937,1980)) + xlab("año") + ylab("num. ejecutados") +theme(axis.text=element_text(size=12), axis.title=element_text(size=14, face="bold")) + annotate("label", x = 1980, y = 2500, label = "personas ejecutadas", fill="#651C32", col="white", size=5, hjust=1, vjust=1)  + scale_y_log10()
grid.arrange(g1,g2, ncol=2)
dev.off()

# fig 7 - distribución de ejecutados
afusellats <- subset(repres, executat=="sí" & !(is.na(any.final)))
bb <- ggplot(afusellats, aes(x=any.final, y=edat))+geom_jitter(size=4, width=0.5, height=0.5, shape=21, colour=alpha("#651C32", 0.5), fill=alpha("#651C32", 0.25), stroke=0.1)+ theme_minimal() + theme(legend.position="none") + scale_x_continuous(breaks=seq(1940,1975,5), sec.axis = dup_axis(name="")) + scale_y_continuous(breaks=seq(15,75,5), sec.axis = dup_axis(name="")) + xlab("año") + ylab("edad")+theme(axis.text=element_text(size=20), axis.title=element_text(size=20, face="bold"))
pdf("fig_07.pdf", width=20, height=14)
ggMarginal(bb, type="histogram", fill="#651C32", col="#651C32", alpha=0.2, xparams=list(binwidth=1), yparams=list(breaks=seq(10,80,5), binwidth=5)) 
dev.off()

# fig 8 - edades represaliados
pdf("fig_08.pdf", width=15, height=5)
ggplot(tots.seq, aes(x=any.final, y=mean)) + geom_line(col="skyblue3", size=2) + geom_point(col="skyblue3", size=4) + geom_line(data=afusellats.seq, aes(x=any.final, y=mean), col="#651C32", size=2) + geom_point(data=afusellats.seq, aes(x=any.final, y=mean), col="#651C32", size=4) + theme_bw() + theme(legend.position="none") + scale_x_continuous(breaks=seq(1940,1980,5), limits=c(1938,1983)) + scale_y_continuous(breaks=seq(15, 80,5))+ annotate("label", x = 1980.8, y = 23.3, label = "represaliados", fill="skyblue3", col="white", size=5, hjust=0) + annotate("label", x = 1980.8, y = 20.7, label = "ejecutados", fill="#651C32", col="white", size=5, hjust=0) + ylab("media de edad") + xlab("año") +theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"))
dev.off()


 
