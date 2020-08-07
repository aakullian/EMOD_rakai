####################################################################################################################
#Adam Akullian
#Rakai Data Manipulation for EMOD
#START date 10/15/2018
####################################################################################################################
library(tidyr)
setwd("C:\\Users\\aakullian\\Dropbox (IDM)\\GitHub\\EMOD_rakai\\Calibration2018\\Data")
load("whopico_modeling_rakaidata.rda")

####################################################################################################################
head(rakaidata_coverage,20)
names(rakaidata_coverage)

table(rakaidata_coverage$agegroup)

rakaidata_coverage$fem_circsize <- NA
rakaidata_coverage$fem_circsize_hivneg <- NA
rakaidata_coverage$fem_circsize_hivpos <- NA

rakaidata_coverage.w <- reshape(rakaidata_coverage, direction="long", 
        varying=list(c(3,4), 
                     c(5,6),
                     c(7,8),
                     c(9,10),
                     c(14,11),
                     c(15,12),
                     c(16,13)),
        sep="_", v.names=(c("el_popsize","pt_popsize","hivsize","artsize","circsize","circsize_hivneg","circsize_hivpos")),
        timevar="gender", times=c("fem","man"))

#create new variables
rakaidata_coverage.w$mccoverage <- rakaidata_coverage.w$circsize/ rakaidata_coverage.w$pt_popsize
rakaidata_coverage.w$artcoverage <- rakaidata_coverage.w$artsize/ rakaidata_coverage.w$hivsize
rakaidata_coverage.w$hivprev <- rakaidata_coverage.w$hivsize/ rakaidata_coverage.w$pt_popsize
rakaidata_coverage.w[is.na(rakaidata_coverage.w)] <- 0
names(rakaidata_coverage.w)
rakaidata_coverage.w <- rakaidata_coverage.w[-c(11)]

head(visdates)
visdates$studyvisit <- visdates$visit
rakaidata_coverage.w <- merge(rakaidata_coverage.w, visdates, by="studyvisit")
rakaidata_coverage.w <- rakaidata_coverage.w[order(rakaidata_coverage.w$agegroup, rakaidata_coverage.w$studyvisit),]
table(rakaidata_coverage.w$median.date)
rakaidata_coverage.w$year <- as.numeric(substring(rakaidata_coverage.w$median.date,1,4))
table(rakaidata_coverage.w$year)
head(rakaidata_coverage.w)
table(rakaidata_coverage.w$agegroup)

###################################################################
#Data for EMOD calibration
setwd("C:\\Users\\aakullian\\Dropbox (IDM)\\HIV\\EMOD\\Rakai\\Data")

rakaidata_coverage.l <- reshape(rakaidata_coverage.w, 
             varying = c("el_popsize", "hivprev", "pt_popsize", "hivsize", "artsize", "circsize", "circsize_hivneg", "circsize_hivpos","mccoverage", "artcoverage"), 
             v.names = "value",
             timevar = "measurement", 
             times = c("el_popsize", "hivprev", "pt_popsize", "hivsize", "artsize", "circsize", "circsize_hivneg", "circsize_hivpos","mccoverage", "artcoverage"), 
             direction = "long")

head(rakaidata_coverage.l,100)

table(rakaidata_coverage.l$agegroup)

rakaidata_coverage.l <- rakaidata_coverage.l[order(rakaidata_coverage.l$measurement, rakaidata_coverage.l$agegroup, rakaidata_coverage.l$gender),]
head(rakaidata_coverage.l,5)
rownames(rakaidata_coverage.l) <- NULL
rakaidata_coverage.l <- rakaidata_coverage.l[ , -which(names(rakaidata_coverage.l) %in% c("studyvisit","visit","start.date","end.date","median.date","id"))]
rakaidata_coverage.l <- rakaidata_coverage.l[c("year", "gender", "agegroup","measurement","value")]

rakaidata_coverage.l$gender2 <- "Male"
rakaidata_coverage.l$gender2[rakaidata_coverage.l$gender=="fem"] <- "Female"
rakaidata_coverage.l$agegroup2 <- "[15:20)"
rakaidata_coverage.l$agegroup2[rakaidata_coverage.l$agegroup=="20to24"] <- "[20:25)"
rakaidata_coverage.l$agegroup2[rakaidata_coverage.l$agegroup=="25to29"] <- "[25:30)"
rakaidata_coverage.l$agegroup2[rakaidata_coverage.l$agegroup=="30to34"] <- "[30:35)"
rakaidata_coverage.l$agegroup2[rakaidata_coverage.l$agegroup=="35to39"] <- "[35:40)"
rakaidata_coverage.l$agegroup2[rakaidata_coverage.l$agegroup=="40to44"] <- "[40:45)"
rakaidata_coverage.l$agegroup2[rakaidata_coverage.l$agegroup=="45to49"] <- "[45:50)"

rakaidata_coverage.l$gender <- rakaidata_coverage.l$gender2
rakaidata_coverage.l$agegroup <- rakaidata_coverage.l$agegroup2

rakaidata_coverage.l <- rakaidata_coverage.l[ , -which(names(rakaidata_coverage.l) %in% c("gender2", "agegroup2"))]
head(rakaidata_coverage.l)
write.csv(rakaidata_coverage.l, "rakai_coverage_data_from_R.csv")
###################################################################

head(rakaidata_coverage.w)

year=2016
rakai.prev.chart <- ggplot() +
  geom_line(data=subset(rakaidata_coverage.w),
            aes(x=year, y=hivprev*100, group = factor(agegroup), color=factor(agegroup)), size=1.5) +
  theme(legend.position="bottom") +
  ylab("HIV prevalence (%)")+
  facet_grid(~gender) +
  theme_bw(base_size=20) +
  #scale_color_manual(labels = c("15-24", "25-34", "35-44"), values = c("blue", "purple", "red")) +
  guides(fill = guide_legend(keywidth = 2, keyheight = 1)) +
  scale_y_continuous(limits=c(0,30), breaks = seq(0,30,5), expand=c(0,0)) +
  scale_x_continuous(limits = c(1999,year+1), breaks = seq(1999,year,1), expand=c(0,0)) +
  theme(legend.position="bottom") +
  theme(legend.direction='vertical') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(strip.background = element_rect(colour="black", fill="white")) +
  guides(colour = guide_legend(nrow = 1, title=""))
rakai.prev.chart

year=2016
rakai.art.chart <- ggplot() +
  geom_line(data=subset(rakaidata_coverage.w),
               aes(x=year, y=artcoverage*100, group = factor(agegroup), color=factor(agegroup)), size=1.5) +
  theme(legend.position="bottom") +
  ylab("ART Coverage %")+
  facet_grid(~gender) +
  theme_bw(base_size=20) +
  #scale_color_manual(labels = c("15-24", "25-34", "35-44"), values = c("blue", "purple", "red")) +
  guides(fill = guide_legend(keywidth = 2, keyheight = 1)) +
  scale_y_continuous(limits=c(0,100), breaks = seq(0,100,10), expand=c(0,0)) +
  scale_x_continuous(limits = c(2000,year+1), breaks = seq(2000,year,1), expand=c(0,0)) +
  theme(legend.position="bottom") +
  theme(legend.direction='vertical') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(strip.background = element_rect(colour="black", fill="white")) +
  guides(colour = guide_legend(nrow = 1, title=""))
rakai.art.chart

rakai.vmmc.chart <- ggplot() +
  geom_line(data=subset(rakaidata_coverage.m),
            aes(x=year, y=mccoverage*100, group = factor(agegroup), color=factor(agegroup)), size=1.5) +
  theme(legend.position="bottom") +
  ylab("MC Coverage %")+
  theme_bw(base_size=20) +
  #scale_color_manual(labels = c("15-24", "25-34", "35-44"), values = c("blue", "purple", "red")) +
  guides(fill = guide_legend(keywidth = 2, keyheight = 1)) +
  scale_y_continuous(limits=c(0,100), breaks = seq(0,100,10), expand=c(0,0)) +
  scale_x_continuous(limits = c(2000,year+1), breaks = seq(2000,year,1), expand=c(0,0)) +
  theme(legend.position="bottom") +
  theme(legend.direction='vertical') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(strip.background = element_rect(colour="black", fill="white")) +
  guides(colour = guide_legend(nrow = 1, title=""))
rakai.vmmc.chart

cbind(rakaidata_coverage.m$year, rakaidata_coverage.m$agegroup, rakaidata_coverage.m$artcoverage.f, rakaidata_coverage.m$artcoverage.m)
names(rakaidata_coverage.m)
