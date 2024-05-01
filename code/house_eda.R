#load packages
library(here)
library(dplyr)
library(janitor)
library(ggplot2)
library(skimr)
library(cowplot)




#I. Data import
#**************
load(file=here("data","tidy_data","tidy_house.rda"))
c_house


#II. Data Summaries
#******************

#Summary statistics
summary(c_house)
skim(c_house)


#Predictors only
#Univariate
#categorical
c_house %>% select(where(is.factor)) %>% names() %>% sort()

#lot_shape
#count
ggplot(c_house) +
  geom_bar(aes(lot_shape),fill="steelblue") +
  xlab("Lot shape") +
  ylab("Number of houses") +
  scale_x_discrete(breaks=c("Reg","IR1","IR2","IR3"),
                   labels=c("Regular","Slightly \nirregular","Moderately \nirregular","Irregular")) +
  scale_y_continuous(expand=c(0,0)) +
  theme_bw()
#percent
ggplot(c_house) +
  geom_bar(aes(lot_shape),fill="steelblue") +
  scale_x_discrete(breaks=c("Reg","IR1","IR2","IR3"),
                   labels=c("Regular","Slightly \nirregular","Moderately \nirregular","Irregular")) +
  scale_y_continuous(expand=c(0,0), labels="percent")+
  xlab("Lot shape") +
  ylab("%")

#roof_style
#count
ggplot(c_house) +
  geom_bar(aes(roof_style),fill="darkgreen") +
  labs(x="Roof style") +
  scale_y_continuous(expand=c(0,0)) + #forces x-axis to start at origin
  theme_classic()
#percent
ggplot(c_house) +
  geom_bar(aes(roof_style),fill="steelblue") +
  scale_y_continuous(labels=scales::percent) +
  labs(x="Roof style",y="%")


#numeric
c_house %>% select(where(is.double),-sale_price) %>% names() %>% sort()

ggplot(c_house) +
  geom_histogram(aes(garage_area),fill="darkred",color="black")

ggplot(c_house) +
  geom_histogram(aes(lot_area),fill="darkblue",color="black") +
  scale_y_continuous(expand=c(0,0)) +
  labs(x="Lot area", y= "Number of houses") +
  theme_classic() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))

ggplot(c_house) +
  geom_boxplot(aes(gr_liv_area),fill="darkgreen",color="black")

ggplot(c_house) +
  geom_density(aes(gr_liv_area),color="black",fill="steelblue") +
  theme_classic()


#Bivariate
#cat-cat
#garage_type-garage_finish
#counts (stacked bars)
ggplot(c_house) +
  geom_bar(aes(garage_type,fill=garage_finish)) +
  scale_fill_manual(values = c("red", "orange", "darkgreen", "dodgerblue"))
#proportions of garage finish levels (for each garage type; stacked bars)
ggplot(c_house) +
  geom_bar(aes(garage_type,fill=garage_finish),position="fill") +
  scale_fill_manual(values = c("red", "orange", "darkgreen", "dodgerblue"))
#counts (grouped bars)
ggplot(c_house) +
  geom_bar(aes(garage_type,fill=garage_finish),position="dodge") +
  scale_fill_manual(values = c("red", "orange", "darkgreen", "dodgerblue")) +
  theme(legend.position="bottom")


#cat-num
#lot_area-lot_config: density plots of lot_area faceted by lot_config
ggplot(c_house) +
  geom_freqpoly(aes(lot_area,..density..,color=lot_config)) +
  facet_wrap(~lot_config)
#seems like large lots associated with corner lots and cul-de-sacs

#boxplot of garage_area by garage_type
ggplot(c_house) +
  geom_boxplot(aes(x=garage_type,y=garage_area),fill="dark red")
#2 garage types clearly the largest, followed by built-in and attached, then remaining three

#barplot of lot_area by bldg_type
ggplot(c_house,aes(bldg_type,lot_area)) +
  stat_summary(geom="bar",fill="steelblue") +
  stat_summary(geom="errorbar",width=0.3) +
  scale_x_discrete(breaks=c("1Fam","2fmCon","Duplex","TwnhsE","Twnhs"),
                   labels=c("Single-family \nDetached","Two-family \nConversion",
                            "Duplex","Townhouse \nEnd Unit","Townhouse \nInside Unit")) +
  labs(x="Building type",y="Number of homes") +
  theme(axis.title.x=element_text(size=13,face="bold"),
        axis.title.y=element_text(size=12,face="bold")) +
  theme_classic()


#num-num
#lot_area-gr_liv_rea
ggplot(c_house, aes(lot_area,gr_liv_area)) +
  geom_point() +
  geom_smooth(method=lm)

#notice few data points > 50000, so let's look at relationship for lots under that size
c_house %>%
  filter(lot_area<50000) %>%
  ggplot(aes(lot_area,gr_liv_area)) +
  geom_point() +
  geom_smooth(method=lm)

#x1st_flor_sf-x2nc_flr_sf
c_house %>%
  ggplot(aes(x1st_flr_sf,x2nd_flr_sf)) +
  geom_point() +
  geom_smooth(method=lm)
#problem: 0 sq ft for second floor creates negative relationship
#once those data (and some large 1st floors) are removed, a clear positive relationship emerges
c_house %>%
  filter(x2nd_flr_sf>0,x1st_flr_sf<2500) %>%
  ggplot(aes(x1st_flr_sf,x2nd_flr_sf)) +
  geom_point() +
  geom_smooth(method=lm)


#Multivariate
#x1st_flr_sf-x2nd_flr_sf-bldg_type
c_house %>%
  filter(x2nd_flr_sf>0,x1st_flr_sf<2500) %>%
  ggplot(aes(x1st_flr_sf,x2nd_flr_sf,color=bldg_type)) +
  geom_point(alpha=0.5) +
  geom_smooth(method=lm) +
  theme(legend.position = "bottom")
#same filtering as above--duplex doesn't follow a positive relationship (but could be
#lack of data)

#bldg_type-heating_qc-lot_area
tabyl(c_house$central_air)
ggplot(c_house,aes(bldg_type,lot_area,fill=c_house$heating_qc)) +
  stat_summary(geom="bar",position="dodge") +
  stat_summary(geom="errorbar",position="dodge") +
  theme_classic()
#shows that 1) lot size varies with building type, and 2) some association b/t heating_qc
#and lot area for each building type (excellent - largest and poor-fair - smallest)


#Predictors and dependent variable (sale_price)
#dependent variable only
#histogram
ggplot(c_house) +
  geom_histogram(aes(sale_price),bins=50,fill="darkred",color="black") +
  labs(x="Sale price",y="Count")

#density plot
ggplot(c_house) +
  geom_density(aes(sale_price),fill="darkred",color="black") +
  labs(x="Sale price",y="Density")

#boxplot
ggplot(c_house) +
  geom_boxplot(aes(sale_price),fill="dodgerblue",color="black") +
  coord_flip() +
  labs(y="Sale price") +
  theme_classic()

#bivariate
#cat-num
#lot_shape-sale_price
tapply(c_house$sale_price,c_house$lot_shape,mean)
tapply(c_house$sale_price,c_house$lot_shape,length)

ggplot(c_house,aes(lot_shape,sale_price)) +
  stat_summary(geom="bar",fill="darkgreen") +
  stat_summary(geom="errorbar",width=0.3) +
  labs(x="Lot shape",y="Sale price ($ x 10^8)") +
  theme(axis.title=element_text(face="bold"),
        axis.text=element_text(size=10)) +
  scale_x_discrete(breaks=c("Reg","IR1","IR2","IR3"),
                   labels=c("Regular","Slightly \nirregular","Moderately \nirregular","Irregular")) 
#shows that the order of sale price for lot shape is mod irregular, irregular, slightly irregular, and regular


#num-num
#lot_area-sale_price
ggplot(c_house,aes(lot_area,sale_price)) +
  geom_point()
#a few 'outliers' above 100,000 ft^2, so let's try without them
s1<-c_house %>% 
  filter(lot_area<100000) %>%
  ggplot(aes(lot_area,sale_price)) +
    geom_point() +
    geom_smooth(method=lm) +
    labs(x="Lot area",y="Sale price ($)") +
    theme(axis.title=element_text(size=12,face="bold")) +
    theme_light()
s1

#gr_liv_area-sale_price
s2<-ggplot(c_house,aes(gr_liv_area,sale_price)) +
  geom_point() +
  geom_smooth(method=lm) +
  labs(x="Above grade living area",y="Sale price ($)") +
  theme(axis.title=element_text(size=12,face="bold")) +
  theme_bw()
s2

cor(c_house$gr_liv_area,c_house$sale_price)^2 #0.502
gr_liv_area_mod<-lm(sale_price~gr_liv_area,data=c_house)
summary(gr_liv_area_mod) #r^2 = 0.5021

#x1st_flr_sf-sale_price
s3<-ggplot(c_house,aes(x1st_flr_sf,sale_price)) +
  geom_point() +
  geom_smooth(method=lm) +
  labs(x="First floor square feet",y="Sale price ($)") +
  theme(axis.title=element_text(size=12,face="bold")) +
  theme_bw()
s3

#x2nd_flr_sf-sale_price
s4<-c_house %>%
  filter(x2nd_flr_sf>0) %>%
  ggplot(aes(x2nd_flr_sf,sale_price)) +
    geom_point() +
    geom_smooth(method=lm) +
    labs(x="Second floor square feet",y="Sale price ($)") +
    theme(axis.title=element_text(size=12,face="bold")) +
    theme_bw()
s4

c_house %>%
  filter(x2nd_flr_sf>0) %>%
  select(x2nd_flr_sf,sale_price) %>%
  cor()

#multi-panel plot
plot_grid(s1,s2,s3,s4,labels=c("A","B","C","D"),label_size=12)


#multivariate with dependent variable
c_house %>% select(where(is.factor),-sale_price) %>% names() %>% sort()
c_house %>% select(where(is.integer),-sale_price) %>% names() %>% sort()
c_house %>% select(where(is.double),-sale_price) %>% names() %>% sort()

ggplot(c_house,aes(x=gr_liv_area,y=sale_price,color=bldg_type)) +
  geom_point(alpha=0.5) +
  geom_smooth(method=lm) +
  labs(x="Above grade living area",y="Sale price ($)",color="Building type") +
  theme(axis.title = element_text(size=12,face="bold"),
        legend.title=element_text(face="bold"))
#shows steepness in slopes of relationship b/t living area and sale price based on building type

c_house %>%
  filter(lot_area<75000) %>%
  ggplot(aes(x=lot_area,y=sale_price,color=lot_shape)) +
    geom_point(alpha=0.5) +
    geom_smooth(method=lm) +
    scale_color_discrete(labels=c("Regular", "Irregular 1", "Irregular 2", "Irregular 3")) +
    labs(x="Lot area",y="Sale price",color="Lot shape") +
    theme(axis.title = element_text(size=12,face="bold"),
          legend.title=element_text(size=12,face="bold"),
          legend.position = "bottom") 

#alternative code for legend using guides()
c_house %>%
  filter(lot_area<75000) %>%
  ggplot(aes(x=lot_area,y=sale_price,color=lot_shape)) +
  geom_point(alpha=0.5) +
  geom_smooth(method=lm) +
  scale_color_discrete(labels=c("Regular", "Irregular 1", "Irregular 2", "Irregular 3")) +
  labs(x="Lot area",y="Sale price") +
  theme(axis.title = element_text(size=12,face="bold"),
        legend.position = "bottom",
        legend.text=element_text(size=11)) +
  guides(color=guide_legend(title="Lot shape",
                            ncol=2,
                            title.theme=element_text(face="bold")))


#enclosed_porch-sale_price
c_house %>%
  filter(garage_area>0) %>%
  ggplot(aes(x=garage_area,y=sale_price,color=garage_type)) +
  geom_point(alpha=0.5)+
  geom_smooth(method=lm) +
  scale_color_discrete(labels=c("Attached","Detached","Built-in","Car port","Basement","2 Types"))+
  scale_x_continuous(name="Garage area (sq ft)",
                     breaks=seq(0,1500,500)) +
  scale_y_continuous(name="Sale price",
                     seq(0,800000,by=100000),
                     labels=scales::label_dollar())+
  labs(color="Garage type") +
  theme(axis.title=element_text(face="bold"),
        legend.title=element_text(size=12,face="bold"),
        legend.text=element_text(size=11),
        legend.background=element_rect(color="black")) +
  annotate("text",label="Built-in and attached garages have the strongest correlations
           between garage area and sale price",
           x=20, y=715000,
           hjust="left",
           size=4)

#x1st_flr_sf + house_style-sale_price
#create labels for faceting
house_style_labs<-as_labeller(c(
                    `2Story` = "Two story",
                    `1Story` = "One story",
                    `1.5Fin` = "1.5 story (top finished)",
                    `1.5Unf` = "1.5 story (top unfinished)",
                    `SFoyer` = "Split foyer",
                    `SLvl` = "Split level",
                    `2.5Unf` = "2.5 story (2nd unfinished)",
                    `2.5Fin` = "2.5 story (2nd finished)"))

c_house %>%
  filter(x1st_flr_sf<3000) %>%
  ggplot(aes(x=x1st_flr_sf,y=sale_price)) +
    geom_point() +
    geom_smooth(method=lm) +
    facet_wrap(~house_style,labeller=house_style_labs) +
    labs(x="1st floor square feet") +
    scale_y_continuous(name="Sale price",
                       seq(0,800000,by=200000),
                       labels=scales::label_dollar()) +
    theme(axis.title=element_text(size=11,face="bold"))

ggplot(c_house, aes(x=gr_liv_area,y=sale_price,color=foundation)) +
  geom_point(alpha=0.5) +
  geom_smooth(method=lm) +
  labs(x="Above ground living area (sq ft)") +
  scale_y_continuous(name="Sale price",
                     seq(0,1000000,by=200000),
                     labels=scales::label_dollar())+
  scale_color_discrete(name="Foundation",
                       labels=c("Poured concrete","Cinder block","Brick & tile", "Other", "Slab")) +
  theme(axis.title=element_text(size=12,face="bold"),
        axis.text=element_text(size=11),
        legend.title=element_text(face="bold"),
        legend.position="top")











