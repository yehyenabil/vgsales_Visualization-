library(ggplot2)
library(plotly)
library(dplyr)


##delete Null valus
na.omit(vgsales)


# vg1<-vgsales %>% group_by(Year,Genre) %>%
#   summarise(Count = n())
# colnames(vg1)[3]<-"Releases"
# vgnumgen<-ggplot(data=vg1,aes(x=Year,y=Releases, fill=reorder(Genre,-Releases), order=-as.numeric(Releases))) + geom_bar(stat="identity")+ scale_x_discrete(breaks=seq(1980,2016,by=4)) + scale_y_continuous(breaks=seq(0,1500,by=100)) + scale_fill_brewer(palette="Set3") + labs(title="Number of Video Game Releases By Year", fill="Genre") + theme(axis.title=element_text(size=14,face="plain"),title = element_text(size=14,face="bold"))
# ggplotly(vgnumgen)

# 
#scetter plot
publisher_count <-vgsales %>%
  group_by(Publisher) %>%
  summarise(GlobalSales = sum(Global_Sales),count_game = length(unique(Name))) %>%
  arrange(desc(count_game)) %>%
  select(Publisher)%>% ##sum of every publisher and select top 5
  head(5)
publisher_count20 <-as.vector(publisher_count$Publisher) ##convert dataframe to a vector

publisher_bubble<- vgsales %>%
  filter(Publisher %in% publisher_count20)%>%
  group_by(Year,Publisher) %>%
  summarise(GlobalSales = sum(Global_Sales),count_game = length(unique(Name)),.groups = 'drop') %>%
  arrange(desc(Year))#arrange year numper descanding


p5=ggplot(publisher_bubble,aes(x=Year, y=GlobalSales, size=count_game, fill=Publisher)) +
  geom_point(alpha=0.5, shape=21, color="black") +
  scale_size(range = c(.1, 24), name="Number of Games") + ##area of ceircal
  ggtitle("Top-5 Publisher Distribution by Yearly Number of Game and Sales") + ##title of plot
  ylab("in millions") + ##y_axis title
  xlab("Year")+ ##x_axis title
  theme(legend.position="right",axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))## set legend position

ggplotly(p5)
# 
# 
#
sumofsales <- vgsales %>%
  group_by(Year) %>%
  summarise(sum_global_sales = sum(Global_Sales),sum_others_sales = sum(Other_Sales),
            sum_jb_sales = sum(JP_Sales),sum_eu_sales = sum(EU_Sales),
            sum_na_sales = sum(NA_Sales),.groups = 'drop')

colors <- c("Global Sales"="red", "North America Sales"="blue", "Europe Sales"="green", "Japan Sales"="orange",
            "The Rest of the World"="yellow")
options(repr.plot.width = 16, repr.plot.height = 8)
p8=ggplot(data=sumofsales, aes(x= Year)) +
  geom_line(aes(y= sum_global_sales,group=1,color="Global Sales"),linetype = "dashed")+
  geom_line(aes(y= sum_na_sales,group=1,color="North America Sales"),linetype = "dashed")+
  geom_line(aes(y= sum_eu_sales,group=1,color="Europe Sales"),linetype = "dashed")+
  geom_line(aes(y= sum_jb_sales,group=1,color="Japan Sales"),linetype = "dashed")+
  geom_line(aes(y= sum_others_sales,group=1,color="The Rest of the World"),linetype = "dashed")+
  geom_point(aes(y= sum_global_sales)) +
  geom_point(aes(y= sum_na_sales)) +
  geom_point(aes(y= sum_eu_sales)) +
  geom_point(aes(y= sum_jb_sales)) +
  geom_point(aes(y= sum_others_sales)) +
  scale_color_manual(name="Sales",values = colors)+
  ggtitle("Sum of Global Sales by Year") +
  xlab("Years") +
  ylab("in millions") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),legend.position="top")

 ggplotly(p8)


temp = vgsales %>% select(Genre,Global_Sales)
p6=ggplot(temp,aes(Genre,Global_Sales,fill=Genre))+
  geom_boxplot(stat="boxplot",outlier.color="red")+ ##box plot for sales and Genre factors
  theme(axis.text.x = element_text(angle=90,vjust=0.3),
        plot.title = element_text(hjust=0.5,face='italic'), ##set text title
        plot.subtitle = element_text(hjust=0.5,face='italic'), ##
        legend.position="bottom")+
  ggtitle("Trend of Overall Sales")+ ##set plot title
  labs(x="Genre",y="Sales(in Millions)"
       ,subtitle="Global Revenue")+
  scale_y_log10()
ggplotly(p6)


##platform count

platform = vgsales %>% group_by(Platform) %>% summarise(Count = n()) ##count every platform numpers
p1 = ggplot(aes(x = Platform , y = Count , fill=Count) , data=platform) +
  geom_bar(colour='black',stat='identity') + #bar plot of platform count
  theme_bw()+
  theme(axis.text.x = element_text(angle=45,hjust=1) ,
        plot.title = element_text(hjust=0.5))+  ## title center
  ggtitle('Platform Count')+ ##title
  scale_fill_distiller(palette = 'RdYlBu') + ##color for every bar
  ylab('Count') ## y axis title

ggplotly(p1)




p2=ggplot(data = popular_genre_per_year ,
          aes(x = Year, y = GlobalSales,fill=Genre)) +
  geom_bar(colour='black',stat='identity') +
  ggtitle('Most popular genre per year') + ## title for plot
  theme_bw() +
  theme(axis.text.x = element_text(angle=45,hjust=1),
        plot.title = element_text(hjust=.5)) +
  scale_fill_brewer(palette = 'RdYlBu') ##color for every bar

ggplotly(p2)



 
top_publisher = vgsales %>% group_by(Publisher) %>%
  summarise(Count = n()) %>%  ##count every Genre numpers
  arrange(desc(Count)) %>% top_n(10) ##arrange publisher numper descanding and return top 10

p3= ggplot(data=top_publisher,aes(x=Publisher,y=Count,fill=Count)) +
  geom_bar(colour='black',stat='identity') +  ##identity statistic leaves the data unchanged
  theme_bw() +
  ggtitle('Top 10 Publisher') + ##title for plot
  theme(axis.text.x = element_text(angle=45,hjust=1),
        plot.title = element_text(hjust=0.5)) +
  scale_fill_distiller(palette = 'RdYlBu') + ##color for every bar
  ylab('Count') ## y axis title

ggplotly(p3)





#Quick Heatmap
platform_per_year = df %>% group_by(Platform,Year) %>%
  summarise(Count = n()) %>% ##count every Genre numpers
  arrange(Year)##arrange year numper
p4 =ggplot(data = platform_per_year , aes(x = Platform , y = Year)) +
  geom_tile(aes(fill=Count),colour='white') + ##Quick Heatmap
  theme(axis.text.x = element_text(angle=90,hjust=1),
        plot.title = element_text(hjust=.5)) +
  ggtitle('platform Distribution per year') + ##title for plot
  scale_fill_gradient(low='white',high='red',breaks=c(50,150,250,350,450),
                      labels=c('Minimum',150,250,350,'Maximum')) + ##set color gradientn low and high
  scale_x_discrete(expand=c(0,1))+ ## distance away from axesllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllll;uiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii
  scale_y_discrete(expand=c(0,1)) ## distance away from top axes

ggplotly(p4)
# 
# 








genre_sales <-vgsales %>%
group_by(Genre) %>%
summarise(sum_global_sales = sum(Global_Sales)) %>%
arrange(desc(sum_global_sales))%>%
mutate(percent = sum_global_sales/sum(sum_global_sales)*100) ##create new var to percentge

options(repr.plot.width = 18, repr.plot.height = 10)
ggplot(data= genre_sales, aes(x= "",y=percent,fill = Genre))+
geom_bar(stat="identity", width=1, color="white")+
coord_polar("y", start=0)+
ggtitle("Genre by % Global Sales") + ##set plot title
theme(legend.position="right")+ ## set legend position
geom_text(aes(label = paste0(round(percent),"%")),  ##set text over ciercal
position = position_stack(vjust = 0.5),color = "black",size=5)##stacks bars on top of each other

# 
