library(dplyr)
library(ggplot2)
library(timetk)
library(data.table)
library(shiny)
library(plotly)


vgsale<- read.csv("Video_Games_Sales_as_at_22_Dec_2016.csv")
console<-read.csv("console.csv")

print(dim(vgsale))
head(vgsale)
str(vgsale)

vgsale<-vgsale%>%select("Name","Platform",
                        "Year_of_Release",
                        "Genre","NA_Sales",
                        "EU_Sales","JP_Sales",
                        "Other_Sales","Global_Sales",
                        "Rating")
vgsale<-vgsale%>% filter(Genre != "")

vgsale<-vgsale%>%unique()
vgsale<-vgsale%>%drop_na()
vgsale<-vgsale%>% filter(Year_of_Release>"0", Year_of_Release <"2017")


print(unique(vgsale$Platform))
print(unique(vgsale$Rating))
print(unique(console$ConsoleID))

#cleaning
#vgsale$Year_of_Release<-strtoi(vgsale$Year_of_Release)
str(vgsale)


print(dim(vgsale))
print(head(vgsale))


vgsale$Year_of_Release<-as.Date(vgsale$Year_of_Release,format = "%Y")
vgsale$Year_of_Release

print(dim(vgsale))
str(vgsale)

question1<-vgsale%>% group_by(Genre)%>% 
  summarize_by_time(.date_var =Year_of_Release, 
                    .by = "5 year",total_NA = sum(NA_Sales),
                    total_EU = sum(EU_Sales),
                    total_JP = sum(JP_Sales),
                    total_other = sum(Other_Sales),
                    total_global = sum(Global_Sales))

question1$Year_of_Release<-format(question1$Year_of_Release,"%Y")

head(question1)
print(dim(question1))
question1

# remove the games that does not have rating
question2<-vgsale%>% filter(Rating != "")
print(dim(question2))
head(question2)

question2_1<-question2%>%
  group_by(Rating,Year_of_Release)%>%
  summarise(total_global = sum(Global_Sales))

question2_1$Year_of_Release<-format(question2_1$Year_of_Release, "%Y")
head(question2_1)


p6 <- ggplot(data = question2_1,
             mapping = aes(
               x = Year_of_Release, 
               y = total_global,group = Rating, color = Rating))+geom_point()+geom_line() + 
  ggtitle("sales VS. rating") +
  xlab("Rating") + ylab("total sale(million)")
p6


#question3
console_sale<-select(console,"ConsoleID", "Sales")
colnames(console_sale) <- c("Platform","consoleSale")

question3<-merge(vgsale,console_sale, by="Platform", all = TRUE)
head(question3)
print(dim(question3))

question3<-question3%>%select("Platform", "Name","Year_of_Release","Global_Sales","consoleSale")
question3$Year_of_Release<-format(question3$Year_of_Release, "%Y")
question3<-question3[complete.cases(question3), ]

head(question3)
print(dim(question3))

dup<-question3$Name[duplicated(question3$Name)]

multi_plat<-question3[(question3$Name %in% dup),]
print(dim(multi_plat))


"%!in%"<- Negate("%in%")
single_plat<-question3[(question3$Name %!in% multi_plat$Name),]
print(dim(single_plat))   

single_plat<-single_plat%>%group_by(Platform,consoleSale)%>%summarise(Global_Sales=sum(Global_Sales))
single_plat<-single_plat%>%group_by(Platform,consoleSale)%>%summarise(salePerConsole=Global_Sales/consoleSale)
single_plat

multi_plat<-multi_plat%>%group_by(Platform,consoleSale)%>%summarise(Global_Sales=sum(Global_Sales))
multi_plat<-multi_plat%>%group_by(Platform,consoleSale)%>%summarise(salePerConsole=Global_Sales/consoleSale)
multi_plat


ui<-fixedPage(
  
  fixedRow(
    titlePanel("THE FUTURE GAMING DEVICE PURCHASE ANALYSIS BASE ON VIDEO GAMES SALE")
  ),
  fixedRow(
    h5("As a Millennial who started to play video games from an early age, I always wanted 
       to explore the stories behind the video game industry. With the recent shortage of CPU 
       chips and the graphic cards, it is still hard to get the latest gaming consoles, or a 
       high-performance PC. My PlayStation 4 was broken recently, and I want to 
       buy a new generation gaming console for my daily gaming. And I think it is important for 
       other gamers to make a wise choice of which device they want to purchase for the next a 
       few years. Therefore I want to find out the key factors that affect the sales in gaming 
       industry to have a clear view for the gaming device purchase decision.")
  ),
  fixedRow(
    column(3, 
           wellPanel(
             selectInput(inputId = "saleOverYear",
                         label = "Year Range",
                         choices = c('1980','1985','1990','1995','2000','2005','2010','2015'),
                         selected = '1980'),
             radioButtons("Region", label="Select region",
                          choices=c('Global_Sales'= 'Global_Sales' , 'JP_Sales'= 'JP_Sales', 'NA_Sales'= 'NA_Sales', 'EU_Sales'= 'EU_Sales', 
                                    'Other_Sales'= 'Other_Sales'), 
                          selected='Global_Sales')
             )
           
    ),
    column(9, 
           h4("Game sale in 5 year interval of different regions"),
           h5("In this section, the plot represents the types of the games sold 
              in the market in a 5-year interval globally and regionally. The year 
              represents the start year of the 5 year interval."),
           plotlyOutput("p1")
    )
    ),
  fixedRow(
    column(10, 
           h4("Rating on game sales"),
           h5("In this section, the plot represents the total sales of each game 
              rating through the years."),
           plotlyOutput("p6")
    )
  ),
  
  fixedRow(
    column(3, 
           wellPanel(
             radioButtons("sale", label="Select sale type",
                          choices=c('multi-platform'= 'multi-platform' , 'single-platform'= 'single-platform'), 
                          selected='single-platform'))
           
    ),
    column(9, 
           h4("console types on the sales of games"),
           h5("In this section, the plots represent the total sales of multi-platform 
              sale per console, and the total sales of single-platform sale per console in the market."),
           plotlyOutput("p7")
    )
  )
  
)
server <- function(input, output) {
  
  output$p1 <- renderPlotly({
    
    if (input$Region == "Global_Sales"){
      df <- subset(question1, Year_of_Release == strtoi(input$saleOverYear))
      ggplot(data = df,
             mapping = aes(
               x = Genre,
               y = total_global,fill = Genre))+geom_bar(stat="identity") + geom_text(aes(label=round(total_global,1)))+
        ggtitle("Type of game sale in 5 year interval for global") +
        xlab("Genre") + ylab("total sale(million)")
    }
    else if (input$Region == "NA_Sales"){
      df <- subset(question1, Year_of_Release == strtoi(input$saleOverYear))
      ggplot(data = df,
             mapping = aes(
               x = Genre,
               y = total_NA,fill = Genre))+geom_bar(stat="identity") + geom_text(aes(label=round(total_NA,1)))+
        ggtitle("Type of game sale in 5 year interval for NA") +
        xlab("Genre") + ylab("total sale(million)")
    }
    else if (input$Region == "EU_Sales"){
      df <- subset(question1, Year_of_Release == strtoi(input$saleOverYear))
      ggplot(data = df,
             mapping = aes(
               x = Genre,
               y = total_EU,fill = Genre))+geom_bar(stat="identity") + geom_text(aes(label=round(total_EU,1)))+
        ggtitle("Type of game sale in 5 year interval for EU") +
        xlab("Genre") + ylab("total sale(million)")
    }
    else if (input$Region == "JP_Sales"){
      df <- subset(question1, Year_of_Release == strtoi(input$saleOverYear))
      ggplot(data = df,
             mapping = aes(
               x = Genre,
               y = total_JP,fill = Genre))+geom_bar(stat="identity") + geom_text(aes(label=round(total_JP,1)))+
        ggtitle("Type of game sale in 5 year interval for JP") +
        xlab("Genre") + ylab("total sale(million)")
    }
    else if (input$Region == "Other_Sales"){
      df <- subset(question1, Year_of_Release == strtoi(input$saleOverYear))
      ggplot(data = df,
             mapping = aes(
               x = Genre,
               y = total_other,fill = Genre))+geom_bar(stat="identity") + geom_text(aes(label=round(total_other,1)))+
        ggtitle("Type of game sale in 5 year interval for other region") +
        xlab("Genre") + ylab("total sale(million)")
    }
    
    
  })
  
  output$p6 <- renderPlotly(p6)
  output$p7 <- renderPlotly({
    
    if (input$sale == "single-platform"){
      ggplot(data = single_plat,
             mapping = aes(
               x = Platform, 
               y = salePerConsole, fill = Platform))+geom_bar(stat="identity") + geom_text(aes(label=round(salePerConsole,2)))+
        ggtitle("sales VS. rating") +
        xlab("console") + ylab("sale per console(million)")
    }
    else if (input$sale == "multi-platform"){
      ggplot(data = multi_plat,
             mapping = aes(
               x = Platform, 
               y = salePerConsole, fill = Platform))+geom_bar(stat="identity") + geom_text(aes(label=round(salePerConsole,2)))+
        ggtitle("sales VS. rating") +
        xlab("console") + ylab("sale per console(million)")
    }
  })
  
}
shinyApp(ui, server)


