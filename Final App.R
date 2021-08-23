library(shiny)
library(shinydashboard)
library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(lubridate)
library(reshape2)
library(wordcloud)
library(wordcloud2)
library(tidytext)
library(DT)
library(ggplot2)
library(plotly)
library(corrplot)
library(vcd)
library(rcompanion)
library(gvlma)
library(ggplot2)
library(lattice)
library(lava)
library(purrr)
library(caret)
library(nnet)
library(tidyverse)
library(broom)
library(modelr)
library(stringr)
library(Hmisc)
library(tidytext)
library(modelr)
library(ggpubr)
#DATA1#

#import topcompanies
#create multiple csv files from folder
#create a new column with years based on file names
topcompanies <- list.files(
  path = "Best Places to Work",
  pattern="*.csv", 
  full.names = T
) %>% 
  map_df(
    function(x) read_csv(x, col_types = cols(.default = "?")) %>% 
      mutate(year=substr(basename(x),21,24))
  )


#clean data
#drop unnecessary columns and retain those that are needed; rename them
topcompanies <- topcompanies %>%
  dplyr::select(company=Title,ranking=h2,rating=Number,year)
#remove anything but alphanumeric
topcompanies$ranking <- str_replace_all(topcompanies$ranking, "[^[:alnum:]]","")
topcompanies$ranking <- as.numeric(topcompanies$ranking)
topcompanies$year <- as.numeric(topcompanies$year)

#DATA2#
#import employeereview
#drop unnecessary colmns 

employeerating <- read.csv("employee_reviews.csv") %>% 
  dplyr::select(company,dates,overall.ratings,work.balance.stars,culture.values.stars,carrer.opportunities.stars,comp.benefit.stars,senior.mangemnet.stars) %>% 
  filter(overall.ratings!="none") %>% 
  filter(work.balance.stars!="none") %>% 
  filter(culture.values.stars!="none") %>% 
  filter(carrer.opportunities.stars!="none") %>% 
  filter(comp.benefit.stars!="none") %>% 
  filter(senior.mangemnet.stars!="none")

employeerating$work.balance.stars <- as.numeric(employeerating$work.balance.stars)
employeerating$culture.values.stars <- as.numeric(employeerating$culture.values.stars)
employeerating$carrer.opportunities.stars <- as.numeric(employeerating$carrer.opportunities.stars)
employeerating$comp.benefit.stars <- as.numeric(employeerating$comp.benefit.stars)
employeerating$senior.mangemnet.stars <- as.numeric(employeerating$senior.mangemnet.stars)

employeerating$dates <- mdy(employeerating$dates)

newemployeerating <- employeerating %>% 
  mutate(newdate=format(dates,"%d/%m/%Y")) %>% 
  drop_na() %>% 
  filter(dates>="2015-01-01" )

glimpse(newemployeerating)


# ADD TO FANMAG tab ********************************************************
#yearlyplot 
employeeratingyearly <- employeerating %>% 
  mutate(newdate=format(dates,"%Y")) %>% 
  drop_na() %>% 
  dplyr::group_by(company,newdate) %>% 
  dplyr::summarise(mean(overall.ratings)) %>% 
  filter(newdate>="2015")

employeeratingyearlyplot <- employeeratingyearly %>% 
  ggplot(mapping=aes(x=newdate,y=`mean(overall.ratings)`,group=company))+
  geom_line(aes(colour=company))+
  xlab("Year")+
  ylab("Overall Rating")+
  theme_bw()
employeeratingyearlyplot

#monthlyplot 
employeeratingmonthly <- employeerating %>% 
  mutate(newdate=format(dates,"%m")) %>% 
  drop_na() %>% 
  dplyr::group_by(company,newdate) %>% 
  dplyr::summarise(mean(overall.ratings)) 

employeeratingmonthlyplot <- employeeratingmonthly %>% 
  ggplot(mapping=aes(x=newdate,y=`mean(overall.ratings)`,group=company))+
  geom_line(aes(colour=company))+
  xlab("Month")+
  ylab("Overall Rating")+
  theme_bw()
employeeratingmonthlyplot

# end of to add portion **************************************************************

newemployeerating$overall.ratings <- factor(newemployeerating$overall.ratings, levels=unique(newemployeerating$overall.ratings))
newemployeerating$work.balance.stars <- factor(newemployeerating$work.balance.stars, levels=unique(newemployeerating$work.balance.stars))
newemployeerating$culture.values.stars <- factor(newemployeerating$culture.values.stars, levels=unique(newemployeerating$culture.values.stars))
newemployeerating$carrer.opportunities.stars <- factor(newemployeerating$carrer.opportunities.stars, levels=unique(newemployeerating$carrer.opportunities.stars))
newemployeerating$comp.benefit.stars <- factor(newemployeerating$comp.benefit.stars, levels=unique(newemployeerating$comp.benefit.stars))
newemployeerating$senior.mangemnet.stars <- factor(newemployeerating$senior.mangemnet.stars, levels=unique(newemployeerating$senior.mangemnet.stars))

factor(newemployeerating$overall,levels=c("5","4","3","2","1"))
factor(newemployeerating$work.balance.stars,levels=c("5","4","3","2","1"))
factor(newemployeerating$culture.values.stars,levels=c("5","4","3","2","1"))
factor(newemployeerating$carrer.opportunities.stars,levels=c("5","4","3","2","1"))
factor(newemployeerating$comp.benefit.stars,levels=c("5","4","3","2","1"))
factor(newemployeerating$senior.mangemnet.stars,levels=c("5","4","3","2","1"))



#EDA#
#overall
overall <- ggplot(data=newemployeerating) +
  geom_bar(aes(x=overall.ratings))+
  theme_bw()

#workbalance
workbalance <- ggplot(data=newemployeerating) +
  geom_bar(aes(x=work.balance.stars))+
  theme_bw()

#culture
culture <- ggplot(data=newemployeerating) +
  geom_bar(aes(x=culture.values.stars))+
  theme_bw()

#careeropp
opportunities <- ggplot(data=newemployeerating) +
  geom_bar(aes(x=carrer.opportunities.stars))+
  theme_bw()

#compbenefit
benefits <- ggplot(data=newemployeerating) +
  geom_bar(aes(x=comp.benefit.stars))+
  theme_bw()

#management
management <- ggplot(data=newemployeerating) +
  geom_bar(aes(x=senior.mangemnet.stars))+
  theme_bw()

# ADD IN SUMMARY ****************************************************************
summary(newemployeerating)

# end of to add portion **********************************************************

##CHISQUARETEST#

newemployeerating <- newemployeerating %>% 
  dplyr::select(overall.ratings,work.balance.stars,
                culture.values.stars,
                carrer.opportunities.stars,
                comp.benefit.stars,
                senior.mangemnet.stars)
newemployeerating <- as.data.frame(newemployeerating)


# factors affect overall rating  **************************************************************************

newemployeerating$overall.ratings <- as.numeric(newemployeerating$overall.ratings)
newemployeerating$work.balance.stars <- as.numeric(newemployeerating$work.balance.stars)
newemployeerating$culture.values.stars <- as.numeric(newemployeerating$culture.values.stars)
newemployeerating$carrer.opportunities.stars <- as.numeric(newemployeerating$carrer.opportunities.stars)
newemployeerating$comp.benefit.stars <- as.numeric(newemployeerating$comp.benefit.stars)
newemployeerating$senior.mangemnet.stars <- as.numeric(newemployeerating$senior.mangemnet.stars)

# OLS
M <-cor(newemployeerating)
corrplot(M, method="color", 
         order="hclust",
         addCoef.col = "black",
         tl.col="black")

newemployeerating %$% 
  cor(tibble(overall.ratings,
             work.balance.stars,
             culture.values.stars,
             carrer.opportunities.stars,
             comp.benefit.stars,senior.mangemnet.stars)) %>%
  round(.,2)

newemployeerating %>% 
  select(overall.ratings,
         work.balance.stars,
         culture.values.stars,
         carrer.opportunities.stars,
         comp.benefit.stars,
         senior.mangemnet.stars) %>% 
  as.matrix(.) %>% 
  Hmisc::rcorr(.) 
  #tidy(.) %>% as_tibble()



model1 <- lm(overall.ratings ~  
               work.balance.stars + 
               culture.values.stars + 
               carrer.opportunities.stars + 
               comp.benefit.stars + 
               senior.mangemnet.stars, 
             newemployeerating)

tidy(model1) %>% as_tibble()
glance(model1)

model2 <- lm(overall.ratings ~ culture.values.stars*
               (work.balance.stars + 
                  carrer.opportunities.stars + 
                  comp.benefit.stars + 
                  senior.mangemnet.stars), 
             newemployeerating)

tidy(model2) %>% as_tibble()
glance(model2)

anova(model2)

anova(model1, model2)

gvlma(model2)
plot(model2,1)
plot(model2,2)
plot(model2,3)
plot(model2,4)
plot(model2,5)


grid <- newemployeerating %>% 
  data_grid(overall.ratings,
            culture.values.stars,
            work.balance.stars, 
            carrer.opportunities.stars, 
            comp.benefit.stars, 
            senior.mangemnet.stars) %>%
  add_predictions(model2)

keycol <- "factors"
valuecol <- "values"
gathercols <- c("work.balance.stars", "culture.values.stars", 
                "carrer.opportunities.stars", 
                "comp.benefit.stars", 
                "senior.mangemnet.stars")

grid2 <- gather_(grid, keycol, valuecol, gathercols) %>% 
  group_by(factors,values) %>% 
  mutate(m=mean(pred)) %>% 
  group_by(factors,values,m) %>% 
  summarise()

ggplot(grid2, aes(x = values, y = m,colour=factor(factors))) +
  geom_line(size = 2)+
  xlab("Actual Ratings")+
  ylab("Predicted Ratings")+
  labs(colour="Factors")+
  theme_bw()


# **************************************************************************************



#WORDCLOUDS#

itcompanyreview <-list.files(path = "Information Technology",
                             pattern="*.csv", 
                             full.names = T) %>% 
  map_df(function(x) read_csv(x, col_types = cols(.default = "c")) %>% 
           mutate(company=substr(basename(x),1,nchar(basename(x))-4)))

itcompanyreview <- itcompanyreview %>% 
  dplyr::select(company,rating=View1,pros=View5,cons=View7)

itcompanyreview$reviews <- paste(itcompanyreview$pros,itcompanyreview$cons)
itcompanyreview$reviews <- gsub("[^[:alnum:]]", " ",itcompanyreview$reviews)
itcompanyreview$reviews <- tolower(itcompanyreview$reviews)

businessservicereviews <- list.files(
  path = "Business Service",
  pattern="*.csv", 
  full.names = T
) %>%
  map_df(
    function(x) read_csv(x, col_types = cols(.default = "c")) %>%
      mutate(company=substr(basename(x),1,nchar(basename(x))-4))
  )

businessservicereviews <- businessservicereviews %>% 
  dplyr::select(company,rating=View1,pros=View5,cons=View7)

businessservicereviews$reviews <- paste(businessservicereviews$pros,businessservicereviews$cons)
businessservicereviews$reviews <- gsub("[^[:alnum:]]", " ",businessservicereviews$reviews)
businessservicereviews$reviews <- tolower(businessservicereviews$reviews)

#it wordclodu2

googlereview <- itcompanyreview %>% 
  dplyr::select(company,rating,reviews=reviews) %>% 
  filter(company=="Google")

tidygoogle <- googlereview %>% 
  group_by(company) %>% 
  tibble(line=1:288) %>% 
  unnest_tokens(word,reviews) %>% 
  group_by(word) %>% 
  filter(n()>5) %>% 
  ungroup()
tidytext::stop_words

cleangoogle <- tidygoogle %>% 
  anti_join(stop_words)

googlesentiment <- cleangoogle %>% 
  filter() %>% 
  group_by(word) %>% 
  dplyr::summarise(frequency=n())

googlesentiment %>% wordcloud2()


googlesentiment <- googlesentiment %>% 
  inner_join(get_sentiments("afinn")) %>% 
  mutate(score=frequency*value) %>% 
  mutate(sentiment=ifelse(value>0,"positive","negative"))

totalscore <- sum(googlesentiment$score)
totalscore
fbtotalscore <- 3129/827
appletotalscore <- 1280/642
netflixtotalscore <- 806/305
microsofttotalscore <- 2260/685
amazontotalscore <- 617/355
googletotalscore <- 661/288
meandat <- mean(c(fbtotalscore,appletotalscore,netflixtotalscore,microsofttotalscore,amazontotalscore,googletotalscore))
fbdev <- ggplot(googlesentiment,aes(x=reorder(word,score),y=score,colour=sentiment,fill=sentiment))+
  geom_col(alpha=0.5)+
  xlab("Words")+
  ylab("Scores")+
  coord_flip()
fbdev
#COMPANIES COMPARISON#


it <- itcompanyreview %>% 
  dplyr::select(company,rating,reviews=reviews)
it$industry ="it"
it$rating=as.integer(it$rating)

tidyit <- it %>% 
  tibble(line=1:3102) %>% 
  unnest_tokens(word,reviews) %>% 
  filter(n()>5)

tidytext::stop_words

cleanit <- tidyit %>% 
  anti_join(stop_words)


it_affin_lexicon <- cleanit %>% 
  inner_join(get_sentiments("afinn")) %>%
  group_by(line,rating) %>% 
  summarise(m=mean(value))

it_plot <- ggplot(it_affin_lexicon,aes(x=m,y=rating,group=rating,colour=rating))+
  geom_boxplot()+
  xlab("Sentiment Scores")+
  ylab("Overall Rating")+
  theme_bw()

it_plot





business <- businessservicereviews %>% 
  dplyr::select(company,rating,reviews=reviews)

business$industry ="business"
business$rating=as.integer(business$rating)

comparison <- rbind(it,business)
comparison$rating <- as.factor(comparison$rating)

tidybusiness <- comparison %>% 
  tibble(line=1:6793) %>% 
  unnest_tokens(word,reviews) %>% 
  filter(n()>5)

tidytext::stop_words

cleanbusiness <- tidybusiness %>% 
  anti_join(stop_words)


business_affin_lexicon <- cleanbusiness %>% 
  inner_join(get_sentiments("afinn")) %>%
  group_by(industry,line,rating) %>% 
  summarise(m=mean(value))

business_plot <- ggplot(business_affin_lexicon,aes(x=m,y=rating,colour=industry))+
  geom_boxplot()+
  xlab("Mean")+
  ylab("Rating")+
  theme_bw()

business_plot



business_affin_lexicon$m <- as.numeric(business_affin_lexicon$m)
business_affin_lexicon$rating <- as.numeric(business_affin_lexicon$rating)


# Two-way interaction plot

interaction.plot(x.factor = business_affin_lexicon$rating, trace.factor = business_affin_lexicon$industry, 
                 response = business_affin_lexicon$m, fun = mean, col=c("red","blue"),
                 type = "b", legend = TRUE)

stat_compare_means(business_affin_lexicon)

model1 <- lm(rating ~ m,it_affin_lexicon)

tidy(model1) %>% as_tibble()
glance(model1)

anova(model1)

grid3 <- it_affin_lexicon %>% 
  data_grid(rating,m) %>%
  add_predictions(model1)


ggplot(grid3,aes(x=m,y=pred))+
  geom_point()+
  geom_smooth()+
  xlab("Sentiment Score")+
  ylab("Overall Rating")+
  theme_bw()

#  REGRESSION ****************************************************






# *******************************************************************************



######################UI####################################################

ui <- dashboardPage(
  header = dashboardHeader(title = ""),
  sidebar = dashboardSidebar(
    sidebarMenu(
      menuItem('About', tabName = 'about'),
      menuItem('Data', tabName = 'data'),
      menuItem(
        'Trend', tabName = 'trend',
        menuSubItem('Top Companies', tabName = 'topCompanies'),
        menuSubItem('FANMAG Companies', tabName = 'faangCompanies')
      ),
      menuItem(
        'Ratings', tabName = 'ratings',
        menuSubItem('Exploratory Data Analysis', tabName = 'eda'),
        menuSubItem('Chi Square Test', tabName = 'chiTest'),
        menuSubItem('Linear Regression', tabName = 'lr')
      ),
      menuItem(
        'Reviews', tabName = 'reviews',
        menuSubItem('Word Cloud', tabName = 'wordCloud'),
        menuSubItem('Affinn Lexicon', tabName = 'affin'),
        menuSubItem('Bing Lexicon', tabName = 'bing'),
        menuSubItem('Linear Regression', tabName = 'lr2')
      )
    )
  ),
  body = dashboardBody(
    tabItems(
      tabItem(
        tabName = 'about',
        fluidRow(
          box(
            title = 'Glassdoor', width = 12, solidHeader = T, status = 'primary',
            p(
              "Glassdoor is one of the world's largest job and recruiting sites. The insights on Glassdoor shed light on an organisation's culture, which is helpful in attracting talents and driving employees' satisfaction. This in turn affects employee's performance and impact on business. In fact, reviews on Glassdoor can be used to predict a range of corporate outcomes, including profitability, innovation, as well as financial fraud. Hence, in this report, we will explore the trends of companies' ratings over the year and investigate the factors that contribute to these ratings. Additionally, we will conduct sentiment analysis to understand the emotional intent of words based on Glassdoor reviews."
            )
          ),
        ),
        fluidRow(
          box(
            title = 'Motivation and Objective', width = 12, solidHeader = T, status = 'primary',
            p(
              "Glassdoor ratings are based on Glassdoor's proprietary awards algorithm, and each employer's rating was determined based on the quantity, quality, and consistency of reviews. Additionally, heavier weightage is placed on more recent reviews. However, the aggregated rating does not provide insights into company reviews such as work-life balance and career opportunities. Moreover, Glassdoor's API is not publicly available, and they do not provide reviews that are of valuable insights into a company's culture."
            ),
            p(
              "This dashboard aims to provide a comprehensive insight on trends, factors affecting ratings, and comprehend nuanced emotions of reviews. With this information, individuals can position themselves to be an informed candidate in their job search. Similarly, companies can better reflect based on ratings and reviews on Glassdoor. Following, concerted effort to improve the culture of companies can increase the productivity of employees, and to attract talents."
            )
          )
        )
      ),
      
      tabItem(
        tabName = 'data',
        tabsetPanel(
          tabPanel(
            'Top Companies',
            DTOutput('topcompanies')
          ),
          tabPanel(
            'Employee Ratings',
            DTOutput('employeerating')
          ),
          tabPanel(
            'Business Service Review',
            DTOutput('businessservicereviews')
          )
        )
      ),
      
      tabItem(
        tabName = 'topCompanies',
        fluidRow(
          box(
            width = 12, height = 540,
            plotlyOutput('rankingPlot')
          )
        ),
        fluidRow(
          column(
            width = 3,
            selectInput(
              inputId = 'numberOfCompanies',
              label = 'No. of Companies',
              choices = c(10,20,30,40,50,60,70,80,90,100)
            )
          ),
          column(
            width = 3,
            sliderInput(
              inputId = 'yearsOfInterest',
              label = 'Years of interest',
              min = min(topcompanies$year),
              max = max(topcompanies$year),
              step = 1,
              value = max(topcompanies$year)
            )
          )
        )
      ),
      
      tabItem(
        tabName = 'faangCompanies',
        fluidRow(
          box(
            width = 12, height = 540,
            plotlyOutput('faangPlot')
          )
        )
      ),
      
      tabItem(
        tabName = 'eda',
        fluidRow(
          box(
            width = 4, solidHeader = T, status = 'primary',
            title = 'Summary Statistics'
          ),
          box(
            width = 8, solidHeader = T, status = 'primary',
            title = 'Plot',
            plotOutput('edaPlot')
          )
        ),
        fluidRow(
          column(
            width = 4,
            selectInput(
              inputId = 'edaPlotSelector',
              label = 'Slect plot to show',
              choices = c('Overall', 'Workbalance', 'Culture', 'Company benefit', 'Management', 'Opportunities')
            )
          )
        )
      ),
      
      tabItem(
        tabName = 'chiTest',
        fluidRow(
          box(
            width = 12, solidHeader = T, status = 'primary',
            title = 'Correlation Plot',
            br(),
            plotOutput('corrPlot'),
            br()
          )
        ),
        br(),
        fluidRow(
          h2('Select variables for correlation plot')
        ),
        fluidRow(
          column(
            width = 6,
            selectInput(
              inputId = 'corrVar1',
              label = 'Select Variable 1',
              choices = c('Overall rating'='overall.ratings', 'Work balance rating'='work.balance.stars', 'Culture value rating'='culture.values.stars', 'Company benefit rating'='comp.benefit.stars', 'Career Opportunity'='carrer.opportunities.stars', 'Senior management Rating'='senior.mangemnet.stars')
            )
          ),
          column(
            width = 6,
            selectInput(
              inputId = 'corrVar2',
              label = 'Select Variable 2',
              choices = c('Overall rating'='overall.ratings', 'Work balance rating'='work.balance.stars', 'Culture value rating'='culture.values.stars', 'Company benefit rating'='comp.benefit.stars', 'Career Opportunity'='carrer.opportunities.stars', 'Senior management Rating'='senior.mangemnet.stars')
            )
          )
        )
      ),
      tabItem(
        tabName = 'lr',
        fluidRow(
          box(
            width = 12, solidHeader = T, status = 'primary',
            title = 'Linear Regression',
            br(),
            plotOutput('lr1'),
            br()
          )
        ),
        br(),
        fluidRow(
          h2('Select variables for Linear Regression')
        ),
        fluidRow(
          column(
            width = 6,
            selectInput(
              inputId = 'corrVar1',
              label = 'Select Variable',
              choices = c('Overall rating'='overall.ratings', 'Work balance rating'='work.balance.stars', 'Culture value rating'='culture.values.stars', 'Company benefit rating'='comp.benefit.stars', 'Career Opportunity'='carrer.opportunities.stars', 'Senior management Rating'='senior.mangemnet.stars')
            )
          )
        )
      ),
      tabItem(
        tabName = 'wordCloud',
        fluidRow(
          box(
            width = 12, height = 540, solidHeader = T, status = 'primary',
            title = 'Word Cloud',
            wordcloud2Output('wordCloud')
          )
        ),
        fluidRow(
          column(
            width = 6,
            selectInput(
              inputId = 'wordCloudCompany',
              label = 'Select Company',
              choices = unique(itcompanyreview$company)
            )
          ),
          column(
            width = 6,
            textInput(
              inputId = 'wordCloudFreq',
              label = 'Specify minimum frequency',
              value = '6'
            )
          )
        )
      ),
      
      tabItem(
        tabName = 'affin',
        fluidRow(
          box(
            width = 4, solidHeader = T, status = 'primary',
            title = 'Affin Summary',
            verbatimTextOutput('affinSummary')
          ),
          box(
            width = 4, solidHeader = T, status = 'primary',
            title = 'Affin Lexicon Graph',
            wordcloud2Output('affinLexiconGram')
          ),
          box(
            width = 4, solidHeader = T, status = 'primary',
            title = 'Sentiment',
            plotOutput('affinPlot')
          )
        ),
        fluidRow(
          column(
            width = 4,
            radioButtons(
              inputId = 'affinChartType',
              label = 'Select chart type',
              choices = c('Unigram', 'Bigram', 'Trigram')
            )
          ),
          column(
            width = 4,
            selectInput(
              inputId = 'affinCompany',
              label = 'Select Company',
              choices = unique(itcompanyreview$company)
            )
          ),
          column(
            width = 4,
            textInput(
              inputId = 'affinFreq',
              label = 'Select minimum frequency',
              value = 6
            )
          )
        )
      ),
      
      tabItem(
        tabName = 'bing',
        fluidRow(
          box(
            width = 4, solidHeader = T, status = 'primary',
            title = 'Bing Summary',
            verbatimTextOutput('bingSummary')
          ),
          box(
            width = 8, height = 550, solidHeader = T, status = 'primary',
            title = 'Bing Plot',
            plotOutput('bingPlot' , height = '500px')
          )
        ),
        fluidRow(
          column(
            width = 4,
            selectInput(
              inputId = 'bingCompany',
              label = 'Select Company',
              choices = unique(itcompanyreview$company)
            )
          ),
          column(
            width = 4,
            textInput(
              inputId = 'bingFreq',
              label = 'Select minimum frequency',
              value = 6
            )
          )
        )
      ),
      tabItem(
        tabName = 'lr2',
        fluidRow(
          box(
            width = 12, solidHeader = T, status = 'primary',
            title = 'Linear Regression',
            br(),
            plotOutput('lr2'),
            br()
          )
        ),
      )
      
    )
  )
)

server <- function(input, output, session) {
  output$topcompanies <- renderDT(
    topcompanies
  )
  
  output$employeerating <- renderDT(
    employeerating
  )
  
  output$businessservicereviews <- renderDT(
    businessservicereviews
  )
  
  output$rankingPlot <- renderPlotly({
    data <- topcompanies %>% filter(year == input$yearsOfInterest & ranking %in%  1:input$numberOfCompanies)
    plot <- plot_ly(
      data = data,
      y = ~reorder(company, rating),
      x = ~rating,
      type = 'bar',
      orientation = 'h'
    ) %>%
      layout(
        title = paste('Top ', input$numberOfCompanies, 'companies to work in ', input$yearsOfInterest),
        xaxis = list(title = 'Ranking'),
        yaxis = list(title = ''),
        height = 500
      )
    plot
  })
  
  output$faangPlot <- renderPlotly({
    data <- topcompanies %>% filter(company %in% c('Google', 'Facebook', 'Amazon', 'Apple', 'Netflix'))
    plot_ly(
      data = data,
      x = ~year,
      y = ~ranking,
      color = ~company,
      type = 'scatter',
      mode = 'lines'
    ) %>%
      layout(
        title = paste('Ranking of FAANG companies across the years'),
        xaxis = list(title = 'Year'),
        yaxis = list(title = 'Ranking'),
        height = 500
      )
  })
  
  output$edaPlot <- renderPlot({
    if(input$edaPlotSelector=='Overall'){
      overall
    } 
    else if(input$edaPlotSelector=='Workbalance'){
      workbalance
    }
    else if(input$edaPlotSelector=='Company benefit'){
      benefits
    }
    else if(input$edaPlotSelector=='Culture'){
      culture
    }
    else if(input$edaPlotSelector=='Management'){
      management
    }
    else{
      opportunities
    }
  })
  
  output$corrPlot <- renderPlot({
    crosstab <- chisq.test(xtabs(~eval(parse(text = input$corrVar1))+eval(parse(text = input$corrVar2)),data=newemployeerating))
    corrPlot <- corrplot(round(crosstab$residuals, 2), method = "color",
                         addCoef.col = "black",
                         tl.col="black", 
                         tl.srt = 0,
                         is.corr = FALSE)
    corrPlot
  })
  
  output$lr1 <- renderPlot(
    ggplot(grid2, aes(x = values, y = m,colour=factor(factors))) +
      geom_line(size = 2)+
      xlab("Actual Ratings")+
      ylab("Predicted Ratings")+
      labs(colour="Factors")+
      theme_bw()
  )
  
  output$wordCloud <- renderWordcloud2(
    itcompanyreview %>% 
      dplyr::select(company,rating,reviews=reviews) %>% 
      filter(company==input$wordCloudCompany) %>%
      group_by(company) %>% 
      unnest_tokens(word,reviews) %>% 
      group_by(word) %>% 
      filter(n()>=as.numeric(input$wordCloudFreq)) %>% 
      ungroup() %>% 
      anti_join(stop_words)%>%
      group_by(word) %>% 
      dplyr::summarise(frequency=n()) %>% 
      wordcloud2(shape = 'square')
  )
  
  output$affinSummary <- renderPrint(
    
    summary(itcompanyreview %>% 
              dplyr::select(company,rating,reviews=reviews) %>% 
              filter(company==input$affinCompany) %>%
              group_by(company) %>%
              unnest_tokens(word,reviews) %>% 
              group_by(word) %>% 
              filter(n()>=as.numeric(input$affinFreq)) %>% 
              ungroup() %>% 
              anti_join(stop_words) %>% 
              group_by(word) %>% 
              dplyr::summarise(frequency=n()) %>% 
              dplyr::select(word) %>% 
              inner_join(get_sentiments("afinn")) %>% 
              mutate(sentiment=ifelse(value>0,"positive","negative"),score=value)
    )
    
  )
  
  output$affinPlot <- renderPlot(
    itcompanyreview %>% 
      dplyr::select(company,rating,reviews=reviews) %>% 
      filter(company==input$affinCompany) %>%
      group_by(company) %>%
      unnest_tokens(word,reviews) %>% 
      group_by(word) %>% 
      filter(n()>=as.numeric(input$affinFreq)) %>% 
      ungroup() %>% 
      anti_join(stop_words) %>% 
      group_by(word) %>% 
      dplyr::summarise(frequency=n()) %>% 
      dplyr::select(word) %>% 
      inner_join(get_sentiments("afinn")) %>% 
      mutate(sentiment=ifelse(value>0,"positive","negative"),score=value) %>%
      ggplot(aes(x=reorder(word,score),y=score,colour=sentiment,fill=sentiment))+
      geom_col(alpha=0.5)+
      coord_flip()
  )
  
  output$affinLexiconGram <- renderWordcloud2({
    ### unigram
    tidyunigram <- itcompanyreview %>% 
      dplyr::select(company,rating,reviews=reviews) %>% 
      filter(company==input$affinCompany) %>%
      unnest_tokens(unigram, reviews, token = "ngrams", n = 1) %>% 
      ungroup() %>%
      count(unigram, sort = TRUE)
    
    ### bigram
    tidybigram <- itcompanyreview %>% 
      dplyr::select(company,rating,reviews=reviews) %>% 
      filter(company==input$affinCompany) %>%
      unnest_tokens(bigram, reviews, token = "ngrams", n = 2) %>% 
      ungroup() %>%
      count(bigram, sort = TRUE)
    
    
    ### trigram
    tidytrigram <- itcompanyreview %>% 
      dplyr::select(company,rating,reviews=reviews) %>% 
      filter(company==input$affinCompany) %>%
      unnest_tokens(trigram, reviews, token = "ngrams", n = 3) %>% 
      ungroup() %>%
      count(trigram, sort = TRUE)
    
    if(input$affinChartType=='Unigram'){
      LexiconGram <- tidyunigram %>% filter(n>=as.numeric(input$affinFreq)) %>% wordcloud2()
    }
    else if(input$affinChartType=='Bigram'){
      LexiconGram <- tidybigram %>% filter(n>=as.numeric(input$affinFreq)) %>% wordcloud2()
    }
    else {
      LexiconGram <- tidytrigram %>% filter(n>=as.numeric(input$affinFreq)) %>% wordcloud2()
    }
    
    LexiconGram
  })
  
  output$bingSummary <- renderPrint(
    
    summary(itcompanyreview %>% 
              dplyr::select(company,rating,reviews=reviews) %>% 
              filter(company==input$bingCompany) %>%
              group_by(company) %>%
              unnest_tokens(word,reviews) %>% 
              group_by(word) %>% 
              filter(n()>=as.numeric(input$bingFreq)) %>% 
              ungroup() %>% 
              anti_join(stop_words) %>% 
              group_by(word) %>% 
              dplyr::summarise(frequency=n()) %>%
              inner_join(get_sentiments("bing")) %>% 
              dplyr::count(word,sentiment,frequency,sort=TRUE) %>% 
              acast(word~sentiment,value.var="frequency",fill=0)
    )
    
  )
  
  output$bingPlot <- renderPlot(
    itcompanyreview %>% 
      dplyr::select(company,rating,reviews=reviews) %>% 
      filter(company==input$bingCompany) %>%
      group_by(company) %>%
      unnest_tokens(word,reviews) %>% 
      group_by(word) %>% 
      filter(n()>=as.numeric(input$bingFreq)) %>% 
      ungroup() %>% 
      anti_join(stop_words) %>% 
      group_by(word) %>% 
      dplyr::summarise(frequency=n()) %>%
      inner_join(get_sentiments("bing")) %>% 
      dplyr::count(word,sentiment,frequency,sort=TRUE) %>% 
      acast(word~sentiment,value.var="frequency",fill=0) %>% 
      comparison.cloud(max.words = 150, fixed.asp=T, title.size = 5)
  )
  
  output$lr2 <- renderPlot(
    ggplot(grid3,aes(x=m,y=pred))+
      geom_point()+
      geom_smooth()+
      xlab("Sentiment Score")+
      ylab("Overall Rating")+
      theme_bw()
  )
  
}

shinyApp(ui, server)



