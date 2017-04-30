library(shiny)
popScoreComposerComplete=readRDS("scoreWNation.rds")

ui=fluidPage(
  mainPanel(
    titlePanel("Art and Social Change -- NY Phil performance history visualization"),
    tabsetPanel(
      tabPanel("by nationalities",helpText("The performance history data from NY Phil's github page, and composer nationality data are from wikipedia.
                        The performance history by nationalities visualizations provide the viewer a lens to see how social change is associated with the high art."),
                 selectInput(inputId="nation",
                             label="choose a nation",
                             choices=c("american","french","german","italian","russian")),
                 plotOutput(outputId="graph")),
      tabPanel("by composers",helpText("."),
               selectInput(inputId="composers",
                           label="choose a composer",
                           choices=c(
                             "J.S. Bach"="Bach, Johann Sebastian",
                           "Beethoven"="Beethoven, Ludwig van",
                           "Berlioz"="Berlioz, Hector",
                           "Brahms"="Brahms, Johannes",
                           
                           "Gershwin"="Gershwin, George",
                           "Haydn"="Haydn, Franz Joseph",
                           "Handel"="Handel, George Frideric",
                           "Liszt"="Liszt, Franz",
                           "Mendelssohn"="Mendelssohn, Felix",
                           "Mozart"=" Mozart, Wolfgang Amadeus",
                           "Prokofiev"="Prokofiev, Sergei",
                           "Wagner"="Wagner, Richard",
                           "Schumann"="Schumann, Robert",
                           "Schubert"="Schubert, Franz",
                           "Strauss"="Strauss, Richard",
                           "Shostakovich"="Shostakovich, Dmitri",
                           "Stravinsky"="Stravinsky, Igor",
                           "Tchaikovsky"="Tchaikovsky, Pyotr Ilyich",
                           "Weber"="Weber, Carl Maria Von")
               ),
               plotOutput(outputId="graph2")),
      tabPanel("statement", textOutput("statement"))

    )
  )
)

server=function(input,output){
  output$graph=renderPlot({
    CountryPop=popScoreComposerComplete[which(popScoreComposerComplete$nation==input$nation),]
    CountryPopSum=colSums(CountryPop[2:175])
    qplot(seq_along(CountryPopSum),CountryPopSum)+geom_line()+ylim(0,1)+scale_x_continuous(breaks=seq(1,175,10),labels=c("1842","1852","1862","1872","1882","1892","1902","1912","1922","1932","1942","1952","1962","1972","1982","1992","2002","2012"))+ theme(axis.text.x = element_text(angle = 45,size=10, hjust = 1))+xlab("seasons")+ylab("percentage of works being performed")+ggtitle(input$nation)
 })
  output$graph2=renderPlot({
      CountryPop=popScoreComposerComplete[which(popScoreComposerComplete$composers==input$composers),]
      CountryPopSum=colSums(CountryPop[2:175])
      qplot(seq_along(CountryPopSum),CountryPopSum)+geom_line()+ylim(0,.5)  +scale_x_continuous(breaks=seq(1,175,10),labels=c("1842","1852","1862","1872","1882","1892","1902","1912","1922","1932","1942","1952","1962","1972","1982","1992","2002","2012"))+ theme(axis.text.x = element_text(angle = 45,size=10, hjust = 1))+xlab("seasons")+ylab("percentage of works being performed")+ggtitle(input$composers)
    })
  output$statement=renderText("Does art still matter in the society? Where is the position of high art nowadays? The high arts are always treated as detached to social changes, but is it true? In this study, I want to answer these questions by creating an interactive app to visualize NY Philharmonic performance history data. 
The original dataset comes from NY Phil's github page, and the nationalities data come from web scraping Wikipedia pages on composers by nationalities. To measure the popularity of the composers, I did not use the number of performance in each season. Rather, I used the percentage of the number of performance of a composer over the total number of performance of the year. 
                              The app is a way for people in the field of humanities to have data to backup their qualitative arguments. German composers works decreased after WWII. American composers works increased after WWII because of the rising nationalism. The cold war is not associated with the popularity of Russian composers works being performed in the US. Wager becomes so popular at the end of 19th century perhaps because of Der Rheingold, yet his popularity decreased after WWII because he is Hitler's favorite composer. The cold war does not seem to affect the popularity Dmitri Shostakovich who is in a love hate relationship with Stalin. On the other hand, Shostakovich seems never so popular in the United States. 
                              This work is just a start. Future work includes creating life line for different performers, and see the rise and fall of their stardom. It is hard to see how gender is played in the classical music composer just because it is just way too few women composers. But seeing how gender changes among performers would be interesting. In addition, it would be fun to give the users the option to interact different variables and create their own graph, like the graph of Russian works played by Russian soloist, etc. 
                              Art is a form of representation. It does not determine, yet it matters. I'd say classical music and other forms of high arts are never as elastic on reflecting social changes as pop art, yet statistics show that they do reflect the changes in society. 
                              ~Aiyi Zhang
                            ")
   }
  

shinyApp(ui=ui,server=server)