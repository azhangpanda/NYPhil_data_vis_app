library(shiny)


ui=fluidPage(
  titlePanel("composer popularity by countries"),
  selectInput(inputId="nation",
              label="choose a nation",
              choices=c("american","french","german","italian","russian")),
  plotOutput(outputId="graph")
)

server=function(input,output){
  output$graph=renderPlot({
    CountryPop=popScoreComposerComplete[which(popScoreComposerComplete$nation==input$nation),]
    CountryPopSum=colSums(CountryPop[2:175])
    qplot(seq_along(CountryPopSum),CountryPopSum)+ylim(0,1)+scale_x_continuous(breaks=seq(1,175,10),labels=c("1842","1852","1862","1872","1882","1892","1902","1912","1922","1932","1942","1952","1962","1972","1982","1992","2002","2012"))+ theme(axis.text.x = element_text(angle = 45,size=10, hjust = 1))+xlab("seasons")+ylab("percentage of works being performed")+ggtitle("Popularity Scores Over the Years")
   
 })
}

shinyApp(ui=ui,server=server)