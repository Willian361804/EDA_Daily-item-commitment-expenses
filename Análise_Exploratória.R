library(shiny)
library(RColorBrewer)

dados = read.csv("dados.csv", sep = ";")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Despesas de empenho da rúbrica diárias no País - Municípios RS"),
    
    fluidRow(
        column(2,
               h3(textOutput("TEmpenho")),
               tableOutput("SEmpenho")),
        column(4,plotOutput("HistEmpenho")),
        column(2,
               h3(textOutput("TPIB")),
               tableOutput("SPIB")),
        column(4,plotOutput("HistPIB"))
    ),
    fluidRow(
        column(3,plotOutput("BoxEmpenho")),
        column(3,plotOutput("BoxPIB")),
        column(6,plotOutput("Disp"))
    ),
    fluidRow(
        column(4,plotOutput("MaioresEmpenhos")),
        column(4,plotOutput("MaioresPIBS")),
        column(4,plotOutput("MaiorProporcao"))
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$TEmpenho = renderText({ "Dados de Empenho" })
    output$SEmpenho = renderTable({ as.array(summary(dados$VALOREMPENHO)) })
    output$TPIB = renderText({ "Dados do PIB" })
    output$SPIB = renderTable({ as.array(summary(dados$PIB)) })
    
    output$HistEmpenho = renderPlot({
        hist(dados$VALOREMPENHO,main="Valores de Empenho",
             col=brewer.pal(n=3,name="Paired"),xlab="Empenho")
    })
    
    output$HistPIB = renderPlot({
        hist(dados$PIB,main="Valores do PIB",
             col=brewer.pal(n=3,name="Pastel1"),xlab="PIB")
    })
    
    output$BoxEmpenho = renderPlot({
        boxplot(dados$VALOREMPENHO,main="Valores de Empenho",
             col=brewer.pal(n=3,name="Paired"),outline = F,horizontal = T)
    })
    
    output$BoxPIB = renderPlot({
        boxplot(dados$PIB,main="Valores do PIB",
                col=brewer.pal(n=3,name="Pastel1"),outline = F,horizontal = T)
    })
    
    output$Disp = renderPlot({
        plot(dados$VALOREMPENHO,dados$PIB,main="Empenho vs PIB",
             xlab = "Empenho", ylab = "PIB", pch=19, col="blue")
    })
    
    
    MEmpenho = head(dados[order(-dados$VALOREMPENHO),],10)
    output$MaioresEmpenhos = renderPlot({
        barplot(MEmpenho$VALOREMPENHO,col=brewer.pal(n=3,name="RdBu"),las=2,
                main="Maiores Empenhos",xlab="Empenhos")
        legend("topright",legend=MEmpenho$MUNICIPIO,
               col=brewer.pal(n=3,name="RdBu"),lty=1:2,cex=0.6,ncol=2,lwd=4)
        box()
    })
    
    MPIBS = head(dados[order(-dados$PIB),],10)
    output$MaioresPIBS = renderPlot({
        pie(MPIBS$PIB,col=brewer.pal(n=3,name="Spectral"),
            labels=MPIBS$MUNICIPIO,main="Maiores PIBS",xlab="PIBs")
    })
    
    dados$PROPORCAO = dados$VALOREMPENHO / dados$PIB
    Mprop = head(dados[order(-dados$PROPORCAO),],10)
    
    output$MaiorProporcao = renderPlot({
        barplot(Mprop$PROPORCAO,col=brewer.pal(n=3,name="Set3"),las=2,
                main="Maiores Gastos em Proporção ao PIB",xlab="Proporção")
        legend("topright",legend=Mprop$MUNICIPIO,
               col=brewer.pal(n=3,name="Set3"),lty=1:2,cex=0.6,ncol=2,lwd=4)
        box()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
