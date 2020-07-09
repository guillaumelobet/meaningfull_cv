#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(rjson)
library(XML)
library(visNetwork)
library(igraph)

metrics_text <- "The Field Citation Ratio (FCR) indicates the relative citation performance of an article, when compared to similarly-aged articles in its subject area (1 = average). FCR are only computed for article more than 2 years old. [from dimensions.ai].\n
The Relative Citation Ratio (RCR) indicates the relative citation performance of an article, when compared to articles in its subject area (1 = average). RCR are only computed for article more than 2 years old. [from dimensions.ai].\n
The Altmetric Score is an automatically calculated, weighted count of all of the attention a research output has received online. [from altmetrics.com]\n\n"


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Meaningful CV"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            textAreaInput("dois","DOIs of your papers, separated by commas", "10.1016/j.tplants.2017.05.002,
10.1093/nar/gkv1054,
10.3389/fpls.2014.00121"),
            checkboxInput("journal", "I still want the journal names", value = F),
            checkboxInput("graph", "Build citation graph from PubMed (takes time, be patient)", value = F),
            actionButton("getInfo", "Launch the CV generator"),
            tags$hr(),
            helpText("Data are coming from crossref.org, altmetric.com, ncbi.nlm.nih.gov/pubmed and dimensions.ai. This is a test version")
        ),

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
                tabPanel("Overview", 
                         tags$hr(),
                         downloadButton("download_txt", "Download as txt file"),
                         tags$hr(),
                         htmlOutput("summary")
                ),
                tabPanel("Table", 
                         tags$hr(),
                         downloadButton("download_csv", "Download as csv file"),
                         tags$hr(),
                         dataTableOutput("paperlist")
                         
                ),
                tabPanel("Plots", 
                         tags$h3("Citation graph"),
                         helpText("The data used for the citation graph is coming from PubMed, which contains only a subset of the sclientific literature. So don't be suprised if it contains less citations than expected."),
                         plotOutput("network", height = "600px")     
                )
            )
        )
    )
)


#---------------------------------------------------------------------------
#---------------------------------------------------------------------------
#---------------------------------------------------------------------------
# Define server logic 
#---------------------------------------------------------------------------
#---------------------------------------------------------------------------
#---------------------------------------------------------------------------
server <- function(input, output) {

    rs <- reactiveValues(papers = NULL,
                         papertext = NULL,
                         citegraph = NULL,
                         pmids = NULL)
    
    observeEvent(input$getInfo, {
        
        pubs <- gsub("http://", "", input$dois)
        pubs <- gsub("https://", "", pubs)
        pubs <- gsub("www.doi.org/", "", pubs)
        pubs <- gsub("doi.org/", "", pubs)
        
        pubs <- strsplit(pubs, ",")[[1]]
        # pubs <- strsplit("10.1146/annurev-arplant-043015-111848, 10.1186/1746-4811-9-1, 10.1104/pp.111.179895", ",")[[1]]
        
        all <- NULL
        
        n <- length(pubs)
        
        withProgress(message = 'Gattering data', value = 0, {
        
            for(p in pubs){
                
                p <- gsub("\n", "", p)
                p <- gsub(" ", "", p)
                print(p)
                incProgress(1/n, detail = paste0("Scrapping webpages ", p))
                
                temp <- tibble("doi" = p, 
                               "title" = "-", 
                               "authors" = "-", 
                               "pages" = "-", 
                               "volume" = "-", 
                               "year" = "-", 
                               "journal" = "-", 
                               "altmetric" = "-", 
                               "rcr" = "-", 
                               "fcr" = "-", 
                               "citations" = "-")
                
                
                # ------------------------
                # Get data from CrossRef
                crossref <- NULL
                tryCatch({
                    crossref<- fromJSON(readLines(paste0("https://api.crossref.org/v1/works/",p)))
                    temp$title <- crossref$message$title
                    
                    text_auth <- ""
                    auths <- crossref$message$author
                    for(au in auths){
                        if(text_auth == ""){
                            text_auth <- paste0(au$family, ", ", au$given)
                        }else{
                            text_auth <- paste0(text_auth, ", ", au$family, ", ", au$given)
                        }
                    }
                    
                    temp$authors <- text_auth
                    temp$year <- crossref$message$created$`date-parts`[[1]][1]
                    temp$journal <- crossref$message$`container-title`
                    if(!is.null(crossref$message$volume)){
                        temp$volume <- crossref$message$volume
                    }
                    if(!is.null(crossref$message$page)){
                        temp$pages <- crossref$message$page
                    }
                }, error = function(e) {
                    print("Error : Crossref data not found")
                })
                
                # 
                # if(!is.null(crossref)){
                # 
                # }
                
                # ------------------------
                # Get data from Altmetric
                tryCatch({
                    altm <- fromJSON(readLines(paste0("https://api.altmetric.com/v1/doi/",p)))
                    temp$altmetric <- round(altm$score)
                }, error = function(e){ 
                    print("Error : Altmetric data not found")
                })
                # if(!is.null(altm)){
                #     temp$altmetric <- round(altm$score)
                # }
                
                # ------------------------
                # Get data from Dimensions
                tryCatch({
                    dims <- fromJSON(readLines(paste0("https://metrics-api.dimensions.ai/doi/",p)))
                    temp$citations <- dims$times_cited
                    if(temp$year != ""){
                        if(!is.null(dims$field_citation_ratio)){
                            temp$fcr <- dims$field_citation_ratio
                        }
                        if(!is.null(dims$relative_citation_ratio)){
                            temp$rcr <- dims$relative_citation_ratio
                        }
                    }
                }, error = function(e) { 
                    print("Error : Dimensions data not found")
                })

                # if(!is.null(dims)){
                #     temp$citations <- dims$times_cited
                #     if(temp$year != ""){
                #         if(!is.null(dims$field_citation_ratio)){
                #             temp$fcr <- dims$field_citation_ratio
                #         }
                #         if(!is.null(dims$relative_citation_ratio)){
                #             temp$rcr <- dims$relative_citation_ratio
                #         }
                #     }
                # }
                
                all <- rbind(all, temp)
    
            }
            
        })
        
        if(input$graph){
            withProgress(message = 'Building graph', value = 0, {
                
                n <- length(all$doi)
                # Get data from Pubmed
                pmids <- c()
                for(d in all$doi){
                    incProgress(1/n, detail = paste0("Getting PMIDs ", d))
                    tryCatch({
                        p <- gsub("/", "%2F", d)
                        path <- paste0("https://www.ncbi.nlm.nih.gov/pubmed/?term=",p,"&report=xml&format=text")
                        f <- file(path)
                        data <- readLines(f, warn = F)
                        pmid <-strsplit(xmlToList(xmlParse(data, asText = T))[1], "\n")[[1]]
                        close(f)
                        pmid <- gsub(" ", "", gsub("</PMID>", "", gsub('<PMID Version=\"1\">', "", pmid[grepl("PMID", pmid)])))
                        pmids <- c(pmids, pmid)
                    }, error = function(e) {
                    })
                }
            })
            withProgress(message = 'Building graph', value = 0, {
                
                n <- length(pmids)
                print(pmids)
                
                citegraph <- NULL
                for(pmid in pmids){
                    # Get more info with the pmid id
                    tryCatch({
                        
                        # We build the URL to retrieve the citing pmids
                        path <- paste("https://www.ncbi.nlm.nih.gov/pubmed?linkname=pubmed_pubmed_citedin&from_uid=",
                                      pmid,
                                      "&report=uilist&format=text&dispmax=200", sep="")
                        f <- file(path)
                        data <- readLines(f, warn = F)
                        xml <- xmlToList(xmlParse(data, asText = T))[1]
                        
                        if(!is.null(xml)){
                            citing <-strsplit(xml, "\n")[[1]]
                            close(f)
                            
                            # Then we create a table containing ll these pmids
                            if(length(citing) < 200){
                                for(pm in citing){
                                    citegraph <- rbind(citegraph, data.frame(pmid=as.numeric(pmid), citing=as.numeric(pm)))
                                }
                            }
                            incProgress(1/n, detail = paste0("Getting citations ", pmid," done: ",length(citing)))
                            
                        }else{
                            close(f)
                        }
                        
                    }, error = function(e) {
                        message(e)
                    })
                }
                rs$citegraph <- citegraph
                rs$pmids <- pmids
            })
        }
        
        
        if(!input$journal){
            all$journal <- "-"
            all$volume <- "-"
            all$pages <- "-"
        }
        rs$papers <- all
        
        cites <- sort(as.numeric(all$citations), decreasing = T)
        
        hindex <- 0
        for(i in 1:length(cites)){
            if(i <= cites[i]) hindex <- hindex+1
        } 
        text1 <- ""
        text1 <- paste0(text1, "published articles = ",nrow(all),"\n")
        text1 <- paste0(text1, "h-index = ",hindex,"\n")
        text1 <- paste0(text1, "total citations = ",sum(as.numeric(all$citations), na.rm = T),"\n")
        text1 <- paste0(text1, "average citations = ",round(mean(cites, na.rm = T)),"\n")
        text1 <- paste0(text1, "median citations = ",median(cites, na.rm = T),"\n")
        text1 <- paste0(text1, "average fcr = ",round(mean(as.numeric(all$fcr), na.rm = T), 1),"\n")
        text1 <- paste0(text1, "median fcr = ",median(as.numeric(all$fcr), na.rm=T),"\n")
        text1 <- paste0(text1, "average rcr = ",round(mean(as.numeric(all$rcr), na.rm = T), 1),"\n")
        text1 <- paste0(text1, "median rcr = ",median(as.numeric(all$rcr), na.rm=T),"\n")
        
        
        text2 <- paste0(all$title, ", ", 
                       all$authors," (",
                       all$year, "), ",
                       all$journal, ", ", all$volume, " (",
                       all$pages, "), ",
                       "[citations = ",all$citations,"] ",
                       "[altmetric = ",all$altmetric,"] ",
                       "[rcr = ",all$rcr,"] ",
                       "[fcr = ",all$fcr,"]",
                       "[http://doi.org/",all$doi,"]", collapse="\n\n")
        
        rs$papertext <- list(text1, text2)

        
    })
    
    output$paperlist = renderDataTable({
        req(rs$papers)
        rs$papers
    })
    
    output$summary <- renderUI({
        HTML(gsub("\n", "</br>", paste(metrics_text, paste(rs$papertext, collapse = "\n\n"))))
    })
    
    
    output$download_txt <- downloadHandler(
        filename = function(){
            paste("meaningfull_cv-", Sys.Date(), ".txt", sep = "")
        },
        content = function(file) {
            writeLines(paste(metrics_text, paste(rs$papertext, collapse = "\n\n")), file)
        }
    )
    
    # Downloadable csv of selected dataset ----
    output$download_csv <- downloadHandler(
        filename = function() {
            paste("meaningfull_cv-", Sys.Date(), ".csv", sep = "")
        },
        content = function(file) {
            write.csv(rs$papers, file, row.names = FALSE)
        }
    )
    
    
    output$network <- renderPlot({
        
        pmids_ch <- paste(rs$pmids, collapse=",")
        nodes <- data.frame(id=unique(c(rs$citegraph$pmid, rs$citegraph$citing)))
        for(i in 1:nrow(nodes)){
            nodes$color[i] <- "lightblue"
            if(grepl(nodes$id[i], pmids_ch)){
                nodes$color[i] <- "red"
            }
        }

        edges <- data.frame(from = rs$citegraph[,2], to = rs$citegraph[,1])
        net <- graph_from_data_frame(d=edges, vertices=nodes, directed=F) 
        net$layout <- layout_with_fr
        l <-layout_with_fr(net)
        plot(net, vertex.size=4, vertex.label=NA, layout=l)
        # 
        # pmids_ch <- paste(rs$pmids, collapse=",")
        # 
        # # minimal example

        # visNetwork(nodes, edges, width = "100%")
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
