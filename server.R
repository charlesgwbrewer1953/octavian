#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# octavian
# /OneDrive/metis2/metic_select_retrieve/octavian
#
#


# SERVER
#
#
# IMPORTANT NOTE - If plyr is loaded after dplyr (in tidyverse()), then group_by statement will fail
# with group factor not carried across
#
#

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

    # #########################
    #                         #
    # Connections             #
    #                         #
    ###########################
    print("server 1 - set up connect")
    remoteuserpassword <- "m3t1sz"
    conR <- dbConnect(RMariaDB::MariaDB(), dbname = 'metis', 'metis', password = remoteuserpassword, host = "178.62.8.181", port = 3306)
    print("Connected remote 1")
    dbListTables(conR)


    # #########################
    #                         #
    # Normal functions        #
    #                         #
    ###########################

    # User parameter selection

    rssSelection <- function(rssSelected,  Source, Orientation, SourceType, Country, Region, Topic){
        print("server 2 - RSS select")
        ifelse(is.null(Source), rssSelected <- rssSelected,
               rssSelected <- filter(rssSelected, Source == ext_name))
        ifelse(is.null(Orientation), rssSelected <- rssSelected,
               rssSelected <- filter(rssSelected, Orientation == orientation))
        ifelse(is.null(SourceType), rssSelected <- rssSelected,
               rssSelected <- filter(rssSelected, SourceType == SourceType))
        ifelse(is.null(Country), rssSelected <- rssSelected,
               rssSelected <- filter(rssSelected, Country  == country))
        ifelse(is.null(Region), rssSelected <- rssSelected,
               rssSelected <- filter(rssSelected, Region == Region))
        ifelse(is.null(Topic), rssSelected <- rssSelected,
               rssSelected<- dplyr::filter(rssSelected, str_detect(rssSelected[,"item_title"], regex(Topic, ignore_case = TRUE))))
        return(rssSelected)
    }


    # Compute time series data
    posneg <- function(SA_scores){
        neg <- -sum(SA_scores[SA_scores <0])
        pos <- sum(SA_scores[SA_scores>0 ])
        both <- neg + pos
        count <- length(SA_scores)
        posneg <- -(neg-pos)/count
    }

    f.sumVals <- function(query_in) {
        sumVals <- query_in %>%
            group_by(item_date_published) %>%
            summarize(
                syuzhet = sum(syuzhet_score),
                afinn = sum(afinn_score),
                bing = sum(bing_score),
                nrc_anger = sum(nrc_score_anger),
                nrc_anticipation = sum(nrc_score_anticipation),
                nrc_disgust = sum(nrc_score_disgust),
                nrc_fear = sum(nrc_score_fear),
                nrc_joy = sum(nrc_score_joy),
                nrc_positive =sum(nrc_score_positive),
                nrc_negative = sum(nrc_score_negative),
                nrc_sadness = sum(nrc_score_sadness),
                nrc_surprise = sum(nrc_score_surprise),
                nrc_trust = sum(nrc_score_trust),
                loughran_constraining = sum(loughran_frame_constraining),
                loughran_litigious = sum(loughran_frame_litigious),
                loughran_negative = sum(loughran_frame_negative),
                loughran_positive = sum(loughran_frame_positive),
                loughran_uncertain = sum(loughran_frame_uncertain),
                ensemble_posneg = sum(ensemble_posneg)
            )
        sumVals <- filter(sumVals, item_date_published >= input$dateRange[1]) # Remove items before selection date
        sumVals <-sumVals %>% gather('syuzhet', 'afinn', 'bing', 'nrc_anger', 'nrc_anticipation', 'nrc_disgust', 'nrc_fear', 'nrc_joy',
                                     'nrc_positive', 'nrc_negative', 'nrc_sadness', 'nrc_surprise', 'nrc_trust', 'loughran_constraining',
                                     'loughran_litigious', 'loughran_negative', 'loughran_positive', 'loughran_uncertain','ensemble_posneg', key = "factorName", value = 'factorValue')
        sumVals$factorName <- as.factor(sumVals$factorName)
        return(sumVals)
    }

    # Compute aggregated data for time period

    f.totVals <- function(query_in){
        totVals <- query_in %>% gather(syuzhet_score, afinn_score, bing_score,
                                       nrc_score_anger, nrc_score_anticipation, nrc_score_disgust, nrc_score_fear,
                                       nrc_score_joy, nrc_score_positive, nrc_score_negative,
                                       nrc_score_sadness, nrc_score_surprise, nrc_score_trust,
                                       loughran_frame_constraining, loughran_frame_litigious,
                                       loughran_frame_negative, loughran_frame_positive, loughran_frame_uncertain,
                                       nrc_comp, loughran_comp, ensemble_posneg, key = "factorName", value = 'factorValue')

        totVals$factorName <- as.factor(totVals$factorName)
        totValsSums <- tapply(totVals$factorValue, totVals$factorName, FUN = sum, na.rm = TRUE)
        totValsSums1 <- melt(totValsSums)

        colnames(totValsSums1) <- c("Factor", "Value")

        totValsSums1 <- rbind(totValsSums1,
                              data.frame(Factor = "PosNeg", Value = 0),
                              data.frame(Factor = "nrc", Value = 0),
                              data.frame(Factor = "loughran", Value = 0))
        totValsSumsGp <- c(1,1,1,2,
                           2,2,1,1,
                           2,3,3,3,
                           3,3,3,1,
                           1,3,3,3,
                           1,0,0,0) # Allocates value to groups of factors for (eg) colour mapping

        totValsSums1$group1 <- as.factor(totValsSumsGp)
        totValsSums1[totValsSums1$loughran_frame_negative, 2] <-  99 #totValsSums1[totValsSums1$loughran_frame_negative, 2] * -1
        totValsSums1
    }



    # Create sequence of dates for insertion into table names for SQL retrieval

    output$sqlDates <- renderText({
        outSeq <- seq(as.Date(input$dateRange[1]) , as.Date(input$dateRange[2]), by = "day")
        outSeq <- paste0("sa_RSS_library", outSeq, sep = "")
    })

    # Graphic output for charts Row 2/3

    time_Series_graph <- function(sumVals, gtitle, line_col, point_col, point_fill){
        p <- ggplot(sumVals,
                    aes(x = item_date_published, y = rollmean(factorValue, input$dateGrouping, na.pad = TRUE) )) +
            geom_smooth(method = input$ismooth, fullrange = TRUE,se = input$iconfidence, level = input$iconfidenceLevel) +
            xlab("Story date") + ylab("Factor score") +
            theme(legend.position = c(0.1,0.95)) +
            labs(colour = "Methods") +
            theme(legend.title = element_text(size = 8),
                  axis.title.x = element_text(size = 8),
                  axis.title.y = element_text(size = 8),
                  plot.title = element_text(size = 8)
            )
        if(isTRUE(input$aColumn)){(p <- p + geom_col(position = "dodge"))}
        if(isTRUE(input$aLine)){(p <- p + geom_line(colour = line_col))}
        if(isTRUE(input$aPoint)){(p <- p + geom_point(size = 4, shape = 22, colour = point_col, fill = point_fill))}

        return(ggplotly(p))
    }

    ###### acf / pacf graphics: Source: https://rh8liuqy.github.io/ACF_PACF_by_ggplot2.html

    ggplot.corr <- function(data, lag.max = 24, ci = 0.95, large.sample.size = TRUE, horizontal = TRUE, title_line, ...) {

        require(ggplot2)
        require(dplyr)
        require(cowplot)

        if(horizontal == TRUE) {numofrow <- 1} else {numofrow <- 2}

        list.acf <- acf(data, lag.max = lag.max, type = "correlation", plot = FALSE)
        N <- as.numeric(list.acf$n.used)
        df1 <- data.frame(lag = list.acf$lag, acf = list.acf$acf)
        df1$lag.acf <- dplyr::lag(df1$acf, default = 0)
        df1$lag.acf[2] <- 0
        df1$lag.acf.cumsum <- cumsum((df1$lag.acf)^2)
        df1$acfstd <- sqrt(1/N * (1 + 2 * df1$lag.acf.cumsum))
        df1$acfstd[1] <- 0
        df1 <- select(df1, lag, acf, acfstd)

        list.pacf <- acf(data, lag.max = lag.max, type = "partial", plot = FALSE)
        df2 <- data.frame(lag = list.pacf$lag,pacf = list.pacf$acf)
        df2$pacfstd <- sqrt(1/N)

        if(large.sample.size == TRUE) {
            plot.acf <- ggplot(data = df1, aes( x = lag, y = acf)) +
                geom_area(aes(x = lag, y = qnorm((1+ci)/2)*acfstd), fill = "#B9CFE7") +
                geom_area(aes(x = lag, y = -qnorm((1+ci)/2)*acfstd), fill = "#B9CFE7") +
                geom_col(fill = "#4373B6", width = 0.7) +
                scale_x_continuous(breaks = seq(0,max(df1$lag),6)) +
                scale_y_continuous(name = element_blank(),
                                   limits = c(min(df1$acf,df2$pacf),1)) +
                ggtitle("ACF ", title_line) +
                theme_bw()

            plot.pacf <- ggplot(data = df2, aes(x = lag, y = pacf)) +
                geom_area(aes(x = lag, y = qnorm((1+ci)/2)*pacfstd), fill = "#B9CFE7") +
                geom_area(aes(x = lag, y = -qnorm((1+ci)/2)*pacfstd), fill = "#B9CFE7") +
                geom_col(fill = "#4373B6", width = 0.7) +
                scale_x_continuous(breaks = seq(0,max(df2$lag, na.rm = TRUE),6)) +
                scale_y_continuous(name = element_blank(),
                                   limits = c(min(df1$acf,df2$pacf),1)) +
                ggtitle("PACF", title_line) +
                theme_bw()
        }
        else {
            plot.acf <- ggplot(data = df1, aes( x = lag, y = acf)) +
                geom_col(fill = "#4373B6", width = 0.7) +
                geom_hline(yintercept = qnorm((1+ci)/2)/sqrt(N),
                           colour = "sandybrown",
                           linetype = "dashed") +
                geom_hline(yintercept = - qnorm((1+ci)/2)/sqrt(N),
                           colour = "sandybrown",
                           linetype = "dashed") +
                scale_x_continuous(breaks = seq(0,max(df1$lag),6)) +
                scale_y_continuous(name = element_blank(),
                                   limits = c(min(df1$acf,df2$pacf),1)) +
                ggtitle("ACF", title_line) +
                theme_bw()

            plot.pacf <- ggplot(data = df2, aes(x = lag, y = pacf)) +
                geom_col(fill = "#4373B6", width = 0.7) +
                geom_hline(yintercept = qnorm((1+ci)/2)/sqrt(N),
                           colour = "sandybrown",
                           linetype = "dashed") +
                geom_hline(yintercept = - qnorm((1+ci)/2)/sqrt(N),
                           colour = "sandybrown",
                           linetype = "dashed") +
                scale_x_continuous(breaks = seq(0,max(df2$lag, na.rm = TRUE),6)) +
                scale_y_continuous(name = element_blank(),
                                   limits = c(min(df1$acf,df2$pacf),1)) +
                ggtitle("PACF", title_line) +
                theme_bw()
        }
        cowplot::plot_grid(plot.acf, plot.pacf, nrow = numofrow)
    }


    ########### End of acf function

    # #########################
    #                         #
    # Reactive functions      #
    #                         #
    ###########################

    # sumvals - sums values by selected

    sumVals <-  reactive({
        print("server 3 - start of reactive functions")
        query_in <- rssSelection(query_out_Date(), input$isource, input$iorientation,input$isourcetype, input$icountry,input$iregion, input$itextinput)
        sumVals_rtn <- f.sumVals(query_in)
        sumVals_rtn

    })

    sumVals2 <-  reactive({
        query_in <- rssSelection(query_out_Date(), input$isource2, input$iorientation2,input$isourcetype2, input$icountry2, input$iregion2, input$itextinput2)
        sumVals_rtn <- f.sumVals(query_in)
        sumVals_rtn
    })


    #totVals - sums values for total period for each SA factor
    totVals <- reactive({
        query_in <- rssSelection(query_out_Date(), input$isource, input$iorientation,input$isourcetype, input$icountry, input$iregion, input$itextinput)
        totVals_rtn <- f.totVals(query_in)
        totVals_rtn

    })

    totVals2 <- reactive({
        query_in <- rssSelection(query_out_Date(), input$isource2, input$iorientation2,input$isourcetype2, input$icountry2,input$iregion2, input$itextinput2)
        totVals_rtn <- f.totVals(query_in)
        totVals_rtn
    })



    ##############
    #
    #   Build full dataframe for dates
    #
    ##############
    query_out_Date <- reactive({
        queryDate <- as.Date(input$dateRange[1])
        print("server 4 - start of query_out_Date")
        queryDate <- format(as.Date(queryDate), "%Y_%m_%d")
        print(paste("queryDate (1) ", queryDate))

        outSeq <- seq(as.Date(input$dateRange[1]) , as.Date(input$dateRange[2]), by = "day")
        outSeq <- format(as.Date(outSeq, "%Y_%m_%d"))
        query_out_frame <- data.frame(ext_name = character(), item_title = character(), item_date_published = character(), orientation = character(),
                                      country = character() , region = character(),
                                      syuzhet_score = numeric(), afinn_score = numeric(), bing_score  = numeric(),
                                      nrc_score_anger = numeric(), nrc_score_anticipation = numeric(), nrc_score_disgust = numeric(), nrc_score_fear = numeric(),
                                      nrc_score_joy = numeric(), nrc_score_positive = numeric(), nrc_score_negative = numeric(),
                                      nrc_score_sadness = numeric(), nrc_score_surprise = numeric(), nrc_score_trust = numeric(),
                                      loughran_frame_constraining = numeric(), loughran_frame_litigious = numeric(), loughran_frame_negative = numeric(),
                                      loughran_frame_positive = numeric(), loughran_frame_uncertain = numeric(),
                                      hash_value = character())
        error_date <- data.frame(fail_date = character())
        ##############
        #
        #   Read database - default date if initial date unavailable
        #
        ##############
        print("Date DB read initiated")
        #### Table dates
        inserted_date_seq <- seq(as.Date(input$dateRange[1]) , as.Date(input$dateRange[2]), by = "day")

        for(i in seq_along(inserted_date_seq)){         # Start of read DB loop
            inserted_date <-  as.character( gsub("-", "_", inserted_date_seq[i]  ))


            queryScript <- paste0("SELECT ext_name, item_title,item_date_published, orientation, country,
            syuzhet_score, afinn_score, bing_score,
            nrc_score_anger, nrc_score_anticipation, nrc_score_disgust, nrc_score_fear,
            nrc_score_joy, nrc_score_positive, nrc_score_negative,
            nrc_score_sadness, nrc_score_surprise, nrc_score_trust,
            loughran_frame_constraining, loughran_frame_litigious,
            loughran_frame_negative, loughran_frame_positive, loughran_frame_uncertain,
            md5(concat(item_title, item_date_published)) AS hash_value
                             FROM sa_RSS_library", inserted_date, "
                            ;" )
            tryCatch(
                expr = {
                    try_date <- paste0("sa_RSS_library", inserted_date)
                    query1  <- dbGetQuery(conR, queryScript)
                    query1$item_date_published <- as.Date(query1$item_date_published, format = "%Y-%m-%d")
                    query_out_frame <- rbind(query_out_frame, query1)
                },
                error = function(e){
                    message(paste0("Error message on date: ", inserted_date, " "))
                    message(queryScript)
                    error_date <- rbind(error_date, inserted_date)
                },
                finally = {
                    message("tryCatch database read finished")
                }
            )  # end of tryCatch
        }

        error_date # LIst dates with missing tables
        query_out_frame <- query_out_frame[!duplicated(query_out_frame$hash_value),]   # Query response with no duplicates
        #query_out_full <- query_out_frame # Contains all values (country / orientation )
        #       query_out <- rssSelection(query_out, Source = input$isource, Orientation = input$iorientation,Country = input$icountry, Topic = input$iTextinput)
        print("SERVER - query_out_Date")

        # Normalize values for ensemble positive / negative

        query_out_frame$nrc_comp <- query_out_frame$nrc_score_positive - query_out_frame$nrc_score_negative
        query_out_frame$loughran_comp <- query_out_frame$loughran_frame_positive - query_out_frame$loughran_frame_negative
        afinn.norm <- max(abs(query_out_frame$afinn_score))
        syuzhet.norm <- max(abs(query_out_frame$syuzhet_score))
        bing.norm <- max(abs(query_out_frame$bing_score))
        nrc.norm <- max(abs(query_out_frame$nrc_comp))
        loughran.norm <- max(abs(query_out_frame$loughran_comp))
        query_out_frame$ensemble_posneg <- query_out_frame$afinn_score/afinn.norm + query_out_frame$bing_score/bing.norm + query_out_frame$syuzhet_score/syuzhet.norm +
            query_out_frame$nrc_comp/nrc.norm + query_out_frame$loughran_comp/loughran.norm

        query_out_frame <- cbind(query_out_frame, rssSources[match(query_out_frame$ext_name, rssSources$Feed), c(6,7)]) # Add region and source type
        query_out_frame # returned

        # end of read DB loop
    })   # End of query_out_Date Retrieves records between dates. This is the only database retrieval. Other selections are done from this




    # #########################
    #                         #
    # Output section          #
    #                         #
    ###########################

    output$Selections <- DT::renderDT({
        print("server 4 - generate output")
        v1 <- c(input$isource,input$isourcetype, input$icountry,input$iregion,  input$iorientation, input$itextinput )
        v2 <- c(input$isource2, input$isourcetype,input$icountry2, input$iregion2, input$iorientation2, input$itextinput2 )
        dataSelection <- rbind(v1, v2)
        query_out_List
    })
    ### COUTPUT LINE 1 omparison chart
    output$SA_by_date_line_comp <- renderPlotly({
        sumValsA <- filter(sumVals(), factorName %in% input$iSentimentFactor )
        sumValsA <-mutate(sumValsA, Selection = "1")
        sumValsB <- filter(sumVals2(), factorName %in% input$iSentimentFactor2 )
        sumValsB <-mutate(sumValsB, Selection = "2")
        sumVals <- rbind(sumValsA, sumValsB)
        if(isTRUE(input$iPosNegNorm)){
            sumVals$factorValue <- sumVals$factorValue * posneg(sumVals$factorValue)
        }

        p <- sumVals %>%
            mutate(mov_avg = rollmean(factorValue, input$dateGrouping, fill = 0)) %>%
            ggplot(aes(x = item_date_published, y = factorValue, group = Selection, fill = Selection, colour = Selection)) +
            xlab("Story date") + ylab("Factor score") +
            theme(legend.position = c(0,0)) +
            geom_smooth(method = input$ismooth, fullrange = TRUE,  show.legend = TRUE,se = input$iconfidence,
                        level = input$iconfidenceLevel, aes(colour = Selection)) +
            ggtitle(paste("Time series analysis", "No R/A")) +
            theme(legend.title = element_text(size = 8),
                  legend.position = c(0,0),
                  axis.title.x = element_text(size = 8),
                  axis.title.y = element_text(size = 8),
                  plot.title = element_text(size = 12)
            )

        if(isTRUE(input$aColumn)){(p <- p + geom_col(position = "dodge"))}
        if(isTRUE(input$aLine)){(p <- p + geom_line())}
        #        if(isTRUE(input$aDensity)){(p <- p + geom_density(aes(y = factorValue)))}
        if(isTRUE(input$aPoint)){(p <- p + geom_point(size = 4, shape = 22, colour = "darkblue", fill = "azure"))}
        p + theme(legend.position = c(0.1, 0.1))
        p
    })


    ########  Correlation plot
    output$SA_correlation <- renderPlotly({
        print("server 5 - SA_correlation")
        sumValsA <- filter(sumVals(), factorName %in% input$iSentimentFactor )
        sumValsA <- mutate(sumValsA, SelectionA = "Selection 1")
        sumValsA <- mutate(sumValsA, rank_SFactorA = rank(factorValue))
        sumValsB <- filter(sumVals2(), factorName %in% input$iSentimentFactor2 )
        sumValsB <- mutate(sumValsB, SelectionB = "Selection 2")
        sumValsB <- mutate(sumValsB, rank_SFactorB = rank(factorValue))
        sumValsX <- sumValsA %>%
            inner_join(sumValsB, by = c("item_date_published", "factorName"))
        sumValsX$index <- tibble::rowid_to_column(sumValsX, "index")




        p <- ggscatter(sumValsX, x = "factorValue.x", y = "factorValue.y",
                       cor.method = input$icorrelate,
                       conf.int = input$iconfidence,
                       add = switch(input$ismooth, "None" = "", "loess" = "loess", "lm" = "reg.line"),
                       add.params = list(color = "blue"),
                       star.plot = input$aStar,
                       #                       cor.coef = TRUE,
                       #                       cor.coeff.args = list(method = input$icorrelate, label.sep = "\n")
        ) +
            ggtitle(paste("Correlation")) +
            xlab("Selection 1") + ylab("Selection 2") +
            theme(legend.title = element_text(size = 8),
                  axis.title.x = element_text(size = 8),
                  axis.text.x = element_text(size = 8),
                  axis.title.y = element_text(size = 8),
                  axis.text.y = element_text(size = 8),
                  plot.title = element_text(size = 12)
            )

        if(isTRUE(input$aPoint)){(p <- p + geom_point(size = 4, shape = 22, colour = "darkgreen", fill = "darkseagreen"))}
        #       p + stat_cor( method = input$icorrelate, aes(label = ..r.label.., label.x = 3, label.y = 30),output.type = "expression", p.accuracy = 0.001, r.accuracy = 0.001)
        return(p)
    })  ######  End of correlation plot



    #######  Correlation statistics table

    output$corrStats <- DT::renderDataTable({
        v1 <- c(input$isource,input$isourcetype, input$icountry,input$iregion,  input$iorientation, input$itextinput )
        v2 <- c(input$isource2, input$isourcetype,input$icountry2, input$iregion2, input$iorientation2, input$itextinput2 )
        s <- c(input$icorrelate, input$icorr.alternative)

        sumValsA <- filter(sumVals(), factorName %in% input$iSentimentFactor )
        sumValsA <- mutate(sumValsA, SelectionA = "Selection 1")
        sumValsA <- mutate(sumValsA, rank_SFactorA = rank(factorValue))
        sumValsB <- filter(sumVals2(), factorName %in% input$iSentimentFactor2 )
        sumValsB <- mutate(sumValsB, SelectionB = "Selection 2")
        sumValsB <- mutate(sumValsB, rank_SFactorB = rank(factorValue))
        sumValsX <- sumValsA %>%
            inner_join(sumValsB, by = c("item_date_published", "factorName"))
        sumValsX$index <- tibble::rowid_to_column(sumValsX, "index")
        #        t <- cor(x = sumValsX$factorValue.x, y = sumValsX$factorValue.y, method = s)
        correlation_result <- cor.test(sumValsX$factorValue.x, sumValsX$factorValue.y,
                                       method = input$icorrelate, alternative = input$icorr.alternative)

        method <- correlation_result$method
        alternative <- correlation_result$alternative
        statistic <- correlation_result$statistic
        p.value <- correlation_result$p.value
        estimate <- correlation_result$estimate
        ifelse(input$icorrelate == "pearson", conf.int <- paste(correlation_result$conf.int[1:1],correlation_result$conf.int[1:2]), conf.int <- NA)


        cor.text <- as.data.frame(do.call(rbind,
                                          list( method, alternative,statistic,
                                                p.value, estimate, conf.int)))
        row.names(cor.text) <- c( "Method", "Alternative","Statistic", "p-value", "estimate", "conf int")
        if(input$icorrelate == "pearson"){cor.text <- select(cor.text, V2)}
        cor.text



    })

    ################### OUTPUT LINE 2-3 Single factor charts

    # First choice line chart
    output$SA_by_date_line <- renderPlotly({
        z <- c(input$aColumn, input$aLine, input$aPoint)
        sumVals <- filter(sumVals(), factorName %in% input$iSentimentFactor )
        gtitle <- paste("Time series analysis / \nMoving average 1 \nComparison", input$ismooth)
        p <- time_Series_graph(sumVals, gtitle, "red", "firebrick4", point_fill = "deeppink")
        p
    })
    # Second choice line chart
    output$SA_by_date_line2 <- renderPlotly({
        sumVals <- filter(sumVals2(), factorName %in% input$iSentimentFactor2 )
        ggtitle <- paste(c("Time series analysis Moving average ", input$ismooth))
        p <- time_Series_graph(sumVals, gtitle, "blue", "deepskyblue", point_fill = "dodgerblue4")
        p
    })


    #########################
    # totVals
    output$SA_summary_by_period <-renderPlotly({
        print("SA_s_b_P")
        x_tick_titles <- ("this")
        q <- ggplot(totVals(), aes(x = Factor, y = Value, fill = group1, colour = "red"))+
            theme(axis.text.x = element_text(angle = 90))+
            ggtitle("Period analysis / Sentiment") +
            scale_x_discrete(limits = c('PosNeg', 'ensemble_posneg', 'afinn_score', 'bing_score', 'syuzhet_score', 'loughran_frame_positive', 'loughran_frame_negative',
                                        'nrc_score_positive', 'nrc_score_negative','Sentiment spectrum',
                                        'nrc_score_anger', 'nrc_score_anticipation', 'nrc_score_disgust', 'nrc_score_fear',
                                        'nrc_score_joy',
                                        'nrc_score_sadness', 'nrc_score_surprise', 'nrc_score_trust', 'Financial',
                                        'loughran_frame_constraining', 'loughran_frame_litigious', 'loughran_frame_uncertain'
            )) +
            geom_bar(stat = "identity", colour = "black") +
            theme(legend.position = "none")

        q
    })

    # totVals
    output$SA_summary_by_period2 <-renderPlotly({
        print("server 5 - SA_summary_by_period")
        x_tick_titles <- ("this")
        q <- ggplot(totVals2(), aes(x = Factor, y = Value, fill = group1, colour = "blue"))+
            theme(axis.text.x = element_text(angle = 90))+
            ggtitle("Period analysis / Sentiment 2") +
            scale_x_discrete(limits = c('PosNeg', 'ensemble_posneg', 'afinn_score', 'bing_score', 'syuzhet_score', 'loughran_frame_positive', 'loughran_frame_negative',
                                        'nrc_score_positive', 'nrc_score_negative','nrc',
                                        'nrc_score_anger', 'nrc_score_anticipation', 'nrc_score_disgust', 'nrc_score_fear',
                                        'nrc_score_joy',
                                        'nrc_score_sadness', 'nrc_score_surprise', 'nrc_score_trust', 'loughran',
                                        'loughran_frame_constraining', 'loughran_frame_litigious', 'loughran_frame_uncertain'
            )) +
            geom_bar(stat = "identity", colour = "black") +
            theme(legend.position = "none")


    })

    ######### Autocorrelation

    output$ACF1_large <- renderPlot({
        v1 <- c(input$isource,input$isourcetype, input$icountry,input$iregion,  input$iorientation, input$itextinput )
        v2 <- c(input$isource2, input$isourcetype,input$icountry2, input$iregion2, input$iorientation2, input$itextinput2 )
        s <- c(input$icorrelate, input$icorr.alternative)

        sumVals <- filter(sumVals(), factorName %in% input$iSentimentFactor )
        print("Got here")
        p <- ggplot.corr(sumVals$factorValue, lag.max = 124, ci = 0.95, large.sample.size = TRUE, horizontal = TRUE, title_line = "Selection 1") +
            ggtitle("Autocorrelation / Selection 1")

        p
    })

    output$ACF2_large <- renderPlot({

        sumVals <- filter(sumVals2(), factorName %in% input$iSentimentFactor2 )
        print("Got here2")
        p <- ggplot.corr(sumVals$factorValue, lag.max = 124, ci = 0.95, large.sample.size = TRUE, horizontal = TRUE, title_line = "Selection 2") +
            ggtitle("Autocorrelation / Selection 2")

        p
    })

    ##########################  Principal componenet analysis

    output$PCA <- renderPlot({
        query_in <- rssSelection(query_out_Date(), input$isource, input$iorientation,input$isourcetype, input$icountry,input$iregion, input$itextinput)
        cluster_frame <- unique(query_in[,c('ext_name', 'orientation', 'country')])
        print("Here PCA")
        cluster_agg <- aggregate(query_in[,c(7:23)], by = list(ext_name=query_in$ext_name), sum)
        cluster_merge <- merge(cluster_agg, cluster_frame)
        cluster_merge_x <- cluster_merge[,2:18]
        cluster_clean <- cluster_merge_x[,apply(cluster_merge_x, 2, var, na.rm = TRUE) !=0]
        model <- prcomp(cluster_clean)
        p <- ggbiplot(model)
        p

    })

    ##########################

    output$tbl <- DT::renderDT({
        print("Got to the end")
        stories1 <- rssSelection(query_out_Date(), input$isource,input$isourcetype, input$orientation, input$icountry, input$iregion, input$itextinput)
        stories2 <- rssSelection(query_out_Date(), input$isource2,input$isourcetype2, input$orientation2, input$icountry2, input$iregion2, input$itextinput2)
        stories <- rbind(stories1, stories2)
        stories

    })

})


#######################################



