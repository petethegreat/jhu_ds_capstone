library(shiny)
# library(leaflet)

library(reshape2)

library(data.table)
library(dtplyr)
library(DT)



`%+na%` <- function(x,y) {ifelse( is.na(x), y, ifelse( is.na(y), x, x+y) )}
# sum data table columns, ignoring NA
# https://stackoverflow.com/questions/13106645/using-in-data-table-to-sum-the-values-of-two-columns-in-r-ignoring-nas

# function to load ngram data

loaddf<-function(i)
{
    fname<-sprintf('./data/cleaned_counts_n%i.csv',i)
    cat('loading ',fname,'\n')
    return(data.table(read.csv(fname,stringsAsFactors=FALSE)))

}



# server function
shinyServer(
  function(input,output,session)
  {
    # this isn't working, maybe click a load data, or select nmax from a list
    # add a progress bar
    output$predictedText<-renderPrint('loading stuff, please wait')
    output$statusText<-renderPrint('loading data, please wait')

    #################################################
    ################ Global Variables ###############
    #################################################
    dflist<-NULL
    Dval<-0.4 # discount
    nmax<-5 # use ngrams of up to 5th order

    ####################################################
    ############### Function Definition ################
    ####################################################

    ## get predictions from cleaned input text
    predictCleaned<-function(instr)
    {
        # trim history, only need last (n-1) words
        if (length(instr) > (nmax-1))
        {
            instr<-instr[-seq_len(length(instr) -(nmax-1))]
        }
        cat(instr,'\n')


        thisnmax<-min(length(instr),nmax-1)+1
        # if the string contains 2 words, then we'll want to use 3 grams and lower. 
        foundcontext<-FALSE
        # have we found a match for the supplied history?
        highcounts<-NULL
        while(!foundcontext && thisnmax >1)
        {

            thestr<-instr[-seq_len(length(instr)-thisnmax +1) ]
            # cat(thestr,'\n')
            cat('trying nmax = ',thisnmax,'\n')
            # want to look up the nth word using the first n-1 words
            # if theres no match for the first n-1 words, then back off to n-1 grams and try looking up the first n-2 words
            # if our 5 grams are useless, then treat our 4 grams as highest order
            collist<-paste('w',1:thisnmax, sep='')
            highest<-dflist[[thisnmax]]
            theexpr<-paste(c('setkey(highest,',
                    paste(collist[1:thisnmax-1],collapse=','),
                    ')'),collapse='')
            eval(parse(text=theexpr))

            #highest is now sorted appropriately, see if there are any context matches
            temp<-paste(collist[1:thisnmax-1],' == thestr[',1:(thisnmax-1),']',sep='')
            temp<-paste(temp,collapse=' & ')
            resultexpr<-paste(c('highcounts<-highest[',temp,',',paste('.(word=w',thisnmax,collapse='',sep=''),',count=counts.total)]'),collapse='')
            # cat(resultexpr,'\n')
            eval(parse(text=resultexpr))


            if(nrow(highcounts) >0)
            {
                # cat('nrow = ',nrow(highcounts[order(-count)]),'\n')
                foundcontext<-TRUE
                # cat(head(highcounts[order(-counts.total)]),'\n')
            }

            # foundcontext<-isTRUE(nrow(highcounts) > 0 ) 
            # if there were no results, decetrnment thisnmax
            if (!foundcontext) {thisnmax<-thisnmax -1}
            # decrement thisnmax

        }


        # if we didn't find a context, then we don't recognize the immediatly preceding words from bigrams (or higher)
        # could either use the most frequent word, or the most frequent 
        if (!foundcontext)
        {
            # will go based on continuations
            bigrams<-dflist[[2]]
            wordprobs<-bigrams[,.(w2,count=.N),by=w2]
            wordprobs<-wordprobs[,prob:=count/nrow(bigrams)]
            wordprobs<-wordprobs[order(-prob),.(word=w2,prob)]
            return(wordprobs[1:10])

        }


        # cat(head(highcounts),'\n')
        # print(head(highcounts))
        wordprobs<-highcounts[,.(word,prob = (count-Dval)/sum(count))]
        wordprobs[prob <0,prob:=0]
        # print(head(wordprobs))
        lval<-1.0 - sum(wordprobs$prob) # probability mass left for lower orders
        lcheck<-Dval*nrow(highcounts)/sum(highcounts$count) # probability mass left for lower orders
        cat('checking stuff, lval = ',lval,', lcheck = ',lcheck,'\n')

        # use continuations for this order and lower orders
        for( j in (thisnmax):2)
        {
            # get the continuationcounts for this order
            thedata<-dflist[[j]]
            
            # sort by last word. if we're doing trigrams, we want to get a word's completion count for unique bigrams
            thecol<-paste('w',j, sep='',collapse='')
            theexpr<-paste(c('setkey(thedata,',thecol,')',collapse='',sep=''))
            eval(parse(text=theexpr))

            # get continuation counts. group things by wj, count is the number of entries in the group
            # i.e., count is the continuation count, the number of unique ngrams that are completed by the word wj
            theexpr<-paste(c('contcounts<-thedata[,.(word=',thecol,',count=.N),by=',thecol,']'),sep='',collapse='')
            eval(parse(text=theexpr))
            # apply discounts 
            contcounts<-contcounts[,.(word,count=count-Dval)]
            contcounts<-contcounts[count < 0,count:=0]
            totalcounts<-nrow(thedata) # number of unique n-grams before discounts applied

            # convert counts to prob, multiply by lambda
            contcounts[,prob:=(lval*count)/totalcounts]
            
            # merge these probabilities with those from higher orders
            wordprobs<-merge(wordprobs,contcounts[,.(word,prob)],by.x='word',by.y='word',all=TRUE,suffixes=c('.total','.new'))
            wordprobs<-wordprobs[,.(word,prob=prob.total %+na% prob.new)]

            # update lambda
            # cat('check2: continuations = ',sum(contcounts$count),', unique ngrams = ',nrow(thedata),', lambda = ',Dval*nrow(thedata)/sum(thedata$counts.total),'\n')
            lval<-lval*(1.0 - (sum(contcounts$count)/totalcounts))
            #check
            lcheck<- 1.0-sum(wordprobs[prob >= 0.0,.(prob)])
            # cat('j = ',j,', lambda = ',lval,', lcheck = ',lcheck,'\n')

        }
        # print(head(wordprobs[order(-prob)]))

        # do lowest order/unigrams
        # each word receives equal weighting, lamda/V (V = vocab size, or nrows in )
        unicounts<-dflist[[1]]
        unicounts<-unicounts[w1 !='UNKNOWN']
        unicounts<-unicounts[,.(word=w1,prob=lval/nrow(unicounts))]
        wordprobs<-merge(wordprobs,unicounts,by.x='word',by.y='word',all=TRUE,suffixes=c('.total','.new'))
        wordprobs<-wordprobs[,.(word,prob=prob.total %+na% prob.new)]
        wordprobs<-wordprobs[order(-prob)]



        return(wordprobs[1:10])

    }

    ################################################################

    # prepare the input string, split into words
    predict<-function(inputstr)
    {
        # prepare input
        thestring<-iconv(inputstr, "latin1", "ASCII", sub="BADCHAR")
        thestring<-gsub('[^ ]*(BADCHAR)+[^ ]','',thestring)
        thestring<-tolower(thestring)
        thestring<-gsub('\\s+',' ',thestring)
        thestring<-gsub('^ ','',thestring)
        thestring<-gsub(' $','',thestring)
        thestring<-gsub('[^a-zA-Z ]','',thestring)

        words<-strsplit(thestring,' ')

        results<-predictCleaned(words[[1]])
        # strsplit returns a list of vectors. In our case, should only be one
        return(results)
    }
    ################################################################
    ################################################################
    # end of function definition, this is server behaviour
    

    
    if (is.null(dflist))
    {
      output$predictedText<-renderPrint('loading data - please wait')
      cat('loading data\n')
      dflist<-lapply(1:nmax,loaddf)

      output$statusText<-renderPrint('data loaded, generating prediction')
    }

    # if input text changes, generate new predictions
    observeEvent(input$inputText,
      {
        # filter data based on slider
        predictions<-predict(input$inputText)

        output$predictedDT<-renderDataTable({predictions})

        output$predictedText<-renderPrint(paste('prediction: ',predictions[1,.(word)],collapse='',sep=''))



        # go through the categories. Show/Hide groups based on input
        
      }
    ) # end observeEvent

    
  } # end ShinyServer block
) # end ShinyServer
