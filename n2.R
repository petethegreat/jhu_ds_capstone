#!/usr/bin/Rscript
# library(RWeka)


# library(tm)
# library(slam)

library(tm)
library(tau)
library(dplyr)

GetWordCounts<-function(source=c('blogs','news','twitter'),split=20,testfrags=1,tokenize)
{

    # source is either blogs, news or twitter
    # load the source as textdata
    # split into split parts (cut(seq_along(textdata)),split=split)
    # create a corpus for each file
    infile<-sprintf('./data/final/en_US/en_US.%s.txt',source)
    outfile<-sprintf('./data/n1_wordcount_%s.txt',source)
    testfile<-sprintf('./data/test_%s.txt',source)

    # load lines
    textData<-readLines(infile,n=1000)
    #sample to split into train and test

    mycorpus<-VCorpus(VectorSource(textData))
        # clean it
    mycorpus<-tm_map(mycorpus,content_transformer(tolower))
    mycorpus<-tm_map(mycorpus,removeNumbers)
    #mycorpus<-tm_map(mycorpus,removeWords, stopwords("english"))
    mycorpus<-tm_map(mycorpus,removePunctuation)
    mycorpus<-tm_map(mycorpus,stripWhitespace)

    cleaned<-lapply(mycorpus,as.character)

    totalcounts<-NULL


    #do lines one at a time
    for (lineno in 1:1000)
    {
        cat('processing line ',lineno,'\n')
        counts<-textcnt(cleaned[[lineno]],method='string',n=1)

        if( is.null(totalcounts))
        {
            totalcounts<-data.frame(counts=unclass(counts),words=names(unclass(counts)),stringsAsFactors=FALSE)
        }
        else
        {
            tempcounts<-data.frame(counts=unclass(counts),words=names(unclass(counts)),stringsAsFactors=FALSE)
            present<- tempcounts$words %in% totalcounts$words
            totalcounts['words' == tempcounts$words[present],'counts']<- 
                totalcounts['words' == tempcounts$words[present],'counts'] + tempcounts[present,'counts']
            if (sum(present) < length(present))
            {
                totalcounts<-bind_rows(totalcounts,tempcounts[!present,])
            }


        }


    }

    counts<-textcnt(cleaned,method='string',n=1)

    # df<-data.frame(counts=unclass(counts))
    write.csv(totalcounts,file='./test.csv')


}

GetWordCounts('blogs')
