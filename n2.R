#!/usr/bin/Rscript
# library(RWeka)


# library(tm)
# library(slam)

library(tm)
library(tau)
library(dplyr)

OldGetWordCounts<-function(source=c('blogs','news','twitter'),split=20,testfrags=1,tokenize)
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

trainTestValSplit<-function(trainfrac=0.8,testfrac=0.1,seedval=37)
{
    set.seed(seedval)
    validfrac<-(1.0 - trainfrac - testfrac)
    # cat('validfrac = ',validfrac,'\n')
    for (source in c('blogs','news','twitter'))
    {
        infile<-sprintf('./data/final/en_US/en_US.%s.txt',source)
        trainfile<-sprintf('./data/train_%s.txt',source)
        testfile<-sprintf('./data/test_%s.txt',source)
        valfile<-sprintf('./data/validate_%s.txt',source)

        data<-readLines(infile,skipNul=TRUE)
        cat('data ',length(data),' lines\n')
        splitcode<-0
        if (validfrac > 0.0)
        {
            splitcode<-cut(runif(length(data)),breaks=c(0.0,trainfrac,trainfrac+testfrac,1.01),labels=FALSE)

        }
        else
        {
            splitcode<-cut(runif(length(data)),breaks=c(0.0,trainfrac,1.01),labels=FALSE)
        }

        writeLines(data[splitcode==1],trainfile)
        writeLines(data[splitcode==2],testfile)
        if (validfrac >0.0)
        {
            writeLines(data[splitcode==3],valfile)
        }

    }


}

GetWordCounts<-function(source=c('blogs','news','twitter'),n=1,lines=1000)
{

    # source is either blogs, news or twitter
    # load the source as textdata
    # split into split parts (cut(seq_along(textdata)),split=split)
    # create a corpus for each file
    infile<-sprintf('./data/final/en_US/en_US.%s.txt',source)
    outfile<-sprintf('./data/n%i_wordcount_%s.txt',n,source)
    testfile<-sprintf('./data/test_%s.txt',source)

    # load lines

    textData<-readLines(infile,n=lines)

    textData<-iconv(textData, "latin1", "ASCII", sub="BADCHAR")
    textData<-gsub('[^ ]*(BADCHAR)+[^ ]','',textData)


    #sample to split into train and test

    mycorpus<-VCorpus(VectorSource(textData))
        # clean it
    mycorpus<-tm_map(mycorpus,content_transformer(tolower))
    mycorpus<-tm_map(mycorpus,removeNumbers)
    #mycorpus<-tm_map(mycorpus,removeWords, stopwords("english"))
    mycorpus<-tm_map(mycorpus,removePunctuation)
    mycorpus<-tm_map(mycorpus,stripWhitespace)

    cleaned<-lapply(mycorpus,as.character)
    rm(mycorpus)

    counts<-textcnt(cleaned,method='string',n=nval)


    meh<-unclass(counts)
    words<-names(counts)

    df<-data.frame(term=words,counts=meh)

    df<-df[order(df$counts,decreasing=TRUE),]
    write.csv(df,file=outfile,row.names=FALSE)
    cat('wrote to ',outfile)

}

# 100k works for blogs.
# 100k - about 45 secs
# 200k - about 90 secs


nval<-3
nolines<-200000
trainTestValSplit()
# 150s to do 2-grams at 200k lines

# cat('doing blogs\n')
# time<-system.time(GetWordCounts('blogs',n=nval,lines=nolines))
# cat('time: ',time,'\n')

# cat('doing news\n')
# time<-system.time(GetWordCounts('news',n=nval,lines=nolines))
# cat('time: ',time,'\n')

# cat('doing twitter\n')
# time<-system.time(GetWordCounts('twitter',n=nval,lines=nolines))
# cat('time: ',time,'\n')

# nval<-4
# cat('doing blogs\n')
# time<-system.time(GetWordCounts('blogs',n=nval,lines=nolines))
# cat('time: ',time,'\n')

# cat('doing news\n')
# time<-system.time(GetWordCounts('news',n=nval,lines=nolines))
# cat('time: ',time,'\n')

# cat('doing twitter\n')
# time<-system.time(GetWordCounts('twitter',n=nval,lines=nolines))
# cat('time: ',time,'\n')


