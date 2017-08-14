#!/usr/bin/Rscript
# library(RWeka)


# library(tm)
# library(slam)

library(tm)
library(tau)
library(dplyr)



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

GetWordCounts<-function(source=c('blogs','news','twitter'),n=1,rmstop=FALSE,temps=20,usetemps=20)
{

    # source is either blogs, news or twitter
    # load the source as textdata
    # split into split parts (cut(seq_along(textdata)),split=split)
    # create a corpus for each file
    stopstr<-''
    if (rmstop)
    {
            stopstr<-'_rmstop'
    }
    infile<-sprintf('./data/train_%s.txt',source)
    outfile<-sprintf('./data/n%i_wordcount%s_%s.txt',n,stopstr,source)

    # load lines

    textData<-readLines(infile)

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
    rm(textData)
    cat('cleaned corpus\n')


    cuts<-cut(runif(length(cleaned)),breaks=temps,labels=FALSE)

    mem<-sapply(mget(ls()),object.size)
    cat(mem,'\n')
    for(i in 1:usetemps)
    {
        cat('counting ',i,' of ',dotemps,'\n')
        counts<-textcnt(cleaned[cuts==i],method='string',n=nval)
        meh<-unclass(counts)
        words<-names(counts)
        df<-data.frame(term=words,counts=meh)
        write.csv(df,file=sprintf('./temp/temp_%i.csv',i),row.names=FALSE)
    }

    rm(cleaned)

    df<-read.csv(sprintf('./temp/temp_%i.csv',i),stringsAsFactors=FALSE)

    for(i in 2:usetemps)
    {
        cat('merging ',i,' of ',usetemps,'\n')
        df2<-read.csv(sprintf('./temp/temp_%i.csv',i),stringsAsFactors=FALSE)
        df<-merge(df,df2,by.x='term',by.y='term',all=TRUE,suffixes=c('','.new'))
        df$counts<-rowSums(df[,c('counts','counts.new')])
        df<-subset(df,select=c('term','counts'))
        rm(df2)
    }
    cat('merging complete, sorting\n')

    df<-df[order(df$counts,decreasing=TRUE),]
    write.csv(df,file=outfile,row.names=FALSE)
    cat('wrote to ',outfile)

}

# 100k works for blogs.
# 100k - about 45 secs
# 200k - about 90 secs


nval<-1
nolines<-200000
GetWordCounts(source='twitter',n=1,temps=40,usetemps=2)

# trainTestValSplit()
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

OldGetWordCounts<-function(source=c('blogs','news','twitter'),n=1,lines=1000)
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
