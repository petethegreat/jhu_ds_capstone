#!/usr/bin/Rscript
# library(RWeka)


# library(tm)
# library(slam)

library(tm)
library(tau)
library(data.table)
library(dtplyr)



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

PrepSource<-function(source=c('blogs','news','twitter'),temps=20,seedval=37)
{
    set.seed(seedval)
    # break the input up into a bunch of small files
    infile<-sprintf('./data/train_%s.txt',source)
    textData<-readLines(infile)
    cuts<-cut(runif(length(textData)),breaks=temps,labels=FALSE)
    print(head(cuts))

    for(i in 1:temps)
    {
        outfilename<-sprintf('./temp/temp_input_%s_%i.txt',source,i)
        writeLines(textData[cuts==i],outfilename)
    }


}
GetWordCounts<-function(source=c('blogs','news','twitter'),n=1,rmstop=FALSE,temps=20)
{

    # source is either blogs, news or twitter
    # load the source as textdata
    # split into split parts (cut(seq_along(textdata)),split=split)
    # create a corpus for each file

    # loop over temp files
    stopstr<-''
    if (rmstop)
    {
            stopstr<-'_rmstop'
    }

    for( i in 1:temps)
    {
        infile<-sprintf('./temp/temp_input_%s_%i.txt',source,i)
        cat('processing file ',i,': ',infile,'\n')

        # load lines
        textData<-readLines(infile)
        # this is taking too long
        textData<-iconv(textData, "latin1", "ASCII", sub="BADCHAR")
        textData<-gsub('[^ ]*(BADCHAR)+[^ ]','',textData)

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
        mem<-sapply(mget(ls()),object.size)
        print(mem)


        counts<-textcnt(cleaned,method='string',n=nval)#,persistent=TRUE,recursive=TRUE)
        # meh<-unclass(counts)
        # words<-names(counts)
        df<-data.frame(counts=unclass(counts),term=names(counts))
        # df$term<-row.names(df)
        # df<-data.frame(term=words,counts=meh)
        write.csv(df,file=sprintf('./temp/temp_%s_n%s_%i.csv',source,n,i),row.names=FALSE)

    }
    
    
    




    # for(i in 1:usetemps)
    # {
    #     cat('counting ',i,' of ',dotemps,'\n')
    #     counts<-textcnt(cleaned,method='string',n=nval,persistent=TRUE,recursive=FALSE)
    #     # meh<-unclass(counts)
    #     # words<-names(counts)
    #     df<-data.frame(counts=unclass(counts),term=names(counts))
    #     # df$term<-row.names(df)
    #     # df<-data.frame(term=words,counts=meh)
    #     write.csv(df,file=sprintf('./temp/temp_%i.csv',i),row.names=FALSE)
    # }

    # rm(cleaned)

    # df<-read.csv(sprintf('./temp/temp_%i.csv',i),stringsAsFactors=FALSE)

    # for(i in 2:usetemps)
    # {
    #     cat('merging ',i,' of ',usetemps,'\n')
    #     df2<-read.csv(sprintf('./temp/temp_%i.csv',i),stringsAsFactors=FALSE)
    #     df<-merge(df,df2,by.x='term',by.y='term',all=TRUE,suffixes=c('','.new'))
    #     df$counts<-rowSums(df[,c('counts','counts.new')])
    #     df<-subset(df,select=c('term','counts'))
    #     rm(df2)
    # }
    # cat('merging complete, sorting\n')

    # df<-df[order(df$counts,decreasing=TRUE),]
    # write.csv(df,file=outfile,row.names=FALSE)
    # cat('wrote to ',outfile)

}

# 100k works for blogs.
# 100k - about 45 secs
# 200k - about 90 secs
MergeTempCounts<-function(source=c('blogs','news','twitter'),n=1,rmstop=FALSE,temps=20)
{
    stopstr<-''
    if (rmstop)
    {
            stopstr<-'_rmstop'
    }
    outfilename<-sprintf('./data/n%i_wordcount%s_%s.csv',n,stopstr,source)

    df<-read.csv(file=sprintf('./temp/temp_%s_n%i_%i.csv',source,n,1),stringsAsFactors=FALSE)

    for(i in 2:temps)
    {
        cat('merging ',i,' of ',temps,'\n')
        df2<-read.csv(file=sprintf('./temp/temp_%s_n%i_%i.csv',source,n,i),stringsAsFactors=FALSE)
        df<-merge(df,df2,by.x='term',by.y='term',all=TRUE,suffixes=c('','.new'))
        rm(df2)
        df$counts<-rowSums(df[,c('counts','counts.new')],na.rm=TRUE)
        df<-subset(df,select=c('term','counts'))
        write.csv(df,sprintf('./temp_merge_%s_n%i_i%i.csv',source,n,i),row.names=FALSE)
        
    }
    df<-df[order(df$counts,decreasing=TRUE),]
    write.csv(df,outfilename,row.names=FALSE)

}

MergeTempCountsTable<-function(source=c('blogs','news','twitter'),n=1,rmstop=FALSE,temps=20)
{
    stopstr<-''
    if (rmstop)
    {
            stopstr<-'_rmstop'
    }
    outfilename<-sprintf('./data/n%i_wordcount%s_%s.csv',n,stopstr,source)

    df<-data.table(read.csv(file=sprintf('./temp/temp_%s_n%i_%i.csv',source,n,1),stringsAsFactors=FALSE),key='term')

    for(i in 2:temps)
    {
        cat('merging ',i,' of ',temps,'\n')
        df2<-data.table(read.csv(file=sprintf('./temp/temp_%s_n%i_%i.csv',source,n,i),stringsAsFactors=FALSE),key='term')
        df<-merge(df,df2,by.x='term',by.y='term',all=TRUE,suffixes=c('','.new'),sort=TRUE)
        rm(df2)
        df$counts<-rowSums(df[,.(counts,counts.new)],na.rm=TRUE)
        df<-subset(df,select=c('term','counts'))
        write.csv(df,sprintf('./temp/temp_merge_%s_n%i_i%i.csv',source,n,i),row.names=FALSE)
        oldfn<-sprintf('./temp/temp_merge_%s_n%i_i%i.csv',source,n,i-1)
        if (file.exists(oldfn)) file.remove(oldfn)

    }
    df<-df[order(df$counts,decreasing=TRUE),]
    write.csv(df,outfilename,row.names=FALSE)
    for( i in 1:temps)
    {
        oldfn<-sprintf('./temp/temp_%s_n%i_%i.csv',source,n,1)
        if (file.exists(oldfn)) file.remove(oldfn)

    }

}

nval<-1
# nolines<-200000

# for( s in c('twitter','news'))
# {
# PrepSource(s,temps=40)
# GetWordCounts(source=s,n=nval,temps=40)
# MergeTempCounts(source=s,n=nval,temps=40)
# }
thetemps<-30
# only use 3/4 for the 3 and 4 grams

# PrepSource('news',temps=thetemps)
# PrepSource('blogs',temps=thetemps)
# PrepSource('twitter',temps=thetemps)
# nval<-2
# for( s in c('twitter','news'))
#     {
    
#     GetWordCounts(source=s,n=nval,temps=thetemps)
#     MergeTempCounts(source=s,n=nval,temps=thetemps)
#     }

# MergeTempCounts(source='news',n=3,temps=thetemps)
# MergeTempCountsTable(source='blogs',n=5,temps=thetemps)
for (nval in 3:1)
{
    for( s in c('blogs','twitter','news'))
    {
    
    GetWordCounts(source=s,n=nval,temps=thetemps)
    MergeTempCountsTable(source=s,n=nval,temps=thetemps)
    }
}
