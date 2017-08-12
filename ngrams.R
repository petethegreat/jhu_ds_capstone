#!/usr/bin/Rscript
library(RWeka)


library(tm)
library(slam)

getTokeniser<-function(n)
{
    toke<-function(x)
    {
        NGramTokenizer(x, Weka_control(min = n, max = n))
    }
    return(toke)
}



makeNgrams<-function(corpus,Nval=1,sparse=0.0)
{
    # define tokeniser
    ngtoke<-function(x)
    {
        NGramTokenizer(x, Weka_control(min = Nval, max = Nval))
    }

    tdm_ngram<-TermDocumentMatrix(corpus,control=list(tokenize=ngtoke))
    if (sparse >0.0)
    {
            tdm_ngram<-removeSparseTerms(tdm_ngram,sparse)
    }
    return(tdm_ngram)



}

loadOneSource<- function(filename,frac=0.20)
{
    raw<-readLines(filename)
    if (frac < 1.0)
    {
        raw<-sample(raw,floor(frac*length(raw)))
    }

    mycorpus<-VCorpus(VectorSource(raw))

    # clean it
    mycorpus<-tm_map(mycorpus,content_transformer(tolower))
    mycorpus<-tm_map(mycorpus,removeNumbers)
    #mycorpus<-tm_map(mycorpus,removeWords, stopwords("english"))
    mycorpus<-tm_map(mycorpus,removePunctuation)
    mycorpus<-tm_map(mycorpus,stripWhitespace)


    return(mycorpus)
}
loadCorpus<- function(frac=0.20)
{
    blogs<-readLines('./data/final/en_US/en_US.blogs.txt')
    news<-readLines('./data/final/en_US/en_US.news.txt')
    tweets<-readLines('./data/final/en_US/en_US.twitter.txt')

    
    # frac<-0.01
    textData<-c(
        sample(blogs,floor(frac*length(blogs))),
        sample(news,floor(frac*length(news))),
        sample(tweets,floor(frac*length(tweets)))
        )


    # rm(blogs,news,tweets)
    mycorpus<-VCorpus(VectorSource(textData))

    # clean it
    mycorpus<-tm_map(mycorpus,content_transformer(tolower))
    mycorpus<-tm_map(mycorpus,removeNumbers)
    #mycorpus<-tm_map(mycorpus,removeWords, stopwords("english"))
    mycorpus<-tm_map(mycorpus,removePunctuation)
    mycorpus<-tm_map(mycorpus,stripWhitespace)


    return(mycorpus)

}

loadPastedCorpus<- function(frac=0.20)
{
    blogs<-readLines('./data/final/en_US/en_US.blogs.txt')
    news<-readLines('./data/final/en_US/en_US.news.txt')
    tweets<-readLines('./data/final/en_US/en_US.twitter.txt')

    textData<- "moose"
    # frac<-0.01

    # paste the (sampled?) text files together. This way we have only 3 documents (blogs, news, tweets)
    # This means our tdm has only 3 columns, rather than thousands, so greatly reduces memory consumption
    if (frac < 1.0)
    {
        textData<-c(
        paste(sample(blogs,floor(frac*length(blogs))),collapse=' '),
        paste(sample(news,floor(frac*length(news))),collapse=' '),
        paste(sample(tweets,floor(frac*length(tweets))),collapse=' ')
        )
    }
    else
    {
    textData<-c(
        paste(blogs,collapse=' '),
        paste(news,collapse=' '),
        paste(tweets,collapse=' ')
        )
    }
    


    # rm(blogs,news,tweets)
    mycorpus<-VCorpus(VectorSource(textData))

    # clean it
    mycorpus<-tm_map(mycorpus,content_transformer(tolower))
    mycorpus<-tm_map(mycorpus,removeNumbers)
    #mycorpus<-tm_map(mycorpus,removeWords, stopwords("english"))
    mycorpus<-tm_map(mycorpus,removePunctuation)
    mycorpus<-tm_map(mycorpus,stripWhitespace)


    return(mycorpus)

}


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
    textData<-readLines(infile,n=40)

    # segment data
    moose<-cut(runif(length(textData)),split,labels=FALSE)

    # write test data
    writeLines(textData[moose %in% 1:testfrags],testfile)
    cat(sprintf('wrote test data to %s\n',testfile))

    wordcount=NULL


    # loop over other segments
    for(i in 1+testfrags:5)#split)
    {

        mycorpus<-VCorpus(VectorSource(textData[moose == i]))
        # clean it
        mycorpus<-tm_map(mycorpus,content_transformer(tolower))
        mycorpus<-tm_map(mycorpus,removeNumbers)
        #mycorpus<-tm_map(mycorpus,removeWords, stopwords("english"))
        mycorpus<-tm_map(mycorpus,removePunctuation)
        mycorpus<-tm_map(mycorpus,stripWhitespace)
        # remove non english characters?

        # tokenise
        tdm_ngram<-TermDocumentMatrix(mycorpus,control=list(tokenize=tokenize))
        wordsums<-slam::row_sums(tdm_ngram,na.rm=TRUE)
        temp<-data.frame(newcounts=wordsums)
        rm(mycorpus)



        if (is.null(wordcount))
        {
                wordcount<-temp
                colnames(wordcount)<-'totalcount'
        }
        else
        {
            wordcount<-merge(wordcount,temp,all=TRUE,by.x=)
            wordcount$totalcount<-rowSums(wordcount[,c('totalcount','newcounts')],na.rm=TRUE)
            wordcount$newcounts<-NULL
        }
        for(j in ls())
        {
            object.size(j)
        }
        cat(sprintf('merged %i of %i\n',i,split))

    }
    cat(sprintf('writing data to %s\n',outfile))
    write.csv(wordcount,file=outfile)










    # for(frag in 1:20)
    # {
        
    # }





}



oldstuff<- function()
{



    # Do Stuff

    # load the corpus

    set.seed(97)

    sample=0.5
    # corpus_timeinfo<- system.time(corpus<-loadPastedCorpus(sample))

    blogfilename<-'./data/final/en_US/en_US.blogs.txt'
    newsfilename<-'./data/final/en_US/en_US.news.txt'
    tweetfilename<-'./data/final/en_US/en_US.twitter.txt'

    corpus_timeinfo<- system.time(corpus<-loadOneSource(blogfilename,sample))
    print('corpus generation:')
    print(corpus_timeinfo)
    # inspect(corpus)


    n1gram_timeinfo<-system.time(n1_tdm<-makeNgrams(corpus,Nval=1,sparse=0.0))
    print('1 gram tdm generation:')
    print(n1gram_timeinfo)


    # n1counts<-rowSums(as.matrix(n1_tdm))
    n1counts<-slam::row_sums(n1_tdm,na.rm=TRUE)
    n1df<-data.frame(n1counts)
    n1df$relfreq<-n1df$n1counts/sum(n1df$n1counts)
    n1df<-n1df[order(n1df$relfreq,decreasing=TRUE),]
    n1df$coverage<-cumsum(n1df$relfreq)

    # write.csv(n1df,file='./data/n1_wordstats.csv')
    write.csv(n1df,file='./data/n1_wordstats_blog.csv')


    library(ggplot2)
    g<-ggplot(data=n1df,aes(x=seq_along(coverage),y=coverage)) + geom_line(color='red')+
        labs(x='number of words',y='coverage',title='corpus coverage vs vocab size') +
        

    pdf('./coverage_1a.pdf')
    print(g)
    dev.off()
    h = g + ylim(low=0.95,high=1.0)


    pdf('./coverage_95.pdf')
    print(h)
    dev.off()

}

set.seed(97)
# token1gram<-getTokeniser(1)
# GetWordCounts('blogs',20,1,token1gram)

oldstuff()













