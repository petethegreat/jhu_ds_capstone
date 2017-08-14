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


GetWordCounts<-function(source=c('blogs','news','twitter'),tokenizer,prefix='n1',testfrac=0.1,sample=0.1,rmsparse=0.9999)
{

    # source is either blogs, news or twitter
    # load the source as textdata
    # split into split parts (cut(seq_along(textdata)),split=split)
    # create a corpus for each file
    infile<-sprintf('./data/final/en_US/en_US.%s.txt',source)
    outfile<-sprintf('./data/%s_wordcount_%s.txt',prefix,source)
    testfile<-sprintf('./data/test_%s.txt',source)

    # load lines
    textData<-readLines(infile)
    cat('read input file',infile,'\n')


    # segment data
    moose<-runif(length(textData))
    if (testfrac > 0.0)
    {
        # moose<-cut(runif(length(textData)),c(-0.1,sample,testfrac+sample),labels=FALSE)
        
        # write test data 

        writeLines(textData[(moose >sample )&& (moose < sample+testfrac)],testfile)
        cat(sprintf('wrote test data to %s\n',testfile))
    }

    # write test data
    sampleLines<-textData[moose< sample]
    rm(textData)

    mycorpus<-VCorpus(VectorSource(sampleLines))
    # clean it
    mycorpus<-tm_map(mycorpus,content_transformer(tolower))
    mycorpus<-tm_map(mycorpus,removeNumbers)
    #mycorpus<-tm_map(mycorpus,removeWords, stopwords("english"))
    mycorpus<-tm_map(mycorpus,removePunctuation)
    mycorpus<-tm_map(mycorpus,stripWhitespace)
    # remove non english characters?

    # tokenise
    # tdm_timeinfo<-system.time(
        tdm_ngram<-TermDocumentMatrix(mycorpus,control=list(tokenize=tokenizer))
        tdm_ngram<-removeSparseTerms(tdm_ngram,rmsparse)
        cat('tdm dimensions:',dim(tdm_ngram),'\n')

    # )
    rm(mycorpus)

    # cat('tdm created, time:\n',tdm_timeinfo,'\n')
    sumtime<-system.time(
        wordsums<-slam::row_sums(tdm_ngram,na.rm=TRUE)
    )
    cat('created counts, time info:\n',sumtime,'\n')
    temp<-data.frame(counts=wordsums,words=names(wordsums),stringsAsFactors=FALSE)

    
    cat(sprintf('writing data to %s\n',outfile))
    write.csv(wordcount,file=outfile)
}



set.seed(97)
token1gram<-getTokeniser(1)

# GetWordCounts<-function(source=c('blogs','news','twitter'),prefix='n1',tokenize,testfrac=0.1,sample=0.1,,rmsparse=0.9999)
GetWordCounts('blogs',tokenizer=token1gram,prefix='n1',testfrac=0.05,sample=0.01,rmsparse=0.99)

