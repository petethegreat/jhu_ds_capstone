#!/usr/bin/Rscript
library(reshape2)

library(data.table)
library(dtplyr)

coverage<-function(n=1,thresh=0,printStuff=FALSE,plot=FALSE,thresh2=0)
{
    fname<-sprintf('./data/n%i_wordcount_blogs.csv',n)
    cat('loading ',fname,'\n')
    all<-data.table(read.csv(fname,stringsAsFactors=FALSE),key='term')
    names(all)<-c('term','counts.blogs')
    all<-all[counts.blogs >= thresh2,]

    fname<-sprintf('./data/n%i_wordcount_news.csv',n)
    cat('loading ',fname,'\n')
    newsdf<-data.table(read.csv(fname,stringsAsFactors=FALSE),key='term')
    names(newsdf)<-c('term','counts.news')
    newsdf<-newsdf[counts.news >= thresh2,]

    cat('merging blogs and news\n')
    all<-merge(all,newsdf,by.x='term',by.y='term',all=TRUE,sort=TRUE)
    cat('merged, deleting news\n')
    rm(newsdf)

    fname<-sprintf('./data/n%i_wordcount_twitter.csv',n)
    cat('loading ',fname,'\n')
    twitterdf<-data.table(read.csv(fname,stringsAsFactors=FALSE),key='term')
    names(twitterdf)<-c('term','counts.twitter')
    twitterdf<-twitterdf[counts.twitter >= thresh2,]

    cat('merging twitter\n')
    all<-merge(all,twitterdf,by.x='term',by.y='term',all=TRUE,sort=TRUE)
    rm(twitterdf)
    


    all$counts.total <- rowSums(all[,c('counts.blogs','counts.news','counts.twitter')],na.rm=TRUE)
    all<-all[order(-counts.total)]
    # all<-all[order(all$counts.total,decreasing=TRUE),]
    totalwords<-all[,sum(counts.total)]
    if (printStuff)
    {
        cat(sprintf('%10s %8s %5s\n','thresh','terms','coverage'))
        for(t in c(1,2,5,10,20))
        {
            thesewords<-all[counts.total >= t]
            thisterms<-thesewords[,.N][1]
            thiscover<-thesewords[,sum(counts.total)][1]/totalwords

            cat(sprintf('%10i %6i %0.4g\n',t,thisterms,thiscover))

        }
    }

    if(plot)
    {
        library(ggplot2)
        fname<-sprintf('./plots/frequency_plot_n%i.pdf',n)
        g<-ggplot(data=all,aes(x=seq_along(term),y=counts.total/totalwords)) + geom_line(color='red') +
        scale_x_log10() + scale_y_log10() + 
        labs(title='term frequency vs vocabulary size',subtitle=sprintf('%i-grams',n),x='vocab size',y='relative word frequency')
        ggsave(fname,g)

        fname<-sprintf('./plots/coverage_plot_n%i.pdf',n)
        g<-ggplot(data=all,aes(x=seq_along(term),y=cumsum(counts.total)/totalwords)) + geom_line(color='red') +
        scale_x_log10() + 
        labs(title='term coverage vs vocabulary size',subtitle=sprintf('%i-grams',n),x='vocab size',y='corpus coverage')+
        geom_hline(yintercept=0.95,color='blue',linetype=3)+
        annotate('text',5,0.94,label='95% coverage')+
        geom_hline(yintercept=0.99,color='blue',linetype=2)+
        annotate('text',5,0.98,label='99% coverage')
        

        ggsave(fname,g)
    }



# coverage<-cumsum(all$counts.total)/sum(all$counts.total)
# head(all)



# for(cno in c(0.75,0.85,0.90,0.95,0.97,0.98,0.99)){
#     cat(sprintf('require %i words for %.2g coverage\n',sum(coverage < cno),cno))
# }

belowThresh<- totalwords - all[,sum(counts.total)]
all<-all[counts.total >= thresh,.(term,counts.total)]
rbind(all,list('UNKNOWN',belowThresh))



return(all[counts.total>=thresh,.(term,counts.total)])

# total=sum(blogsdf$counts)
# blogsdf$freq<-blogsdf$count/total
# blogsdf<-blogsdf[order(blogsdf$counts,decreasing=TRUE),]
# blogsdf$cumulative<-cumsum(blogsdf$freq)

# cat('total words = ',total,'\n')
# head(blogsdf)
# can do some more stuff here, non english characters, profanity filtering

}


clean<-function(covered,n)
{
    wordcolnames<-'w1'
    while (length(wordcolnames) < n)
    {
        wordcolnames<-c(wordcolnames,sprintf('w%i',length(wordcolnames)+1))
    }

    if (n==1) 
    {
        names(covered)[1]<-'w1'
    }
    else
    {
        # wordcols<-colsplit(covered$term,' ',wordcolnames)
        # names(wordcols)<-wordcolnames
        # covered[,wordcolnames := colsplit(.(term),' ',wordcolnames) ,]
        covered[,(wordcolnames) := tstrsplit(term,' ',fixed=TRUE)]
        covered<-covered[,term:=NULL]
        # return(covered[,(term) := NULL])
        #<-data.table(data.frame(wordcols,subset(covered,select=-c(term)))

    }
return(covered)
}



oldDoPredictions<-function()
{
    n1df<-read.csv('./data/cleaned_n1.csv',stringsAsFactors=FALSE)
    n2df<-read.csv('./data/cleaned_n2.csv',stringsAsFactors=FALSE)
    n3df<-read.csv('./data/cleaned_n3.csv',stringsAsFactors=FALSE)
    n4df<-read.csv('./data/cleaned_n4.csv',stringsAsFactors=FALSE)

    # n4total<-sum(n4df$counts.total)
    # n3total<-sum(n3df$counts.total)
    # n2total<-sum(n2df$counts.total)
    # n1total<-sum(n1df$counts.total)



    getwords<-function(str,nmax=3)
    {
        # clean non english
        str<-iconv(str, "latin1", "ASCII", sub="BADCHAR")
        str<-gsub('[^ ]*(BADCHAR)+[^ ]','',str)

        # lower case
        str<-tolower(str)
        # remove punctuation (non-a-z)
        str<-gsub('[^a-z ]','',str)
        words<-strsplit(str,' ')[[1]]
        thelength<-length(words)
        if(thelength > nmax)
        {
            words<-words[(thelength-nmax+1):thelength]
        }
        return(words)

    }


    
    # from 4grams
    getfrom4<-function(words,n,df,cnorm=1)
    {
        results<-df %>% 
            filter(w1==words[1]) %>%
            filter(w2==words[2] ) %>% 
            filter(w3==words[3]) %>% 
            mutate(prob=counts.total/cnorm) %>%
            select(w4,counts.total,prob) %>%
            arrange(desc(counts.total)) %>% 
            top_n(3)
        return(results)
    }

    # from 3 grams
    getfrom3<-function(words,n,df,cnorm=1)
    {
        results<-df %>% 
            filter(w1==words[1]) %>%
            filter(w2==words[2] ) %>% 
            mutate(prob=counts.total/cnorm) %>%
            select(w3,counts.total,prob) %>%
            arrange(desc(counts.total)) %>% 
            top_n(3)
        return(results)
    }

    getfrom2<-function(words,n,df,cnorm=1)
    {
        results<-df %>% 
            filter(w1==words[1]) %>%
            mutate(prob=counts.total/cnorm) %>%
            select(w2,counts.total,prob) %>%
            arrange(desc(counts.total)) %>% 
            top_n(3)
        return(results)
    }





    predict<-function(thestr)
    {
        words3<-getwords(thestr,3)
        print(words3)

        cnorm<-n3df %>%filter(w1==words3[1]) %>% filter(w2==words3[2]) %>% filter(w3==words3[3]) %>% select(counts.total)
        res4<-getfrom4(words3,4,n4df,cnorm=cnorm[[1]])
        print(res4)


        cnorm<-n2df %>%filter(w1==words3[2]) %>% filter(w2==words3[3]) %>% select(counts.total)
        res3<-getfrom3(words3[2:3],3,n3df,cnorm=cnorm[[1]])
        print(res3)
        # count4<-n3df %>%filter(w1==words3[1]) %>% filter(w2==words3[2]) %>% filter(w3==words3[3]) %>% select(counts.total) 

        cnorm<-n1df %>%filter(w1==words3[3]) %>% select(counts.total)
        res2<-getfrom2(words3[3],2,n2df,cnorm=cnorm[[1]])
        print(res2)

    }

    # predict('a bouquet, and a case of')
    # predict('It would mean the')
    # predict('can you follow me and make me the')
    # predict('Bills game: Offense still struggling but the')

    # predict('romantic date at the')
    #  predict('dust them off and be on my')
    #  predict("Love that film and haven't seen it in quite some")
    #  predict('wet hair out of his eyes with his little')

    predict('and keep the faith during the')
    predict('ever seen, then you must be')



}

DoPredictions<-function()
{


# Will use interpolated kneser ney smoothing

# Ideally, use highest order ngrams (n=5 for us here, say)
# We need to save some probability for ngrams that weren't encountered in our training set, so we discount.

# The rough 5-gram probability is P5(w|w1_w4) = [c(w1_w4w) -d]/c(w1_w4), where d is our discount (0<d<1)

# We then interpolate, and assign the discounted probability mass to our 4 gram distribution

# P = P5(w|w1_w4) + lambda(w1-w4) P4(w,w2_w4)
# If P5 is zero, then w is occuring in an unknown context, we haven't seen this 5 gram before.
# We then use information from the 4 gram distribution, but instead of assigning probability proportional to c(w2_w4w)/c(w2_w4),
# we consider the number of times that w3w4w completes 4 grams, so our 4 gram probability is count( c(xw3w4w) > 0, for all x)/count(c(vxyz) >0 for all vxyz)

# Similarly, we discount our 4 gram counts, and reallocate the discounted probability to our 3 grams, which we discount, and realocate to 2, etc.


# load all our data

cat('loading data\n')
nmax<-5 # have only up to 5 grams
loaddf<-function(i)
{
    fname<-sprintf('./data/cleaned_counts_n%i.csv',i)
    cat('loading ',fname,'\n')
    return(data.table(read.csv(fname,stringsAsFactors=FALSE)))

}

dflist<-lapply(1:nmax,loaddf)
# setkey(dflist[[1]],cols=c('w1'))
# setkey(dflist[[2]],cols=c('w1','w2'))
# setkey(dflist[[3]],cols=c('w1','w2','w3'))
# setkey(dflist[[4]],cols=c('w1','w2','w3','w4'))

# head(dflist[[1]])
# setkey(dflist[[1]],w1,w2,w3,w4)



    # fname<-sprintf('./data/cleaned_counts_n%i.csv',5)
    # n5df<-data.table(read.csv(fname,stringsAsFactors=FALSE))
    # setkey(n5df,w1,w2,w3,w4,w5) # want to predict based on first 4

    # fname<-sprintf('./data/cleaned_counts_n%i.csv',4)
    # n4df<-data.table(read.csv(fname,stringsAsFactors=FALSE))
    # setkey(n4df,w2,w3,w4,w1) # want to know when (w2,w3,w4) completes something

    # fname<-sprintf('./data/cleaned_counts_n%i.csv',3)
    # n3df<-data.table(read.csv(fname,stringsAsFactors=FALSE))
    # setkey(n3df,w2,w3,w1) # want to know when (w3,w4) completes something

    # fname<-sprintf('./data/cleaned_counts_n%i.csv',2)
    # n2df<-data.table(read.csv(fname,stringsAsFactors=FALSE))
    # setkey(n2df,w2,w1) # want to know when (w2) completes something

    # fname<-sprintf('./data/cleaned_counts_n%i.csv',1)
    # n1df<-data.table(read.csv(fname,stringsAsFactors=FALSE))
    # setkey(n1df,w1) # want to know when (w2) completes something
    # cat('data loaded')


    Dval<-0.4 # discount, could train this maybe


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
            cat(thestr,'\n')
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
            cat(resultexpr,'\n')
            eval(parse(text=resultexpr))


            if(nrow(highcounts) >0)
            {
                cat('nrow = ',nrow(highcounts[order(-count)]),'\n')
                foundcontext<-TRUE
                # cat(head(highcounts[order(-counts.total)]),'\n')
            }

            # foundcontext<-isTRUE(nrow(highcounts) > 0 ) 
            # if there were no results, decetrnment thisnmax
            if (!foundcontext) {thisnmax<-thisnmax -1}
            # decrement thisnmax

        }

        # cat(head(highcounts),'\n')
        hc2<-highcounts[,.(word,prob = (count-Dval)/sum(count))]
        hc2[prob <0,prob:=0]
        print(head(hc2))
        lval<-1.0 - sum(highcounts$prob) # probability mass left for lower orders

        # now do continuation counts for lower orders
        
        # highcounts is good. Do the kneser ney smoothing




    }


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

        # print(inputstr)
        # print(thestring)


        words<-strsplit(thestring,' ')
        for (vec in words)
        {
            predictCleaned(vec)
        }
        # this returns a list of character vectors, each vector containing single words





    }

    predict(c('you never know until you ','my dog has a '))











}
 


# keep words occuring at least 5 times, 99% coverage

# closed vocab
# keep fixed vocab size, the ~120k words that give 99% coverage (occur at least 5 times)
# when we clean things, replace words not in our vocab by 'UNKNOWN'


# smoothing





# Discounting





# will require 10 counts, gives 97.5% coverage
# nval<-4
# n_terms<-coverage(n=nval,thresh=2,printStuff=TRUE)
# cleaned<-clean(n_terms,n=nval)
# cleanname<-sprintf('./data/cleaned_n%i.csv',nval)
# write.csv(cleaned,file=cleanname,row.names=FALSE)

# head(n_terms)
# dim(n_terms)

# doPredictions()
# coverage(n=1,thresh=0,printStuff=TRUE,plot=TRUE)
# thresh    terms coverage
 #         1 634420 1
 #         2 241911 0.9934
 #         5 118840 0.9881
 #        10  78049 0.9836
 #        20  53338 0.978

# used threshold of 2 for 2 and 3 grams, will use 5 for 4 and 5 grams
# nval<-5
# thresh<-5
# thresh2=2
# outname<-sprintf('./data/cleaned_counts_n%i.csv',nval)
# covered<-coverage(n=nval,thresh=thresh,printStuff=TRUE,plot=FALSE,thresh2=thresh2)
# cleaned<-clean(covered,n=nval)
# rm(covered)
# head(cleaned)
# # want to discount and convert to probability before writing
# write.csv(cleaned[order(-counts.total)],file=outname,row.names=FALSE)

DoPredictions()

# todo: 
# clean our ngrams
# convert things to probability
# define perplexity and estimate coefficients
# do quiz doPredictions


# Model:
# kneser kney smoothing
# probability is discounted ngram counts + lambda Pcontinuation, where Pcontinuation is the number of bigrams that are completed by word under consideration.
# knesser kney shouldn't be too hard. We'll need to compute perplexity and come up with some coefficients.

# predict

# stupid backoff (to start)
# look at 4 grams, look at the most probable 4 gram that matches the previous 3 (w-3,w-2,w-1) words

# look at the most probable 3 grams that match w-1, w-2. If these would predict a word not suggested by 4 grams, weight these by some alpha (alpha <1)
# look at the most probable 2 grams that match w-1. If these would predict something not covered by the 4 or 3 grams, then consider this with weight alpha ^2 (or something)
# otherwise consider the most probable words, if they haven't already been suggested.






