#!/usr/bin/Rscript
library(reshape2)

library(data.table)
library(dtplyr)

coverage<-function(n=1,thresh=0,printStuff=FALSE,plot=FALSE)
{
    fname<-sprintf('./data/n%i_wordcount_blogs.csv',n)
    cat('loading ',fname,'\n')
    blogsdf<-data.table(read.csv(fname,stringsAsFactors=FALSE),key='term')

    fname<-sprintf('./data/n%i_wordcount_news.csv',n)
    cat('loading ',fname,'\n')
    newsdf<-data.table(read.csv(fname,stringsAsFactors=FALSE),key='term')

    fname<-sprintf('./data/n%i_wordcount_twitter.csv',n)
    cat('loading ',fname,'\n')
    twitterdf<-data.table(read.csv(fname,stringsAsFactors=FALSE),key='term')


    names(blogsdf)<-c('term','counts.blogs')
    names(newsdf)<-c('term','counts.news')
    names(twitterdf)<-c('term','counts.twitter')

    # all<-merge(blogsdf,newsdf,by.x='term',by.y='term',all=TRUE)#,suffixes=c('.blogs','.news'))
    all<-merge(blogsdf,newsdf,by.x='term',by.y='term',all=TRUE,sort=TRUE)
    rm(blogsdf)
    rm(newsdf)
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

belowThresh<- totalWords - all[,sum(counts.total)]
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
        wordcols<-colsplit(covered$term,' ',wordcolnames)
        # names(wordcols)<-wordcolnames
        covered<-data.frame(wordcols,subset(covered,select=-c(term)))

    }
return(covered)
}



doPredictions<-function()
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

 # thresh    terms coverage
 #         1 634420 1
 #         2 241911 0.9934
 #         5 118840 0.9881
 #        10  78049 0.9836
 #        20  53338 0.978


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
coverage(n=1,thresh=0,printStuff=TRUE,plot=TRUE)

# predict

# stupid backoff (to start)
# look at 4 grams, look at the most probable 4 gram that matches the previous 3 (w-3,w-2,w-1) words

# look at the most probable 3 grams that match w-1, w-2. If these would predict a word not suggested by 4 grams, weight these by some alpha (alpha <1)
# look at the most probable 2 grams that match w-1. If these would predict something not covered by the 4 or 3 grams, then consider this with weight alpha ^2 (or something)
# otherwise consider the most probable words, if they haven't already been suggested.






