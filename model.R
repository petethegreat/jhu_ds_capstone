#!/usr/bin/Rscript
library(reshape2)

library(dplyr)
coverage<-function(n=1,thresh=0,printStuff=FALSE)
{
blogsdf<-read.csv(sprintf('./data/n%i_wordcount_blogs.txt',n),stringsAsFactors=FALSE)
newsdf<-read.csv(sprintf('./data/n%i_wordcount_news.txt',n),stringsAsFactors=FALSE)
twitterdf<-read.csv(sprintf('./data/n%i_wordcount_twitter.txt',n),stringsAsFactors=FALSE)


names(blogsdf)<-c('term','counts.blogs')
names(newsdf)<-c('term','counts.news')
names(twitterdf)<-c('term','counts.twitter')

all<-merge(blogsdf,newsdf,by.x='term',by.y='term',all=TRUE)#,suffixes=c('.blogs','.news'))
all<-merge(all,twitterdf,by.x='term',by.y='term',all=TRUE)#,suffixes=c('.all','.twitter'))







all$counts.total <- rowSums(all[,c('counts.blogs','counts.news','counts.twitter')],na.rm=TRUE)
all<-all[order(all$counts.total,decreasing=TRUE),]
totalwords<-sum(all$counts.total)
if (printStuff)
{
    cat(sprintf('%10s %8s %5s\n','thresh','terms','coverage'))
    for(t in c(1,2,5,10,20))
    {
        thesewords<-(all$counts.total >= t)
        thisterms<-sum(thesewords)
        thiscover<-sum(all$counts.total[thesewords])/totalwords

        cat(sprintf('%10i %6i %0.4g\n',t,thisterms,thiscover))

    }
}

# coverage<-cumsum(all$counts.total)/sum(all$counts.total)
# head(all)



# for(cno in c(0.75,0.85,0.90,0.95,0.97,0.98,0.99)){
#     cat(sprintf('require %i words for %.2g coverage\n',sum(coverage < cno),cno))
# }




return(all[all$counts.total>=thresh,c('term','counts.total')])

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
#          1 275646 1
#          2 118496 0.9909
#          5  62115 0.9824
#         10  41641 0.9746
#         20  28186 0.964

# will require 10 counts, gives 97.5% coverage
# nval<-4
# n_terms<-coverage(n=nval,thresh=2,printStuff=TRUE)
# cleaned<-clean(n_terms,n=nval)
# cleanname<-sprintf('./data/cleaned_n%i.csv',nval)
# write.csv(cleaned,file=cleanname,row.names=FALSE)

# head(n_terms)
# dim(n_terms)

doPredictions()


# predict

# stupid backoff (to start)
# look at 4 grams, look at the most probable 4 gram that matches the previous 3 (w-3,w-2,w-1) words

# look at the most probable 3 grams that match w-1, w-2. If these would predict a word not suggested by 4 grams, weight these by some alpha (alpha <1)
# look at the most probable 2 grams that match w-1. If these would predict something not covered by the 4 or 3 grams, then consider this with weight alpha ^2 (or something)
# otherwise consider the most probable words, if they haven't already been suggested.






