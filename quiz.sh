#!/bin/bash

# coursera capstone quiz 1
# pretty sure I can answer all of these just via shell commands

blogsfile="./data/final/en_US/en_US.blogs.txt"
newsfile="./data/final/en_US/en_US.news.txt"
twitterfile="./data/final/en_US/en_US.twitter.txt"
# 1. size of en_us blogs
size=$(ls -alh  ${blogsfile} | awk ' { print $5 }')
echo -E "blogs size is ${size}"

# 2. how man lines in twitter
twitlines=$(wc -l ${twitterfile})
echo -E "twitter file is ${twitlines} lines"

# 3. maximum line length 
echo -E "twitter max line = $(wc -L ${twitterfile})"
echo -E "news max line = $(wc -L ${newsfile})"
echo -E "blogs max line = $(wc -L ${blogsfile})"

# 4.  love/hate ratio for twitter
lovelines=$(grep -c 'love' ${twitterfile})
hatelines=$(grep -c 'hate' ${twitterfile})
echo -E "love hate ratio is $(echo ${lovelines}/${hatelines} | bc -l)"

# 5. biostats in twitter
grep 'biostats' ${twitterfile}
# looks no match? maybe the corpora was updated or something?


# 6. 
grep -c 'A computer once beat me at chess, but it was no match for me at kickboxing' ${twitterfile}