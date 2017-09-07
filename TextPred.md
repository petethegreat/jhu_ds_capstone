TextPred
========================================================
author: Peter Thompson
css: pete.css
date: September 5, 2017
autosize: true
font-import: https://fonts.googleapis.com/css?family=Montserrat

Slide 1
========================================================
type: pete

[TextPred](https://petethegreat.shinyapps.io/textpred/) - text prediction app


Slide 2
========================================================
type: pete

Text pred - shiny app for next word prediction
based of data from twitter, news and blog posts
based on shiny/R
hosted on shinyapps

Slide 3
========================================================
type: pete

loading screen displays while app initialises
bar chart displays most likely words, and their predicted probabilities
<img src="TextPred-figure/plotslide-1.png" title="plot of chunk plotslide" alt="plot of chunk plotslide" style="display: block; margin: auto;" />

Slide 4
==========================================================
type: pete

  * Prediction is based off an interpolated Kneser-Ney model using N-grams of up to 5th order
  * stuff
code in github


