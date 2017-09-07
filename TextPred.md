TextPred
========================================================
author: Peter Thompson
css: pete.css
date: September 5, 2017
autosize: true
font-import: https://fonts.googleapis.com/css?family=Montserrat



Features
========================================================
type: pete

  * [TextPred](https://petethegreat.shinyapps.io/textpred/) - Predicts the next word from a input text fragment.
  * loading screen displays while app initialises
  * bar chart displays most likely words, and their predicted probabilities

<img src="TextPred-figure/plotslide-1.png" title="plot of chunk plotslide" alt="plot of chunk plotslide" style="display: block; margin: auto;" />

Features
========================================================
type: pete

  * TextPred utilises Shiny/R framework
  * DataTables used for efficient data lookup
  * Prediction is based off an interpolated Kneser-Ney model using N-grams of up to 5th order
  * Model trained on data from twitter, news and blog posts
  * Hosted on Shinyapps.io



Further information
==========================================================
type: pete

  
  * Code available in [github](https://github.com/petethegreat/jhu_ds_capstone)
  * Additional notes can be found [here](https://petethegreat.github.io/jhu_ds_capstone/TextPrediction.html)




