# MA415 Final project

#### There are five files in total for this final project. To make it easier to look, I basically extract the plotting part into a "result.rmd" file and also include a pdf report as well. The shiny link is at the end of the report file. And in case if there is a problem with my shiny, I include a full data file "data_epl_final.RData" and  the "app.R." 
#### I also include csv file necessary to Shiny app.

#### Lastly, the file "epl_analysis1.Rmd" is the full code including all data collecting method, data cleaning, and plotiting.
#### Here is a Shiny link: https://aomnut.shinyapps.io/R415/

### Methodology

#### This project aims explore the the public opinions to Manchester United and Manchester City for both individual players and whole teams through tweeting data collected from Twitter. The way to do it is using a Text analysis. I basically collect texts with certain tweet keyword and see how many positive and negative words are in that tweet. Using a standard Lexington Dictionary defining positive and negative words, I then assign a score 1 to each positive owrd and -1 to each negative word. At the end I am able to compute total number of these two types of word and even be able to calculate relative percentage of positive words for each certain hashtag. With ths standard method, I divide the analysis in to three parts for each team
#### 1) Individual analysis : a text analysis of a tweet on a certain name hashtag and collecting positive and negative words that show up. For example, "#pogba you are the best" contribute +1 score ("best") to #pogba. I do this for 11 players for each team
#### 2) Team analyis: a text analysis of a tweet on a certain keyword used to reepresent each team. There are 5 most used hashtag for ManUtd and 5 for Mancity. This way, I can see which hashtag contain the most positive or negative words. For example, "#mcfc worst team of epl" contribute -1 ("worst") to #mcfc which belong to ManCity team. 
#### 3) Head-to-head analysis: I basically compare the total number of positive and negative words for each team using T-Test to test whether  it is true that the public favor a specific team more than the other. Please see report file as a full report. 

