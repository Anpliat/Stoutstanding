# Stoutstanding


## Case Study 1

### Contents
We investigated a dataset of tweets made by Members of the European Parliament. We used data collected by Darko Cherepnalkoski, Andreas Karpf, Igor Mozetič, and Miha Grčar for their paper Cohesion and Coalition Formation in the European Parliament: Roll-Call Votes and Twitter Activities.

In particular, the dataset was obtained from CLARIN repository and can be [downloaded by clicking here](https://www.clarin.si/repository/xmlui/handle/11356/1071). We used the "retweets.csv" file and kept only the records where the original tweet was written in English. We also obtained the original tweet's text and added it as an extra column to the dataset. We only kept the records for which we were able to download the tweet text. We grouped the records by the European group of the MEP who posted the original tweet, and we removed the groups with a small number of tweets (i.e. less than 50).

### Text representation (Bag of Words methods)¶
1) Bag of word matrix
2) TF-IDF Vectors
    *	a] Word level
    *	b] N-Gram level
    *	c] Character level


* Dataset Description
* Visualizations
    * Correlations
2.2. Contingency Tables
2.3. Other Visualizations
3. Methodology
3.1. Linear Probability model
3.2. Regression Tree
4. Test results visualization
4.1. For Linear Regression Model
4.2. For Regression Tree



### Classification Algorithms
*	Decision trees
*	Random Forest (Bagging model)
*	Boosting Model
*	Multinomial Naïve Bayes
*	Gaussian Naïve Bayes
