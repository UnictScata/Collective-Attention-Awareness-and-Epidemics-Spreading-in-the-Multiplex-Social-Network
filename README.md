# Collective-Attention-Awareness-and-Epidemics-Spreading-in-the-Multiplex-Social-Network


Starting from a large corpus of Twitter communications dataset (from: https://www.trackmyhashtag.com/blog/twitter-datasets-free/ and https://www.kaggle.com/smid80/coronavirus-covid19-tweets ) and the evaluation of the most popular terms of search in Google search, regarding a time interval ranged between 1-Dec-2019 and 30-Mar-2020, we have analysed and measured the collective attention and its impact on the awareness diffusion and epidemics spreading in relation to covid-19 pandemic. 
This study is carried out using R-statistics, datasets have been integrated through the public Twitter API, in order to retrieve additional information about users, and queries popularity has been investigated thanks to Gtrends tools. 
Source Code is composed of: 
•	TwitterData_integration.R, where we extract data from datasets, integrate them through rtweet package of R and analyse different time windows. 
•	Gtrends.R, through gtrendsR package, we extract the most 25 popular terms of search and their related queries, in the different states under the considered time interval.
•	User_sampling.R, where we apply a sampling approach, based on activeness and connectedness, to identify the most influential users and their relationships.
•	Hashtags_coadoption, we extract hashtags from text of the large corpus of tweets posted by sampled users, evaluate the “co-adoption” of different hashtags in the same text and construct a graph, where unique hashtags are the edges and “co-adoptions” relationships are the links.
•	Multiplex-network.R, where we consider a weighted network of two layers, the first from the real “reply-cite-mention” network obtained from Twitter, the second one a theoretical scale-free representation of the same set of nodes (users). 
•	Baseline.R, we calculate the heterogeneous distribution of awareness in the baseline case. 
•	Data-driven.R, we calculate the distribution of awareness in network structure considering multiplex parameters and user-generated data from Twitter and GTrends under the covid-19 pandemic. 
•	Epidemics.R, we apply the Dynamic Microscopic Markov Chain Approach (MMCA) in order to explore the spreading dynamics of the coevolution of epidemic spreading (SEIR model) and awareness spreading processes (UAF model). 
This code can be redistributed and/or modified under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. 
This program is distributed to the authors in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
If you use this code please cite: 
