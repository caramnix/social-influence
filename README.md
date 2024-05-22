# Social Influence and its Translation to the Legislative Arena
code for social influence and its translation to the legislative arena

## Description
Code for chapter 1 of my dissertation titled, Social Influence and its Translation to the Legislative Arena. 
Contains code for scraping tweets from Twitter using the academictwitteR package Code/Scrape/scrape_twitter.R. 

Also contains code to take that ouput and create the directed network of legislator interactions on Twittter for the 115th House and Senate and the 116th House and Senate, Scrape_Twitter_data-115H.ipynb, Scrape_Twitter_data-115s.ipynb, Scrape_Congress_data.ipynb, Scrape_Twitter_data-116S.ipynb respectively. 

Then convert these networks into adjacency matrices which represent tweets and mentions between legsialtors, ADJ_Fowler_Twitter.ipynb. 

The Connectedness folder uses these matrices to caluclate Connectedness wihtin each contextual enviornment for each chamber and session of Congress. 
The output of this procedure is availible in  ** folder. 

## Resources 

Bill data: https://www.govinfo.gov/bulkdata/BILLS/116. 


Twitter Data: 
House 115th  
Littman, Justin, 2017, "representatives-accounts-1.tab", 115th U.S. Congress Tweet Ids, https://doi.org/10.7910/DVN/UIVHQR/5UXBOC, Harvard Dataverse, V5, UNF:6:brZWYO9FQkqRnjUwKnZ01A== [fileUNF] 
Senate 115th 
Littman, Justin, 2017, "senators-accounts-1.tab", 115th U.S. Congress Tweet Ids, https://doi.org/10.7910/DVN/UIVHQR/FVKRIA, Harvard Dataverse, V5, UNF:6:8RaZETnZba8tACCjfl1DLA== [fileUNF] 

116th 
https://dataverse.harvard.edu/file.xhtml?persistentId=doi:10.7910/DVN/MBOJNS/WXZE5N&version=1.0

LES SCORES: 
https://thelawmakers.org/data-download

LES data: 
Social Influence/Data 
CELHouse93to116.xlsx
CELSenate93to116.xlsx

CODE: 
Scrape_Congress_Data.ipynb

CSV’s: 
leginfo_116H.csv 
leginfo_116S.csv 
leginfo_115H.csv
leginfo_115S.csv 

Twitter handles & other useful information 
moc_joined116.csv
115_congress.csv

