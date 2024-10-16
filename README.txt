This folder contains five files

1. helper.R : This R script contains all the libraries and helping functions that we use for creating data and building models in the
model and analysis.R  please make sure you have all of them in your enviroment before you run the code in model and analysis.R

2. model and analysis.R : This is the main section for our proejct which includes all data generation and models

3. tweet_elon.csv : a csv file contains the tweets that Elon Musk post in the past year, which use to build logistic model. Please make
sure you have it in your working directory. 

4. README.txt

5. written_responses.pdf : The final written report



Important Note: Since we use tq_get() under tidyquant package to obtain the data, you may receive a warning message that says: 
“Error in new.session() : Could not establish session after 5 attempts.” 
If it happens, please run the following alternative code: 
remotes::install_github("joshuaulrich/quantmod@358-getsymbols-new.session")
library(quantmod)

This is the technical issue on the package server’s side, and they have already fixed this issue on April 30th. 
In case you still encounter this issue, please use the alternative code which I provided above. 
Also, here is the link to the fix if you need further help. 
https://github.com/joshuaulrich/quantmod/issues/358