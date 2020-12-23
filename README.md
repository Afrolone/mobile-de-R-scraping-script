# mobile-de-R-scraping-script
A script written in R to get the cars pages data from mobile.de based upon the query

If you want to try it just run it in RStudio, and run the "runThis()" function and put a search query.

![alt text](https://raw.githubusercontent.com/Afrolone/mobile-de-R-scraping-script/main/querry.png)

For ex. https://suchen.mobile.de/fahrzeuge/search.html?dam=0&isSearchRequest=true&ms=17200;;11&sfmr=false&vc=Car

It was written to handle around 20 - 25 000 car results and without any price selection at main query,
it usually takes around couple of hours to get the data and the resulting file is a csv file with
131 features and rows that should very narrowly match the numer of search results of the main query.

If you want to make the script scrape more cars then you should make more price ranges and links for
the prices ranges. The whole reason for doing this is because Mobile.de sends up to 1000 (50 pages)
car offers for each form query.
