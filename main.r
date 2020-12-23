
#libraries
library(XML)
library(xml2)
library(rvest)
library(stringr)
library(magrittr)


categories_names <- read.csv("catnames.csv")
categories_names <- categories_names$categories_names


getNumberOfCarsByPriceRange <- function(url) {
  a = 1
  b = 500
  price_range = paste(format(a, scientific = FALSE), format(b, scientific = FALSE), sep = "-")
  prices = 0
  
  c = 0
  
  for (i in 1:100){
    urli = paste(url, "&p=", a, ":", b , sep="" )
    price_range[i] = paste(format(a, scientific = FALSE), format(b, scientific = FALSE), sep="-")
    prices[i] = getNumberOfResults(urli)
    a = a + 500
    b = b + 500
    c = c + 1
    print(c)
  }
  
  b = 55000
  
  for (i in 1:10) {
    urli = paste(url, "&p=", a, ":", b , sep="" )
    price_range[i+100] = paste(format(a, scientific = FALSE), format(b, scientific = FALSE), sep="-")
    prices[i+100] = getNumberOfResults(urli)
    a = a + 5000
    b = b + 5000
    c = c + 1
    print(c)
  }
  
  { 
    urli = paste(url, "&p=", "100000", ":", "" , sep="" )
    price_range[111] = paste("100000", "NA", sep="-")
    prices[111] = getNumberOfResults(urli)
  }
  
  names(prices) = price_range
  return(prices)
}
getNumberOfResults <- function(url) {
  webpage = xml2::read_html(url)
  css_number_of_items <- ".rbt-result-list-headline"
  number_of_items <- rvest::html_node(webpage, css_number_of_items)
  text_number_of_items <- rvest::html_text(number_of_items)
  list_number_of_items <- strsplit(text_number_of_items, "[ ]")
  
  if (is.na(as.numeric(gsub("\\.", "", list_number_of_items[[1]][1]))) == FALSE) {
    number <- as.numeric(gsub("\\.", "", list_number_of_items[[1]][1]))
  } else {
    number <- as.numeric(gsub("\\,", "", list_number_of_items[[1]][1]))
  }
  return(number)
}
# Scrapes the links for different price ranges
# example link for the query in range 
# of 25000-25500 euros
getLinksFromPriceRange <- function(url) {
  totalLinks <- NULL
  c = 0
  num = getNumberOfResults(url = url)
  number_of_pages <- ceiling(num/20)
  number_of_pages[number_of_pages > 50] <- 50
  
  for (i in 1:number_of_pages) {
    links <- NULL
    eyeCatchers <- NULL
    url2 <- paste(url,"&pageNumber=",i, sep = "")
    #print(url2)
    # codes to get links in the page (~20 links)
    xml_doc <- read_html(url2)
    links <- xml_doc %>%
      html_nodes(css = "div.cBox-body--resultitem") %>%
      extract() %>%
      html_nodes("a") %>%
      extract() %>%
      html_attr("href")
    eyeCatchers <- xml_doc %>%
      html_nodes(css = "div.cBox-body--eyeCatcher") %>%
      extract() %>%
      html_nodes("a") %>%
      extract() %>%
      html_attr("href")
    if (length(eyeCatchers) > 0){
      links <- append(links, eyeCatchers)
    }
    print(paste0("length of links: ", length(links)))
    print(paste0("progress: ", format(as.double((c+length(links))/num)*100, digits = 3), "%"))
    c = c + 20
    totalLinks <- append(totalLinks,links)
    
  }
  return(totalLinks)
}
# Gets every link of every price range
# there is 111 price ranges (see tje
# definiton of getNumberOfCarsByPriceRange()
getTotalLinks <- function(url) {
  num_of_results <- getNumberOfResults(url)
  cars_by_price_range <- getNumberOfCarsByPriceRange(url)
  totalLinks <- NULL
  for(i in 1:length(cars_by_price_range)) { 
    a = strsplit(names(cars_by_price_range)[i], "-")[[1]][1]
    b = strsplit(names(cars_by_price_range)[i], "-")[[1]][2]
    print(paste0("range: ",a,"-",b," index: ",i))
    if(!is.numeric(as.numeric(b))) {
      b = ""
    }
    aburl = paste(url, "&p=", a, ":", b , sep="" )
    totalLinks = append(totalLinks, getLinksFromPriceRange(url = aburl))
    print(paste0("TOTAL progress with links: ",format(as.double(length(totalLinks)/num_of_results)*100, digits = 3), "%" ))
  }
  return(totalLinks)
}
# This is the function that does
# of the web-scraping and data 
# collection work
getDataFromOneUrl <- function(url) {
  # This is the data vector that is going to be
  # filled with meaningful data if it is existant
  # on the car page or left with "" if there is 
  # no data present for that category
  dataVector <- c(rep("", length(categories_names)))
  names(dataVector) <- categories_names
  
  xml_doc <- NULL
  
  try(
    xml_doc <- read_html(url),
    silent = T
  )
  
  
  # Name and initial checking of a link
  if (is.null(xml_doc)){
    print("The link is broken")
  } else {
    nameOfTheCar <- xml_doc %>%
      html_nodes("h1") %>%
      extract() %>%
      html_text()
    nameOfTheCar <- nameOfTheCar[1]
    dataVector["Name"] = nameOfTheCar
    
    #Address of the seller
    addressOfTheCar <- xml_doc %>%   
      html_nodes("div.cBox.cBox--content.u-overflow-inherit") %>%  
      extract() %>%   
      html_nodes("#rbt-seller-address") %>%
      html_text()
    dataVector["Address"] <- addressOfTheCar
    
    #Rating of the user  
    userRating <- xml_doc %>%
      html_nodes("div.cBox.cBox--content.u-overflow-inherit") %>% 
      extract() %>%
      html_nodes(".mde-rating--s > a:nth-child(1) > span:nth-child(1)") %>%
      html_attr("data-rating")
    if(length(userRating) != 0){
      dataVector["UserRating"] <- userRating
    }
    
    # Price and technical data
    technical_data <- NULL
    technical_data <- xml_doc %>%
      html_nodes(xpath = '//*[@id="rbt-td-box"]' ) %>%
      extract() %>%
      html_nodes("div.u-margin-bottom-9") %>%
      html_children() %>%
      html_text()

    if(length(technical_data) != 0 || is.null(technical_data)) {
      for(i in 4 : 29) {
        for(j in 1: length(technical_data)){
          if(j == 0) {
            break
          }
          if(grepl(names(dataVector[i]), technical_data[j], fixed = TRUE)){
            print(paste0("datavec ", dataVector[i], " tecdata ", technical_data[j]))
            dataVector[i] <- technical_data[j+1]
            technical_data <- technical_data[c(-j, -(j+1))]
          }
        }
      }
    }
    
    
    # viermatic is actually a vector of 
    # strings that are present on the
    # 'austattung' part of the link
    # ex. 'ABS', 'Allradantrieb', etc.
    viermatic <- NULL
    try(
      viermatic <- xml_doc %>%
        html_nodes(xpath = '//*[@id="rbt-features"]') %>%
        extract() %>%
        html_nodes("p") %>%
        html_text(),
      silent = T
    )
    
    if(!is.null(viermatic) && length(viermatic) != 0){
      if(length(viermatic) != 0) {
        featureset <- paste(viermatic, collapse = " ")
        for(i in 30:length(categories_names)) {
          dataVector[i] <- grepl(categories_names[i], featureset, fixed = "TRUE")
        }
      }
    }
  }
  return(dataVector)
}
getAllDataInOneDataframe <- function(url){
  
  df <- data.frame(matrix(ncol = 131, nrow = 0))
  colnames(df) <- categories_names
   
  links = getTotalLinks(url = url)
  num_of_links = length(links)
  
  # for every carpage it stores the
  # already scraped data in a csv 
  # so that if an error occures
  # the progress is not lost
  for (i in 1:num_of_links){ 
    df[length(df$Name)+ 1 , ] <- getDataFromOneUrl(url = links[i])
    write.csv(df, "totalData.csv")
    print(paste0("Progress: ",format(as.double(i/num_of_links)*100, digits = 3), "%"))
  }
  return(df)
}
runThis <- function(url) {
  df <- getAllDataInOneDataframe(url = url)
  write.csv(df, file = "totalData.csv")
}


runThis(url = "https://suchen.mobile.de/fahrzeuge/search.html?dam=0&isSearchRequest=true&ms=17200;;11&sfmr=false&vc=Car")

