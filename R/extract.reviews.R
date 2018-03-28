#' @title  pulls product reviews from GsmArena
#'
#' @description pull reviews/opinions from GSMARENA and store in excel sheet.
#'
#' @param url url of the products second page
#' @param n total number of review pages.
#' @param filename name of the excel file.
#' @param sheetname name of the sheet of excel file.
#'
#'@depends XML,httr,xlsx
#'
#' @return Dataframe
#'
#' @examples extract.reviews("https://www.gsmarena.com/vivo_v5-reviews-8430p2.php",3,"reviews","vivo")
#'
#' @export extract.reviews
#'
extract.reviews <- function(url,n,filename,sheetname)
{
  req.packages <- c("XML","httr","xlsx","stringr","utils")
  new.pkg <- req.packages[!(req.packages %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(req.packages, require, character.only = TRUE)

  res <- c()
  res1 <- c()

  pb <- txtProgressBar(min = 1, max = n, style = 3)

  split.url <- unlist(strsplit(url,"p2"))
  url.constant <- split.url[1]
  url.variable <- split.url[2]

  for (i in 1:n) {

    if(i==1)
    {
      url <- paste(url.constant,url.variable,sep="")
    }
    else
    {
      url <- paste(url.constant,"p",i,url.variable,sep="")

    }

    url.content = htmlParse(rawToChar(GET(url)$content))
    res <- c(unlist(lapply(getNodeSet(url.content,"//*[@id]/p/text()"),xmlValue)))

    x <- grep("\\r.",res)
    y <- which(str_length(res[x])>25)
    res1<- c(res1,res[-x],res[x[y]])


    setTxtProgressBar(pb, i)

  }
  res1 <- unique(res1)
  res1 <-  gsub("<.*?>", "", res1)       # regex for Removing HTML character

  res1 = gsub("^\\s+|\\s+$", "", res1)

  res1 <- as.data.frame(res1)

  write.xlsx(res1,file=paste(filename,".xlsx",sep = ""),sheetName=sheetname)
  print(paste("file saved as ",filename,".xlsx in ",getwd(),sep = ""))
  #return(res1)
}


