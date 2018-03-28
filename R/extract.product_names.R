#' @title  pulls product names from GsmArena
#'
#' @description pull product names of a brand from GSMARENA.
#'
#' @param filename name of the excel file.
#' @param sheetname name of the sheet of excel file.
#' @param brandname name of the brand.
#' @param brand_url url of the products second page
#' @param brand_pages total number of pages.
#'
#'@depends XML,httr,xlsx
#'
#' @return Dataframe
#'
#' @examples extract.product_names("myfile","sheet1","Apple","https://www.gsmarena.com/apple-phones-f-48-0-p2.php",2)
#'
#' @export extract.product_names
#'
#'

extract.product_names <- function(filename,sheetname,brandname,brand_url,brand_pages)
{
  req.packages <- c("XML","httr","xlsx")
  new.pkg <- req.packages[!(req.packages %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(req.packages, require, character.only = TRUE)

  write.xlsx((brand(brandname,brand_url,brand_pages)),file=paste(filename,".xlsx",sep = ""),sheetName=sheetname)
  print(paste("file saved as",filename,".xlsx in ",getwd(),sep = ""))
}


mobile.names<- function(url,n)
{
  res <- c()
  split.url <- unlist(strsplit(url,"-f"))
  url.constant <- split.url[1]
  url.variable <- split.url[2]
  category.number <- unlist(strsplit(url.variable,"-"))[2]

  for (i in 1:n) {

    if(i==1)
    {
      url <- paste(url.constant,"-",category.number,".php",sep="")
    }
    else
    {
      url <- paste(url.constant,"-f-",category.number,"-0-p",i,".php",sep="")

    }

    url.content = htmlParse(rawToChar(GET(url)$content))

    res <- c(res,unlist(lapply(getNodeSet(url.content,"//strong//span"),xmlValue)))
  }
  return(res)
}



brand <- function(brandname,product_url,product_pages)
{
  names <- mobile.names(product_url,product_pages)
  names <- paste(brandname,names,sep = " ")
  return(names)
}



