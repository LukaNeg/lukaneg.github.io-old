### Script for presenting all the figures and visualizations of my home page

amazon_raw <- read_csv("private_data/my_amazon_history.csv")

amazon_clean <- select(amazon_raw, 
                       order_date=`Order Date`,
                       category=Category,
                       title=Title,
                       cost=`Item Total`) %>%
  mutate(cost = as.numeric(gsub("\\$","",cost)),
         order_date = as.Date(order_date, format="%m/%d/%y"),
         category=as.factor(category),
         cum_spending = cumsum(cost))

plot(data=amazon_clean, cum_spending~order_date, type="l")
abline(v=as.Date("2018-05-13")) # date I graduated PhD

#dat <- amazon_books
plot_amazon <- function(dat, main="", cost=T){
  years <- unique(year(dat$order_date))
  years <- c(years,"2020")
  jan1year <- as.Date(paste(years,01,01,sep="-"))
  total=nrow(dat)
  
  if(cost){
    plot(data=dat, cum_spending~order_date, type="n", axes=F, ylab="",xlab="", xlim=c(min(jan1year),as.Date("2020-01-01")),
         main="")
    axis(1, at=jan1year,labels=years, las=2)
    axis(2, at=seq(0,20000,2000),labels=seq(0,20000,2000), las=2)
    polygon(x=c(dat$order_date, rev(dat$order_date)), y=c(dat$cum_spending,rep(0,total)),
            border=F, col=t_col("darkgreen",50))
    text("Ph.D. in Ecology", x=as.Date("2018-05-13"), y=1000, adj=.9, cex=.7)
    
  } else {
    par(mai=c(1,1.2,1,.5))
    plot(data=dat, cum_total~order_date, type="n", axes=F, ylab="",xlab="", xlim=c(min(jan1year),as.Date("2020-01-01")),
        ylim=c(0,240))
    axis(1, at=jan1year,labels=years, las=2, cex=.5, cex=.5)
    axis(2, at=seq(0,200,50),labels=seq(0,200,50), las=2)
    polygon(x=c(dat$order_date, rev(dat$order_date)), y=c(dat$cum_total,rep(0,total)),
            border=F, col=t_col("darkgreen",50))
    mtext("Year",side=1, line=3.5, cex=1.2)
    mtext("Number of Books",side=2, line=3, cex=1.2)
    
    #title("Cumulative number of books purchased", adj=0)
    mtext(side=3, cex=1.4,"Cumulative number of books purchased", font=2, adj=0)
    
    arrows(x0=as.Date("2011-06-06"), x1=as.Date("2011-06-06"), y0=0, y1=70, length=.1, lwd=2) # graduated college
    text("B.A. in Human Ecology", x=as.Date("2011-06-06"), y=80, adj=.9, cex=.7)
    
    arrows(x0=as.Date("2012-08-16"), x1=as.Date("2012-08-16"), y0=0, y1=105, length=.1, lwd=2) # graduated college
    text("Started grad school", x=as.Date("2012-08-16"), y=115, adj=.9, cex=.7)
    
    arrows(x0=as.Date("2014-10-16"), x1=as.Date("2014-10-16"), y0=0, y1=150, length=.1, lwd=2) # graduated college
    text("Passed the qualifying exam", x=as.Date("2014-10-16"), y=160, adj=.9, cex=.7)
    
    arrows(x0=as.Date("2018-05-13"), x1=as.Date("2018-05-13"), y0=0, y1=200, length=.1, lwd=2) # graduated college
    text("Ph.D. in Ecology", x=as.Date("2018-05-13"), y=210, adj=.9, cex=.7)
    
    arrows(x0=as.Date("2019-04-10"), x1=as.Date("2019-04-10"), y0=0, y1=225, length=.1, lwd=2) # graduated college
    text("Moved to Galápagos", x=as.Date("2019-04-10"), y=235, adj=.9, cex=.7)
  }
}

unique(amazon_clean$category)
amazon_books <- filter(amazon_clean,
                       category %in% c("Paperback","Staple Bound","Pamphlet",
                                       "Hardcover","Mass Market Paperback",
                                       "Perfect Paperback")) %>%
  mutate(cum_spending = cumsum(cost),
         cum_total = cumsum(cost>=0))
plot_amazon(amazon_books, cost=F)
plot_amazon(amazon_books, cost=T)

