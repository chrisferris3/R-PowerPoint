##########################################################
# Step 0: Load Libraries                                ##
##########################################################

library(tidyverse)
library(tidyquant)
library(knitr)


##########################################################
# Step 1: Prepare for data                              ##
##########################################################

# Get a list of mnemonics
# This could be a text file if we wanted a long list
# Note, these data are all monthly frequencies
tickers <- c('UNRATE',
             'PAYEMS',
             'HOUST',
             'TB3MS',
             'TB6MS',
             'GS1',
             'GS2',
             'GS5',
             'GS10',
             'CPIAUCSL',
             'CIVPART',
             'LNS11300060',
             'LNS11300001',
             'LNS11300002',
             'MORTGAGE30US',
             'HPIPONM226S',
             'HSN1F')

# Next, list human readable variable names
myvars <- c('Civilian Unemployment Rate',
            'All Employees: Total nonfarm',
            'Housing Starts: Total New Privately Owned',
            '3-Month Treasury Bill:',
            '6-Month Treasury Bill:',
            '1-Year Treasury Rate',
            '2-Year Treasury Rate',
            '5-Year Treasury Rate',
            '10-Year Treasury Rate',
            'CPI : All Items',
            'Civilian Labor Force Participation Rate',
            'Civilian Labor Force Participation Rate: 25 to 54 years',
            'Civilian Labor Force Participation Rate: Men',
            'Civilian Labor Force Participation Rate: Women',
            '30-Year Fixed Rate Mortgage Average in the United States',
            'Purchase Only House Price Index for the United States',
            'New Home Sales')

mytransform<-c('none',
               'diff',    # difference
               'none',
               'none',
               'none',
               'none',
               'none',
               'none',
               'none',
               'pdiff12',  # 12-month percent difference
               'none',
               'none',
               'none',
               'none',
               'none',
               'pdiff12',  # 12-month percent difference
               'none')


# Units variables (after transform) are measured in
myunit <- c('%',
            'Monthly Change in Thousands',
            'Thousands, SAAR',
            '%',
            '%',
            '%',
            '%',
            '%',
            '%',
            '12-month % change',
            '%',
            '%',
            '%',
            '%',
            '%',
            '12-month % change',
            'Thousands, SAAR'
)


# Create a lookup dataset
mylookup <- data.frame(symbol = tickers,
                       var = myvars,
                       trans = mytransform,
                       unit = myunit)

# Take a look:
knitr::kable(mylookup)


#####################################################################################
## Step 2: Pull data  ##
#####################################################################################

tickers %>% tq_get(get="economic.data", 
                   from="2000-01-01"   # we start from January 2000
) -> df

df <- merge(df,mylookup,by="symbol") %>% 
  rename(value=price) %>%
  map_if(is.factor, as.character) %>%  #convert factors to character!
  as.tibble()


# Make a function to handle transformations
mytransf <- function(x,trans="none"){
  switch(trans,
         none = x,
         diff=c(NA,diff(x)),
         pdiff12 = 100*quantmod::Delt(x,k=12) # use Quantmod Delt() function 
  )
}

# make a function to save images (here in ~/img directory)

myplot <- function(in.var="UNRATE", in.trans="none", save="N"){
  dfp <- filter(df, symbol == in.var)
    g <-
    ggplot(data = dfp, aes(x = date, y = mytransf(value,in.trans)))+
    geom_line(color = "royalblue")+
    labs(title = paste0(head(dfp,1)$var," (",head(dfp,1)$unit,")"),
         caption = "@lenkiefer Source: St. Louis Federal Reserve Economic Database (FRED)\nTHIS IS A TEST ONLY",
         x = "",y = "")+
    theme_minimal()+
    theme(plot.caption = element_text(hjust = 0))
  if (save=="N") {print(g)} # plot image}
  if (save=="Y") {ggsave(filename = paste0("img/",in.var,".png"),
                         width = 8,height = 6)}
}
myplot()
myplot("PAYEMS", "diff", save = "Y")


# make a function to save images (here in ~/img directory)

myplot2 <- function(in.var="UNRATE", in.trans="none", save="N"){
  dfp <- filter(df, symbol == in.var)
  g <-
    ggplot(data = dfp, aes(x = date, y = mytransf(value,in.trans)))+
    geom_line(color = "royalblue")+
    labs(title = paste0(head(dfp,1)$var," (",head(dfp,1)$unit,")"),
         caption = "@chrisferris3 Source: St. Louis Federal Reserve Economic Database (FRED)\nTHIS IS A TEST ONLY",
         x = "",y = "")+
    theme_minimal()+
    theme(plot.caption = element_text(hjust = 0))
  if (save=="N") {print(g)} # plot image}
  if (save=="Y") {ggsave(filename = paste0("img/",in.var,".png"),
                         width = 8,height = 6)}
}
myplot2("PAYEMS", "diff", save = "Y")



##### save images
mylookup %>% mutate(test = walk2(symbol, trans, myplot(save = "Y")))








# Make chartbook  ####################
 
 library(officer)
 library(magrittr)
 

# Set a footer
myftr <- "@lenkiefer Purrrty PowerPoint"

# presentation

my_pres <- read_pptx("data/blank.pptx") %>%
  
  # Add a slide
  add_slide(layout = "Title Slide", master = "Office Theme") %>%
  
  # Add some text to the title
  ph_with_text(type = "ctrTitle", str = "Our totally awesome chartbook") %>%
  
  # Add some text to the subtitle
  
  ph_with_text(type = "subTitle", str = "A purrrfect example") %>%
  ph_with_text(type = "ftr", str = myftr)

# function to write slides
slidef <- function(var, preso = my_pres){
  my_pres %>%
    add_slide(layout = "Title and Content", master = "Office Theme") %>%
    ph_with_text(type = "ftr", str = myftr) %>%
    ph_with_img(type = "body", index = 1,
                src = paste0("img/", var, ".png" # got to get our images
                             )) -> my_pres
}
