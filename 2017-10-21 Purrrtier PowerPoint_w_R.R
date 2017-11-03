#################################################################
## Load Libraries                                              ##
#################################################################

library(data.table)
library(tidyverse)
library(ggridges)
library(tidyquant)
library(officer)
library(rvg)
library(viridis)
library(scales)

#####################################################################################
## Get house price data
#####################################################################################

# read in data available as a text file from the FHFA website:

fhfa.data <- fread("http://www.fhfa.gov/DataTools/Downloads/Documents/HPI/HPI_PO_state.txt")
fhfa.data[,date:=as.Date(ISOdate(yr,qtr*3,1))] # make a date 

df.hpi <- fhfa.data %>% 
  group_by(state) %>% 
  mutate(hpa=Delt(index_nsa,k=4)) %>%   # create 4-quarter % change in index
  ungroup()

# Let's a take a look
knitr::kable(tail(df.hpi %>% arrange(date,state) %>% 
                    select(date,state,hpa) %>% 
                    map_if(is.numeric,percent) %>% 
                    data.frame(),10), row.names = FALSE)


# Get a list of states, will be useful later
s.list<-unique(df.hpi$state)

#####################################################################################
## Filter dataset function
#####################################################################################

f.state<-function(s="CA"){
  filter(df.hpi,state==s) %>% 
    map_if(is.character,as.factor) %>% 
    data.frame()
}

#####################################################################################
## Make plot function##
#####################################################################################

plotf <- function(in.df = f.state("CA")){
  ggplot(data = in.df, aes(x = date, y = hpa))+
    geom_line(data = df.hpi, aes(group = state),alpha = 0)+
    theme_minimal()+
    geom_ridgeline_gradient(aes(y = 0,height = hpa,fill = hpa),min_height = -1)+
    scale_fill_viridis(option = "C",limit = c(-.35,.35))+
    #geom_area(fill = "royalblue", alpha = 0.35)+
    geom_line(color = "black",size = 1.1)+
    labs(x = "",y = "",
         title = paste0("House price index for ", head(in.df,1)$state), 
         subtitle="4-quarter percent change in index, not seasonally adjusted",
         caption="@lenkiefer Source U.S. Federal Housing Finance Agency,  Purchase-only house price index by state")+
    theme(legend.position = "none",plot.caption = element_text(hjust = 0))+
    scale_y_continuous(labels = scales::percent, sec.axis = dup_axis(), breaks = seq(-1,1,.05))+
    scale_x_date(date_breaks = "5 years", date_labels = "%Y")
}

plotf()

#####################################################################################
## Make PowerPoint Deck ##
#####################################################################################

# Load blank.pptx, an empty powerpoint that serves as a template
my_pres<-read_pptx("data/blank.pptx")

# function for adding slides
# updated 11/3/2017 to fix function references (was mp2, should be mp)
myp <- function(i){
  my_pres %>% 
    add_slide(layout = "Blank", master = "Office Theme") %>%
    ph_with_vg_at( code=print(plotf(f.state(s.list[i]))) , 0.1, 0.1, 9.8, 7.3) ->
    my_pres
}

# use purrr::walk() to write the files
purrr::walk(1:51,myp)

# save the .pptx file
my_pres %>%
  print( target = "data/hpi.pptx") %>% 
  invisible()
