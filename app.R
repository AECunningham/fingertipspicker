library(shiny)
library(jsonlite)
library(dplyr)
library(ggplot2)
library(stringr)
library(tidyjson) # NB - must be development version of tidyjson
library(reshape2)
library(curl)
# library(DT)

# Obtain list of regions including their names
regionsJson <- "http://fingertips.phe.org.uk/api/areas/by_area_type?area_type_id=6"
regions <- fromJSON(regionsJson)

# Find LTLAs, UTLAs and CCGs, and the regions they belong to. Profile 51 is used because it has LTLAs, UTLAs and CCGs.

# LTLAs
LTLA_json <- "http://fingertips.phe.org.uk/api/areas/by_area_type?area_type_id=101"
LTLAs <- fromJSON(LTLA_json)
regionPerLTLA_json <- "http://fingertips.phe.org.uk/api/parent_to_child_areas?profile_id=51&child_area_type_id=101&parent_area_type_id=6"
regionPerLTLA <- melt(fromJSON(regionPerLTLA_json))
colnames(regionPerLTLA) <- c('areaCode','Code.Region')
LTLAs <- left_join(LTLAs,regionPerLTLA,by=c("Code"="areaCode"))
LTLAs <- left_join(LTLAs,regions, by=c("Code.Region" = "Code"), suffix=c(".Area",".Region")) # Attach region names to area rows
names(LTLAs)[names(LTLAs) == "Code"] <- "Code.Area" # Rename first column for consistency

# UTLAs
UTLA_json <- "http://fingertips.phe.org.uk/api/areas/by_area_type?area_type_id=102"
UTLAs <- fromJSON(UTLA_json) %>% filter(Code != "09") # Filter out old Beds CC (has anomalous code "09")
regionPerUTLA_json <- "http://fingertips.phe.org.uk/api/parent_to_child_areas?profile_id=51&child_area_type_id=102&parent_area_type_id=6"
regionPerUTLA <- melt(fromJSON(regionPerUTLA_json))
colnames(regionPerUTLA) <- c('areaCode','Code.Region')
UTLAs <- left_join(UTLAs,regionPerUTLA,by=c("Code"="areaCode"))
UTLAs <- left_join(UTLAs,regions, by=c("Code.Region" = "Code"), suffix=c(".Area",".Region")) # Attach region names to area rows
names(UTLAs)[names(UTLAs) == "Code"] <- "Code.Area" # Rename first column for consistency

# CCGs
CCG_json <- "http://fingertips.phe.org.uk/api/areas/by_area_type?area_type_id=19"
CCGs <- fromJSON(CCG_json)
regionPerCCG_json <- "http://fingertips.phe.org.uk/api/parent_to_child_areas?profile_id=51&child_area_type_id=19&parent_area_type_id=6"
regionPerCCG <- melt(fromJSON(regionPerCCG_json))
colnames(regionPerCCG) <- c('areaCode','Code.Region')
CCGs <- left_join(CCGs,regionPerCCG,by=c("Code"="areaCode"))
CCGs <- left_join(CCGs,regions, by=c("Code.Region" = "Code"), suffix=c(".Area",".Region")) # Attach region names to area rows
names(CCGs)[names(CCGs) == "Code"] <- "Code.Area" # Rename first column for consistency

parentareacodes <- LTLAs %>% select(Code.Region) %>% distinct # list of Region codes
names(parentareacodes)[names(parentareacodes) == "Code.Region"] <- "Code" # Rename column to agree with Seb's loop below
  
# Find descriptions of sexes and ages 
sex <- fromJSON("http://fingertips.phe.org.uk/api/sexes")
sex <- sex %>% rename(sex = Id,gender = Name)
age <- fromJSON("http://fingertips.phe.org.uk/api/ages")
age <- age %>% rename(age=Id,age_group = Name)

# Find available profiles
prof_json <- "http://fingertips.phe.org.uk/api/profiles"
prof <- prof_json %>%
  gather_array %>%
  spread_values( ID = jnumber( "Id") ,
                 Name = jstring( "Name") ,
                 Key = jstring( "Key")
  ) 

# Restrict to profiles that have some associated area types (not all do):
prof <- mutate(prof,APIcall = paste0("http://fingertips.phe.org.uk/api/area_types","?profile_ids=",ID))  # specify the API call to find area types for each profile
prof <- prof %>% filter(apply(prof[,c('APIcall'),drop=F],1,function(x) length(fromJSON(x)))>0) #  filter out any profiles that have no associated area types at all

# Work out how many of the area types "District & UA", "County & UA" and "CCG" are represented (answer will be 0, 1, 2 or 3):
prof <- mutate(prof,numAreaTypes = apply(prof[,c('APIcall'),drop=F],1,function(x) nrow(fromJSON(x) %>% filter(Short %in% c("District & UA","County & UA","CCG")))))
prof <- prof %>% filter(numAreaTypes > 0) # filter out any profiles that have none of those three area types

profileListDF <- prof %>% select(ID,Name) %>% arrange(Name) %>% as.data.frame()
profileList <- split(profileListDF$ID,profileListDF$Name) # Shiny drop-down list expects a list, not a data frame

# Find out which groups are in which profiles
profGroup <- prof_json %>%
  gather_array %>% 
  spread_values(ID = jnumber("Id"),
                Name = jstring("Name"),
                Key = jstring("Key")
  ) %>%
  enter_object("GroupIds") %>%
  gather_array %>%
  append_values_number("groupid") %>%
  select(ID, Name, Key, groupid)

areaTypes_json <- "http://fingertips.phe.org.uk/api/area_types"
areaTypesDF <- fromJSON(areaTypes_json) %>% filter(Short %in% c("District & UA","County & UA","CCG"))


ui <- fluidPage(
  titlePanel("Download latest data for selected Profile and Area(s)"),
  sidebarLayout(
    sidebarPanel(
                 br(),
                 uiOutput("Box1"),
                 br(),
                 uiOutput("Radio1"),
                 br(),
                 uiOutput("Box2"),
                 actionButton("makeChoice","Click to choose indicators",style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                 p(em("May take some time to appear....")),
                 br(),
                 uiOutput("Box3"),
                 h4("Disclaimer: Please note this app is experimental, and is", em(strong("not")), "based on a full understanding of the Fingertips API")),
    mainPanel(
#              textOutput("text1"),
#              textOutput("text2"),
#              textOutput("text3intro"),
#              textOutput("text3"),
#              textOutput("text4intro"),
#              textOutput("text4"),
               dataTableOutput("Table1"),
               p(class = 'text-center', downloadButton('x3', 'Download Data')))
  )
)


server <- function(input,output) {
  
  output$Box1 <- renderUI({selectInput("profile","Select a profile",profileList)})
#  output$text1 <- renderText({paste("You have selected",input$profile,profileListDF[profileListDF$ID == input$profile,2][1])})
  
  output$Radio1 <- renderUI({
    areaType <- fromJSON(paste0("http://fingertips.phe.org.uk/api/area_types","?profile_ids=",input$profile))
    LAsOrCCGs <- areaType %>% filter(Short %in% c("District & UA","County & UA","CCG"))
    LAsOrCCGsList <- split(LAsOrCCGs$Id,LAsOrCCGs$Short) # Shiny drop-down list expects a list, not a data frame
    radioButtons("atype","Select an area type",LAsOrCCGsList)
  })
#  output$text2 <- renderText({paste("You have selected",input$atype,areaTypesDF[areaTypesDF$Id == input$atype,3][1])})
  
  output$Box2 <- renderUI({
    if(input$atype == 101) {areaListDF <- LTLAs}
    else if(input$atype == 102) {areaListDF <- UTLAs}
    else {areaListDF <- CCGs}
    codeList <- areaListDF$Code.Area
    names(codeList) <- areaListDF$Name.Area # want to display names but return codes
    selectInput("areaList","Select some areas",multiple = TRUE,
                choices = lapply(split(codeList,areaListDF$Short.Region),as.list))
  })
#  output$text3intro <- renderText({paste0("You have selected ",length(input$areaList)," areas:")})
#  output$text3 <- renderPrint(input$areaList)
 
  getIndList <- function(chosenProfile,chosenAreaType)   {
       
	  groupids <- profGroup[profGroup$ID == chosenProfile,4] # List the groupids for the chosen profile
	  df <- data_frame()
	  indList <- data_frame()
	  
	  for(groupid in groupids$groupid) {
		for(parentcode in parentareacodes$Code) {
		  hp_url1 <- paste0("http://fingertips.phe.org.uk/api/latest_data/all_indicators_in_profile_group_for_child_areas",
							 "?profile_id=",chosenProfile,
							 "&group_id=",groupid,
							 "&area_type_id=", chosenAreaType,
							 "&parent_area_code=", parentcode)
		  ## Extract data
		  data <- hp_url1 %>%
			gather_array %>%
			spread_values(ind_id = jnumber("IID") ) %>%
			enter_object("Data") %>%
			gather_array %>%
			spread_values(
			  area = jstring("AreaCode") ,
			  value = jnumber("Val") ,
			  lci = jnumber("LoCI") ,
			  uci = jnumber("UpCI") ,
			  denom = jnumber("Denom") ,
			  count = jnumber("Count") ,
			  age = jnumber("AgeId") ,
			  sex = jnumber("SexId")
			) %>%
			select(ind_id, area, age, sex, value, lci, uci, denom, count)
		  
# Build up a list of combinations of indicator and group ids		  
		  newInds <- data %>% select(ind_id) %>% distinct
		  newInds$groupCode <- rep(groupid,nrow(newInds))
		  indList <- bind_rows(indList,newInds)

# Don't want to add in the same indicator a 2nd or 3rd time just because it is in 2 or 3 groups
      if (nrow(df) > 0) {data <- anti_join(data,df, by=c("ind_id","area"))}
      
## Build data frame
		  df <- bind_rows(df, data)
		}
	  }
	  
	  
	  # Add time period ( uses the same looping process)
	  dft <- data_frame()
	  for(groupid in groupids$groupid) {
		for(parentcode in parentareacodes$Code) {
		  hp_url1 <- paste0("http://fingertips.phe.org.uk/api/latest_data/all_indicators_in_profile_group_for_child_areas",
							 "?profile_id=",chosenProfile,
							 "&group_id=",groupid,
							 "&area_type_id=",chosenAreaType,
							 "&parent_area_code=", parentcode)
		  data1 <- hp_url1 %>%
			gather_array() %>%
			spread_values(ind_id = jnumber("IID") ) %>%
			enter_object("Grouping") %>%
			gather_array %>%
			spread_values(
			  time = jstring("Period")
			)
		  dft <- bind_rows(dft,data1)
		}
	  }
	  

	timePeriods <- dft %>% select(ind_id,time) %>% distinct %>% rename(period = time)
	  
	# Join time periods to main data frame
	df <- df %>%
	  left_join(timePeriods) %>%
	  select(ind_id,area,period,value,lci,uci,count,denom,age,sex) %>%
	  distinct

ind_names <- data_frame()	

for(groupid in groupids$groupid) {
  ind_url <- paste0("http://fingertips.phe.org.uk/api/indicator_metadata/by_group_id?group_ids=",groupid)
  
  ind_url %>%
    gather_object() %>%
    spread_all() %>%
    select(ind_id = IID,Descriptive.Name) -> more_ind_names
  ind_names <- bind_rows(ind_names,more_ind_names)
}
	ind_names <- ind_names %>% distinct
	
	df <- df %>%
	  left_join(ind_names, by = "ind_id")
	
	indList <- indList %>%
	  left_join(ind_names, by = "ind_id") %>% rename(Ind_name = Descriptive.Name)

	df %>%
	  left_join(age) %>%
	  left_join(sex) %>%
	  select(Ind_name = Descriptive.Name,1:9,age_group,gender) -> df_analysis

	group_hp <- paste(unique(groupids$groupid),collapse="%2C")
	group_url <- paste0("http://fingertips.phe.org.uk/api/group_metadata?group_ids=",group_hp)

	group_url %>%
	  gather_array() %>%
	  spread_all() %>%
	  select(groupCode = Id,profileSection = Name) -> group_names

	df_with_groups <- indList %>%
	  left_join(group_names, by = "groupCode") 

# Join area names to data frame
	if(chosenAreaType == 101) {areaInfo <- LTLAs}
	else if(chosenAreaType == 102) {areaInfo <- UTLAs}
	else {areaInfo <- CCGs}
	areaInfo <- areaInfo %>% select(Code.Area,Name.Area)
	df_analysis <- df_analysis %>%
	  left_join(areaInfo, by = c("area" = "Code.Area")) %>%
	  select(ind_id,Ind_name,area, area_name = Name.Area,
	         age_group,gender,period,value,lci,uci,count,denom)

	return(list(df_analysis,df_with_groups %>% select(ind_id,Ind_name,profileSection) %>% distinct))
}

indLists <- eventReactive(input$makeChoice,{getIndList(input$profile,input$atype)}) 

output$Box3 <- renderUI({
  indListDF <- indLists()[[2]]
  indCodeList <- indListDF$ind_id
  names(indCodeList) <- indListDF$Ind_name # want to display names but return codes
  selectInput("indList","Select some indicators",multiple = TRUE,
              choices = lapply(split(indCodeList,indListDF$profileSection),as.list))
})

#output$text4intro <- renderText({paste0("You have selected ",length(input$indList)," indicators:")})
#output$text4 <- renderPrint(input$indList)

# output$Table1 <- DT::renderDataTable({
output$Table1 <- renderDataTable({
  indLists()[[1]] %>% filter(area %in% input$areaList) %>% filter(ind_id %in% input$indList)
})

output$x3 <- 
  
  downloadHandler(filename = function() {paste('Fingertips-data','.csv',sep='')},
                  content = function(file) {
  write.csv(indLists()[[1]] %>% filter(area %in% input$areaList) %>% filter(ind_id %in% input$indList), file)

})  
}

shinyApp(ui=ui, server=server)