autoswithout <- read.csv("autoswithout.csv", stringsAsFactors = F)
modelsandbrand <- read.csv("model_and_brand.csv", stringsAsFactors = F)
library(stringr)
library(dplyr)
library(tidyr)

#create a vector of all the names, in lowercase
autosnames = tolower(autoswithout$name)

#change "mercedes_benz" to "merce" to remove duplicate brand.
modelsandbrand$brand[modelsandbrand$brand == "mercedes_benz"] = "merce"
#get the list of brands
brandnames = levels(factor(modelsandbrand$brand))
#insert "vw" as a brand
brandnames=c(brandnames,"vw")

#initialise a blank vector of the same length as the one with all the names, to store brand results
brands_result = rep(NA_character_, length(autosnames))

#Brand Analysis
for(i in 1:length(brands_result)){
  counter = 0 #used to check how many brand matches
  vwcheck = 0 #used to prevent double-counting of 'volkswagen'
  for(j in 1:length(brandnames)){
    if(grepl(pattern = brandnames[j], autosnames[i])){
      if(brandnames[j]=="vw"|brandnames[j]=="volkswagen"){ #prevent double-counting of volkswagen
        if(vwcheck==0){
          vwcheck=1
          counter = counter + 1 
        }
      }
      else{
        counter = counter + 1
        index = j #store the matching indices
      }
    }
  }
  if (counter == 1){
    if(vwcheck == 1){ #check if flag was raised, to ensure "volkswagen" is stored, instead of "vw"
      brands_result[i] = "volkswagen"
    }
    else{
      brands_result[i] = brandnames[index]
    }
  } else if(counter >1){
    brands_result[i] = "toomanybrands" #temporary placeholder, to prevent these controversial names from being tested again.
  }
}

#create new dataframe modelsandbrand2 with NAs, duplicate models removed
modelsandbrand2 = na.omit(modelsandbrand) #remove rows with NA
dupes = unique(modelsandbrand2$model[duplicated(modelsandbrand2$model)]) #create a list of duplicated models
for(i in dupes){
  indices = grep(pattern = i, x = modelsandbrand2$model, fixed = TRUE) #get vector of indices for the rows with duplicate models
  modelsandbrand2 = modelsandbrand2[-indices,] #remove the rows with duplicate models for modelsandbrand2 
}
row.names(modelsandbrand2) = 1:nrow(modelsandbrand2) #renumber the rownames

#Duplicate Models Detection
#check and flag out any names containing a duplicate model
for(i in 1:length(brands_result)){
  if(is.na(brands_result[i])){ #only check rows without an entry yet
    for(j in 1:length(dupes)){
      #Search for "_pattern_",  "pattern_" at start of string, or "_pattern" at end of string, where _ is a punctuation
      pat = paste("[[:punct:]]",dupes[j], "[[:punct:]]", "|", "^",dupes[j], "[[:punct:]]",  "|", "[[:punct:]]", dupes[j], "$", sep = "")
      if(grepl(pattern = pat, autosnames[i])){
        brands_result[i] = "modelaredupes"
      }
    }
  }
}

#Model Analysis
for(i in 1:length(brands_result)){
  if(is.na(brands_result[i])){ #only check rows without an entry yet
    counter = 0 #used to check how many brand matches
    for(j in 1:length(modelsandbrand2$model)){
      #Search for "_pattern_",  "pattern_" at start of string, or "_pattern" at end of string, where _ is a punctuation
      pat = paste("[[:punct:]]",modelsandbrand2$model[j], "[[:punct:]]", "|", "^",modelsandbrand2$model[j], "[[:punct:]]",  "|", "[[:punct:]]", modelsandbrand2$model[j], "$", sep = "")
      if(grepl(pattern = pat, autosnames[i])){
        counter = counter + 1 
        index = j
      }
    }
    if (counter == 1){
      brands_result[i] = modelsandbrand2$brand[index] #check the corresponding brand and assign it to results vector
    } else if (counter > 1){
      brands_result[i] = "toomanymodels" #temporary placeholder, to prevent these controversial names from being tested again.
    } 
  }
}

#change "merce" back to "mercedes_benz"
brands_result[brands_result == "merce"] = "mercedes_benz" 

#create a final results vector, removing the placeholders
brands_result2 = brands_result 
brands_result2[brands_result2 == "toomanybrands" | brands_result2 == "toomanymodels"] = NA

#Creating Reasons vector
#duplicate brands_result, and assigning "nobrandormodel" to the NA entries
reasons_result = brands_result
reasons_result[which(is.na(reasons_result))] = "nobrandormodel"
#Indexes of all the NA entries
indicesNA = which(is.na(brands_result2))
#Creating a blank Reasons vector
reasonNA = rep(NA_character_, length(indicesNA))
#assigning values to the Reasons vector according to the reasons_result
for (i in 1:length(reasonNA)){
  if (reasons_result[indicesNA[i]]=="toomanybrands"){
    reasonNA[i] = "Too many Brands found in Name"
  }
  else if (reasons_result[indicesNA[i]]=="toomanymodels"){
    reasonNA[i] = "Too many Models found in Name"
  }
  else if (reasons_result[indicesNA[i]]=="modelaredupes"){
    reasonNA[i] = "Model is a duplicate"
  }
  else if (reasons_result[indicesNA[i]]=="nobrandormodel"){
    reasonNA[i] = "Brand nor Model is found in Name"
  }
}

#create df on NA results
returnNA = data.frame(indicesNA,autosnames[indicesNA],brands_result2[indicesNA],reasonNA)
colnames(returnNA) = c("Index", "Name", "Brand","Reason for NA")

#fringe cases analysis

#Fuzzy Search
#new blank dataframe to store results
fringe_fuzzy=data.frame()

for (i in 1:length(autosnames)){
  if(is.na(brands_result[i])){ #only check rows without an entry yet
    for (j in 1:nrow(modelsandbrand2)){
      if (agrepl(modelsandbrand2$brand[j],autosnames[i])){ #agrepl to do a fuzzy search for brand in name
        temp = c(i, autosnames[i], modelsandbrand$brand[j]) #create vector for index, name, brand
        fringe_fuzzy=rbind(fringe_fuzzy, temp, stringsAsFactors = F) #add it to the blank dataframe above
      }
    }
  }
}
names(fringe_fuzzy) = c("Index", "Name", "Brand")

#Mercedes_Benz
#get list of all Mercedes_Benz Classes
mbclasses = modelsandbrand2 %>% 
  subset(brand == "merce") %>% 
  subset(grepl("klasse", model) == T) %>% 
  separate(model, into = c("model1", "model2"), sep = "_")
mbclasses = mbclasses$model1

#new blank dataframe to store results
fringe_mb = data.frame()
temp = c()
for(i in 1:length(autosnames)){ 
  if(is.na(brands_result[i])){ #only check rows without an entry yet
    for(j in mbclasses){ #for each Mercedes_Benz class
      #Search for "_class_xxx_",  "class_xxx_" at start of string, or "_class_xxx" at end of string, where _ is a punctuation, and x is a digit
      pat = paste("^", j, "[[:punct:]][[:digit:]][[:digit:]][[:digit:]][[:punct:]]|[[:punct:]]", j, "[[:punct:]][[:digit:]][[:digit:]][[:digit:]][[:punct:]]|[[:punct:]]", j, "[[:punct:]][[:digit:]][[:digit:]][[:digit:]]$", sep = "")
      if(grepl(pat, autosnames[i])){ 
        temp = c(i, autosnames[i], "mercedes_benz") #create vector for index, name, brand
        fringe_mb = rbind(fringe_mb, temp, stringsAsFactors = F) #add it to the blank dataframe above
      }
    }
  }
}
names(fringe_mb) = c("Index", "Name", "Brand")

#BMW

#get list of all BMW Classes
bmwclasses = modelsandbrand2 %>% 
  subset(brand == "bmw") %>% 
  subset(grepl("er", model) == T) %>% 
  separate(model, into = c("model1", "model2"), sep = "e")
bmwclasses = bmwclasses$model1

#new blank dataframe to store results
fringe_bmw=data.frame()

for(i in 1:length(autosnames)){
  if(is.na(brands_result[i])){ #only check rows without an entry yet
    for(j in bmwclasses){ #for each BMW class
      #Search for "_cxx_",  "cxx_" at start of string, or "_cxx" at end of string, where _ is a punctuation, and x is a digit, and c is the class
      pat = paste("^", j, "[[:digit:]][[:digit:]][[:punct:]]|[[:punct:]]", j, "[[:digit:]][[:digit:]][[:punct:]]|[[:punct:]]", j, "[[:digit:]][[:digit:]]$", sep = "")
      if(grepl(pat, autosnames[i])){
        temp = c(i, autosnames[i], "bmw") #create vector for index, name, brand
        fringe_bmw = rbind(fringe_bmw, temp, stringsAsFactors = F) #add it to the blank dataframe above
      }
    }
  }
}
names(fringe_bmw) = c("Index", "Name", "Brand")