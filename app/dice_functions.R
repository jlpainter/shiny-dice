## ###########################################################
##
## dice/dice_functions.R
##
## Educational shiny app to teach students the ideas
## behind a chi-square distribution using the investigation
## of different dice making companies.
##
## This module contains some functions that will be reused
## in the app.R program.
##
## @author: Craig Lazarski & Jeffery Painter
## @modified: 2020-Dec-24
##
## ###########################################################

# Returns string without leading white space
trim.leading <- function (x)  sub("^\\s+", "", x)

# Returns string without trailing white space
trim.trailing <- function (x) sub("\\s+$", "", x)

# Returns string without leading or trailing white space
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

## ###########################################################
## Return the list of company names as a vector
## ###########################################################
getDiceCompanyNamesAsVector <- function()
{
  company_ids = c()
  company_names = c()
  allCompanies = getDiceCompanyNames()
  for (row in 1:nrow(allCompanies) )
  {
    entry = allCompanies[row,]
    company_ids = c( company_ids, entry$CompanyId  )
    company_names = c( company_names, entry$CompanyName  )
  }

  # Assign the names to the IDs
  names(company_ids) <- company_names
  return(company_ids)
}

## ###########################################################
## Return the list of company names as a dataframe
## ###########################################################
getDiceCompanyNames <- function()
{
  # Our dice company names
  infile = paste("company_names.txt", sep="")
  companyNames = read.csv(infile, header = T, stringsAsFactors = F)
  return(companyNames)
}

## ###########################################################
## Lookup a company name from it's ID value
## ###########################################################
getCompanyById <- function(companyId)
{
  allCompanies = getDiceCompanyNames()
  for (row in 1:nrow(allCompanies) )
  {
    entry = allCompanies[row,]
    if ( entry$CompanyId == companyId )
    {
      return(entry$CompanyName)
    }
  }
  return("")
}


## ###########################################################
##
## Load the company weights from text file
##
## Note, this expects the weights to be specified
## in fractional notation in the CSV input file.
## ###########################################################
getAllCompanyWeights <- function()
{
  library(stringr)
  # How many companies are configured?
  totalCompanies = nrow(getDiceCompanyNames())
  
  # Our dice company names
  infile = paste("company_weights.csv", sep="")
  companyWeights = read.csv(infile, header = T, stringsAsFactors = F)
  allWeights = list()
  index = 1
  for ( row in 1:nrow(companyWeights) )
  {
    entry = companyWeights[row, ]
    entryWeights = c()
    for ( colidx in 1:length(colnames(entry) ) )
    {
      value = trim(entry[, colidx ])
      fraction = str_split(value, "/")
      numerator = as.numeric(fraction[[1]][1])
      denominator = as.numeric(fraction[[1]][2])
      decimal = (numerator*1.0) / (denominator*1.0)
      entryWeights = c(entryWeights, decimal)
    }
    
    allWeights[[index]] = entryWeights
    index = index + 1
  }

  return(allWeights)
}



## ###########################################################
## Compute the distributions of the company dice rolls
## ###########################################################
getMyDistributions <- function() {
  my_weights = getAllCompanyWeights()
  
  my_distributions = list()
  current_company_index = 1
  for ( company_weights in my_weights )
  {
    max = 10000
    distribution = c()
    dice_value = 1
    for ( dice_weight in company_weights )
    {
      repeat_roll = floor(max*dice_weight)
      repeated_roll = rep.int( dice_value, repeat_roll )
      distribution = c(distribution, repeated_roll)
      dice_value = dice_value + 1
    }
    
    # Adding vec to list 
    my_distributions[[current_company_index]]<- distribution
    current_company_index = current_company_index + 1
  }
  return(my_distributions)
}


## ###########################################################
## Get the weights of a single company
## ###########################################################
getCompanyWeight <- function(companyId)
{
  current_company_index = 1
  for ( weights in getAllCompanyWeights() )
  {
    if ( current_company_index == companyId )
      return(weights)
    current_company_index = current_company_index + 1
  }
  return(NULL)
}