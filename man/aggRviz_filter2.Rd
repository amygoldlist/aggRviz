% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/aggRviz_filter2.R
\name{aggRviz_filter2}
\alias{aggRviz_filter2}
\title{Filter aggregated data to a chosen level}
\usage{
aggRviz_filter2(data, col_2_delete = NULL, col_2_keep = NULL,
  features = NULL, all_symbol = "", fix_place = TRUE,
  places = c("State.or.Province", "Region", "Country"))
}
\arguments{
\item{data}{data.frame}

\item{col_2_delete}{vector}

\item{col_2_keep}{vector}

\item{features}{vector}

\item{all_symbol}{character}

\item{fix_place}{logical}

\item{places}{vector}
}
\value{
data.frame
}
\description{
The columns to delete are the features that you DO NOT want to stratify by.
This function filters out any row, stratified by those columns,
and filters out any unstratified rows from the other features.
}
\details{
OPTION:  col_2_keep lets you filter "in"

Option:  if it's not a blank for all categories, set all_symbol
This could be NA, or " " or "all".

Option:  if you set features, it will only delete columns in that set

Option:  fix_place allows you to deal with nested places, that is State.or.Province is in Region is in Country.  For now, these are the 3 fixed places:
places[1] subset places[2] subset places[3]

In future release, this will be a variable number of places!  but for now it only works with 3 (ordered) places
}
\examples{
load("../example_data/yummy.Rda")
dat_1

aggRviz_filter2(data = dat_1,col_2_delete= c("Dessert", "Fruit"))


}
