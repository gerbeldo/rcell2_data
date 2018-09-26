\name{aggregate}
\alias{aggregate.cell.data}
\alias{aggregateBy}
\alias{aggregateBy.cell.data}
\alias{aggregateBy.data.frame}
\alias{aggregateBy.default}
                             
\title{Compute Summary Statistics of Cell Data Subsets}

\description{
 Splits the data into subsets, computes summary statistics for each, and returns the result in a data frame
}
\usage{
\method{aggregate}{cell.data}(x, form.by, ..., FUN=mean, subset=TRUE, select=NULL
  ,exclude=NULL, QC.filter=TRUE)
  
aggregateBy(x,.by,...)

\method{aggregateBy}{cell.data}(x, .by, select, ..., FUN=mean, subset=TRUE, exclude=NULL, QC.filter=TRUE)

\method{aggregateBy}{data.frame}(x,.by,select="all",...,FUN=mean,subset=NULL,exclude=NULL)

\method{aggregateBy}{default}(x,.by,select="all",...,FUN=mean,subset=NULL,exclude=NULL)
  
}

\arguments{
  \item{x}{ cell.data object }
  \item{form.by}{either a formula or variables to split data frame by, as quoted variables or character vector}
  \item{.by}{variables to split data frame by, as quoted variables or character vector}
  \item{\dots}{further arguments passed to or used by methods}
  \item{FUN}{a function to compute the summary statistics which can be applied to all data subsets}
  \item{subset}{a boolean vector of length equal to the number of rows of the
  dataset, or a conditional statement using the datasets variable, that
  specifies which registers should be included}
  \item{select}{character vector defining variables names to be included in the returned data.frame}
  \item{exclude}{character vector defining variables names to be excluded from the returned data.frame}
  \item{QC.filter}{a boolean value indicating if the quality control filter should 
    be applied over the data}
}


\details{
  \code{\link{aggregate}} is a generic function. This version applies to cell.data objects. Two notations are allowed. 
  If the second argument \code{form.by} is a formula it should be of the form
 \code{cbind(measure.var1,measure.var2)~group.var1+group.var2}
  If the second argument \code{form.by} are quoted variables or a character vector with variable names, these variables are taken as group.vars to split the dataset. The measure variables over which to apply FUN should be selected using the \code{select} and \code{exclude} arguments.
 
 \code{aggregateBy} works as aggregate, but forces the use of quoted variables (or variable names) to define the groups by which the dataset is going to be split. This function also has a implementation for data frames. \code{aggregateBy} calls \code{flatten} before returning the data frame.
 
 }
\value{
  a data frame with columns corresponding to the grouping variables followed by aggregated columns of the measure variables.
}
\author{ Alan Bush }
\seealso{ \code{\link{aggregate}} }
\examples{
if(require(RcellData)){
 
  #load example dataset
   data(ACL394filtered)
  
  #aggregate by pos and calculate mean f.tot.y
  aggregate(X,.(pos),select="f.tot.y")
  
  #do the same aggregation using the formula notation
  aggregate(X,f.tot.y~pos)
  
  #aggregate by pos and t.frame
  aggregate(X,.(pos,t.frame),select="f.tot.y")
  aggregate(X,f.tot.y~pos+t.frame) #formula notation
  
  #aggregate several variables
  aggregate(X,.(pos),select="f.tot.?") # using wildcard pattern matching
  aggregate(X,cbind(f.tot.y,f.tot.c)~pos) #formula notation
  
  #subset before aggregating
  aggregate(X,.(pos),select="f.tot.y",subset=t.frame==13)
  
  #calculate the median instead of the mean
  aggregate(X,.(pos),select="f.tot.y",FUN=median)
  
  #dont apply the QC filter to the daset before aggregation
  aggregate(X,.(pos),select="f.tot.y",QC.filter=FALSE)
  
  #use aggregateBy on a cell.data object
  (agg<-aggregateBy(X,.(pos,AF.nM,t.frame),select="f.tot.y"))
  
  #use aggregateBy on a data.frame, calculate mean and sd among position means
  aggregateBy(agg,.(AF.nM,t.frame),select="f.tot.y",FUN=funstofun(mean,sd))

}
}
\keyword{manip}
\keyword{methods}

