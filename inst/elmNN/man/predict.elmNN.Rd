\name{predict.elmNN}
\alias{predict.elmNN}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculate the output of the ELM-trained neural network
}
\description{
Calculate the output predictions from a neural network trained using \code{elmtrain}. It is possible to calculate output predictions from a new data set or returns the output predictions from the previous training data set ( \code{fitted} data ).
}
\usage{
\method{predict}{elmNN}(object, newdata = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
an object of class \code{elmNN} ( a trained neural network )
}
  \item{newdata}{
(optional) a new data set to calculate output from the model. If missing, \code{predict} returns the output prediction from the training set.  
}
\item{\dots}{not used.}
}

\value{
returns a vector containing the output predictions.
}
\references{
see \code{\link[elmNN]{elmNN-package}} documentation.
}
\author{
Alberto Gosso
}

\seealso{
\code{\link[elmNN]{elmtrain.default}},\code{\link[elmNN]{elmtrain.formula}},\code{\link[elmNN]{elmNN-package}}
}
\examples{
set.seed(1234)
Var1 <- runif(50, 0, 100) 
sqrt.data <- data.frame(Var1, Sqrt=sqrt(Var1))
model <- elmtrain.formula(Sqrt~Var1, data=sqrt.data, nhid=10, actfun="sig")
new <- data.frame(Sqrt=0,Var1 = runif(50,0,100))
p <- predict(model,newdata=new)

## The function is currently defined as
function (object, newdata = NULL, ...) 
{
    if (is.null(newdata)) 
        predictions <- fitted(object)
    else {
        if (!is.null(object$formula)) {
            x <- model.matrix(object$formula, newdata)
        }
        else {
            x <- newdata
        }
        inpweight <- object$inpweight
        biashid <- object$biashid
        outweight <- object$outweight
        actfun <- object$actfun
        nhid <- object$nhid
        TV.P <- t(x)
        tmpHTest = inpweight \%*\% TV.P
        biasMatrixTE <- matrix(rep(biashid, ncol(TV.P)), nrow = nhid, 
            ncol = ncol(TV.P), byrow = F)
        tmpHTest = tmpHTest + biasMatrixTE
        if (actfun == "sig") 
            HTest = 1/(1 + exp(-1 * tmpHTest))
        else {
            if (actfun == "sin") 
                HTest = sin(tmpHTest)
            else {
                if (actfun == "radbas") 
                  HTest = exp(-1 * (tmpHTest^2))
                else {
                  if (actfun == "hardlim") 
                    HTest = hardlim(tmpHTest)
                  else {
                    if (actfun == "hardlims") 
                      HTest = hardlims(tmpHTest)
                    else {
                      if (actfun == "satlins") 
                        HTest = satlins(tmpHTest)
                      else {
                        if (actfun == "tansig") 
                          HTest = 2/(1 + exp(-2 * tmpHTest)) - 
                            1
                        else {
                          if (actfun == "tribas") 
                            HTest = tribas(tmpHTest)
                          else {
                            if (actfun == "poslin") 
                              HTest = poslin(tmpHTest)
                            else {
                              if (actfun == "purelin") 
                                HTest = tmpHTest
                              else stop(paste("ERROR: ", actfun, 
                                " is not a valid activation function.", 
                                sep = ""))
                            }
                          }
                        }
                      }
                    }
                  }
                }
            }
        }
        TY = t(t(HTest) \%*\% outweight)
        predictions <- t(TY)
    }
    predictions
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{neural}
