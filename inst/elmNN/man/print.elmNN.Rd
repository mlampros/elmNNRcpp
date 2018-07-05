\name{print.elmNN}
\alias{print.elmNN}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Print a summary of the attributes of a trained neural network 
}
\description{
Print the attributes of a \code{elmNN} object.
}
\usage{
\method{print}{elmNN}(x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
an object of class \code{elmNN}.
}
\item{\dots}{not used.}
}
\value{
Furthermore a list of the printed attributes.
\item{nhid}{number of hidden neurons selected}
\item{actfun}{activation function used}
\item{inpweight}{\code{head} of the matrix of input weights ( randomly calculated )}
\item{biashid}{\code{head} of the vector of hidden layer bias ( randomly calculated )}
\item{outweight}{\code{head} of the matrix of output weights ( calculated by the algorithm )}
\item{fitted(x)}{\code{head} of the vector with the output prediction of the training set}
}
\references{
see \code{\link[elmNN]{elmNN-package}} documentation.
}
\author{
Alberto Gosso
}

\seealso{
\code{\link[elmNN]{elmtrain.default}},\code{\link[elmNN]{elmtrain.formula}},\code{\link[elmNN]{predict.elmNN}},\code{\link[elmNN]{elmNN-package}}
}
\examples{
set.seed(1234)
Var1 <- runif(50, 0, 100) 
sqrt.data <- data.frame(Var1, Sqrt=sqrt(Var1))
model <- elmtrain.formula(Sqrt~Var1, data=sqrt.data, nhid=10, actfun="sig")
print(model)

## The function is currently defined as
function (x, ...) 
{
    cat("Call:\n")
    cat(paste(x$call, "\n"))
    cat("Number of hidden neurons:\n")
    cat(paste(x$nhid, "\n"))
    cat("Activation function:\n")
    cat(paste(x$actfun, "\n"))
    cat("Input arc weights:\n")
    cat(paste(head(x$inpweight), "...\n"))
    cat("Bias of hidden neurons:\n")
    cat(paste(head(x$biashid), "...\n"))
    cat("Output arc weights:\n")
    cat(paste(head(x$outweight), "...\n"))
    cat("Predictions on training set:\n")
    cat(paste(head(fitted(x)), "...\n"))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
