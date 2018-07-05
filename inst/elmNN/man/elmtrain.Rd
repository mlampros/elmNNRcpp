\name{elmtrain}
\alias{elmtrain}
\alias{elmtrain.default}
\alias{elmtrain.formula}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Training of a SLFN (Single Hidden-layer Feedforward Neural Network)
}
\description{
Training of a generic SLFN using ELM algorithm. First it generates input weights and hidden layer bias ( both randomly choosen ), then calculates the output from the hidden layer ( given a particular activation function as a parameter ) and at the end calculates output weights of the neural network. It returns an ELM model ( an object of class \code{elmNN} ) representing the trained neural network. 
}
\usage{
elmtrain(x, ...)
\method{elmtrain}{formula}(formula, data, nhid, actfun, ...)
\method{elmtrain}{default}(x, y, nhid, actfun, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{
a symbolic description of the model to be fitted.
}
  \item{data}{
training data frame containing the variables specified in \code{formula.}
}
  \item{x}{
training dataset.
}
  \item{y}{
target output of the training dataset.
}
  \item{nhid}{
number of hidden neurons. Must be >= 1.
}
  \item{actfun}{
type of activation function. Furthermore a list of the implemented activation functions.\cr
- sig: sigmoid\cr
- sin: sine\cr
- radbas: radial basis\cr
- hardlim: hard-limit\cr
- hardlims: symmetric hard-limit\cr
- satlins: satlins\cr
- tansig: tan-sigmoid\cr
- tribas: triangular basis\cr
- poslin: positive linear\cr
- purelin: linear\cr
}
\item{\dots}{not used.}
}
\details{
\bold{Note:} since part of ELM algorithm is random ( setting of the input weights and hidden layer bias ), output results of same activation function and same number of hidden neurons may change. To find the most accurate error rate for a fixed setting, it is convenient to make various test on the datasets (i.e. 20 times) using the same settings and calculate the mean error from these tests. 
}
\value{
returns the trained neural network, an object of class \code{elmNN}.
\item{nhid }{number of hidden neurons selected}
\item{actfun }{activation function used}
\item{inpweight }{matrix of input weights ( randomly choosen )}
\item{biashid }{vector of hidden layer bias ( randomly choosen )}
\item{outweight }{matrix of output weights ( calculated by the algorithm )}
}
\references{
see \code{\link[elmNN]{elmNN-package}} documentation.
}
\author{
Alberto Gosso
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link[elmNN]{elmtrain.formula}},\code{\link[elmNN]{elmtrain.default}},\code{\link[elmNN]{predict.elmNN}},\code{\link[elmNN]{elmNN-package}}
}
\examples{
set.seed(1234)
##'formula' version
Var1 <- runif(50, 0, 100) 
sqrt.data <- data.frame(Var1, Sqrt=sqrt(Var1))
model <- elmtrain(Sqrt~Var1, data=sqrt.data, nhid=10, actfun="sig")
new <- data.frame(Sqrt=0,Var1 = runif(50,0,100))
p <- predict(model,newdata=new)

##Default version
Var2 <- runif(50, 0, 10) 
quad.data <- data.frame(Var2, Quad=(Var2)^2)
model <- elmtrain(x=quad.data$Var2, y=quad.data$Quad, nhid=10, actfun="sig")
new <- data.frame(Quad=0,Var2 = runif(50,0,10))
p <- predict(model,newdata=new$Var2)

## The function is currently defined as
function (x, ...) 
UseMethod("elmtrain")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{neural}
