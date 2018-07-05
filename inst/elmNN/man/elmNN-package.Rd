\name{elmNN-package}
\alias{elmNN-package}
\alias{elmNN}
\docType{package}
\title{
Implementation of ELM ( Extreme Learning Machine ) algorithm for neural networks
}
\description{
ELM algorithm is an alternative training method for SLFN ( Single Hidden Layer Feedforward Networks ) which does not need any iterative tuning nor setting parameters such as learning rate, momentum, etc., which are current issues of the traditional gradient-based learning algorithms ( like backpropagation ).\cr
Training of a SLFN with ELM is a three-step learning model:\cr
Given a training set P = \{(xi , ti )|xi E R , ti E R , i = 1,..., N\}, hidden node
output function G(a, b, x), and the number of hidden nodes L\cr
1) Assign randomly hidden node parameters (ai , bi ), i = 1,..., L. It means that the arc weights between the input layer and the hidden layer and the hidden layer bias are randomly generated.\cr
2) Calculate the hidden layer output matrix H using one of the available activation functions.\cr
3) Calculate the output weights B: B = ginv(H) \%*\% T ( matrix multiplication ), where T is the target output of the training set.\cr
ginv(H) is the Moore-Penrose generalized inverse of hidden layer output matrix H. This is calculated by the MASS package function \code{\link[MASS]{ginv}}.\cr
Once the SLFN has been trained, the output of a generic test set is simply Y = H \%*\% B ( matrix multiplication ).
Salient features:\cr
- The learning speed of ELM is extremely fast.\cr
- Unlike traditional gradient-based learning algorithms which only work for differentiable activation functions, ELM works for all bounded nonconstant piecewise continuous activation functions.\cr
- Unlike traditional gradient-based learning algorithms facing several issues like local minima, improper learning rate and overfitting, etc, ELM tends to reach the solutions straightforward without such trivial issues.\cr
- The ELM learning algorithm looks much simpler than other popular learning algorithms: neural networks and support vector machines.\cr
}
\details{
\tabular{ll}{
Package: \tab elmNN\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2012-07-17\cr
License: \tab GPL (>= 2) \cr
}
To fit a neural network, the function to use is \code{elmtrain} ( default version is \code{elmtrain.formula} ). To predict values, the function to use is \code{predict}. Other functions are used internally by the training and predict functions.
}
\author{
Alberto Gosso

Maintainer: Alberto Gosso <gosso.alberto@gmail.com>
}
\references{
\url{http://www.ntu.edu.sg/home/egbhuang/}\cr
G.-B. Huang, H. Zhou, X. Ding, R. Zhang (2011) \emph{Extreme Learning Machine for Regression and Multiclass Classification} IEEE Transactions on Systems, Man, and Cybernetics - part B: Cybernetics, vol. 42, no. 2, 513-529\cr
G.-B. Huang, Q.-Y. Zhu, C.-K. Siew (2006) \emph{Extreme learning machine: Theory and applications} Neurocomputing 70 (2006) 489-501
}
\keyword{ models|regression|nonlinear}
\seealso{
\code{\link[elmNN]{elmtrain.formula}} to train a neural network,\code{\link[elmNN]{predict.elmNN}} to predict values from a trained neural network
}
\examples{
set.seed(1234)
Var1 <- runif(50, 0, 100) 
sqrt.data <- data.frame(Var1, Sqrt=sqrt(Var1))
model <- elmtrain(Sqrt~Var1, data=sqrt.data, nhid=10, actfun="sig")
new <- data.frame(Sqrt=0,Var1 = runif(50,0,100))
p <- predict(model,newdata=new)

Var2 <- runif(50, 0, 10) 
quad.data <- data.frame(Var2, Quad=(Var2)^2)
model <- elmtrain(Quad~Var2, data=quad.data, nhid=10, actfun="sig")
new <- data.frame(Quad=0,Var2 = runif(50,0,10))
p <- predict(model,newdata=new)
}
