#===================================================================================================================================

# REGRESSION
#-----------

data(Boston, package = 'KernelKnn')

df_bst = Boston[1:350, -dim(Boston)[2]]                                           # data.frame to raise an error

Boston = as.matrix(Boston)
dimnames(Boston) = NULL

X = Boston[, -dim(Boston)[2]]
xtr = X[1:350, ]
xte = X[351:nrow(X), ]

ytr_error = as.numeric(Boston[1:350, dim(Boston)[2]])

ytr = matrix(Boston[1:350, dim(Boston)[2]], nrow = length(Boston[1:350, dim(Boston)[2]]), ncol = 1)
yte = matrix(Boston[351:nrow(X), dim(Boston)[2]], nrow = length(Boston[351:nrow(X), dim(Boston)[2]]), ncol = 1)


# CLASSIFICATION
#---------------

data(ionosphere, package = 'KernelKnn')

xtr_class = ionosphere[1:200, -c(2, ncol(ionosphere))]                       # remove second column which has a single unique value
xte_class = ionosphere[201:nrow(ionosphere), -c(2, ncol(ionosphere))]

xtr_class = as.matrix(xtr_class)
dimnames(xtr_class) = NULL

xte_class = as.matrix(xte_class)
dimnames(xte_class) = NULL

ytr_class = as.numeric(ionosphere[1:200, dim(ionosphere)[2]])
yte_class = as.numeric(ionosphere[201:nrow(ionosphere), dim(ionosphere)[2]])

ytr_class = onehot_encode(ytr_class - 1)                                     # class labels should begin from 0
yte_class = onehot_encode(yte_class - 1)                                     # class labels should begin from 0


#===================================================================================================================================
