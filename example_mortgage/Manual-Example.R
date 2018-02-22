library("ncpen");

# ~20MB file. This may take a couple of minutes depending on network speed.
prepay.data = read.csv(file = "https://raw.githubusercontent.com/zeemkr/data/master/mtg_term_2011_2012.csv");
head(prepay.data);
dim(prepay.data);

# Data manipulation
ncpen.data = prepay.data;

# Convert a cateorical variables to multiple indicator variables.
ncpen.data = cbind(ncpen.data, to.indicators(subset(ncpen.data, select = "CHANNEL"), exclude.base = TRUE, base = "B"));

# Now remove the categorical variable.
ncpen.data$CHANNEL = NULL;

# Include all possible interactions in X.
# The first column is the beginning of year (BOY) and
# the second column is prepaid indicator (y variable). 
# So, only interact X, ncpen.data[, -c(1,2)].
# Then, bind to the original data set.
ncpen.data = cbind(ncpen.data, interact.data(ncpen.data[, -c(1,2)], base.cols = c("CHANNEL", "loan_age"),
                                       exclude.pair = list(c("FTHB", "Purchase"), c("FTHB", "Primary"))));
head(ncpen.data);

# Train data set, BOY == 2011 and test data set, BOY == 2012
# The second column is y (prepaid = 0 or 1)
# X starts from the third column
y.vec.train = ncpen.data[ncpen.data$BOY == 2011, 2];
x.mat.train = ncpen.data[ncpen.data$BOY == 2011, -c(1,2)];

set.seed(123);
sample.idx = sample(1:length(y.vec.train), 5000);
y.vec.train = y.vec.train[sample.idx];
x.mat.train = x.mat.train[sample.idx,];


y.vec.test = ncpen.data[ncpen.data$BOY == 2012, 2];
x.mat.test = ncpen.data[ncpen.data$BOY == 2012, -c(1,2)];


# 1. GLM test
train.df = as.data.frame(cbind(y.vec.train, x.mat.train));
glm.fit = glm(y.vec.train~., data=train.df,family="binomial");
summary(glm.fit);

# number of coefficients
sum(!is.na(coef(glm.fit)));

# MAE
glm.fit.coef = coef(glm.fit);
glm.fit.coef[is.na(glm.fit.coef)] = 0;
exb.vec = exp(drop(as.matrix(cbind(1, x.mat.test))%*%glm.fit.coef));
ph.vec = exb.vec/(1+exb.vec);
nyh.vec = ph.vec > 0.5;
mean(abs(y.vec.test - nyh.vec));

#sqrt(mean((y.vec.test - ph.vec)^2));

# 2. ncpen test
# This may take a couple of minutes...
cv.ncpen.fit = cv.ncpen(y.vec.train, as.matrix(x.mat.train), family = "binomial", penalty = "scad");

cv.ncpen.coef = as.matrix(cv.ncpen.fit$opt.ebeta);
rownames(cv.ncpen.coef) = c("Intercept", colnames(x.mat.train));
cv.ncpen.coef;

# number of coefficients selected
sum(cv.ncpen.coef!=0)

# MAE
exb.vec = exp(drop(as.matrix(cbind(1, x.mat.test))%*%cv.ncpen.fit$opt.ebeta));
ph.vec = exb.vec/(1+exb.vec);
nyh.vec = ph.vec > 0.5;
mean(abs(y.vec.test - nyh.vec));

#sqrt(mean((y.vec.test - ph.vec)^2));

