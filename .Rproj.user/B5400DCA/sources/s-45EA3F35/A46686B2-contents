# Subset Selection Methods - Best Subset Selection.

library(ISLR)

names(Hitters)


dim(Hitters)

# Removing NA from Hitters 

Hitters = na.omit(Hitters)

# Dimensions after removing NA.
dim(Hitters)


# the Regsubsets function is part of the leaps library 

library(leaps)


# calling regsubsets 

regfit.full = regsubsets(Salary~.,Hitters)


summary(regfit.full)

summary(regfit.full)$which 

summary(regfit.full)$rss


# Set number of variables to 19 

regfit.full19 = regsubsets(Salary~.,data=Hitters,nvmax=19)


regfit.full19.summary = summary(regfit.full19)

regfit.full19.summary$which 

regfit.full19.summary$rss


#          AtBat Hits HmRun Runs RBI Walks Years CAtBat CHits CHmRun CRuns CRBI CWalks LeagueN DivisionW PutOuts Assists
# 1  ( 1 )  " "   " "  " "   " "  " " " "   " "   " "    " "   " "    " "   "*"  " "    " "     " "       " "     " "    
# 2  ( 1 )  " "   "*"  " "   " "  " " " "   " "   " "    " "   " "    " "   "*"  " "    " "     " "       " "     " "    
# 3  ( 1 )  " "   "*"  " "   " "  " " " "   " "   " "    " "   " "    " "   "*"  " "    " "     " "       "*"     " "    
# 4  ( 1 )  " "   "*"  " "   " "  " " " "   " "   " "    " "   " "    " "   "*"  " "    " "     "*"       "*"     " "    
# 5  ( 1 )  "*"   "*"  " "   " "  " " " "   " "   " "    " "   " "    " "   "*"  " "    " "     "*"       "*"     " "    
# 6  ( 1 )  "*"   "*"  " "   " "  " " "*"   " "   " "    " "   " "    " "   "*"  " "    " "     "*"       "*"     " "    
# 7  ( 1 )  " "   "*"  " "   " "  " " "*"   " "   "*"    "*"   "*"    " "   " "  " "    " "     "*"       "*"     " "    
# 8  ( 1 )  "*"   "*"  " "   " "  " " "*"   " "   " "    " "   "*"    "*"   " "  "*"    " "     "*"       "*"     " "   

# We can see in #7 instance , CRBI is not choosen even though it was choosen from #1 to #6

# plotting Values 


plot(regfit.full19.summary$rss, xlab = "Number of Variables used",ylab="RSS") + 
points(which.min(regfit.full19.summary$rss),min(regfit.full19.summary$rss),col="red",cex=1.5,pch=15)


plot(regfit.full19.summary$adjr2, xlab = "Number of Variables used",ylab="Adjusted RSq") + 
points(which.max(regfit.full19.summary$adjr2),max(regfit.full19.summary$adjr2),col="red",cex=1.5,pch=15)


plot(regfit.full19.summary$cp, xlab = "Number of Variables used",ylab="Cp") + 
points(which.min(regfit.full19.summary$cp),min(regfit.full19.summary$cp),col="red",cex=1.5,pch=15)

plot(regfit.full19.summary$bic, xlab = "Number of Variables used",ylab="Cp") + 
points(which.min(regfit.full19.summary$bic),min(regfit.full19.summary$bic),col="red",cex=1.5,pch=15)


