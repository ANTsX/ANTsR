library(ANTsR)
library(nlme)
library(lme4)
library(robust)
library(timeDate)
mycsv<-'scripts/bvFTD_longitudinal_summary.csv'
demog<-read.csv(mycsv)
glb<-paste("*jacnorm*.nii.gz",sep='')
fnl<-list.files(path="./jacsmooth/", pattern = glob2rx(glb),full.names = T)
mfn<-"/Volumes/LaCie/MurrayLong/templates/labels/sub_mask.nii.gz"
mask<-antsImageRead(mfn, dimension = 3)
maskv<-( mask == 1 )
nmatcol<-sum(maskv)
mydata<-matrix( rep(0,length(fnl)*nmatcol),ncol=nmatcol,nrow=length(fnl))
mydatadif<-matrix( rep(0,(length(fnl)*nmatcol)/2),ncol=nmatcol,nrow=(length(fnl)/2))
subjid<-rep("",length(fnl))
age<-rep(0,length(fnl))
dx<-rep("",length(fnl))
edu<-rep(NA,length(fnl))
dtime<-rep(NA,length(fnl))
duration<-rep(0,length(fnl))
tauabeta<-rep(NA,length(fnl))
for ( x in c(1:length(fnl)) )
  {
  img<-antsImageRead(fnl[x], dimension=3)
  vecimg<-img[ maskv ]
  mydata[x,]<-vecimg
  subjid[x]<-substr(fnl[x],14,19)
  wh<-which( demog$INDDID == subjid[x] )
  edu[x]<-demog$Education[wh]
  tauabeta[x]<-demog$Tau_Abeta_ratio[wh]
  newbdate<-paste(substr(demog$DOB[wh],1,7),"19",substr(demog$DOB[wh],8,12),sep='')
  newt1date<-paste(substr(demog$Time1[wh],1,7),"20",substr(demog$Time1[wh],8,12),sep='')
  newt2date<-paste(substr(demog$Time2[wh],1,7),"20",substr(demog$Time2[wh],8,12),sep='')
  if ( !is.na(demog$FTDYearOnset[wh]) )
    duration[x]<-as.real(substr(newt1date,8,12))-as.real(demog$FTDYearOnset[wh])+0.5
  dx[x]<-demog$X[wh]
  age[x]<-(as.Date(newt1date,format="%d-%b-%Y")-as.Date(newbdate,format="%d-%b-%Y"))[[1]]/365
  dtime[x]<-(as.Date(newt2date,format="%d-%b-%Y")-as.Date(newt1date,format="%d-%b-%Y"))[[1]]/365
  if ( ( x %% 2 == 0 )  )
    {
    if ( !is.na(demog$FTDYearOnset[wh]) )
      duration[x]<-as.real(substr(newt2date,8,12))-as.real(demog$FTDYearOnset[wh])+0.5
    age[x]<-(as.Date(newt2date,format="%d-%b-%Y")-as.Date(newbdate,format="%d-%b-%Y"))[[1]]/365
    ind<-x/2
    mydatadif[ind,]<-mydata[x,]-mydata[x-1,]
    }
  }
dx<-as.factor(dx)
mxtest<-nmatcol
pvals<-rep(NA,mxtest)
tstat<-rep(NA,mxtest)
inds<-c(1:nrow(mydatadif))*2
ctl<-lmrob.control( "KS2011", max.it = 1000 )
vox<-1
while ( vox <= mxtest )
  {
  mydatafrm<-data.frame( age=age[inds], tau=tauabeta[inds], dtime=dtime[inds], dx=dx[inds], edu=edu[inds],jacobian = mydatadif[,vox] , subjid = subjid[inds] , duration=duration[inds] )
  fm1<-lm( jacobian ~ dx + dtime + edu  , data = mydatafrm )
#  try( fm1<-lmrob( jacobian ~ dx + age + dtime + edu  , data = mydatafrm , control = ctl ) )
  try( pvals[vox]<-summary(fm1)$coeff[2,4] )
  if ( vox %% 1000 == 0 ) { densityplot(mydatafrm$jacobian,groups=mydatafrm$dx); print(summary(fm1)); print(vox/nmatcol*100) ; print(min(p.adjust(pvals,method='BH'),na.rm=T)) }
  vox<-vox+1
  }
# see http://cran.r-project.org/web/packages/lme4/vignettes/Implementation.pdf
# see =>  names(attributes(fm1))
qvals<-1-p.adjust(pvals,method="BH")
mask[maskv]<-qvals
antsImageWrite(mask,'testqvals.nii.gz')

ctl<-lmeControl(maxIter=10000,msMaxIter = 10000, tolerance=1.e-3, msVerbose = FALSE,returnObject=TRUE)
mydatafrm<-data.frame( age=age, duration=duration, dtime=dtime, dx=dx, edu=edu, jacobian = mydata[,vox] , subjid = subjid )
vox<-1
while ( vox <= mxtest )
  {
  jacobian = mydata[,vox]
  fm1<-lmer( jacobian ~ age + edu + ( age | subjid ) )
  fm2<-lmer( jacobian ~ dx + age + edu +  ( age | subjid ) )
  myanv<-anova( fm1, fm2 )
  tstat[vox]<-attributes(summary(fm2))$coefs[2,3]
  pvals[vox]<-myanv$P[[2]] # 
  if ( vox %% 1000 == 0 ) { densityplot(jacobian,groups=dx); print(summary(fm2)); print(vox/nmatcol*100) ; print(min(p.adjust(pvals,method='BH'),na.rm=T)) }
  vox<-vox+1
  }
