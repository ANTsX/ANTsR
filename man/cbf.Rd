\name{CBF}
\alias{cbf}
\alias{casl}
\alias{pasl}

\title{
CBF Computation
}

\description{
Calculate CBF from an ASL image using CASL or PASL.
}

\usage{
# ASLtbx
cbf_casl1_ASLtbx( aslimg_filename , m0img_filename )
cbf_casl2_ASLtbx( aslimg_filename )
cbf_pasl_ASLtbx( aslimg_filename , m0img_filename )
# Chen 2011 paper
cbf_casl_Chen2011( aslimg_filename )
cbf_pasl_Chen2011( aslimg_filename , m0img_filename )
cbf_pcasl_Chen2011( aslimg_filename )
# Wang 2012 paper
cbf_casl_Wang2012( aslimg_filename , Xvar , Xideal = NULL , c = NULL )
} 

\arguments{
\item{aslimg_filename}{
Name of the 4D ASL image file. 
}

\item{m0img_filename}{
Name of the 3D M0 image file.
}

\item{Xvar}{
Matrix or vector. For an image of size 64 * 64 * 18 * 116, Xvar can be a vector of length 116 or a matrix of size 116 * (64*64*18)
}

\item{Xideal}{
Matrix or vector or unspecified. For an image of size 64 * 64 * 18 * 116, Xideal can be a vector of length 116 or a matrix of size 116 * (64*64*18) or left unspecified to use the version from Wang2012 paper.
}

\item{c}{
Matrix or numeric scalar or unspecified. For an image of size 64 * 64 * 18 * 116, c can be a matrix of size 64 * 64 * 18 or a scalar or left unspecified to use the version from Wang2012 paper.
}
}

\details{
In all of the scripts, the cbf calculate is of dimensions [ x , y , z , t ] \cr

ASLtbx( https://cfn.upenn.edu/~zewang/ASLtbx.php ) \cr 
----------------------------------------------------------------------------

CBF calculation using 'cbf_casl1_ASLtbx'. \cr
# Following are the parameters used in the ASLtbx \cr
# First image: label \cr
# Subtraction Method: simple subtraction \cr
# Subtraction Order: control - label \cr
# Applying Mask for Output: yes \cr
# Create Mean Images: yes \cr
# Calculate qCBF: yes \cr
# Output PseudoBOLD Images: no \cr
# Save deltaM Images: no \cr
# Save qCBF Images: yes \cr
# Using a unique M0 value for all voxels: no \cr
# Output Image Format: nifti \cr
# Saving 4D Image Series: yes \cr
# Select ASL type: CASL \cr
# Magnetic Field: 3T \cr
# Label Time (sec): 2 \cr
# Post-Labeling Delay Time (sec): 0.8 \cr
# Slice Acquisition Time (msec): 45 \cr
# Use Additionally acquired M0 Image for CASL: no \cr
# Label Efficiency: 0.68 \cr
for every voxel in cbf image, \cr
cbf <- ( 6000 * 1000 * lambda * r1a * efftcbf ) /
       ( 2 * labeff * ( exp( -omega * r1a ) - exp( -( labeltime + omega ) * r1a ) ) ) \cr
where, \cr
lambda <- 0.9 \cr
bloodt1 <- 1664 \cr
r1a <- 1 / bloodt1 \cr
control <- control voxel value -- ASL image at t = 2 , 4 , 6 , ... even times \cr
label <- label voxel value -- ASL image at t = 1 , 3 , 5 , ... odd times \cr
efftcbf <- ( control - label ) / m0 \cr
labeff <- 0.68 \cr
delaytime <- 800 \cr
slicetime <- 45 \cr
slice <- slice the voxel belongs to ( 1 , 2 , 3 , ... , num-of-slices ) \cr
omega <- delaytime + (slice-1) * slicetime \cr
labeltime <- 2000 \cr
----------------------------------------------------------------------------

CBF calculation using 'cbf_casl2_ASLtbx'. \cr
# Following are the parameters used in ASLtbx \cr
# First image: label \cr
# Subtraction Method: simple subtraction \cr
# Subtraction Order: control - label \cr
# Applying Mask for Output: yes \cr
# Create Mean Images: yes \cr
# Calculate qCBF: yes \cr
# Output PseudoBOLD Images: no \cr
# Save deltaM Images: no \cr
# Save qCBF Images: yes \cr
# Using a unique M0 value for all voxels: yes \cr
# Output Image Format: nifti \cr
# Saving 4D Image Series: yes \cr
# Select ASL type: CASL \cr
# Magnetic Field: 3T \cr
# Label Time (sec): 2 \cr
# Post-Labeling Delay Time (sec): 0.8 \cr
# Slice Acquisition Time (msec): 45 \cr
# Use Additionally acquired M0 Image for CASL: no \cr
# Label Efficiency: 0.68 \cr
for every voxel in the cbf image, \cr
cbf <- ( 6000 * 1000 * lambda * r1a * efftcbf ) /
       ( 2 * labeff * ( exp( -omega * r1a ) - exp( -( labeltime + omega ) * r1a ) ) ) \cr
where, \cr
lambda <- 0.9 \cr
bloodt1 <- 1664 \cr
r1a <- 1 / bloodt1 \cr
control <- control voxel value -- ASL image at t = 2 , 4 , 6 , ... even times \cr
label <- label voxel value -- ASL image at t = 1 , 3 , 5 , ... odd times \cr
efftcbf <- ( control - label ) / control \cr
labeff <- 0.68 \cr
slicetime <- 45 \cr
slice <- slice the voxel belongs to ( 1 , 2 , 3 , ... , num-of-slices ) \cr
omega <- delaytime + (slice-1) * slicetime \cr
labeltime <- 2000 \cr
----------------------------------------------------------------------------

CBF calculation using cbf_pasl_ASLtbx. \cr
# Following are the parameters used in the ASLtbx \cr
# First image: label \cr
# Subtraction Method: simple subtraction \cr
# Subtraction Order: control - label \cr
# Applying Mask for Output: yes \cr
# Create Mean Images: yes \cr
# Calculate qCBF: yes \cr
# Output PseudoBOLD Images: no \cr
# Save deltaM Images: no \cr
# Save qCBF Images: yes \cr
# Using a unique M0 value for all voxels: no \cr
# Output Image Format: nifti \cr
# Saving 4D Image Series: yes \cr
# Select ASL type: PASL \cr
# Magnetic Field: 3T \cr
# Post-Labeling Delay Time (sec): 0.8 \cr
# Slice Acquisition Time (msec): 45 \cr
# TE (msec): 20 \cr
# Select M0 Image: m0.nii \cr
# Draw ROI for calculating CBF: yes \cr
# Label Efficiency: 0.95 \cr
for every voxel in the cbf image, \cr
cbf <- ( 6000 * 1000 * lambda * effperf ) /
       ( 2 * m0 * exp( -TI / bloodt1 ) * TI1 * labeff * qTI ) \cr
where, \cr
lambda <- 0.9 \cr
control <- control voxel value -- ASL image at t = 2 , 4 , 6 , ... , even times \cr
label <- label voxel value -- ASL image at t = 1 , 3 , 5 , ... , odd times \cr
effperf <- control - label \cr
slicetime <- 45 \cr
slice <- slice the voxel belongs to ( 1 , 2 , 3 , ... , num-of-slices ) \cr
TI1 <- 700 \cr
TI <- TI1 + delaytime + (slice-1) * slicetime \cr
bloodt1 <- 1664 \cr
labeff <- 0.95 \cr
qTI <- 0.85 \cr
----------------------------------------------------------------------------

'Test–Retest Reliability of Arterial Spin Labeling With Common Labeling Strategies' \cr
Yufen Chen, PhD, Danny J.J. Wang, PhD and John A. Detre, MD \cr
JOURNAL OF MAGNETIC RESONANCE IMAGING 33:940–949 (2011) \cr
----------------------------------------------------------------------------

CBF calculation for CASL. \cr
for every voxel in the cbf image, \cr
cbf <- ( lambda * deltaM ) / 
       ( 2 * alpha * M0 * T1b * ( exp( -omega / T1b ) - exp( -( tau + omega ) / T1b ) ) ) \cr
where, \cr
lamda <- 0.9
control <- control voxel value -- ASL image at t = 2 , 4 , 6 , ... , even times \cr
label <- label voxel value -- ASL image at t = 1 , 3 , 5 , ... , odd times \cr
deltaM <- control - label \cr
alpha <- 0.68 \cr
M0 <- mean( control ) \cr
T1b <- 1664 \cr
omega <- 1 \cr
tau <- 2 \cr
----------------------------------------------------------------------------

CBF calculation for pCASL. \cr
for every voxel in the cbf image, \cr
cbf <- ( lambda * deltaM ) / 
       ( 2 * alpha * M0 * T1b * ( exp( -omega / T1b ) - exp( -( tau + omega ) / T1b ) ) ) \cr
where, \cr
lambda <- 0.9
control <- control voxel value -- ASL image at t = 2 , 4 , 6 , ... , even times \cr
label <- label voxel value -- ASL image at t = 1 , 3 , 5 , ... , odd times \cr
deltaM <- control - label \cr
alpha <- 0.85 \cr
M0 <- mean( control ) \cr
T1b <- 1664 \cr
omega <- 1 \cr
tau <- 1.5 \cr
----------------------------------------------------------------------------

CBF calculation for PASL. \cr
for every voxel in the cbf image, \cr
cbf <- ( lambda * deltaM ) / 
       ( 2 * alpha * M0 * TI1 * exp( -TI2 / T1b ) ) \cr
where, \cr
lambda <- 0.9 \cr
control <- control voxel value -- ASL image at t = 2 , 4 , 6 , ... , even times \cr
label <- label voxel value -- ASL image at t = 1 , 3 , 5 , ... , odd times \cr
deltaM <- control - label \cr
alpha <- 0.95 \cr
M0 <- equilibrium magnetization from M0 acquisition (input image) \cr
TI1 <- 700 \cr
TI2 <- 1700 \cr
T1b <- 1664 \cr
----------------------------------------------------------------------------

'Improving cerebral blood flow quantification for arterial spin labeled perfusion MRI by removing residual motion artifacts and global signal fluctuations' \cr
Wang \cr
Magnetic Resonance Imaging xx (2012) xxx – xxx \cr
----------------------------------------------------------------------------

CBF calculation for CASL. \cr
for every voxel in the cbf image, \cr
cbf <- ( Bideal * lambda * R1a * exp( w * R1a ) ) / 
       ( 2 * c * alpha * ( 1 - exp( -tau * R1a ) ) ) \cr
where, \cr
Xideal <- ideal label and control signals in the time series \cr
Xvar <- represents variations in Xideal in the time series (provide by user) \cr
Y <- voxel time series \cr
Bideal <- fitting coefficient for Xideal under a general linear model ( Y ~ Xideal + Xvar ) \cr
lamda <- 0.9 # stolen \cr
T1a <- 1.6 # stolen \cr
R1a <- 1 / T1a \cr
w <- 1 \cr
alpha <- 0.95 \cr
tau <- 2 \cr
c <- mean( Y ) or value provided by user \cr
----------------------------------------------------------------------------
}

\value{
cbf -- 4D array of cbf values with extent of the last dimension equal to number of terms in the finite difference
}

\author{
Shrinidhi KL
}

\examples{
\dontrun{
cbf <- cbf_casl1( "asl.nii" , "m0.nii" )cbf <- cbf_casl2( "asl.nii" )
cbf <- cbf_pasl( "asl.nii" , "m0.nii" )
}
}
