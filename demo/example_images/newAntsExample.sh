#!/bin/bash 
#
dim=2 # image dimensionality 
AP="" # /home/yourself/code/ANTS/bin/bin/  # path to ANTs binaries 
ITK_GLOBAL_DEFAULT_NUMBER_OF_THREADS=2  # controls multi-threading 
export ITK_GLOBAL_DEFAULT_NUMBER_OF_THREADS
f=$1 ; m=$2    # fixed and moving image file names 
if [[ ! -s $f ]] ; then echo no $f ; exit; fi
if [[ ! -s $m ]] ; then echo no $m ;exit; fi
nm1=` basename $f | cut -d '.' -f 1 `
nm2=` basename $m | cut -d '.' -f 1 `
nm=${D}${nm1}_fixed_${nm2}_moving   # construct output prefix 
reg=${AP}antsRegistration           # path to antsRegistration
echo affine $m $f outname is $nm 
$reg -d $dim  \
                        -m mattes[  $f, $m , 1 , 32, regular, 0.1 ] \
                         -t affine[ 0.5 ] \
                         -c [500000x30000x30000,2.e-7,20]  \
                        -s 3x2x1  \
                        -f 4x2x1 -l 1  -u 1 \
                        -m mattes[  $f, $m , 1 , 32 ] \
                         -t syn[ .5, 3, 0.0 ] \
                         -c [30x30,1.e-8,20]  \
                        -s 1x0  \
                        -f 2x1 -l 1 \
                       -o [${nm},${nm}_diff.nii.gz,${nm}_inv.nii.gz] 

# ${AP}antsApplyTransforms -d $dim -i $m -r $f -n linear -t ${nm}1Warp.nii.gz -t ${nm}0Affine.mat -o ${nm}_warped.nii.gz
ANTSJacobian $dim ${nm}1Warp.nii.gz  ${nm} 1 0