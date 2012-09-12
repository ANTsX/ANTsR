\name{Motion-Correction}
\alias{MotionCorrection}
\alias{motioncorrection}

\title{
Motion Correction
}

\description{
Perform motion-correction on the given image
}

\usage{
motion_correction( img )
}

\arguments{
\item{img}{
Input image. Can be a filename of an 3D image or
an 'antsImage' of pixeltype 'float' and dimension '3'.
}
}

\value{
Success -- a list of named elements containing motion-corrected image
(name 'moco_img' type 'antsImage'), motion-correction parameters (name
'moco_params' type 'antsMatrix'), average image
(name 'moco_avg_img' type 'antsImage').\cr
Failure -- NULL
}

\author{
Shrinidhi KL
}

\examples{
\dontrun{
moco_results <- motion_correction( "input_img.nii" )
# motion-corrected image
moco_results$moco_img
# motion-correction parameters
moco_results$moco_params
# average image
moco_results$moco_avg_img
}
}
