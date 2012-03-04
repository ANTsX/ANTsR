/*=========================================================================

  Program:   Advanced Normalization Tools
  Module:    $RCSfile: itkLogTensorImageFilter.hxx,v $
  Language:  C++
  Date:      $Date: 2009/03/17 18:59:48 $
  Version:   $Revision: 1.3 $

  Copyright (c) ConsortiumOfANTS. All rights reserved.
  See accompanying COPYING.txt or 
 http://sourceforge.net/projects/advants/files/ANTS/ANTSCopyright.txt for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkLogTensorImageFilter_hxx
#define _itkLogTensorImageFilter_hxx

#include "itkConstNeighborhoodIterator.h"
#include "itkNeighborhoodInnerProduct.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkImageRegionConstIterator.h"
#include "itkOffset.h"
#include "itkProgressReporter.h"
#include "itkObjectFactory.h"
#include "vnl/vnl_matrix.h"
#include "vnl/algo/vnl_symmetric_eigensystem.h"
#include "itkLogTensorImageFilter.h"
#include "TensorFunctions.h"

namespace itk
{

template <class TInputImage, class TOutputImage>
LogTensorImageFilter<TInputImage, TOutputImage>
::LogTensorImageFilter()
{

}

template< class TInputImage, class TOutputImage>
void
LogTensorImageFilter< TInputImage, TOutputImage >
::GenerateData()
{
  InputImagePointer input = this->GetInput();
  OutputImagePointer output = this->GetOutput();
  
  ImageRegionConstIterator< InputImageType > inputIt( input, input->GetLargestPossibleRegion() );

  InputSizeType inputSize = input->GetLargestPossibleRegion().GetSize();
  output->SetRegions( input->GetLargestPossibleRegion() );
  output->Allocate();

  ImageRegionIteratorWithIndex< OutputImageType > outputIt( output, output->GetLargestPossibleRegion() );

  for ( inputIt.GoToBegin(), outputIt.GoToBegin(); 
	!inputIt.IsAtEnd() && !outputIt.IsAtEnd(); 
	++inputIt, ++outputIt)
    {
    InputPixelType result = TensorLog<InputPixelType>(inputIt.Value());
    outputIt.Set( result );
    }

}
  

/**
 * Standard "PrintSelf" method
 */
template <class TInputImage, class TOutput>
void
LogTensorImageFilter<TInputImage, TOutput>
::PrintSelf(
std::ostream& os, 
Indent indent) const
{
  Superclass::PrintSelf( os, indent );

}

} // end namespace itk

#endif
