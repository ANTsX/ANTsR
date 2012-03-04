/*=========================================================================

  Program:   Advanced Normalization Tools
  Module:    $RCSfile: antsLogEuclideanGaussianListSampleFunction.hxx,v $
  Language:  C++
  Date:      $Date: $
  Version:   $Revision: $

  Copyright (c) ConsortiumOfANTS. All rights reserved.
  See accompanying COPYING.txt or
  http://sourceforge.net/projects/advants/files/ANTS/ANTSCopyright.txt
  for details.

  This software is distributed WITHOUT ANY WARRANTY; without even
  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
  PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __antsLogEuclideanGaussianListSampleFunction_hxx
#define __antsLogEuclideanGaussianListSampleFunction_hxx

#include "antsLogEuclideanGaussianListSampleFunction.h"

#include "itkDecomposeTensorFunction.h"

#include "vnl/vnl_trace.h"

namespace itk {
namespace ants {
namespace Statistics {

template <class TListSample, class TOutput, class TCoordRep>
LogEuclideanGaussianListSampleFunction<TListSample, TOutput, TCoordRep>
::LogEuclideanGaussianListSampleFunction()
{
}

template <class TListSample, class TOutput, class TCoordRep>
LogEuclideanGaussianListSampleFunction<TListSample, TOutput, TCoordRep>
::~LogEuclideanGaussianListSampleFunction()
{
}

template <class TListSample, class TOutput, class TCoordRep>
void
LogEuclideanGaussianListSampleFunction<TListSample, TOutput, TCoordRep>
::SetInputListSample( const InputListSampleType * ptr )
{
  Superclass::SetInputListSample( ptr );

  if( !this->GetInputListSample() )
    {
    return;
    }

  if( this->GetInputListSample()->Size() > 1 )
    {
    RealType L = static_cast<RealType>(
      this->GetInputListSample()->GetMeasurementVectorSize() );
    unsigned int D = static_cast<unsigned int>( 0.5 * ( -1 + vcl_sqrt( 1.0 +
      8.0 * L ) ) );
    this->m_MeanTensor.SetSize( D, D );
    this->m_MeanTensor.Fill( 0.0 );

    unsigned long N = 0;
    RealType totalWeight = 0.0;

    typename InputListSampleType::ConstIterator It =
      this->GetInputListSample()->Begin();
    while( It != this->GetInputListSample()->End() )
      {
      InputMeasurementVectorType measurement = It.GetMeasurementVector();

      TensorType T( D, D );

      unsigned int index = 0;
      for( unsigned int i = 0; i < D; i++ )
        {
        for( unsigned int j = i; j < D; j++ )
          {
          T( i, j ) = measurement( index++ );
          T( j, i ) = T( i, j );
          }
        }
      T = this->LogTensorTransform( T );

      RealType weight = 1.0;
      if( this->GetListSampleWeights()->Size() == this->GetInputListSample()->Size() )
        {
        weight = ( *this->GetListSampleWeights() )[N++];
        }
      totalWeight += weight;
      this->m_MeanTensor += ( T * weight );
      ++It;
      }
    if( totalWeight > 0.0 )
      {
      this->m_MeanTensor /= totalWeight;
      }
    this->m_MeanTensor = this->ExpTensorTransform( this->m_MeanTensor );

    /**
     * Now calculate the dispersion (i.e. variance)
     */
    this->m_Dispersion = 0.0;

    N = 0;

    It = this->GetInputListSample()->Begin();
    while( It != this->GetInputListSample()->End() )
      {
      InputMeasurementVectorType measurement = It.GetMeasurementVector();

      TensorType T( D, D );

      unsigned int index = 0;
      for( unsigned int i = 0; i < D; i++ )
        {
        for( unsigned int j = i; j < D; j++ )
          {
          T( i, j ) = measurement( index++ );
          T( j, i ) = T( i, j );
          }
        }
      RealType distance = this->CalculateTensorDistance( T, this->m_MeanTensor );

      RealType weight = 1.0;
      if( this->GetListSampleWeights()->Size() == this->GetInputListSample()->Size() )
        {
        weight = ( *this->GetListSampleWeights() )[N++];
        }

      this->m_Dispersion += ( weight * vnl_math_sqr( distance ) );
      ++It;
      }
    this->m_Dispersion /= static_cast<RealType>( N );
    }
  else
    {
    itkWarningMacro( "The input list sample has <= 1 element." <<
      "Function evaluations will be equal to 0." );
    }
}

template <class TListSample, class TOutput, class TCoordRep>
typename LogEuclideanGaussianListSampleFunction<TListSample, TOutput, TCoordRep>
::TensorType
LogEuclideanGaussianListSampleFunction<TListSample, TOutput, TCoordRep>
::LogTensorTransform( const TensorType &T ) const
{
  TensorType V;
  TensorType W;

  TensorType Tc = T;

  typedef DecomposeTensorFunction<TensorType> DecomposerType;
  typename DecomposerType::Pointer decomposer = DecomposerType::New();
  decomposer->EvaluateSymmetricEigenDecomposition( Tc, W, V );

  for( unsigned int i = 0; i < W.Rows(); i++ )
    {
    if( W( i, i ) > 0.0 )
      {
      W( i, i ) = vcl_log( W( i, i ) );
      }
    else
      {
      W( i, i ) = 0.0;
      }
    }
  W *= V.GetTranspose();
  TensorType logT = V * W;
  return logT;
}

template <class TListSample, class TOutput, class TCoordRep>
typename LogEuclideanGaussianListSampleFunction<TListSample, TOutput, TCoordRep>
::TensorType
LogEuclideanGaussianListSampleFunction<TListSample, TOutput, TCoordRep>
::ExpTensorTransform( const TensorType &T ) const
{
  TensorType V;
  TensorType W;

  TensorType Tc = T;

  typedef DecomposeTensorFunction<TensorType> DecomposerType;
  typename DecomposerType::Pointer decomposer = DecomposerType::New();
  decomposer->EvaluateSymmetricEigenDecomposition( Tc, W, V );

  for( unsigned int i = 0; i < W.Rows(); i++ )
    {
    W( i, i ) = vcl_exp( W( i, i ) );
    }
  W *= V.GetTranspose();
  TensorType expT = V * W;
  return expT;
}

template <class TListSample, class TOutput, class TCoordRep>
typename LogEuclideanGaussianListSampleFunction<TListSample, TOutput, TCoordRep>
::RealType
LogEuclideanGaussianListSampleFunction<TListSample, TOutput, TCoordRep>
::CalculateTensorDistance( const TensorType &S, const TensorType &T ) const
{
  TensorType logS = this->LogTensorTransform( S );
  TensorType logT = this->LogTensorTransform( T );

  TensorType diff = logS - logT;
  TensorType diffSq = diff * diff;
  RealType distance = vcl_sqrt( vnl_trace( ( diffSq ).GetVnlMatrix() ) );

//  RealType distance = ( ( logS - logT ).GetVnlMatrix() ).frobenius_norm();
  return distance;
}

template <class TListSample, class TOutput, class TCoordRep>
TOutput
LogEuclideanGaussianListSampleFunction<TListSample, TOutput, TCoordRep>
::Evaluate( const InputMeasurementVectorType &measurement ) const
{
  unsigned int D = this->m_MeanTensor.Rows();

  TensorType T( D, D );

  unsigned int index = 0;
  for( unsigned int i = 0; i < D; i++ )
    {
    for( unsigned int j = i; j < D; j++ )
      {
      T( i, j ) = measurement( index++ );
      T( j, i ) = T( i, j );
      }
    }
  RealType distance = this->CalculateTensorDistance( T, this->m_MeanTensor );
  RealType preFactor = 1.0 /
    ( vcl_sqrt( 2.0 * vnl_math::pi * this->m_Dispersion ) );
  RealType probability = preFactor * vcl_exp( -0.5 *
    vnl_math_sqr( distance ) / this->m_Dispersion );

  return probability;
}

/**
 * Standard "PrintSelf" method
 */
template <class TListSample, class TOutput, class TCoordRep>
void
LogEuclideanGaussianListSampleFunction<TListSample, TOutput, TCoordRep>
::PrintSelf(
  std::ostream& os,
  Indent indent) const
{
  os << indent << "Mean tensor = [";

  for( unsigned int r = 0; r < this->m_MeanTensor.Rows(); r++ )
    {
    for( unsigned int c = 0; c < this->m_MeanTensor.Cols() - 1; c++ )
      {
      os << this->m_MeanTensor( r, c ) << ", ";
      }
    if( r == this->m_MeanTensor.Rows() - 1 )
      {
      os << this->m_MeanTensor( r, this->m_MeanTensor.Cols() - 1 ) << "], ";
      }
    else
      {
      os << this->m_MeanTensor( r, this->m_MeanTensor.Cols() - 1 ) << "; ";
      }
    }
  os << "Dispersion (variance) = " << this->m_Dispersion << std::endl;
}

} // end of namespace Statistics
} // end of namespace ants
} // end of namespace itk

#endif
