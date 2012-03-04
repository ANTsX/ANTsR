/*=========================================================================


  Program:   Advanced Normalization Tools
  Module:    $RCSfile: antsSCCANObject.txx,v $
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
#include "itkMinimumMaximumImageFilter.h"
#include "itkConnectedComponentImageFilter.h"
#include "itkRelabelComponentImageFilter.h"
#include <vnl/vnl_random.h>
#include <vnl/vnl_trace.h>
#include <vnl/algo/vnl_matrix_inverse.h>
#include <vnl/algo/vnl_generalized_eigensystem.h>
#include "antsSCCANObject.h"
#include <time.h>

namespace itk {
namespace ants {

template <class TInputImage, class TRealType>
antsSCCANObject<TInputImage, TRealType>::antsSCCANObject( ) 
{
  this->m_MinClusterSizeP=1;
  this->m_MinClusterSizeQ=1;
  this->m_KeptClusterSize=0;
  this->m_Debug=false;
  this->m_CorrelationForSignificanceTest=0;
  this->m_SpecializationForHBM2011=false;
  this->m_AlreadyWhitened=false;
  this->m_PinvTolerance=1.e-3;
  this->m_PercentVarianceForPseudoInverse=0.9;
  this->m_MaximumNumberOfIterations=20;
  this->m_MaskImageP=NULL;
  this->m_MaskImageQ=NULL;
  this->m_MaskImageR=NULL;
  this->m_KeepPositiveP=true;
  this->m_KeepPositiveQ=true;
  this->m_KeepPositiveR=true;
  this->m_FractionNonZeroP=0.5;
  this->m_FractionNonZeroQ=0.5;
  this->m_FractionNonZeroR=0.5;
  this->m_ConvergenceThreshold=1.e-6;
  this->m_Epsilon=1.e-12;
} 



template <class TInputImage, class TRealType>
typename TInputImage::Pointer
antsSCCANObject<TInputImage, TRealType>
::ConvertVariateToSpatialImage(  typename antsSCCANObject<TInputImage, TRealType>::VectorType w_p  , typename TInputImage::Pointer mask , bool threshold_at_zero  ) 
{
      typename TInputImage::Pointer weights = TInputImage::New();
      weights->SetOrigin( mask->GetOrigin() );
      weights->SetSpacing( mask->GetSpacing() );
      weights->SetRegions( mask->GetLargestPossibleRegion() );
      weights->SetDirection( mask->GetDirection() );
      weights->Allocate();
      weights->FillBuffer( itk::NumericTraits<PixelType>::Zero );

      // overwrite weights with vector values;
      unsigned long vecind=0;
      typedef itk::ImageRegionIteratorWithIndex<TInputImage> Iterator;
      Iterator mIter(mask,mask->GetLargestPossibleRegion() );
      for(  mIter.GoToBegin(); !mIter.IsAtEnd(); ++mIter )
	if (mIter.Get() >= 0.5) 
	  {
	    TRealType val=0;
	    if ( vecind < w_p.size() ) val=w_p(vecind); 
	    else {
               Rcpp::Rcout << "vecind too large " << vecind << " vs " << w_p.size() << std::endl;
	       Rcpp::Rcout <<" this is likely a mask problem --- exiting! " << std::endl;
	       throw std::exception() ;
	    }
	    if ( threshold_at_zero && fabs(val) > 0  )  weights->SetPixel(mIter.GetIndex(),1);
	    else weights->SetPixel(mIter.GetIndex(),val);
	    vecind++;
	  } 
	else mIter.Set(0);

	return weights;
}


template <class TInputImage, class TRealType>
 typename antsSCCANObject<TInputImage, TRealType>::VectorType
antsSCCANObject<TInputImage, TRealType>
::ClusterThresholdVariate(  typename antsSCCANObject<TInputImage, TRealType>::VectorType& w_p   , typename TInputImage::Pointer mask , unsigned int minclust ) 
{
  if ( minclust <= 1 ) return w_p; 
  typedef unsigned long ULPixelType;
  typedef itk::Image<ULPixelType, ImageDimension> labelimagetype;
  typedef TInputImage InternalImageType;
  typedef TInputImage OutputImageType;
  typedef itk::ImageRegionIteratorWithIndex<ImageType> fIterator;
  typedef itk::ImageRegionIteratorWithIndex<labelimagetype> Iterator;
  typedef itk::ConnectedComponentImageFilter< TInputImage, labelimagetype > FilterType;
  typedef itk::RelabelComponentImageFilter< labelimagetype, labelimagetype > RelabelType;

// we assume w_p has been thresholded by another function 
  bool threshold_at_zero=true;
  typename TInputImage::Pointer image=this->ConvertVariateToSpatialImage( w_p , mask , threshold_at_zero ); 

  typename FilterType::Pointer filter = FilterType::New();
  typename RelabelType::Pointer relabel = RelabelType::New();
  filter->SetInput( image );
  filter->SetFullyConnected( 0 );
  relabel->SetInput( filter->GetOutput() );
  relabel->SetMinimumObjectSize( 1 );    
  try
  {
    relabel->Update();
  }
  catch( itk::ExceptionObject & excep )
  {
    Rcpp::Rcout << "Relabel: exception caught !" << std::endl;
    Rcpp::Rcout << excep << std::endl;
  }
    
  Iterator vfIter( relabel->GetOutput(),  relabel->GetOutput()->GetLargestPossibleRegion() );      
  float maximum=relabel->GetNumberOfObjects();  
  std::vector<unsigned long> histogram((int)maximum+1);
  for (int i=0; i<=maximum; i++) 
    {
      histogram[i]=0;
    }
  for(  vfIter.GoToBegin(); !vfIter.IsAtEnd(); ++vfIter ) {
    float vox=vfIter.Get();
    if (vox > 0 )
    {
      if (vox > 0 ) {
        histogram[(unsigned long)vox]=histogram[(unsigned long)vox]+1;
      }
    }
  }

  // get the largest component's size
  unsigned long largest_component_size=0;
  for (int i=0; i<=maximum; i++) 
    {
      if ( largest_component_size < histogram[i] ) largest_component_size=histogram[i];
    }

  if (  largest_component_size < minclust ) minclust=largest_component_size-1;
//  now create the output vector 
  // iterate through the image and set the voxels where  countinlabel[(unsigned long)(labelimage->GetPixel(vfIter.GetIndex()) - min)] 
  // is < MinClusterSize 
  unsigned long vecind=0, keepct=0;
  fIterator mIter( mask,  mask->GetLargestPossibleRegion() );      
  for(  mIter.GoToBegin(); !mIter.IsAtEnd(); ++mIter ) {
    if (mIter.Get() > 0 )
    {
      float vox=mask->GetPixel(vfIter.GetIndex());
      unsigned long clustersize=0;
      if ( vox >= 0  ) {
        clustersize=histogram[(unsigned long)(relabel->GetOutput()->GetPixel(mIter.GetIndex()) )];
	if ( clustersize > minclust ) { keepct+=1; } // get clusters > minclust
	//	if ( clustersize == largest_component_size ) { keepct++; } // get largest cluster 
	else { w_p(vecind)=0;  }
	vecind++;
      }
    }
  }
  this->m_KeptClusterSize=histogram[1];// only records the size of the largest cluster in the variate
  //  for (unsigned int i=0; i<histogram.size(); i++) 
  //  if ( histogram[i] > minclust ) this->m_KeptClusterSize+=histogram[i];
  //  Rcpp::Rcout << " Cluster Threshold Kept % of sparseness " <<  ( (float)keepct/(float)w_p.size() ) / this->m_FractionNonZeroP   << " kept clust size " << keepct << std::endl;
  return w_p;
}


template <class TInputImage, class TRealType>
typename antsSCCANObject<TInputImage, TRealType>::VectorType
antsSCCANObject<TInputImage, TRealType>
::InitializeV( typename antsSCCANObject<TInputImage, TRealType>::MatrixType p ) 
{
  VectorType w_p( p.columns() );
  w_p.fill(0);
  for (unsigned int its=0; its<1; its++) {
  vnl_random randgen(time(0));
  for (unsigned long i=0; i < p.columns(); i++)
    { 
      w_p(i)=randgen.normal();
      //      w_p(i)=randgen.drand32();//1.0/p.rows();//
      w_p(i)=1.0;//+randgen.normal()*this->m_Epsilon;
//1.0+fabs(randgen.drand32())*1.e-3; 
    }
  }
  w_p=w_p/p.columns();
  return w_p;
}

template <class TInputImage, class TRealType>
typename antsSCCANObject<TInputImage, TRealType>::MatrixType
antsSCCANObject<TInputImage, TRealType>
::NormalizeMatrix( typename antsSCCANObject<TInputImage, TRealType>::MatrixType p ) 
{
  MatrixType np( p.rows() , p.columns() );
  for (unsigned long i=0; i < p.columns(); i++)
  { 
    VectorType wpcol=p.get_column(i);
    VectorType wpcol2=wpcol-wpcol.mean();
    double sd=wpcol2.squared_magnitude();
    sd=sqrt( sd/(p.rows()-1) );
    if ( sd <= 0 && i== 0 ) {
      Rcpp::Rcout << " row " << i << " has zero variance --- exiting " <<std::endl;
      throw std::exception() ;
    }
    if ( sd <= 0 && i > 0 ) {
      Rcpp::Rcout << " row " << i << " has zero variance --- copying the previous row " <<std::endl;
      Rcpp::Rcout << " the row is " << wpcol << std::endl;
      np.set_column(i,np.get_column(i-1));
    }
    else 
    {
      wpcol=wpcol2/sd;
      np.set_column(i,wpcol);
    }
  }
  return np;
}



template <class TInputImage, class TRealType>
typename antsSCCANObject<TInputImage, TRealType>::MatrixType
antsSCCANObject<TInputImage, TRealType>
::VNLPseudoInverse( typename antsSCCANObject<TInputImage, TRealType>::MatrixType rin , bool take_sqrt ) {
  double pinvTolerance=1.e-9; // this->m_PinvTolerance;
      MatrixType dd=rin;
      unsigned int ss=dd.rows();
      if ( dd.rows() > dd.columns() ) ss=dd.columns();
      vnl_svd<RealType> eig(dd,pinvTolerance);
      for (unsigned int j=0; j<ss; j++) 
	{
	  RealType eval=eig.W(j,j);
	  if ( eval > pinvTolerance )  {// FIXME -- check tolerances against matlab pinv
	    eig.W(j,j)=1/(eval);// need eval for inv cov 
	    if ( take_sqrt ) eig.W(j,j)=1/sqrt(eval);
	  }
	  else eig.W(j,j)=0; 
	} 
      /** there is a scaling problem with the pseudoinverse --- this is a cheap fix!!
          it is based on the theoretical frobenious norm of the inverse matrix */
  MatrixType pinv=( eig.recompose() ).transpose();
  double a=sqrt((double)dd.rows());
  double b=(pinv*dd).frobenius_norm();
  pinv=pinv*a/b;
  return pinv;
}

template <class TInputImage, class TRealType>
void
antsSCCANObject<TInputImage, TRealType>
::ReSoftThreshold( typename antsSCCANObject<TInputImage, TRealType>::VectorType&
 v_in, TRealType fractional_goal , bool keep_positive )
{
  //  Rcpp::Rcout <<" resoft " << fractional_goal << std::endl;
  if ( fabs(fractional_goal) >= 1 || fabs((float)(v_in.size())*fractional_goal) <= 1 ) return ;
  RealType minv=v_in.min_value();
  RealType maxv=v_in.max_value();
  if ( fabs(v_in.min_value()) > maxv ) maxv=fabs(v_in.min_value());
  minv=0;
  RealType lambg=this->m_Epsilon;
  RealType frac=0;
  unsigned int its=0,ct=0;
  RealType soft_thresh=lambg;
  for ( unsigned int i=0; i<v_in.size(); i++) {
    if (  keep_positive && v_in(i) < 0 ) v_in(i)=0;
  }


  RealType minthresh=0,minfdiff=1;
  unsigned int maxits=1000;
  for ( its=0; its<maxits; its++) 
  {
    soft_thresh=(its/(float)maxits)*maxv;
    ct=0; 
    for ( unsigned int i=0; i<v_in.size(); i++) {
      RealType val=v_in(i);
      if ( ! keep_positive ) val=fabs(val);
      else if ( val < 0 ) val=0; 
      if ( val < soft_thresh ) ct++;
    }
    frac=(float)(v_in.size()-ct)/(float)v_in.size();
    //    Rcpp::Rcout << " cur " << frac << " goal "  << fractional_goal << " st " << soft_thresh << " th " << minthresh << std::endl;
    if ( fabs(frac - fractional_goal) < minfdiff ) {
      minthresh=soft_thresh;
      minfdiff= fabs(frac - fractional_goal) ;
    }
  }
  //  Rcpp::Rcout << " goal "  << fractional_goal << " st " << soft_thresh << " th " << minthresh << " minfdiff " << minfdiff << std::endl;
  
// here , we apply the minimum threshold to the data. 
  ct=0;
  for ( unsigned int i=0; i<v_in.size(); i++) {
      RealType val=v_in(i);
      if ( ! keep_positive ) val=fabs(val);
      else if ( val < 0 ) val=0; 
      if ( val < minthresh ) 
      {
	v_in(i)=0;
	ct++;
      }
      else
      {
	//	v_in(i)=v_in(i)-minthresh;
      }
  }
  double tminv=v_in.min_value();
  double tmaxv=v_in.max_value();
  //  Rcpp::Rcout << " post minv " << tminv << " post maxv " << tmaxv <<  std::endl;
  frac=(float)(v_in.size()-ct)/(float)v_in.size();
  //  Rcpp::Rcout << " frac non-zero " << frac << " wanted " << fractional_goal << std::endl;
  if ( v_in.two_norm() > this->m_Epsilon ) v_in=v_in/v_in.two_norm();
  //  Rcpp::Rcout << v_in <<std::endl;
  return;
}


template <class TInputImage, class TRealType>
void
antsSCCANObject<TInputImage, TRealType>
::ConstantProbabilityThreshold( typename antsSCCANObject<TInputImage, TRealType>::VectorType&
 v_in, TRealType probability_goal , bool keep_positive )
{
  VectorType v_out(v_in);
  RealType minv=v_in.min_value();
  RealType maxv=v_in.max_value();
  if ( fabs(v_in.min_value()) > maxv ) maxv=fabs(v_in.min_value());
  minv=0;
  RealType lambg=this->m_Epsilon;
  RealType frac=0;
  unsigned int its=0;
  RealType probability=0;
  RealType probability_sum=0;
  RealType soft_thresh=lambg;

  for ( unsigned int i=0; i<v_in.size(); i++) {
    if ( keep_positive && v_in(i) < 0 ) v_in(i)=0;
    v_out(i)=v_in(i);
    probability_sum+=fabs(v_out(i));
  }
  //  Rcpp::Rcout <<" prob sum " << probability_sum << std::endl;
  RealType minthresh=0,minfdiff=1;
  unsigned int maxits=1000;
  for ( its=0; its<maxits; its++) 
  {
    soft_thresh=(its/(float)maxits)*maxv;
    probability=0; 
    for ( unsigned int i=0; i<v_in.size(); i++) {
      RealType val=v_in(i);
      if ( ! keep_positive ) val=fabs(val);
      else if ( val < 0 ) val=0; 
      if ( val < soft_thresh ) 
      {
	v_out(i)=0;
      }
      else
	{
	  v_out(i)=v_in(i);
	  probability+=fabs(v_out(i));
	}
    }
    probability/=probability_sum;
    //    Rcpp::Rcout << " cur " << probability << " goal "  << probability_goal << " st " << soft_thresh << " th " << minthresh << std::endl;
    if ( fabs(probability - probability_goal) < minfdiff ) {
      minthresh=soft_thresh;
      minfdiff= fabs(probability - probability_goal) ;
    }
  }

// here , we apply the minimum threshold to the data. 
  probability=0;
  unsigned long ct=0;
  for ( unsigned int i=0; i<v_in.size(); i++) {
      RealType val=v_in(i);
      if ( ! keep_positive ) val=fabs(val);
      else if ( val < 0 ) val=0; 
      if ( val < minthresh ) 
      {
	v_in(i)=0;
	ct++;
      }
      else
      {
	// v_in(i)-=minthresh;
	probability+=fabs(v_in(i));
      }
  }
//  tminv=v_in.min_value();
//  tmaxv=v_in.max_value();
//  Rcpp::Rcout << " post minv " << tminv << " post maxv " << tmaxv << " allow-neg? " <<  allow_negative_weights << std::endl;
//  Rcpp::Rcout << " frac non-zero " << frac << " wanted " << fractional_goal << " allow-neg " << allow_negative_weights << std::endl;
  frac=(float)(v_in.size()-ct)/(float)v_in.size();
  if ( frac < 1 ) Rcpp::Rcout <<" const prob "<< probability/probability_sum << " sparseness " << frac << std::endl;
  if ( v_in.two_norm() > this->m_Epsilon ) v_in=v_in/v_in.sum();

  return;
}




template <class TInputImage, class TRealType>
typename antsSCCANObject<TInputImage, TRealType>::VectorType
antsSCCANObject<TInputImage, TRealType>
::TrueCCAPowerUpdate( TRealType penalty1,  typename antsSCCANObject<TInputImage, TRealType>::MatrixType p , typename antsSCCANObject<TInputImage, TRealType>::VectorType  w_q ,  typename antsSCCANObject<TInputImage, TRealType>::MatrixType q, bool keep_pos , bool factorOutR )
{
  RealType norm=0;
  // recall that the matrices below have already be whitened ....
  // we bracket the computation and use associativity to make sure its done efficiently 
  //vVector wpnew=( (CppInv.transpose()*p.transpose())*(CqqInv*q) )*w_q;
  VectorType wpnew;
  if ( factorOutR ){
    VectorType temp=q*w_q;
    wpnew=p.transpose()*( temp - this->m_MatrixRRt*temp ); 
  }
  else {
    VectorType temp=q*w_q;
    wpnew=p.transpose()*temp;
  }
  this->ReSoftThreshold( wpnew , penalty1 , keep_pos );
  norm=wpnew.two_norm();
  if ( norm > this->m_Epsilon ) wpnew=wpnew/(norm);
  return wpnew;
}

template <class TInputImage, class TRealType>
void
antsSCCANObject<TInputImage, TRealType>
::UpdatePandQbyR( ) 
{
// R is already whitened 
    switch( this->m_SCCANFormulation )
    {
    case PQ:
      {
// do nothing
      break;
      }
    case PminusRQ:
      {
        this->m_MatrixP=(this->m_MatrixP-this->m_MatrixRRt*this->m_MatrixP);
      break;
      }
    case PQminusR:
      {
        this->m_MatrixQ=(this->m_MatrixQ-this->m_MatrixRRt*this->m_MatrixQ);
      break;
      }
    case PminusRQminusR :
      {
/** P_R =   P - R_w R_w^T P */
/** Q_R =   Q - R_w R_w^T Q */
        this->m_MatrixP=(this->m_MatrixP-this->m_MatrixRRt*this->m_MatrixP);
        this->m_MatrixQ=(this->m_MatrixQ-this->m_MatrixRRt*this->m_MatrixQ);
      break;
    case PQR :
      Rcpp::Rcout <<" You should call mscca not pscca " << std::endl;
      break;
      }
    }

}


template <class TInputImage, class TRealType>
void antsSCCANObject<TInputImage, TRealType>
::RunDiagnostics( unsigned int n_vecs ) 
{ 
  Rcpp::Rcout << "Quantitative diagnostics: "<<std::endl;
  Rcpp::Rcout << "Type 1: correlation from canonical variate to confounding vector "<<std::endl;
  Rcpp::Rcout << "Type 2: correlation from canonical variate to canonical variate "<<std::endl;
  RealType corrthresh=0.3;
  MatrixType omatP=this->NormalizeMatrix(this->m_OriginalMatrixP);
  MatrixType omatQ;
  bool doq=true;
  if ( this->m_OriginalMatrixQ.size() > 0 ) omatQ=this->NormalizeMatrix(this->m_OriginalMatrixQ);
  else { doq=false ; omatQ=omatP; }
  if (this->m_OriginalMatrixR.size()>0){
   for (unsigned int wv=0; wv<n_vecs; wv++)
     for (unsigned int col=0; col<this->m_MatrixR.columns(); col++) 
      {
      RealType a=this->PearsonCorr(omatP*this->m_VariatesP.get_column(wv) , this->m_OriginalMatrixR.get_column(col) ); 
      RealType b=this->PearsonCorr(omatQ*this->m_VariatesQ.get_column(wv) , this->m_OriginalMatrixR.get_column(col) );
//      Rcpp::Rcout << "Pvec " << wv << " confound " << col << " : " << a <<std::endl; 
//      Rcpp::Rcout << "Qvec " << wv << " confound " << col << " : " << b <<std::endl; 
      if ( fabs(a) > corrthresh && fabs(b) > corrthresh ) {
       Rcpp::Rcout << " correlation with confound too high for variate " << wv << " corrs " << a << " and " << b <<  std::endl;  
  //     this->m_CanonicalCorrelations[wv]=0;
      }
      }
  }
  for (unsigned int wv=0; wv<n_vecs; wv++)
    for (unsigned int yv=wv+1; yv<n_vecs; yv++)
      {
      RealType a=this->PearsonCorr(omatP*this->m_VariatesP.get_column(wv) ,omatP*this->m_VariatesP.get_column(yv)); 
      if ( fabs(a) > corrthresh ) {
      Rcpp::Rcout << " not orthogonal p " <<  a << std::endl; 
 //       this->m_CanonicalCorrelations[yv]=0; 
      }
      Rcpp::Rcout << "Pvec " << wv << " Pvec " << yv << " : " << a <<std::endl; 
      if ( doq ) {
      RealType b=this->PearsonCorr(omatQ*this->m_VariatesQ.get_column(wv) ,omatQ*this->m_VariatesQ.get_column(yv));
      if ( fabs(b) > corrthresh ) { 
      Rcpp::Rcout << " not orthogonal q " <<  a << std::endl; 
   //     this->m_CanonicalCorrelations[yv]=0; 
      }
      Rcpp::Rcout << "Qvec " << wv << " Qvec " << yv << " : " << b <<std::endl; 
      }// doq
      }
 
}


struct my_sccan_sort_class {
  bool operator() (double i, double j) { return (i>j);}
} my_sccan_sort_object;

template <class TInputImage, class TRealType>
TRealType antsSCCANObject<TInputImage, TRealType>
::SparseCCA(unsigned int nvecs)
{
  Rcpp::Rcout <<" ed sparse cca " << std::endl;
  unsigned int nsubj=this->m_MatrixP.rows();
  this->m_MatrixP=this->NormalizeMatrix(this->m_OriginalMatrixP);  
  this->m_MatrixQ=this->NormalizeMatrix(this->m_OriginalMatrixQ);

  RealType tau=0.01;
  if (this->m_Debug) Rcpp::Rcout << " inv view mats " << std::endl;
  MatrixType inviewcovmatP=( (this->m_MatrixP*this->m_MatrixP.transpose())*(this->m_MatrixP*this->m_MatrixP.transpose()) )*(1-tau)+  ( this->m_MatrixP*this->m_MatrixP.transpose() )*tau*(RealType)nsubj;
  MatrixType inviewcovmatQ=( (this->m_MatrixQ*this->m_MatrixQ.transpose())*(this->m_MatrixQ*this->m_MatrixQ.transpose()) )*(1-tau)+( this->m_MatrixQ*this->m_MatrixQ.transpose() )*tau*(RealType)nsubj;

/** standard cca */
//  MatrixType CppInv=this->PseudoInverseCovMat(this->m_MatrixP);
//  MatrixType CqqInv=this->PseudoInverseCovMat(this->m_MatrixQ);
//  MatrixType cov=(CppInv*this->m_MatrixP).transpose()*(CqqInv*this->m_MatrixQ);
//  MatrixType TT=(this->m_MatrixP.transpose()*this->m_MatrixQ)*(this->m_MatrixP.transpose()*this->m_MatrixQ).transpose();
//  MatrixType TT=( (CppInv*this->m_MatrixP).transpose()*(CqqInv*this->m_MatrixQ))*
//                         (this->m_MatrixP.transpose()*this->m_MatrixQ).transpose();

/** dual cca */
  MatrixType CppInv=this->PseudoInverse(inviewcovmatP); 
  MatrixType CqqInv=this->PseudoInverse(inviewcovmatQ);
 
  /** need the eigenvectors of this reduced matrix */ 
  //  eMatrix pccain=CppInv*Cpq*CqqInv*Cqp;
  MatrixType ccap=( (CppInv*(this->m_MatrixP*this->m_MatrixP.transpose())).transpose() *
                  (CqqInv*(this->m_MatrixQ*this->m_MatrixQ.transpose()))*
                        ( (this->m_MatrixP*this->m_MatrixP.transpose() ).transpose()*
                          (this->m_MatrixQ*this->m_MatrixQ.transpose() ) ).transpose() );
// convert to eigen3 format
/*
  eMatrix pccain=this->mVtoE(ccap);
 
  typedef Eigen::EigenSolver<eMatrix> eigsolver;
  eigsolver pEG( pccain );
//  eigsolver qEG( qccain );
  eMatrix pccaVecs = pEG.pseudoEigenvectors();
  eMatrix pccaSquaredCorrs=pEG.pseudoEigenvalueMatrix();

// call this function to check we are doing conversions correctly , matrix-wise 
  this->mEtoV(pccaVecs);
// map the variates back to P, Q space and sort them 
  this->m_CanonicalCorrelations.set_size(nvecs);
  this->m_CanonicalCorrelations.fill(0); 
// copy to stl vector so we can sort the results  
  MatrixType projToQ=( CqqInv*( (this->m_MatrixQ*this->m_MatrixQ.transpose() )* (this->m_MatrixP*this->m_MatrixP.transpose() ) ));
  std::vector<TRealType> evals(pccaSquaredCorrs.cols(),0);
  std::vector<TRealType> oevals(pccaSquaredCorrs.cols(),0);
  for ( long j=0; j<pccaSquaredCorrs.cols(); ++j){
    RealType val=pccaSquaredCorrs(j,j);
    if ( val > 0.05 ){
      VectorType temp=this->vEtoV( pccaVecs.col(  j ) );
      VectorType tempq=projToQ*temp;
      VectorType pvar=temp*this->m_MatrixP;
      this->ReSoftThreshold( pvar , this->m_FractionNonZeroP , this->m_KeepPositiveP ); 
      VectorType qvar= tempq*this->m_MatrixQ;
      this->ReSoftThreshold( qvar , this->m_FractionNonZeroQ , this->m_KeepPositiveQ );
      evals[j]=fabs(this->PearsonCorr(this->m_MatrixP*pvar,this->m_MatrixQ*qvar));
      oevals[j]=evals[j];
    }
  }

// sort and reindex the eigenvectors/values 
  sort (evals.begin(), evals.end(), my_sccan_sort_object); 
  std::vector<int> sorted_indices(nvecs,-1);
  for (unsigned int i=0; i<evals.size(); i++) {
  for (unsigned int j=0; j<evals.size(); j++) {
    if ( evals[i] == oevals[j] &&  sorted_indices[i] == -1 ) {
      sorted_indices[i]=j;
      oevals[j]=0;
    }
  }}

  this->m_VariatesP.set_size(this->m_MatrixP.cols(),nvecs);
  this->m_VariatesQ.set_size(this->m_MatrixQ.cols(),nvecs);
  for (unsigned int i=0; i<nvecs; i++) {
    VectorType temp=this->vEtoV( pccaVecs.col(  sorted_indices[i] ) );
    VectorType tempq=projToQ*temp;
    VectorType pvar= temp*this->m_MatrixP;
    this->ReSoftThreshold(pvar , this->m_FractionNonZeroP , this->m_KeepPositiveP ); 
    VectorType qvar= tempq*this->m_MatrixQ ;
    this->ReSoftThreshold(qvar , this->m_FractionNonZeroQ , this->m_KeepPositiveQ );
    this->m_VariatesP.set_column( i, pvar  );
    this->m_VariatesQ.set_column( i, qvar  );
  }

  for (unsigned int i=0; i<nvecs; i++) {
    this->m_CanonicalCorrelations[i]=
      this->PearsonCorr(this->m_MatrixP*this->GetVariateP(i),this->m_MatrixQ*this->GetVariateQ(i) );
    Rcpp::Rcout << "correlation of mapped back data " << this->m_CanonicalCorrelations[i] << 
     " eval " << pccaSquaredCorrs(sorted_indices[i],sorted_indices[i]) << std::endl;
  }
  for (unsigned int i=0; i<nvecs; i++) {
    Rcpp::Rcout << "inner prod of projections 0 vs i  " <<  this->PearsonCorr( this->m_MatrixP*this->GetVariateP(0) , this->m_MatrixP*this->GetVariateP(i) ) << std::endl;
  }
*/
//  this->RunDiagnostics(nvecs);
  return this->m_CanonicalCorrelations[0];

}


template <class TInputImage, class TRealType>
TRealType antsSCCANObject<TInputImage, TRealType>
::SparsePartialCCA(unsigned int nvecs)
{
  /*
  Rcpp::Rcout <<" ed sparse partial cca " << std::endl;
  unsigned int nsubj=this->m_MatrixP.rows();
  this->m_MatrixP=this->NormalizeMatrix(this->m_OriginalMatrixP);  
  this->m_MatrixQ=this->NormalizeMatrix(this->m_OriginalMatrixQ);
  this->m_MatrixR=this->NormalizeMatrix(this->m_OriginalMatrixR);
  
  RealType tau=0.01;
  if (this->m_Debug) Rcpp::Rcout << " inv view mats " << std::endl;
  this->m_MatrixRRt=this->ProjectionMatrix(this->m_OriginalMatrixR);
  MatrixType PslashR=this->m_MatrixP;
  if ( this->m_SCCANFormulation == PminusRQ ||  this->m_SCCANFormulation == PminusRQminusR )  
    PslashR=this->m_MatrixP-(this->m_MatrixRRt*this->m_MatrixP);
  MatrixType QslashR=this->m_MatrixQ;
  if ( this->m_SCCANFormulation == PQminusR ||  this->m_SCCANFormulation == PminusRQminusR )  
    QslashR=this->m_MatrixQ-this->m_MatrixRRt*this->m_MatrixQ;

  if (this->m_Debug) {
    Rcpp::Rcout <<" corr-pre " << this->PearsonCorr( this->m_MatrixP.get_column(0) , this->m_OriginalMatrixR.get_column(0)  ) << std::endl  ; Rcpp::Rcout <<" corr-post " << this->PearsonCorr( PslashR.get_column(0) , this->m_OriginalMatrixR.get_column(0)  ) << std::endl; 
    Rcpp::Rcout <<" corr-pre " << this->PearsonCorr( this->m_MatrixQ.get_column(0) , this->m_OriginalMatrixR.get_column(0)  ) << std::endl  ; Rcpp::Rcout <<" corr-post " << this->PearsonCorr( QslashR.get_column(0) , this->m_OriginalMatrixR.get_column(0)  ) << std::endl;
  }
  MatrixType inviewcovmatP=( (PslashR*PslashR.transpose())*(PslashR*PslashR.transpose()) )*(1-tau)+  ( PslashR*PslashR.transpose() )*tau*(RealType)nsubj;
  MatrixType inviewcovmatQ=( (QslashR*QslashR.transpose())*(QslashR*QslashR.transpose()) )*(1-tau)+( QslashR*QslashR.transpose() )*tau*(RealType)nsubj;

// dual cca 
  if (this->m_Debug) Rcpp::Rcout << " inv view mats pinv " << std::endl;
  MatrixType CppInv=this->PseudoInverse(inviewcovmatP); 
  MatrixType CqqInv=this->PseudoInverse(inviewcovmatQ);
  if (this->m_Debug) Rcpp::Rcout << " inv view mats pinv done " << std::endl;
 
  // need the eigenvectors of this reduced matrix 
  MatrixType ccap=( (CppInv*(PslashR*PslashR.transpose())).transpose() *
                  (CqqInv*(QslashR*QslashR.transpose()))*
                        ( (PslashR*PslashR.transpose() ).transpose()*
                          (QslashR*QslashR.transpose() ) ).transpose() );
// convert to eigen3 format
  eMatrix pccain=this->mVtoE(ccap);
 
  typedef Eigen::EigenSolver<eMatrix> eigsolver;
  eigsolver pEG( pccain );
  eMatrix pccaVecs = pEG.pseudoEigenvectors();
  eMatrix pccaSquaredCorrs=pEG.pseudoEigenvalueMatrix();
// call this function to check we are doing conversions correctly , matrix-wise 
  this->mEtoV(pccaVecs);
// map the variates back to P, Q space and sort them 
  this->m_CanonicalCorrelations.set_size(nvecs);
  this->m_CanonicalCorrelations.fill(0); 
  MatrixType projToQ=( CqqInv*( (QslashR*QslashR.transpose() )* (PslashR*PslashR.transpose() ) ));
// copy to stl vector so we can sort the results  
  std::vector<TRealType> evals(pccaSquaredCorrs.cols(),0);
  std::vector<TRealType> oevals(pccaSquaredCorrs.cols(),0);
  for ( long j=0; j<pccaSquaredCorrs.cols(); ++j){
    RealType val=pccaSquaredCorrs(j,j);
    if ( val > 0.05 ){
      VectorType temp=this->vEtoV( pccaVecs.col(  j ) );
      VectorType tempq=projToQ*temp;
      VectorType pvar= temp*PslashR;
      this->ReSoftThreshold(pvar  , this->m_FractionNonZeroP , this->m_KeepPositiveP ); 
      VectorType qvar=tempq*QslashR;
      this->ReSoftThreshold(qvar , this->m_FractionNonZeroQ , this->m_KeepPositiveQ );
      evals[j]=fabs(this->PearsonCorr(PslashR*pvar,QslashR*qvar));
      oevals[j]=evals[j];
    }
  }

// sort and reindex the eigenvectors/values 
  sort (evals.begin(), evals.end(), my_sccan_sort_object); 
  std::vector<int> sorted_indices(nvecs,-1);
  for (unsigned int i=0; i<evals.size(); i++) {
  for (unsigned int j=0; j<evals.size(); j++) {
    if ( evals[i] == oevals[j] &&  sorted_indices[i] == -1 ) {
      sorted_indices[i]=j;
      oevals[j]=0;
    }
  }}

  this->m_VariatesP.set_size(PslashR.cols(),nvecs);
  this->m_VariatesQ.set_size(QslashR.cols(),nvecs);
  for (unsigned int i=0; i<nvecs; i++) {
    VectorType temp=this->vEtoV( pccaVecs.col(  sorted_indices[i] ) );
    VectorType tempq=projToQ*temp;
    VectorType pvar=temp*PslashR ;
    this->ReSoftThreshold(pvar  , this->m_FractionNonZeroP , this->m_KeepPositiveP ); 
    VectorType qvar=tempq*QslashR;
    this->ReSoftThreshold(qvar  , this->m_FractionNonZeroQ , this->m_KeepPositiveQ );
    this->m_VariatesP.set_column( i, pvar  );
    this->m_VariatesQ.set_column( i, qvar  );
  }

  for (unsigned int i=0; i<nvecs; i++) {
    this->m_CanonicalCorrelations[i]=
      this->PearsonCorr(PslashR*this->GetVariateP(i),QslashR*this->GetVariateQ(i) );
    Rcpp::Rcout << "correlation of mapped back data " << this->m_CanonicalCorrelations[i] <<  " eval " << pccaSquaredCorrs(sorted_indices[i],sorted_indices[i]) << std::endl;
  }
  for (unsigned int i=0; i<nvecs; i++) {
    Rcpp::Rcout << "inner prod of projections 0 vs i  " <<  this->PearsonCorr( PslashR*this->GetVariateP(0) ,  PslashR*this->GetVariateP(i) ) << std::endl;
  }

  this->RunDiagnostics(nvecs);
  */
  return this->m_CanonicalCorrelations[0];

}


template <class TInputImage, class TRealType>
void antsSCCANObject<TInputImage, TRealType>
::SortResults(unsigned int n_vecs)
{
// sort and reindex the eigenvectors/values 
  std::vector<TRealType> evals(n_vecs,0);
  std::vector<TRealType> oevals(n_vecs,0);
  //Rcpp::Rcout<<"sort-a"<<std::endl;
  for ( long j=0; j<n_vecs; ++j){
    RealType val=fabs(this->m_CanonicalCorrelations[j]);
    evals[j]=val;
    oevals[j]=val;
  }  
  //Rcpp::Rcout<<"sort-b"<<std::endl;
  sort (evals.begin(), evals.end(), my_sccan_sort_object); 
  std::vector<int> sorted_indices(n_vecs,-1);
  for (unsigned int i=0; i<evals.size(); i++) {
  for (unsigned int j=0; j<evals.size(); j++) {
    if ( evals[i] == oevals[j] &&  sorted_indices[i] == -1 ) {
      sorted_indices[i]=j;
      oevals[j]=0;
    }
  }}
  //  for (unsigned int i=0; i<evals.size(); i++) {
  //    Rcpp::Rcout << " sorted " << i << " is " << sorted_indices[i] << " ev " << evals[i] <<" oev "<<oevals[i]<< std::endl;
  //  }
  //Rcpp::Rcout<<"sort-c"<<std::endl;
  VectorType newcorrs(n_vecs,0);
  MatrixType varp(this->m_MatrixP.cols(),n_vecs,0);
  MatrixType varq(this->m_MatrixQ.cols(),n_vecs,0);
  //Rcpp::Rcout<<"sort-d"<<std::endl;
  for (unsigned int i=0; i<n_vecs; i++) {
    // if ( sorted_indices[i] > 0 ) {
      varp.set_column(i,this->m_VariatesP.get_column( sorted_indices[i] ));
      if ( varq.columns() > i ) varq.set_column(i,this->m_VariatesQ.get_column( sorted_indices[i] ));
      newcorrs[i]=(this->m_CanonicalCorrelations[sorted_indices[i]]);
      // }
  }
  //  Rcpp::Rcout<<"sort-e"<<std::endl;
  for (unsigned int i=0; i<n_vecs; i++) {
    this->m_VariatesP.set_column(i,varp.get_column( i ));
    if (  this->m_VariatesQ.columns() > i ) this->m_VariatesQ.set_column(i,varq.get_column( i ));
  }
  //  Rcpp::Rcout<<"sort-f"<<std::endl;
  this->m_CanonicalCorrelations=newcorrs;
}

template <class TInputImage, class TRealType>
TRealType antsSCCANObject<TInputImage, TRealType>
::SparseArnoldiSVD(unsigned int n_vecs)
{
  this->m_CanonicalCorrelations.set_size(n_vecs);
  this->m_CanonicalCorrelations.fill(0); 
  Rcpp::Rcout <<" arnoldi sparse svd " << std::endl;
  std::vector<RealType> vexlist;
  this->m_MatrixP=this->NormalizeMatrix(this->m_OriginalMatrixP);  
  this->m_MatrixQ=this->m_MatrixP;
  if ( this->m_OriginalMatrixR.size() > 0 ) {
    this->m_MatrixRRt=this->ProjectionMatrix(this->m_OriginalMatrixR);
    if ( this->m_SCCANFormulation == PminusRQ ||  this->m_SCCANFormulation == PminusRQminusR )  {
      Rcpp::Rcout <<" Subtracting nuisance matrix from P matrix " << std::endl;
      this->m_MatrixP=this->m_MatrixP-(this->m_MatrixRRt*this->m_MatrixP);
    }
  }
  this->m_ClusterSizes.set_size(n_vecs);
  this->m_ClusterSizes.fill(0);
  //  MatrixType covmat=this->m_MatrixP*this->m_MatrixP.transpose(); 
  double trace=0; // vnl_trace<double>(covmat); 
  for ( unsigned int i = 0 ; i < this->m_MatrixP.cols(); i++ ) trace+=inner_product(this->m_MatrixP.get_column(i),this->m_MatrixP.get_column(i));
  this->m_VariatesP.set_size(this->m_MatrixP.cols(),n_vecs);
  for (unsigned int kk=0;kk<n_vecs; kk++) {
    this->m_VariatesP.set_column(kk,this->InitializeV(this->m_MatrixP));
  }
  unsigned int maxloop=this->m_MaximumNumberOfIterations;
// Arnoldi Iteration SVD/SPCA
  unsigned int loop=0;
  bool debug=false;
  bool condition2=false;
  /** these methods are sensitive to initialization.   we therefore perform a two stage optimization.
   *    1. stage 1 'anneals' from sparseness 1 to the user-specified sparseness value while performing arnoldi iteration.
   *      this makes the solution space smoother and allows information about the eigenvectors to propagate down to the
   *      locally optimal sparse solution.
   *   2. as we reach the desired sparseness, we employ indicator functions that allow us to estimate the true eigenvector 
   *      for the selected sub-matrix.  note that sub-matrices may be redundant with other submatrices so that eigenvectors 
   *      will usually not be perfectly orthogonal to each other.  this often leads to explained variance > 1 which indicates
   *      the degree of redundancy.  it is possible to estimate the 'true' explained variance which is often fairly low. 
   *      indicator functions lead to a solid convergent solution.  
   */
  double convcrit=1;
  float ann[100]; // set up the sparseness schedule 
  for (unsigned int i=0; i<100; i++) {
    if ( i < 20 ) ann[i]=1;
    else {
    ann[i]=1.0-(float)i/100.0;
    if (ann[i] < fabs(this->m_FractionNonZeroP) ) ann[i]=this->m_FractionNonZeroP; // dont go below user specified sparseness
    }
  }   
  bool anneal=false; // reduce sparseness slowly  
  bool doind=true;  // use indicator function when we are itoff iterations from the maximum iterations & if the solution has not yet converged.
  unsigned int itoff=10; // the last itoff iterations just optimize wrt user selected sparseness
  RealType fnp=1;
  while ( loop < maxloop && convcrit > 1.e-8 ) {
    fnp=this->m_FractionNonZeroP;
    //if (loop<20) fnp=1; else 
    if ( !condition2 ) condition2=( loop > (maxloop-itoff+1) ||  convcrit < 1.e-5   );
    if ( anneal && loop < (maxloop-itoff) ) {
      if ( fabs(fnp) < fabs(this->m_FractionNonZeroP) ) fnp=this->m_FractionNonZeroP;
      if ( loop < 100 ) fnp=ann[loop];
    }
  for ( unsigned int k=0; k<n_vecs; k++) {
    if (debug) Rcpp::Rcout<<"kloopstart"<<std::endl;
    VectorType ptemp=this->m_VariatesP.get_column(k);
    vnl_diag_matrix<TRealType> indicator(this->m_MatrixP.cols(),1);
    MatrixType pmod=this->m_MatrixP;
    //don't use the indicator function if you are not even close to the solution 
    if ( doind && condition2 && !this->m_KeepPositiveP ) for ( unsigned int j=0; j< ptemp.size(); j++) if ( fabs(ptemp(j)) < this->m_Epsilon ) indicator(j,j)=0; 
    if ( doind && condition2 &&  this->m_KeepPositiveP ) { for ( unsigned int j=0; j< ptemp.size(); j++) if ( fabs(ptemp(j)) < this->m_Epsilon ) indicator(j,j)=0; fnp=1; }
    if ( doind && condition2                           ) pmod=pmod*indicator; 
    VectorType pveck=(pmod.transpose()*(pmod*ptemp));
    //  X^T X x
    RealType hkkm1=pveck.two_norm();
    if ( hkkm1 > this->m_Epsilon /* && k == 0 */  ) pveck=pveck/hkkm1;
    //   \forall j \ne i x_j \perp x_i 
    for ( unsigned int j=0; j< k; j++) {
      VectorType qj=this->m_VariatesP.get_column(j);
      RealType ip=inner_product(qj,qj);
      if (ip < this->m_Epsilon) ip=1; 
      RealType hjk=inner_product(qj,pveck)/ip;
      pveck=pveck-qj*hjk;
    }
    //  x_i is sparse  
    if ( fnp < 1 ){
      if ( this->m_KeepPositiveP ) this->ConstantProbabilityThreshold( pveck , fnp , this->m_KeepPositiveP );  else
      this->ReSoftThreshold( pveck , fnp , !this->m_KeepPositiveP );
      VectorType pveck_temp(pveck);
      this->ClusterThresholdVariate( pveck_temp , this->m_MaskImageP, this->m_MinClusterSizeP );
      if ( pveck_temp.two_norm() > 0 ) pveck=pveck_temp;
    }
    if ( doind && condition2 && this->m_KeepPositiveP ) pveck=indicator*pveck; 
    double pveckabssum=0;
    for (unsigned int pp=0;pp<pveck.size();pp++) 
      {
      double v=fabs(pveck(pp));
      pveckabssum+=v;
      }
    if ( pveckabssum > 0 ) pveck=pveck/pveckabssum;
    this->m_ClusterSizes[k]=this->m_KeptClusterSize;
    this->m_VariatesP.set_column(k,pveck);
    if (debug) Rcpp::Rcout<<"kloopdone"<<k<<" pveckabssum " << pveckabssum << std::endl;
  } //kloop 
  this->m_VariatesQ=this->m_VariatesP;
  if (debug) Rcpp::Rcout<<" get evecs "<<std::endl;
  RealType vex=this->ComputeSPCAEigenvalues(n_vecs,trace);
  vexlist.push_back(   vex    );
  this->SortResults(n_vecs); 
  convcrit=fabs( this->ComputeEnergySlope(vexlist, 20) );
  Rcpp::Rcout <<"Iteration: " << loop << " Eigenvals: " << this->m_CanonicalCorrelations/trace << " Sparseness: " << fnp  << " convergence-criterion: " << convcrit <<  " vex " << vex << std::endl;
  loop++;
  if (debug) Rcpp::Rcout<<"wloopdone"<<std::endl;
  }//opt-loop
  Rcpp::Rcout << " cluster-sizes " << this->m_ClusterSizes << std::endl;
  return fabs(this->m_CanonicalCorrelations[0]);
}

template <class TInputImage, class TRealType>
TRealType antsSCCANObject<TInputImage, TRealType>
::ComputeSPCAEigenvalues(unsigned int n_vecs, TRealType trace)
{
  //   we have   variates  P = X  ,  Q = X^T  ,    Cov \approx \sum_i eval_i E_i^t E_i 
  //   where E_i - eigenvector,  eval_i eigenvalue 
  unsigned long mind=this->m_MatrixP.rows();
  if ( mind > this->m_MatrixP.cols() ) mind=this->m_MatrixP.cols();
  double avgdifffromevec=0;
  double evalsum=0;
  // we estimate variance explained by  \sum_i eigenvalue_i / trace(A) ...
  
  // shen and huang 
  //  MatrixType kcovmat=this->VNLPseudoInverse( this->m_VariatesP.transpose()*this->m_VariatesP )*this->m_VariatesP.transpose();
  // kcovmat=(this->m_MatrixP*this->m_VariatesP)*kcovmat;
  // double ktrace=vnl_trace<double>(   kcovmat  ); 
  // Rcpp::Rcout <<" ktr  " << ktrace << std::endl;
  for ( unsigned int i=0; i < n_vecs ; i++ ) 
  {
    VectorType  u=this->m_VariatesP.get_column(i);
    vnl_diag_matrix<TRealType> indicator(this->m_MatrixP.cols(),1);
    //    for ( unsigned int j=0; j< u.size(); j++) if ( fabs(u(j)) <= this->m_Epsilon ) indicator(j,j)=0; 
    MatrixType pmod=this->m_MatrixP*indicator; 
    VectorType proj=(pmod*u);
    VectorType m=pmod.transpose()*proj;
    double eigenvalue_i=0;
    double unorm=u.two_norm();
    if (unorm > this->m_Epsilon) eigenvalue_i=m.two_norm()/unorm; else unorm=1;
    // factor out the % of the eigenvector that is not orthogonal to higher ranked eigenvectors 
    for ( unsigned int j=0; j < i ; j++ ) 
      {
	//	VectorType  v=this->m_MatrixP*this->m_VariatesP.get_column(j);
	VectorType  v=this->m_VariatesP.get_column(j);
	double vn=v.two_norm();
	double ip=1;
	// double p2n=proj.two_norm();
	//	if ( vn > this->m_Epsilon && p2n >  this->m_Epsilon  ) ip=1-inner_product( proj/p2n ,  v/vn );
	if ( vn > this->m_Epsilon && unorm >  this->m_Epsilon  ) ip=1-inner_product( u/unorm ,  v/vn );
	eigenvalue_i*=ip;
	//	Rcpp::Rcout <<" ip " << j << " is " << ip << " ei " << eigenvalue_i << std::endl;
      }
    if ( i < mind-1 ) evalsum+=eigenvalue_i;
    VectorType diff=u*eigenvalue_i-m;
    /** this is a good metric of the difference from the eigenvalue 
     *  because, over iterations, we minimize \|  u*eigenvalue_i-m \|^2  
     */
    double d=diff.two_norm()/(double)diff.size();
    avgdifffromevec+=d;
    this->m_CanonicalCorrelations[i]=eigenvalue_i;
  }
  double vex=evalsum;
  if (trace>0) vex/=trace;  
  //  Rcpp::Rcout <<" variance explained: " << vex << std::endl;
  //Rcpp::Rcout <<" eval diff " <<   avgdifffromevec/n_vecs << std::endl;
  return vex;// avgdifffromevec/(TRealType)n_vecs; 
  /*****************************************/
  MatrixType ptemp(this->m_MatrixP);
  VectorType d_i(n_vecs,0);
  for ( unsigned int i=0; i < n_vecs ; i++ ) 
  {
    // find d_i
    MatrixType m=outer_product(this->m_VariatesQ.get_column(i),this->m_VariatesP.get_column(i));
    // now find d_i to bring m close to ptemp
    RealType a=ptemp.frobenius_norm();
    RealType b=m.frobenius_norm();
    //    m=m*(a/b);
    RealType hypod=inner_product(this->m_VariatesQ.get_column(i),this->m_MatrixP*this->m_VariatesP.get_column(i));
    Rcpp::Rcout <<" hypod " << hypod << " a " << a << " b " << b << " a/b " << a/b << " " << std::endl;
    ptemp=ptemp+m*a;
  }
  return 0;

}

template <class TInputImage, class TRealType>
TRealType antsSCCANObject<TInputImage, TRealType>
::SparsePartialArnoldiCCA(unsigned int n_vecs_in)
{
  unsigned int n_vecs=n_vecs_in;
  if (n_vecs<2) n_vecs=2;
  this->m_CanonicalCorrelations.set_size(n_vecs);
  this->m_CanonicalCorrelations.fill(0); 
  Rcpp::Rcout <<" arnoldi sparse partial cca " << std::endl;
  Rcpp::Rcout << "  pos-p " << this->GetKeepPositiveP() << " pos-q "<< this->GetKeepPositiveQ() << std::endl;
  this->m_MatrixP=this->NormalizeMatrix(this->m_OriginalMatrixP);  
  this->m_MatrixQ=this->NormalizeMatrix(this->m_OriginalMatrixQ);
  this->m_MatrixR=this->NormalizeMatrix(this->m_OriginalMatrixR);

  if ( this->m_OriginalMatrixR.size() > 0 ) {
    this->m_MatrixRRt=this->ProjectionMatrix(this->m_OriginalMatrixR);
    if ( this->m_SCCANFormulation == PminusRQ ||  this->m_SCCANFormulation == PminusRQminusR )  
      this->m_MatrixP=this->m_MatrixP-(this->m_MatrixRRt*this->m_MatrixP);
    if ( this->m_SCCANFormulation == PQminusR ||  this->m_SCCANFormulation == PminusRQminusR )  
      this->m_MatrixQ=this->m_MatrixQ-this->m_MatrixRRt*this->m_MatrixQ;
  }

  this->m_VariatesP.set_size(this->m_MatrixP.cols(),n_vecs);
  this->m_VariatesQ.set_size(this->m_MatrixQ.cols(),n_vecs);
  for (unsigned int kk=0;kk<n_vecs; kk++) {
    this->m_VariatesP.set_column(kk,this->InitializeV(this->m_MatrixP));
    this->m_VariatesQ.set_column(kk,this->InitializeV(this->m_MatrixQ));
  }
  unsigned int maxloop=this->m_MaximumNumberOfIterations;
  if ( maxloop < 25 ) maxloop=25;
  for ( unsigned int loop=0; loop<maxloop; loop++) {
  RealType fnp=this->m_FractionNonZeroP;
  RealType fnq=this->m_FractionNonZeroQ;
// Arnoldi Iteration SCCA 
  for ( unsigned int k=0; k<n_vecs; k++) {
    VectorType ptemp=this->m_VariatesP.get_column(k);
    VectorType qtemp=this->m_VariatesQ.get_column(k);
    vnl_diag_matrix<TRealType> indicatorp(this->m_MatrixP.cols(),1);
    vnl_diag_matrix<TRealType> indicatorq(this->m_MatrixQ.cols(),1);
    bool condition2=( loop > ( maxloop-10 ) );
    if ( condition2  ) {
      if ( !  this->m_KeepPositiveP ) for ( unsigned int j=0; j< ptemp.size(); j++) if ( fabs(ptemp(j)) < this->m_Epsilon ) indicatorp(j,j)=0; 
      if ( !  this->m_KeepPositiveQ ) for ( unsigned int j=0; j< qtemp.size(); j++) if ( fabs(qtemp(j)) < this->m_Epsilon ) indicatorq(j,j)=0; 
      if (    this->m_KeepPositiveP ) { for ( unsigned int j=0; j< ptemp.size(); j++) if ( fabs(ptemp(j)) < this->m_Epsilon ) indicatorp(j,j)=0; fnp=1 ;}
      if (    this->m_KeepPositiveQ ) { for ( unsigned int j=0; j< qtemp.size(); j++) if ( fabs(qtemp(j)) < this->m_Epsilon ) indicatorq(j,j)=0; fnp=1 ;}
    }
    MatrixType pmod=this->m_MatrixP*indicatorp; 
    MatrixType qmod=this->m_MatrixQ*indicatorq; 
    VectorType pveck=qmod*qtemp;      
    VectorType qveck=pmod*ptemp;     
    pveck=pmod.transpose()*pveck;
    qveck=qmod.transpose()*qveck;
    if ( k > 0 )
    for ( unsigned int j=0; j< k; j++) {
      VectorType qj=this->m_VariatesP.get_column(j);
      VectorType pmqj=qj; // pmqj=pmod*qj;
      RealType ip=inner_product(pmqj,pmqj);
      if (ip < this->m_Epsilon) ip=1;
      RealType hjk=inner_product(pmqj,pveck)/ip;
      pveck=pveck-hjk*qj; 

      qj=this->m_VariatesQ.get_column(j); 
      pmqj=qj;      //      pmqj=qmod*qj;
      ip=inner_product(pmqj,pmqj);
      if (ip < this->m_Epsilon) ip=1;
      hjk=inner_product(pmqj,qveck)/ip;
      qveck=qveck-hjk*qj; 
    }
    if ( this->m_KeepPositiveP ) 
      this->ConstantProbabilityThreshold( pveck , fnp , this->m_KeepPositiveP );   
    else this->ReSoftThreshold( pveck , fnp , !this->m_KeepPositiveP );
    if ( this->m_KeepPositiveQ ) 
      this->ConstantProbabilityThreshold( qveck , fnq , this->m_KeepPositiveQ );
    else this->ReSoftThreshold( qveck , fnq , !this->m_KeepPositiveQ );
    if ( condition2 && this->m_KeepPositiveP ) pveck=indicatorp*pveck; 
    if ( condition2 && this->m_KeepPositiveQ ) qveck=indicatorq*qveck; 
    if ( loop > 2 ) {
      if ( this->m_MaskImageP ) 
        this->ClusterThresholdVariate( pveck , this->m_MaskImageP, this->m_MinClusterSizeP );
      if ( this->m_MaskImageQ ) 
	this->ClusterThresholdVariate( qveck , this->m_MaskImageQ, this->m_MinClusterSizeQ );
    }
    RealType hkkm1=pveck.two_norm();
    if ( hkkm1 > this->m_Epsilon ) this->m_VariatesP.set_column(k,pveck/hkkm1);
             hkkm1=qveck.two_norm();
    if ( hkkm1 > this->m_Epsilon ) this->m_VariatesQ.set_column(k,qveck/hkkm1);
    this->NormalizeWeightsByCovariance(k); 
    this->m_CanonicalCorrelations[k]=this->PearsonCorr(  this->m_MatrixP*this->m_VariatesP.get_column(k)   , this->m_MatrixQ*this->m_VariatesQ.get_column(k)  ); 
  }
  if ( loop > 0 ) this->SortResults(n_vecs);  
  Rcpp::Rcout <<" Loop " << loop << " Corrs : " << this->m_CanonicalCorrelations << " sparp " << fnp << " sparq " << fnq << std::endl;
  } // outer loop 
  this->SortResults(n_vecs);  
  //  this->RunDiagnostics(n_vecs);
  double ccasum=0; for (unsigned int i=0; i<this->m_CanonicalCorrelations.size(); i++) ccasum+=fabs(this->m_CanonicalCorrelations[i]);
  if ( n_vecs_in > 1 ) return ccasum; // fabs(this->m_CanonicalCorrelations[1])+fabs(this->m_CanonicalCorrelations[0]);
  else return fabs(this->m_CanonicalCorrelations[0]);


/* **************************************************************************************************

// now deal with covariates --- this could work but needs to be fixed. 
    for ( unsigned int j=0; j< this->m_MatrixR.cols(); j++) {
    // FIXME is this really what qj should be?  it should be the projection of the nuisance variable 
    //    into the space of qj ...  
      VectorType cov=this->m_MatrixR.get_column(j);
      VectorType qj=cov*this->m_MatrixP;
      RealType hjk=inner_product(cov,this->m_MatrixP*pveck)/
                   inner_product(cov,cov);
      if ( this->m_SCCANFormulation == PminusRQ ||  this->m_SCCANFormulation == PminusRQminusR )  
        for (unsigned int i=0; i<pveck.size(); i++)  pveck(i)=pveck(i)-hjk*qj(i); 
      qj=cov*this->m_MatrixQ; 
      hjk=inner_product(cov,this->m_MatrixQ*qveck)/
          inner_product(cov,cov);
      if ( this->m_SCCANFormulation == PQminusR ||  this->m_SCCANFormulation == PminusRQminusR )  
        for (unsigned int i=0; i<qveck.size(); i++)  qveck(i)=qveck(i)-hjk*qj(i); 
    }

// evil code below ....

    if ( this->m_SCCANFormulation != PQ ) {
    for ( unsigned int dd=0; dd<20 ; dd++) {
      VectorType upp(this->m_MatrixP.cols(),0);
      VectorType upq(this->m_MatrixQ.cols(),0);
        for ( unsigned int rr=0; rr<this->m_MatrixR.cols(); rr++) {
          pveck=this->m_MatrixQ*qtemp;      
          qveck=this->m_MatrixP*ptemp;	
          pveck=pveck/pveck.two_norm();
          qveck=qveck/qveck.two_norm();
          pveck=this->Orthogonalize(pveck,this->m_OriginalMatrixR.get_column(rr));
          qveck=this->Orthogonalize(qveck,this->m_OriginalMatrixR.get_column(rr));
          VectorType tempp=this->m_MatrixP.transpose()*pveck;
          VectorType tempq=this->m_MatrixQ.transpose()*qveck;
          pveck=pveck/pveck.two_norm(); 
          upp=upp+tempp/tempp.two_norm()*1.0/(this->m_MatrixR.cols());
          qveck=qveck/qveck.two_norm(); 
          upq=upq+tempq/tempq.two_norm()*1.0/(this->m_MatrixR.cols());
        } //rr 
      RealType eps=.01;
      if ( dd % 2 == 0 )  
      {
        qtemp=qtemp+eps*upq;
//      RealType c1=this->PearsonCorr(this->m_OriginalMatrixR.get_column(rr),this->m_MatrixQ*qtemp);
//      RealType c2=this->PearsonCorr(this->m_OriginalMatrixR.get_column(rr),this->m_MatrixQ*(qtemp+eps*upq));
//      RealType c3=this->PearsonCorr(this->m_OriginalMatrixR.get_column(rr),this->m_MatrixQ*(qtemp-eps*upq));
//      if ( fabs(c2) < fabs(c1) )  qtemp=qtemp+eps*upq;
//      if ( fabs(c3) < fabs(c1) )  qtemp=qtemp-eps*upq;
      }
      else 
      { 
        ptemp=ptemp+eps*upp;
//      RealType c1=this->PearsonCorr(this->m_OriginalMatrixR.get_column(rr),this->m_MatrixP*ptemp);
//      RealType c2=this->PearsonCorr(this->m_OriginalMatrixR.get_column(rr),this->m_MatrixP*(ptemp+eps*upp));
//      RealType c3=this->PearsonCorr(this->m_OriginalMatrixR.get_column(rr),this->m_MatrixP*(ptemp-eps*upp));
//      if ( fabs(c2) < fabs(c1) )  ptemp=ptemp+eps*upp;
//      if ( fabs(c3) < fabs(c1) )  ptemp=ptemp-eps*upp;
      }
      ptemp=ptemp/ptemp.two_norm();
      qtemp=qtemp/qtemp.two_norm();
      pveck=this->m_MatrixQ*qtemp;
      qveck=this->m_MatrixP*ptemp;
    } //dd
    } //      if ( this->m_SCCANFormulation != PQ ) {



       VectorType proj=this->m_MatrixQ*this->m_WeightsQ;
	if ( false && ( this->m_SCCANFormulation == PminusRQ ||  this->m_SCCANFormulation == PminusRQminusR ) )
        for (unsigned int kk=0; kk< this->m_OriginalMatrixR.cols(); kk++) 
          proj=this->Orthogonalize(proj,this->m_MatrixR.get_column(kk));
        this->m_WeightsP=this->m_MatrixP.transpose()*(proj);
*/ 

}




template <class TInputImage, class TRealType>
void antsSCCANObject<TInputImage, TRealType>
::WhitenDataSetForRunSCCANMultiple(unsigned int nvecs)
{
    if ( this->m_Debug ) Rcpp::Rcout << " now whiten and apply R " << std::endl;
    if ( this->m_OriginalMatrixR.size() > 0 || nvecs > 0  ) {
      this->m_MatrixP=this->NormalizeMatrix(this->m_OriginalMatrixP);  
      if ( this->m_VariatesP.size() > 0 ) {
        this->m_MatrixRp.set_size(this->m_MatrixP.rows(),this->m_OriginalMatrixR.cols()+nvecs);
        this->m_MatrixRp.fill(0);
	if ( this->m_OriginalMatrixR.size() > 0  && ( this->m_SCCANFormulation == PminusRQ ||  this->m_SCCANFormulation == PminusRQminusR ) ) {
    	  this->m_MatrixRp.set_columns(0,this->m_OriginalMatrixR);
	  this->m_MatrixRp.set_columns(this->m_OriginalMatrixR.cols(), 
            this->m_MatrixP*(this->m_VariatesP.get_n_columns(0,nvecs)));
        }
	else {
    	  this->m_MatrixRp.set_columns(0, 
            this->m_MatrixP*(this->m_VariatesP.get_n_columns(0,nvecs)));
        }
      } 
      else {
        this->m_MatrixRp=this->NormalizeMatrix(this->m_OriginalMatrixR);  
      }
      this->m_MatrixRp=ProjectionMatrix(this->m_MatrixRp);
  if (this->m_Debug && this->m_VariatesP.cols() > 1 ) {
    Rcpp::Rcout <<" corr-pre " << this->PearsonCorr( (this->m_OriginalMatrixP*this->m_VariatesP).get_column(0) , (this->m_OriginalMatrixP*this->m_VariatesP).get_column(1) ) << std::endl  ; Rcpp::Rcout <<" corr-post " << this->PearsonCorr( (this->m_MatrixP*this->m_VariatesP).get_column(0) , (this->m_MatrixP*this->m_VariatesP).get_column(1) ) << std::endl; 
  }
  

      this->m_MatrixQ=this->NormalizeMatrix(this->m_OriginalMatrixQ);  
      if ( this->m_VariatesQ.size() > 0 ) {
        this->m_MatrixRq.set_size(this->m_MatrixQ.rows(),this->m_OriginalMatrixR.cols()+nvecs);
        this->m_MatrixRq.fill(0);
	if ( this->m_OriginalMatrixR.size() > 0  && ( this->m_SCCANFormulation == PQminusR ||  this->m_SCCANFormulation == PminusRQminusR )  )
        {
    	  this->m_MatrixRq.set_columns(0,this->m_OriginalMatrixR);
	  this->m_MatrixRq.set_columns(this->m_OriginalMatrixR.cols(), 
            this->m_MatrixQ*(this->m_VariatesQ.get_n_columns(0,nvecs)));
        }
	else {
    	  this->m_MatrixRq.set_columns(0, 
            this->m_MatrixQ*(this->m_VariatesQ.get_n_columns(0,nvecs)));
        }
      }
      else {
        this->m_MatrixRq=this->NormalizeMatrix(this->m_OriginalMatrixR); 
      }
      this->m_MatrixRq=ProjectionMatrix(this->m_MatrixRq);
    }
    else {
      this->m_MatrixP=this->NormalizeMatrix(this->m_OriginalMatrixP);  
      this->m_MatrixQ=this->NormalizeMatrix(this->m_OriginalMatrixQ);
 //     this->m_MatrixP=this->WhitenMatrix(this->m_MatrixP);  
 //     this->m_MatrixQ=this->WhitenMatrix(this->m_MatrixQ);
    }


    if ( this->m_Debug ) Rcpp::Rcout << "  whiten and apply R done " << std::endl;
}


template <class TInputImage, class TRealType>
void antsSCCANObject<TInputImage, TRealType>
::NormalizeWeightsByCovariance(unsigned int k)
{
//  for ( unsigned int k=0; k<this->m_VariatesP.cols(); k++) 
  {
  this->m_WeightsP=this->m_VariatesP.get_column(k);
  this->m_WeightsQ=this->m_VariatesQ.get_column(k);
  VectorType w=this->m_MatrixP*this->m_WeightsP;
  RealType normP=0;
  if ( this->m_MatrixRp.size() > 0 ) normP=inner_product( w , (this->m_MatrixP-this->m_MatrixRp*this->m_MatrixP)*this->m_WeightsP ); 
  else normP=inner_product(w,w);
  if (normP>this->m_Epsilon) this->m_WeightsP=this->m_WeightsP/sqrt(normP);

  w=this->m_MatrixQ*this->m_WeightsQ;
  RealType normQ=0;
  if ( this->m_MatrixRq.size() > 0 ) normQ=inner_product( w , (this->m_MatrixQ-this->m_MatrixRq*this->m_MatrixQ)*this->m_WeightsQ ); 
  else normQ=inner_product(w,w);
  if (normQ>this->m_Epsilon) this->m_WeightsQ=this->m_WeightsQ/sqrt(normQ);
  }
}

template <class TInputImage, class TRealType>
TRealType
antsSCCANObject<TInputImage, TRealType>
::RunSCCAN2multiple( unsigned int n_vecs ) 
{
  this->m_Debug=false;
//  this->m_Debug=true;
  Rcpp::Rcout << " power iteration (partial) scca " <<std::endl;
  this->m_CanonicalCorrelations.set_size(n_vecs);
  this->m_CanonicalCorrelations.fill(0); 
  RealType truecorr=0;
  unsigned int nr1=this->m_MatrixP.rows();
  unsigned int nr2=this->m_MatrixQ.rows();
  this->m_VariatesP.set_size(0,0);
  this->m_VariatesQ.set_size(0,0);
  if ( nr1 != nr2 ) 
  {
    Rcpp::Rcout<< " P rows " << this->m_MatrixP.rows() << " cols " << this->m_MatrixP.cols() << std::endl;
    Rcpp::Rcout<< " Q rows " << this->m_MatrixQ.rows() << " cols " << this->m_MatrixQ.cols() << std::endl;
    Rcpp::Rcout<< " R rows " << this->m_MatrixR.rows() << " cols " << this->m_MatrixR.cols() << std::endl;
    Rcpp::Rcout<<" N-rows for MatrixP does not equal N-rows for MatrixQ " << nr1 << " vs " << nr2 << std::endl;
    throw std::exception() ;
  }
  if  (  !this->m_AlreadyWhitened  ) {
    if ( this->m_Debug ) Rcpp::Rcout << " whiten " << std::endl;
    this->WhitenDataSetForRunSCCANMultiple();    
    this->m_AlreadyWhitened=true; 
    if ( this->m_Debug ) Rcpp::Rcout << " whiten done " << std::endl;
  } 
  this->m_VariatesP.set_size(this->m_MatrixP.cols(),n_vecs);
  this->m_VariatesQ.set_size(this->m_MatrixQ.cols(),n_vecs);
  for (unsigned int kk=0;kk<n_vecs; kk++) {
    this->m_VariatesP.set_column(kk,this->InitializeV(this->m_MatrixP));
    this->m_VariatesQ.set_column(kk,this->InitializeV(this->m_MatrixQ));
  }

// begin computing solution 
  for ( unsigned int makesparse=1; makesparse<2 ; makesparse++ ) {
  unsigned int which_e_vec=0; 
  bool notdone=true;
  while ( notdone ) {
    if ( this->m_Debug ) Rcpp::Rcout << " get canonical variate number " << which_e_vec+1 << std::endl;
    double initcorr=1.e-5;
    truecorr=initcorr;
    double deltacorr=1,lastcorr=initcorr*0.5;
    this->m_WeightsP= this->m_VariatesP.get_column(which_e_vec);
    this->m_WeightsQ= this->m_VariatesQ.get_column(which_e_vec);
    unsigned long its=0, min_its=5;
    if ( this->m_Debug ) Rcpp::Rcout << " Begin " << std::endl;
    while ( its < this->m_MaximumNumberOfIterations && deltacorr > this->m_ConvergenceThreshold  || its < min_its )
    {
      if ( its == 0 ) this->WhitenDataSetForRunSCCANMultiple(which_e_vec);
      bool doorth=true;  //this->m_Debug=true;
      {
        VectorType proj=this->m_MatrixQ*this->m_WeightsQ;
	if ( this->m_MatrixRp.size() > 0 && ( this->m_SCCANFormulation == PminusRQ ||  this->m_SCCANFormulation == PminusRQminusR )  ) { this->m_WeightsP=this->m_MatrixP.transpose()*(proj - this->m_MatrixRp*proj);}
	else this->m_WeightsP=this->m_MatrixP.transpose()*(proj);
        if ( doorth ) for (unsigned int kk=0; kk<which_e_vec; kk++) 
          this->m_WeightsP=this->Orthogonalize(this->m_WeightsP,this->m_VariatesP.get_column(kk),&this->m_MatrixP,&this->m_MatrixP);
        this->m_WeightsP=this->SoftThreshold( this->m_WeightsP , this->m_FractionNonZeroP , !this->m_KeepPositiveP );
	if ( its > 0 ) 
 	  this->m_WeightsP=this->ClusterThresholdVariate( this->m_WeightsP , this->m_MaskImageP );
	if (which_e_vec > 0   && this->m_Debug   ){
 	  Rcpp::Rcout << " p orth-b " << this->PearsonCorr( this->m_MatrixP*this->m_WeightsP,this->m_MatrixP*this->m_VariatesP.get_column(0) ) << std::endl;
        }
      }
      VectorType projp=this->m_MatrixQ*this->m_WeightsQ;
      VectorType projq=this->m_MatrixP*this->m_WeightsP;

      {
        VectorType proj=this->m_MatrixP*this->m_WeightsP;
	if ( this->m_MatrixRq.size() > 0  && ( this->m_SCCANFormulation == PQminusR ||  this->m_SCCANFormulation == PminusRQminusR )   ) this->m_WeightsQ=this->m_MatrixQ.transpose()*(proj - this->m_MatrixRq*proj);
	else this->m_WeightsQ=this->m_MatrixQ.transpose()*(proj);
        if ( doorth ) for (unsigned int kk=0; kk<which_e_vec; kk++) 
          this->m_WeightsQ=this->Orthogonalize(this->m_WeightsQ,this->m_VariatesQ.get_column(kk),&this->m_MatrixQ,&this->m_MatrixQ);
        this->m_WeightsQ=this->SoftThreshold( this->m_WeightsQ , this->m_FractionNonZeroQ , !this->m_KeepPositiveQ );
 	if (which_e_vec > 0 && this->m_Debug ){
  	  Rcpp::Rcout << " q orth-b " << this->PearsonCorr( this->m_MatrixQ*this->m_WeightsQ,this->m_MatrixQ*this->m_VariatesQ.get_column(0) ) << std::endl;
        }
      }
      this->m_WeightsP=this->m_MatrixP.transpose()*(projp);
      this->m_WeightsQ=this->m_MatrixQ.transpose()*(projq);
      this->ReSoftThreshold( this->m_WeightsP , this->m_FractionNonZeroP , this->m_KeepPositiveP );
      this->ReSoftThreshold( this->m_WeightsQ , this->m_FractionNonZeroQ , this->m_KeepPositiveQ );
      if ( its > 1 ) {
        this->m_WeightsP=this->ClusterThresholdVariate( this->m_WeightsP , this->m_MaskImageP, this->m_MinClusterSizeP );
        this->m_WeightsQ=this->ClusterThresholdVariate( this->m_WeightsQ , this->m_MaskImageQ, this->m_MinClusterSizeQ );
      }
      for (unsigned int kk=0; kk< which_e_vec; kk++) 
        this->m_WeightsP=this->Orthogonalize(this->m_WeightsP,this->m_VariatesP.get_column(kk),&this->m_MatrixP,&this->m_MatrixP);
      if ( ( this->m_SCCANFormulation == PminusRQ ||  this->m_SCCANFormulation == PminusRQminusR ) )
        for (unsigned int kk=0; kk< this->m_OriginalMatrixR.cols(); kk++) {
          this->m_WeightsP=this->Orthogonalize(this->m_WeightsP,this->m_MatrixR.get_column(kk)*this->m_MatrixP,&this->m_MatrixP,&this->m_MatrixP);
        }
      for (unsigned int kk=0; kk< which_e_vec; kk++) 
        this->m_WeightsQ=this->Orthogonalize(this->m_WeightsQ,this->m_VariatesQ.get_column(kk),&this->m_MatrixQ,&this->m_MatrixQ);
      if ( ( this->m_SCCANFormulation == PQminusR ||  this->m_SCCANFormulation == PminusRQminusR ) )
        for (unsigned int kk=0; kk< this->m_OriginalMatrixR.cols(); kk++) {
          this->m_WeightsQ=this->Orthogonalize(this->m_WeightsQ,this->m_MatrixR.get_column(kk)*this->m_MatrixQ,&this->m_MatrixQ,&this->m_MatrixQ);
        }

      this->NormalizeWeightsByCovariance(which_e_vec); 
      this->m_VariatesP.set_column(which_e_vec,this->m_WeightsP);
      this->m_VariatesQ.set_column(which_e_vec,this->m_WeightsQ);
      truecorr=this->PearsonCorr( this->m_MatrixP*this->m_WeightsP , this->m_MatrixQ*this->m_WeightsQ );
      if ( this->m_Debug ) 
        Rcpp::Rcout << " corr " << truecorr << " it " << its << std::endl;
      deltacorr=fabs(truecorr-lastcorr);
      lastcorr=truecorr;
      ++its;
      this->m_CanonicalCorrelations[which_e_vec]=truecorr; 
      Rcpp::Rcout << "  canonical variate number " << which_e_vec+1 << " corr " << this->m_CanonicalCorrelations[which_e_vec]  << " kept cluster " << this->m_KeptClusterSize << std::endl;
    }// inner_it
    if ( fabs(truecorr) < 1.e-2 || (which_e_vec+1) == n_vecs ) notdone=false;
    else which_e_vec++;
  }

  if ( this->m_Debug ) Rcpp::Rcout << " done with loop " << std::endl;
  std::vector<TRealType> evals(n_vecs,0);
  std::vector<TRealType> oevals(n_vecs,0);
  for ( long j=0; j<n_vecs; ++j){
    RealType val=fabs(this->m_CanonicalCorrelations[j]);
    evals[j]=val;
    oevals[j]=val;
  }

// sort and reindex the eigenvectors/values 
  sort (evals.begin(), evals.end(), my_sccan_sort_object); 
  std::vector<int> sorted_indices(n_vecs,-1);
  for (unsigned int i=0; i<evals.size(); i++) {
  for (unsigned int j=0; j<evals.size(); j++) {
    if ( evals[i] == oevals[j] &&  sorted_indices[i] == -1 ) {
      sorted_indices[i]=j;
      oevals[j]=0;
    }
  }}
  
  VectorType newcorrs(n_vecs,0);
  MatrixType varp(this->m_MatrixP.cols(),n_vecs,0);
  MatrixType varq(this->m_MatrixQ.cols(),n_vecs,0);
  for (unsigned int i=0; i<n_vecs; i++) {
    varp.set_column(i,this->m_VariatesP.get_column( sorted_indices[i] ));
    varq.set_column(i,this->m_VariatesQ.get_column( sorted_indices[i] ));
    newcorrs[i]=(this->m_CanonicalCorrelations[sorted_indices[i]]);
  }
  for (unsigned int i=0; i<n_vecs; i++) {
    this->m_VariatesP.set_column(i,varp.get_column( i ));
    this->m_VariatesQ.set_column(i,varq.get_column( i ));
  }
  this->m_CanonicalCorrelations=newcorrs;
//  this->RunDiagnostics(n_vecs);

  }// makesparse

  RealType corrsum=0; 
  for ( unsigned int i=0; i < this->m_CanonicalCorrelations.size(); i++) {
    corrsum+=fabs(this->m_CanonicalCorrelations[i]);
  }
  return this->m_CanonicalCorrelations[0]; //corrsum;
}


template <class TInputImage, class TRealType>
TRealType antsSCCANObject<TInputImage, TRealType>
::RunSCCAN2( ) 
{
  RealType truecorr=0;
  unsigned int nr1=this->m_MatrixP.rows();
  unsigned int nr2=this->m_MatrixQ.rows();
  if ( nr1 != nr2 ) 
  {
    Rcpp::Rcout<< " P rows " << this->m_MatrixP.rows() << " cols " << this->m_MatrixP.cols() << std::endl;
    Rcpp::Rcout<< " Q rows " << this->m_MatrixQ.rows() << " cols " << this->m_MatrixQ.cols() << std::endl;
    Rcpp::Rcout<< " R rows " << this->m_MatrixR.rows() << " cols " << this->m_MatrixR.cols() << std::endl;
    Rcpp::Rcout<<" N-rows for MatrixP does not equal N-rows for MatrixQ " << nr1 << " vs " << nr2 << std::endl;
    throw std::exception() ;
  }
  else {
//  Rcpp::Rcout << " P-positivity constraints? " <<  this->m_KeepPositiveP << " frac " << this->m_FractionNonZeroP << " Q-positivity constraints?  " << m_KeepPositiveQ << " frac " << this->m_FractionNonZeroQ << std::endl;
  }
  this->m_WeightsP=this->InitializeV(this->m_MatrixP);
  this->m_WeightsQ=this->InitializeV(this->m_MatrixQ);

//  if ( !this->m_AlreadyWhitened ) 
  {
   if ( this->m_Debug ) Rcpp::Rcout <<" norm P " << std::endl;
    this->m_MatrixP=this->NormalizeMatrix(this->m_MatrixP);  
   if ( this->m_Debug ) Rcpp::Rcout <<" norm Q " << std::endl;
    this->m_MatrixQ=this->NormalizeMatrix(this->m_MatrixQ);  
    if ( this->m_OriginalMatrixR.size() > 0 ) {
      this->m_MatrixR=this->NormalizeMatrix(this->m_OriginalMatrixR);  
      this->m_MatrixR=this->WhitenMatrix(this->m_MatrixR);  
      this->m_MatrixRRt=this->m_MatrixR*this->m_MatrixR.transpose(); 
      this->UpdatePandQbyR( );
    }
    this->m_MatrixP=this->WhitenMatrix(this->m_MatrixP);  
    this->m_MatrixQ=this->WhitenMatrix(this->m_MatrixQ);
    this->m_AlreadyWhitened=true;
  }  
  for (unsigned int outer_it=0; outer_it<2; outer_it++) {
  truecorr=0;
  double deltacorr=1,lastcorr=1;
  unsigned long its=0;
  while ( its < this->m_MaximumNumberOfIterations && deltacorr > this->m_ConvergenceThreshold  )
  {
    this->m_WeightsP=this->TrueCCAPowerUpdate(this->m_FractionNonZeroP,this->m_MatrixP,this->m_WeightsQ,this->m_MatrixQ,this->m_KeepPositiveP,false);
    this->m_WeightsQ=this->TrueCCAPowerUpdate(this->m_FractionNonZeroQ,this->m_MatrixQ,this->m_WeightsP,this->m_MatrixP,this->m_KeepPositiveQ,false);    
    truecorr=this->PearsonCorr( this->m_MatrixP*this->m_WeightsP , this->m_MatrixQ*this->m_WeightsQ );
    deltacorr=fabs(truecorr-lastcorr);
    lastcorr=truecorr;
    ++its;
  }// inner_it 
  }//outer_it 
  this->m_CorrelationForSignificanceTest=truecorr;
  return truecorr;
}

template <class TInputImage, class TRealType>
TRealType
antsSCCANObject<TInputImage, TRealType>
::RunSCCAN3( ) 
{
  unsigned int nc1=this->m_MatrixP.rows();
  unsigned int nc2=this->m_MatrixQ.rows();
  unsigned int nc3=this->m_MatrixR.rows();
  if ( nc1 != nc2 || nc1 != nc3 || nc3 != nc2) 
  {
    Rcpp::Rcout<< " P rows " << this->m_MatrixP.rows() << " cols " << this->m_MatrixP.cols() << std::endl;
    Rcpp::Rcout<< " Q rows " << this->m_MatrixQ.rows() << " cols " << this->m_MatrixQ.cols() << std::endl;
    Rcpp::Rcout<< " R rows " << this->m_MatrixR.rows() << " cols " << this->m_MatrixR.cols() << std::endl;
    Rcpp::Rcout<<" N-rows do not match "  << std::endl;
    throw std::exception() ;
  }

  this->m_WeightsP=this->InitializeV(this->m_MatrixP);
  this->m_WeightsQ=this->InitializeV(this->m_MatrixQ);
  this->m_WeightsR=this->InitializeV(this->m_MatrixR);
  if ( !this->m_AlreadyWhitened ) {
  this->m_MatrixP=this->NormalizeMatrix(this->m_MatrixP);  
  this->m_MatrixP=this->WhitenMatrix(this->m_MatrixP);  
  this->m_MatrixQ=this->NormalizeMatrix(this->m_MatrixQ);  
  this->m_MatrixQ=this->WhitenMatrix(this->m_MatrixQ);  
  this->m_MatrixR=this->NormalizeMatrix(this->m_MatrixR);  
  this->m_MatrixR=this->WhitenMatrix(this->m_MatrixR);  
  this->m_AlreadyWhitened=true;
  }
  RealType truecorr=0;
  RealType norm=0,deltacorr=1,lastcorr=1;
  unsigned long its=0;
  while ( its < this->m_MaximumNumberOfIterations && deltacorr > this->m_ConvergenceThreshold  )
  {
  /** for sparse mcca 
   *     w_i \leftarrow \frac{ S( X_i^T ( \sum_{j \ne i} X_j w_j  ) }{norm of above } 
   */
    this->m_WeightsP=this->m_MatrixP.transpose()*(this->m_MatrixQ*this->m_WeightsQ+this->m_MatrixR*this->m_WeightsR);
    this->ReSoftThreshold( this->m_WeightsP , this->m_FractionNonZeroP,this->m_KeepPositiveP);
    norm=this->m_WeightsP.two_norm();
    this->m_WeightsP=this->m_WeightsP/(norm);

    this->m_WeightsQ=this->m_MatrixQ.transpose()*(this->m_MatrixP*this->m_WeightsP+this->m_MatrixR*this->m_WeightsR);
    this->ReSoftThreshold( this->m_WeightsQ , this->m_FractionNonZeroQ,this->m_KeepPositiveQ);
    norm=this->m_WeightsQ.two_norm();
    this->m_WeightsQ=this->m_WeightsQ/(norm);

    this->m_WeightsR=this->m_MatrixR.transpose()*(this->m_MatrixP*this->m_WeightsP+this->m_MatrixQ*this->m_WeightsQ);
    this->ReSoftThreshold( this->m_WeightsR , this->m_FractionNonZeroR,this->m_KeepPositiveR);
    norm=this->m_WeightsR.two_norm();
    this->m_WeightsR=this->m_WeightsR/(norm);

    VectorType pvec=this->m_MatrixP*this->m_WeightsP;
    VectorType qvec=this->m_MatrixQ*this->m_WeightsQ;
    VectorType rvec=this->m_MatrixR*this->m_WeightsR;

    double corrpq=this->PearsonCorr( pvec , qvec );
    double corrpr=this->PearsonCorr( pvec , rvec );
    double corrqr=this->PearsonCorr( rvec , qvec );
    truecorr=corrpq+corrpr+corrqr;
    deltacorr=fabs(truecorr-lastcorr);
    lastcorr=truecorr;
   // Rcpp::Rcout << " correlation of projections: pq " << corrpq << " pr " << corrpr << " qr " << corrqr << " at-it " << its << std::endl;
    its++;

  }
  //  Rcpp::Rcout << " PNZ-Frac " << this->CountNonZero(this->m_WeightsP) << std::endl;
  //  Rcpp::Rcout << " QNZ-Frac " << this->CountNonZero(this->m_WeightsQ) << std::endl;
  //  Rcpp::Rcout << " RNZ-Frac " << this->CountNonZero(this->m_WeightsR) << std::endl;

  this->m_CorrelationForSignificanceTest=truecorr;
  return truecorr;

}

} // namespace ants
} // namespace itk




/*

      if (its == 0 )
      if ( which_e_vec > 0   && false ) 
       {
// here, factor out previous evecs globally 
	MatrixType temp;
	MatrixType pp;
	unsigned int basect=this->m_MatrixR.columns(); 
	basect=0;
	pp.set_size(this->m_MatrixP.rows(),which_e_vec+basect);
//	for (unsigned int kk=0; kk<this->m_MatrixR.columns(); kk++) 
//          pp.set_column(kk,this->m_MatrixR.get_column(kk));
	unsigned int colcount=0; //this->m_MatrixR.columns();
	for (unsigned int kk=which_e_vec-1; kk<which_e_vec; kk++) { 
          pp.set_column(colcount,this->m_MatrixP*this->m_VariatesP.get_column(kk));
	  colcount++;
        }
        temp=this->NormalizeMatrix(pp);  
        temp=this->WhitenMatrix(temp);  
        temp=temp*temp.transpose(); 
        this->m_MatrixP=(this->m_MatrixP-temp*this->m_MatrixP);

	MatrixType qq;
	qq.set_size(this->m_MatrixQ.rows(),which_e_vec+this->m_MatrixR.columns());
	for (unsigned int kk=0; kk<this->m_MatrixR.columns(); kk++) 
          qq.set_column(kk,this->m_MatrixR.get_column(kk));
	colcount=this->m_MatrixR.columns();
	for (unsigned int kk=which_e_vec-1; kk<which_e_vec; kk++) { 
          qq.set_column(colcount,this->m_MatrixQ*this->m_VariatesQ.get_column(kk));
	  colcount++;
        }
        temp=this->NormalizeMatrix(qq);  
        temp=this->WhitenMatrix(temp);  
        temp=temp*temp.transpose(); 
        this->m_MatrixQ=(this->m_MatrixQ-temp*this->m_MatrixQ);
      }




//m_Debug=true; 
      double ip=1; unsigned long ct=0,max_ip_its=50;
      double deltaip=1,lastip=0;
      while ( (deltaip) > 1.e-3 && ct < max_ip_its && which_e_vec > 0 || ct < 4 ) {
        ip=0;
        this->ReSoftThreshold( this->m_WeightsP , this->m_FractionNonZeroP , this->m_KeepPositiveP );
        VectorType ptem=this->m_WeightsP;
	if ( which_e_vec >= 1 ) 
          ptem=this->Orthogonalize(ptem,this->m_VariatesP.get_column(0),&this->m_MatrixP);
  	if ( which_e_vec >= 2 ) 
          ptem=this->Orthogonalize(ptem,this->m_VariatesP.get_column(1),&this->m_MatrixP);
	this->m_WeightsP=ptem;
        this->ReSoftThreshold( this->m_WeightsP , this->m_FractionNonZeroP , this->m_KeepPositiveP );
        ip+=this->PearsonCorr(this->m_MatrixP*this->m_WeightsP,this->m_MatrixP*this->m_VariatesP.get_column(0));
	if ( which_e_vec >= 2) 
          ip+=this->PearsonCorr(this->m_MatrixP*this->m_WeightsP,this->m_MatrixP*this->m_VariatesP.get_column(1));
	deltaip=fabs(lastip)-fabs(ip);
	lastip=ip;
	ct++;
         if ( this->m_Debug ) Rcpp::Rcout << " pip-b " << ip << " delt " << deltaip << std::endl;
        }

       ip=1; ct=0;
       deltaip=1;lastip=0;
      while ( (deltaip) > 1.e-3 && ct < max_ip_its && which_e_vec > 0  || ct < 4 ) {
        ip=0;
        this->ReSoftThreshold( this->m_WeightsQ , this->m_FractionNonZeroQ , this->m_KeepPositiveQ );
        VectorType ptem=this->m_WeightsQ;
	if ( which_e_vec >= 1 ) 
          ptem=this->Orthogonalize(ptem,this->m_VariatesQ.get_column(0),&this->m_MatrixQ);
  	if ( which_e_vec >= 2 ) 
          ptem=this->Orthogonalize(ptem,this->m_VariatesQ.get_column(1),&this->m_MatrixQ);
	this->m_WeightsQ=ptem;
        this->ReSoftThreshold( this->m_WeightsQ , this->m_FractionNonZeroQ , this->m_KeepPositiveQ );
        ip+=this->PearsonCorr(this->m_MatrixQ*this->m_WeightsQ,this->m_MatrixQ*this->m_VariatesQ.get_column(0));
	if ( which_e_vec >= 2) 
          ip+=this->PearsonCorr(this->m_MatrixQ*this->m_WeightsQ,this->m_MatrixQ*this->m_VariatesQ.get_column(1));
	deltaip=fabs(lastip)-fabs(ip);
	lastip=ip;
	ct++;
         if ( this->m_Debug ) Rcpp::Rcout << " qip-b " << ip << " delt " << deltaip << std::endl;
        }


// alternative tools for factoring out evecs 
//      if ( its == 0 && which_e_vec > 0  ) {
//        this->WhitenDataSetForRunSCCANMultiple();   
        q_evecs_factor=this->m_MatrixQ*this->m_VariatesQ.get_n_columns(0,which_e_vec);
        q_evecs_factor=this->NormalizeMatrix( q_evecs_factor );  
        q_evecs_factor=this->WhitenMatrix(q_evecs_factor);  
        q_evecs_factor=q_evecs_factor*q_evecs_factor.transpose(); 
 //       MatrixType temp=this->m_MatrixP-q_evecs_factor*this->m_MatrixP;
 //       temp=this->InverseCovarianceMatrix(temp,&this->m_MatrixP);  
 //       this->m_MatrixP=temp;

        p_evecs_factor=this->m_MatrixP*this->m_VariatesP.get_n_columns(0,which_e_vec);
        p_evecs_factor=this->NormalizeMatrix( p_evecs_factor );  
        p_evecs_factor=this->WhitenMatrix(p_evecs_factor);  
        p_evecs_factor=p_evecs_factor*p_evecs_factor.transpose();
//        temp=this->m_MatrixQ-p_evecs_factor*this->m_MatrixQ;
//        temp=this->InverseCovarianceMatrix(temp,&this->m_MatrixQ);  
//        this->m_MatrixQ=temp;

      }

*/

