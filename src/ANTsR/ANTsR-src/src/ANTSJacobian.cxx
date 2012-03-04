
#include <Rcpp.h>

//#include "DoSomethingToImage.cxx"
#include "itkVectorIndexSelectionCastImageFilter.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "vnl/algo/vnl_determinant.h"

#include "ReadWriteImage.h"


#include "vnl/algo/vnl_determinant.h"
#include "itkDiscreteGaussianImageFilter.h"

#include "itkGradientRecursiveGaussianImageFilter.h"
#include "itkVectorCurvatureAnisotropicDiffusionImageFilter.h"

//#include "itkVectorImageFileReader.h"
#include "itkMatrixOffsetTransformBase.h"
#include "itkWarpImageMultiTransformFilter.h"

template <class TImage>
typename TImage::Pointer VectorAniDiff(typename TImage::Pointer img, unsigned int iters)
{

  double timeStep = 0.065;
  typedef TImage VectorImageType;
  typedef itk::VectorCurvatureAnisotropicDiffusionImageFilter< VectorImageType, VectorImageType >  FilterType;

  typename FilterType::Pointer filter = FilterType::New();
  filter->SetInput( img );
  filter->SetNumberOfIterations( iters );
  filter->SetTimeStep( timeStep );
  filter->SetConductanceParameter(1.0);
  filter->Update();
  // Software Guide : EndCodeSnippet

  return filter->GetOutput();

  }



template <class TImage>
typename TImage::Pointer GenerateGridImage(TImage* img, unsigned int gridsize)
  {

  typedef TImage ImageType;
  enum { ImageDimension = TImage::ImageDimension };

  itk::ImageRegionIteratorWithIndex<ImageType> wimIter( img, img->GetLargestPossibleRegion()  );
    wimIter.GoToBegin();
    for( ; !wimIter.IsAtEnd(); ++wimIter ) wimIter.Set(2);

    wimIter.GoToBegin();
    for( ; !wimIter.IsAtEnd(); ++wimIter )
      {
      typename ImageType::IndexType ind=wimIter.GetIndex();

      for (int i=0; i<2; i++)
        {
          if (ind[i] % gridsize == 0) wimIter.Set(0);
//          if (ind[i] % (gridsize+1) == 0) wimIter.Set(0);// tartan
        }

      }

    wimIter.GoToBegin();
    for( ; !wimIter.IsAtEnd(); ++wimIter )
      {
      typename ImageType::IndexType ind=wimIter.GetIndex();
      typename ImageType::IndexType ind2=wimIter.GetIndex();
      for (int i=0; i<2; i++)
	  {
	    ind2[i]=ind[i]-1;
	    if (ind2[i] < 0) ind2[i]=0;
	  }

      for (int i=0; i<2; i++)
        {
          //this creates a 3-d effect
	  //if (ind[i] % gridsize == 0) img->SetPixel(ind2,2);
          //this gives double thickness
	  if (ind[i] % gridsize == 0) img->SetPixel(ind2,0);
        }

      }

    return img;
  }

template <class ImageType>
typename ImageType::Pointer ReadAnImage(char* fn)
{
  // Read the image files begin
  typedef itk::ImageFileReader< ImageType >      FileSourceType;
  typename FileSourceType::Pointer reffilter = FileSourceType::New();
  reffilter->SetFileName( fn );
  try
  {
    reffilter->Update();
  }
  catch( ... )
  {
    return NULL;
  }
 return reffilter->GetOutput();
}

template <class TImage, class TDisplacementField>
typename TDisplacementField::PixelType
TransformVector(TDisplacementField* field, typename TImage::IndexType index )
{
  enum { ImageDimension = TImage::ImageDimension };
  typename TDisplacementField::PixelType vec=field->GetPixel(index);
  typename TDisplacementField::PixelType newvec;
  newvec.Fill(0);

  for (unsigned int row=0; row<ImageDimension; row++)
    for (unsigned int col=0; col<ImageDimension; col++)
      newvec[row]+=vec[col]*field->GetDirection()[row][col];

  return newvec;
}

template <class TImage, class TDisplacementField>
void
ComputeJacobian(TDisplacementField* field,char* fnm, char* maskfn, bool uselog=false, bool norm=false, bool use2ndorder=false)
{
  typedef TImage ImageType;
  typedef TDisplacementField FieldType;
  enum { ImageDimension = TImage::ImageDimension };
  typedef itk::Image<float,ImageDimension> FloatImageType;
  typename FloatImageType::RegionType m_JacobianRegion;
  typename FloatImageType::Pointer mask=NULL;

  mask=ReadAnImage<FloatImageType>(maskfn);

  if (!field)
  {
    return;
  }
  typename TImage::SizeType s= field->GetLargestPossibleRegion().GetSize();
  typename TImage::SpacingType sp= field->GetSpacing();

  typename FloatImageType::Pointer m_FloatImage=NULL;
  m_FloatImage = FloatImageType::New();
  m_FloatImage->SetLargestPossibleRegion( field->GetLargestPossibleRegion() );
  m_FloatImage->SetBufferedRegion( field->GetLargestPossibleRegion().GetSize() );
  m_FloatImage->SetSpacing(field->GetSpacing());
  m_FloatImage->SetDirection( field->GetDirection() );
  m_FloatImage->SetOrigin(field->GetOrigin());
  m_FloatImage->Allocate();
  m_FloatImage->FillBuffer(0);

  typename ImageType::Pointer grid = GenerateGridImage<ImageType>(m_FloatImage,7);

  if (grid)
  {
    typedef itk::MatrixOffsetTransformBase< double, ImageDimension, ImageDimension > TransformType;
    typedef itk::WarpImageMultiTransformFilter<ImageType,ImageType, FieldType, TransformType> WarperType;
    typename WarperType::Pointer  warper = WarperType::New();
    warper->SetInput(grid);
    warper->SetEdgePaddingValue( 0);
    warper->SetSmoothScale(1);
    warper->PushBackDisplacementFieldTransform(field);
    warper->SetOutputOrigin(field->GetOrigin());
    warper->SetOutputSize(field->GetLargestPossibleRegion().GetSize());
    warper->SetOutputSpacing(field->GetSpacing());
    warper->SetOutputDirection(field->GetDirection());
    warper->Update();
    grid=warper->GetOutput();
    typedef  itk::ImageFileWriter<ImageType> writertype;
    typename writertype::Pointer writer = writertype::New();
    std::string fng=std::string(fnm)+"grid.nii.gz";
    writer->SetFileName(fng.c_str());
    writer->SetInput(grid);
    writer->Write();
    Rcpp::Rcout << " Grid done ";
  }
  typename FloatImageType::SizeType m_FieldSize = field->GetLargestPossibleRegion().GetSize();

  typedef itk::ImageRegionIteratorWithIndex<FloatImageType>        Iterator;
  Iterator wimIter( m_FloatImage, m_FloatImage->GetLargestPossibleRegion()  );
  wimIter.GoToBegin();
  for( ; !wimIter.IsAtEnd(); ++wimIter ) wimIter.Set(1.0);


  typedef  vnl_matrix<double> MatrixType;
  MatrixType jMatrix,idMatrix,avgMatrix;
  jMatrix.set_size(ImageDimension,ImageDimension);
  avgMatrix.set_size(ImageDimension,ImageDimension);
  avgMatrix.fill(0);
  itk::ImageRegionIteratorWithIndex<TDisplacementField>
    m_FieldIter( field, field->GetLargestPossibleRegion() );
  typename TImage::IndexType rindex;
  typename TImage::IndexType ddrindex;
  typename TImage::IndexType ddlindex;

  typename TImage::IndexType difIndex[ImageDimension][2];

  double det=0.0;
  unsigned int posoff=1;
  float difspace=1.0;
  float space=1.0;
  if (posoff == 0) difspace=1.0;

    typedef itk::Vector<float,ImageDimension>         VectorType;

  typename FieldType::PixelType dPix;
  typename FieldType::PixelType lpix;
  typename FieldType::PixelType llpix;
  typename FieldType::PixelType rpix;
  typename FieldType::PixelType rrpix;
  typename FieldType::PixelType cpix;

  float volumeelt=1.0;
  for (int j=0;j<ImageDimension;j++) volumeelt*=sp[j];
  //   double totaljac=0.0;

  ///the finite difference equations
  float wC, wLL, wL, wR, wRR;
  // 3rd deriv - 4th order
  wC = 0.0 ;
  wLL=1.; wL= -2.0; wR =  2.0; wRR = -1.0;
  // 4th deriv - 4th order
  wC = -6.0 ;
  wLL=1.; wL= -4.0; wR = -4.0; wRR = 1.0;
  // 2nd deriv - 4th order
  wC = 30.0 ;
  wLL= -1.0; wL= 16.0; wR = 16.0; wRR = -1.0;
  float total = wC;// wLL + wL + wR + wRR;
  if (total == 0.0) total = 1.0;

  unsigned long ct =0;


 for(  m_FieldIter.GoToBegin() ; !m_FieldIter.IsAtEnd(); ++m_FieldIter )
  {
    rindex=m_FieldIter.GetIndex();
    float mindist=1.0;
    bool oktosample=true;
    float dist=100.0;
    for (unsigned int row=0; row<ImageDimension; row++)
    {
      dist=fabs((float)rindex[row]);
      if (dist < mindist) oktosample=false;
      dist = fabs((float)s[row]-(float)rindex[row]);
      if (dist < mindist) oktosample=false;
    }
    if (oktosample)
      {
	ct++;
	typename TImage::IndexType temp=rindex;
	cpix=TransformVector<ImageType,FieldType>(field,rindex);
	for(unsigned int row=0; row< ImageDimension;row++)
	  {
	    difIndex[row][0]=rindex;
	    difIndex[row][1]=rindex;
	    ddrindex=rindex;
	    ddlindex=rindex;
	    if ((unsigned int) rindex[row] < (unsigned int) m_FieldSize[row]-2)
	      {
		difIndex[row][0][row]=rindex[row]+posoff;
		ddrindex[row]=rindex[row]+posoff*2;
	      }
	    if (rindex[row] > 1 )
	      {
		difIndex[row][1][row]=rindex[row]-1;
		ddlindex[row]=rindex[row]-2;
	      }

	    float h=1;
	    space=1.0; // should use image spacing here?

	    rpix = TransformVector<ImageType,FieldType>(field,difIndex[row][1]);
	    rpix = rpix*h+cpix*(1.-h);
	    lpix = TransformVector<ImageType,FieldType>(field,difIndex[row][0]);
	    lpix = lpix*h+cpix*(1.-h);
	    //    dPix = ( rpix - lpix)*(1.0)/(2.0);

	    rrpix = TransformVector<ImageType,FieldType>(field,ddrindex);
      rrpix = rrpix*h+rpix*(1.-h);
      llpix = TransformVector<ImageType,FieldType>(field,ddlindex);
      llpix = llpix*h+lpix*(1.-h);
      dPix=( rrpix*(-1.0) + rpix*8.0 - lpix*8.0 + lpix )*(-1.0)*space/(12.0*h); //4th order centered difference
      if ( use2ndorder) dPix=( lpix - rpix )*(1.0)*space/(2.0*h); //4th order centered difference

	    for(unsigned int col=0; col< ImageDimension;col++){
	      float val;
	      if (row == col) val=dPix[col]/sp[col]+1.0;
	      else val = dPix[col]/sp[col];
	      //        Rcpp::Rcout << " row " << row << " col " << col << " val " << val << std::endl;
	      jMatrix.put(col,row,val);
	      avgMatrix.put(col,row,avgMatrix.get(col,row)+val);
	    }
	  }

	//the determinant of the jacobian matrix
	// Rcpp::Rcout << " get det " << std::endl;
	det = vnl_determinant(jMatrix);
	//    float prodval = m_FloatImage->GetPixel(rindex);
      if (det < 0.0) det = 0;


	m_FloatImage->SetPixel(rindex,  det );

      //totaljac+=det;
      }//oktosample if
  }
 Rcpp::Rcout <<" avg Mat " << avgMatrix / (float)ct << std::endl;

  if (norm && mask)
    {
      Rcpp::Rcout <<" using mask && normalizing " << std::endl;
      /*
      typedef itk::DiscreteGaussianImageFilter<TImage, TImage> dgf;
      float sig=2.0;
      typename FloatImageType::Pointer temp;
      {
      typename dgf::Pointer filter = dgf::New();
      filter->SetVariance(sig);
      filter->SetUseImageSpacingOff();
      filter->SetMaximumError(.01f);
      filter->SetInput(m_FloatImage);
      filter->Update();
      //  m_FloatImage=filter->GetOutput();
      temp=filter->GetOutput();
      }
      {
      typename dgf::Pointer filter = dgf::New();
      filter->SetVariance(sig);
      filter->SetUseImageSpacingOff();
      filter->SetMaximumError(.01f);
      filter->SetInput(temp);
      filter->Update();
      //  m_FloatImage=filter->GetOutput();
      temp=filter->GetOutput();
      } */

      double total = 0.0;
      unsigned long ct = 0;
      for(  m_FieldIter.GoToBegin() ; !m_FieldIter.IsAtEnd(); ++m_FieldIter )
	{
	  rindex=m_FieldIter.GetIndex();
	  if (mask->GetPixel(rindex) > 0 )
	    {
	      total+=m_FloatImage->GetPixel(rindex);
	      ct++;
	    }
	  else m_FloatImage->SetPixel(rindex,0);
	}
      total /= (double) ct;
      for(  m_FieldIter.GoToBegin() ; !m_FieldIter.IsAtEnd(); ++m_FieldIter )
	{
	  rindex=m_FieldIter.GetIndex();
	  double val = m_FloatImage->GetPixel(rindex)/total;
          if (mask->GetPixel(rindex) > 0 ) m_FloatImage->SetPixel(rindex,val);
	  else m_FloatImage->SetPixel(rindex,0);
	}

    }

    for(  m_FieldIter.GoToBegin() ; !m_FieldIter.IsAtEnd(); ++m_FieldIter )
        {
          rindex=m_FieldIter.GetIndex();
          double val = m_FloatImage->GetPixel(rindex);
          if (uselog && val > 0) val=log(val);
          else if (uselog && val < 0) val = log(0.01);
          if (uselog  ) m_FloatImage->SetPixel(rindex,val);
        }



  typedef  itk::ImageFileWriter<TImage> writertype;
  typename writertype::Pointer writer = writertype::New();
  std::string fn=std::string(fnm)+"jacobian.nii.gz";
  if (uselog) fn=std::string(fnm)+"logjacobian.nii.gz";
  writer->SetFileName(fn.c_str());
  writer->SetInput(m_FloatImage);
  writer->Write();

  return;

}


template <unsigned int ImageDimension>
int Jacobian(int argc, char *argv[])
{

  //  Rcpp::Rcout << " enter " << ImageDimension << std::endl;
  if ( argc < 3 )
  {
    Rcpp::Rcout << "Usage:   Jacobian gWarp outfile uselog maskfn normbytotalbool  " << std::endl;
    return 1;
  }
  typedef float  PixelType;
  typedef itk::Vector<float,ImageDimension>         VectorType;
  typedef itk::Image<VectorType,ImageDimension>     FieldType;
  typedef itk::Image<PixelType,ImageDimension> ImageType;
  typedef itk::ImageFileReader<ImageType> readertype;
  typedef itk::ImageFileWriter<ImageType> writertype;
  typedef typename  ImageType::IndexType IndexType;
  typedef typename  ImageType::SizeType SizeType;
  typedef typename  ImageType::SpacingType SpacingType;
  typedef itk::LinearInterpolateImageFunction<ImageType,double>  InterpolatorType;

  typedef itk::ImageFileReader<FieldType> ReaderType;
  //Rcpp::Rcout << "read warp " << std::string(argv[1]) << std::endl;
  typename ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );
  //  reader->SetUseAvantsNamingConvention( true );
  reader->Update();
  typename FieldType::Pointer gWarp=reader->GetOutput();
  //
  //Rcpp::Rcout << "read warp 2 " << std::endl;
  // typename FieldType::Pointer gWarp = ReadWarpFromFile<ImageType,FieldType>(argv[1],"vec.nii");

  // here hWarp is changed in place to be fWarp
//  Jacobian<ImageType,FieldType>(gWarp,argv[2]);
//  Rcpp::Rcout << " vecanidiff " << std::endl;
//  gWarp = VectorAniDiff<FieldType>(gWarp , atoi(argv[3]) );
//  Rcpp::Rcout << " vecanidiffdone " << std::endl;
  bool uselog=false;
  if (argc > 3) uselog=(bool)atoi(argv[3]);
  bool norm=false;
  if (argc > 5) norm=(bool)atoi(argv[5]);
  bool use2ndorder=true;
  if (argc > 6) use2ndorder=(bool)atoi(argv[6]);
  //  Rcpp::Rcout << " name "<< argv[2] <<  " mask " << argv[4] << " norm " << norm << " Log " << uselog << std::endl;
  ComputeJacobian<ImageType,FieldType>(gWarp,argv[2],argv[4],uselog,norm,use2ndorder);
//  DiffeomorphicJacobian<ImageType,ImageType,FieldType>(gWarp,1,argv[2]);
  //  if (argc > 3) DiffeomorphicMetric<ImageType,ImageType,FieldType>(gWarp,argv[2]);


  return 0;

}



RcppExport SEXP ANTSJacobian( SEXP r_args )
try
{
  // put the arguments coming from R into standard (argc,argv) format;
  // arguments coming from R don't have the command name as first, argument, so add it manually;
  // arguments coming from R may have adjacent arguments concatenated into one argument, 
  // which the parser should handle
  std::deque<std::string> args = Rcpp::as< std::deque<std::string> >( r_args ) ;
  args.push_front( "antsRegistration" ) ;
  
  int argc = args.size() ;
  char** argv = new char*[args.size()+1] ;
  for( int i = 0 ; i < args.size() ; ++i )
    {
      // allocate space for the string plus a null character
      argv[i] = new char[args[i].length()+1] ;
      std::strncpy( argv[i] , args[i].c_str() , args[i].length() ) ;
      // place the null character in the end
      argv[i][args[i].length()] = '\0' ;
    }
  argv[argc] = 0 ;


  if ( argc < 3 )
  {
    Rcpp::Rcout << "Usage: " << argv[0] << " ImageDim gWarp outfile uselog maskfn normbytotalbool use-2nd-order-central-difference-boolean " << std::endl;
    Rcpp::Rcout <<" for example " << std::endl << " ANTSJacobian 3  myWarp.nii   Output  1   templatebrainmask.nii   1  " << std::endl;
    Rcpp::Rcout << " the last 1 normalizes the jacobian by the total in the mask.  use this to adjust for head size.  " << std::endl;
    return Rcpp::wrap( EXIT_FAILURE ) ;
  }

  switch( atoi( argv[1] ) )
   {
   case 2:
     Jacobian<2>(argc-1,argv+1);
      break;
   case 3:
     Jacobian<3>(argc-1,argv+1);
      break;
   default:
      Rcpp::Rcout << "Unsupported dimension" << std::endl;
      return Rcpp::wrap( EXIT_FAILURE ) ;
   }

  // cleanup of argv
  for( int i = 0 ; i < args.size() ; ++i )
    {
      delete[] argv[i] ;
    }
  delete[] argv ;

  return Rcpp::wrap( EXIT_SUCCESS ) ;
}
 catch( const std::exception& exc )
   {
     Rcpp::Rcout<< exc.what() << std::endl ;
     return Rcpp::wrap( EXIT_FAILURE ) ;
   }
