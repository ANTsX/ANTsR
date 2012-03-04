#include <Rcpp.h>

#include "antsCommandLineOption.h"
#include "antsCommandLineParser.h"
#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkRecursiveGaussianImageFilter.h"
#include "itkDiscreteGaussianImageFilter.h"
#include "itkResampleImageFilter.h"
#include "itkBSplineInterpolateImageFunction.h"
#include <sstream>
#include <iostream>
#include <string>
#include <algorithm>
#include <vector>
#include <vnl/vnl_random.h>
#include <vnl/algo/vnl_qr.h>
#include <vnl/algo/vnl_svd.h>
#include <vnl/algo/vnl_svd_economy.h>
#include <vnl/algo/vnl_symmetric_eigensystem.h>
#include <vnl/algo/vnl_real_eigensystem.h>
#include <vnl/algo/vnl_generalized_eigensystem.h>
#include "antsSCCANObject.h"
#include "itkCSVNumericObjectFileWriter.h"
#include "itkCSVArray2DDataObject.h"
#include "itkCSVArray2DFileReader.h"
#include "itkExtractImageFilter.h"



template <class TImageType> 
bool SCCANReadImage(itk::SmartPointer<TImageType> &target, const char *file)
{
  if ( std::string(file).length() < 3 ) { Rcpp::Rcout << " bad file name " << std::string(file) << std::endl ;    target=NULL;  return false;  }
 // Read the image files begin
    typedef TImageType ImageType;
    typedef itk::ImageFileReader< ImageType >      FileSourceType;
    typedef typename ImageType::PixelType PixType;
    typename FileSourceType::Pointer reffilter = FileSourceType::New();
    reffilter->SetFileName( file );
    try
    {
      reffilter->Update();
    }
    catch( itk::ExceptionObject & e )
    {
      Rcpp::Rcout << "Exception caught during reference file reading " << std::endl;
      Rcpp::Rcout << e << " file " << file << std::endl;
      target=NULL;
      return false;
    }
   target=reffilter->GetOutput();
   return true;
}


template <class TComp>
double vnl_pearson_corr( vnl_vector<TComp> v1, vnl_vector<TComp> v2 )
{
  double xysum=0;
  for ( unsigned int i=0; i<v1.size(); i++) xysum+=v1(i)*v2(i);
  double frac=1.0/(double)v1.size();
  double xsum=v1.sum(),ysum=v2.sum();
  double xsqr=v1.squared_magnitude();
  double ysqr=v2.squared_magnitude();
  double numer=xysum - frac*xsum*ysum;
  double denom=sqrt( ( xsqr - frac*xsum*xsum)*( ysqr - frac*ysum*ysum) );
  if ( denom <= 0 ) return 0;
  return numer/denom;
}

template <class TImage,class TComp>
void WriteVectorToSpatialImage( std::string filename , std::string post , vnl_vector<TComp> w_p , typename TImage::Pointer  mask )
{
      typedef itk::Image<TComp,2> MatrixImageType;
      typedef typename TImage::PixelType PixelType;
      std::string::size_type pos = filename.rfind( "." );
      std::string filepre = std::string( filename, 0, pos );
      std::string extension;
      if ( pos != std::string::npos ){
        extension = std::string( filename, pos, filename.length()-1);
        if (extension==std::string(".gz")){
	  pos = filepre.rfind( "." );
	  extension = std::string( filepre, pos, filepre.length()-1 )+extension;
          filepre = std::string( filepre, 0, pos );
        }
      }

      typename TImage::Pointer weights = TImage::New();
      weights->SetOrigin( mask->GetOrigin() );
      weights->SetSpacing( mask->GetSpacing() );
      weights->SetRegions( mask->GetLargestPossibleRegion() );
      weights->SetDirection( mask->GetDirection() );
      weights->Allocate();
      weights->FillBuffer( itk::NumericTraits<PixelType>::Zero );

      // overwrite weights with vector values;
      unsigned long vecind=0;
      typedef itk::ImageRegionIteratorWithIndex<TImage> Iterator;
      Iterator mIter(mask,mask->GetLargestPossibleRegion() );
      for(  mIter.GoToBegin(); !mIter.IsAtEnd(); ++mIter )
	if (mIter.Get() >= 0.5)
	  {
	    TComp val=0;
	    if ( vecind < w_p.size() ) val=w_p(vecind);
	    else {
               Rcpp::Rcout << "vecind too large " << vecind << " vs " << w_p.size() << std::endl;
	       Rcpp::Rcout <<" this is likely a mask problem --- exiting! " << std::endl;
	       // exception instead of exit
	       throw std::exception() ;
	    }
	    //	    Rcpp::Rcout << " val " << val << std::endl;
	    weights->SetPixel(mIter.GetIndex(),val);
	    vecind++;
	  }
	else mIter.Set(0);

      typedef itk::ImageFileWriter<TImage> WriterType;
      std::string fn1=filepre+post+extension;
      Rcpp::Rcout << fn1 << std::endl;
      typename WriterType::Pointer writer = WriterType::New();
      writer->SetFileName( fn1 );
      writer->SetInput( weights );
      writer->Update();

}

template <class T>
inline std::string sccan_to_string (const T& t)
{
  std::stringstream ss;
  ss << t;
  std::string stringout=ss.str();
  if ( t < 100 )
    {
      std::stringstream ss0;
      ss0 << 0;
      std::string extend=ss0.str();
      stringout=std::string(extend+stringout);
    }
  if ( t < 10 )
    {
      std::stringstream ss0;
      ss0 << 0;
      std::string extend=ss0.str();
      stringout=std::string(extend+stringout);
    }
  return stringout;
}

template <class TImage,class TComp>
void WriteVariatesToSpatialImage( std::string filename , std::string post , vnl_matrix<TComp> varmat, typename TImage::Pointer  mask,  vnl_matrix<TComp> data_mat , bool have_mask )
{
  vnl_matrix<TComp> projections=data_mat*varmat;
  std::string::size_type pos = filename.rfind( "." );
  std::string filepre = std::string( filename, 0, pos );
  std::string extension;
  if ( pos != std::string::npos ){
    extension = std::string( filename, pos, filename.length()-1);
    if (extension==std::string(".gz")){
      pos = filepre.rfind( "." );
      extension = std::string( filepre, pos, filepre.length()-1 )+extension;
      filepre = std::string( filepre, 0, pos );
    }
  }
  std::string post2;
  std::ofstream myfile;
  std::string fnmp=filepre+std::string("projections")+post+std::string(".csv");
  std::vector<std::string> ColumnHeaders;
  for (unsigned int nv=0; nv<projections.cols(); nv++)
    {
      std::string colname=std::string("Variate")+sccan_to_string<unsigned int>(nv);
      ColumnHeaders.push_back( colname );
    }
  typedef itk::CSVNumericObjectFileWriter<double> WriterType;
  WriterType::Pointer writer = WriterType::New();
  writer->SetFileName( fnmp.c_str() );
  writer->SetColumnHeaders(ColumnHeaders);
  writer->SetInput( &projections );
  try
    {
      writer->Write();
    }
  catch (itk::ExceptionObject& exp)
    {
      Rcpp::Rcout << "Exception caught!" << std::endl;
      Rcpp::Rcout << exp << std::endl;
      return ;
    }
  if ( have_mask ) {
    Rcpp::Rcout << " have_mask " << have_mask << std::endl;
  for (unsigned int vars=0; vars < varmat.columns(); vars++  ){
    post2=post+sccan_to_string<unsigned int>(vars);
    vnl_vector<TComp> temp=varmat.get_column(vars);
    WriteVectorToSpatialImage<TImage,TComp>( filename, post2, temp , mask);
  }
  }
  else {
    std::vector<std::string> ColumnHeaders;
    // write out the array2D object
    std::string fnmp=filepre+std::string("ViewVecs")+std::string(".csv");
    for (unsigned int nv=0; nv<varmat.cols(); nv++)
      {
      std::string colname=std::string("Variate")+sccan_to_string<unsigned int>(nv);
      ColumnHeaders.push_back( colname );
      }
    typedef itk::CSVNumericObjectFileWriter<double> WriterType;
    WriterType::Pointer writer = WriterType::New();
    writer->SetFileName( fnmp.c_str() );
    writer->SetColumnHeaders(ColumnHeaders);
    writer->SetInput( &varmat );
    try
    {
      writer->Write();
    }
    catch (itk::ExceptionObject& exp)
    {
      Rcpp::Rcout << "Exception caught!" << std::endl;
      Rcpp::Rcout << exp << std::endl;
      return ;
    }
  }
}

template <class TImage,class TComp>
vnl_matrix<TComp>
CopyImageToVnlMatrix( typename TImage::Pointer   p_img )
{
  typedef vnl_matrix<TComp> vMatrix;

  typename TImage::SizeType  pMatSize=p_img->GetLargestPossibleRegion().GetSize();
  vMatrix p(pMatSize[0],pMatSize[1]);         // a (size)x(size+1)-matrix of int's
  for ( long j=0; j<p.columns(); ++j) {  // loop over columns
    for ( long i=0; i<p.rows(); ++i) { // loop over rows
	typename TImage::IndexType ind;
	ind[0]=i;
	ind[1]=j;
	TComp val=p_img->GetPixel(ind);
	p(i,j) = val;  // to access matrix coefficients,
      }
  }
  return p;
}

template <class TImage,class TComp>
vnl_matrix<TComp>
DeleteRow(vnl_matrix<TComp> p_in , unsigned int row)
{
  typedef vnl_matrix<TComp> vMatrix;
  unsigned int nrows=p_in.rows()-1;
  if ( row >= nrows ) nrows=p_in.rows();
  vMatrix p(nrows,p_in.columns());
  unsigned int rowct=0;
  for ( long i=0; i<p.rows(); ++i) { // loop over rows
    if ( i != row ) {
      p.set_row(rowct,p_in.get_row(i));
      rowct++;
    }
  }
  return p;
}

int sccanRandom(int n)
{
    return rand() % n ;
}


template <class TComp>
vnl_matrix<TComp>
PermuteMatrix( vnl_matrix<TComp> q , bool doperm=true)
{
  typedef vnl_matrix<TComp> vMatrix;
  typedef vnl_vector<TComp> vVector;

  std::vector<unsigned long> permvec;
  for (unsigned long i=0; i < q.rows(); i++)
    permvec.push_back(i);
  std::random_shuffle(permvec.begin(), permvec.end(),sccanRandom);
  //    for (unsigned long i=0; i < q.rows(); i++)
  //  Rcpp::Rcout << " permv " << i << " is " << permvec[i] << std::endl;
  // for (unsigned long i=0; i < q.rows(); i++)
  //  Rcpp::Rcout << " permv " << i << " is " << permvec[i] << std::endl;
  // 1. permute q
  vMatrix q_perm(q.rows(),q.columns());
  for (unsigned long i=0; i < q.rows(); i++)
    {
      unsigned long perm=permvec[i];
      if ( doperm )
	q_perm.set_row(i,q.get_row(perm));
      else q_perm.set_row(i,q.get_row(i));
    }
  return q_perm;
}


template <unsigned int ImageDimension, class PixelType>
int matrixOperation( itk::ants::CommandLineParser::OptionType *option,
  itk::ants::CommandLineParser::OptionType *outputOption = NULL )
{
  std::string funcName=std::string("matrixOperation");
  typedef itk::Image<PixelType, ImageDimension> ImageType;
  typedef double  matPixelType;
  typedef itk::Image<matPixelType,2> MatrixImageType;
  typename ImageType::Pointer outputImage = NULL;

  //   option->SetUsageOption( 2, "multires_matrix_invert[list.txt,maskhighres.nii.gz,masklowres.nii.gz,matrix.mhd]" );

  std::string value = option->GetValue( 0 );
  if( strcmp( value.c_str(), "multires_matrix_invert" ) == 0 )
    {
      std::string listfn=option->GetParameter( 0 );
      std::string maskhfn=option->GetParameter( 1 );
      std::string masklfn=option->GetParameter( 2 );
      //      vnl_matrix<matPixelType> matrixinv=MultiResMatrixInvert<ImageDimension,matPixelType>( listfn, maskhfn, masklfn );
    }

  return EXIT_SUCCESS;

}


template <class RealType>
int
CompareMatrixSizes(  vnl_matrix<RealType> & p ,  vnl_matrix<RealType> & q )
{
  if ( p.rows() != q.rows() ) {
    Rcpp::Rcout << " The number of rows must match !!" << std::endl;
    Rcpp::Rcout << " matrix-1 has " << p.rows() << " rows " << std::endl;
    Rcpp::Rcout << " matrix-2 has " << q.rows() << " rows " << std::endl;
    // exception instead of exit
    throw std::exception() ;
  }
  return 0;
}

template <class PixelType>
void 
ReadMatrixFromCSVorImageSet( std::string matname , vnl_matrix<PixelType> & p )
{
  typedef PixelType Scalar;
  typedef itk::Image<PixelType,2> MatrixImageType;
  typedef itk::ImageFileReader<MatrixImageType> matReaderType;
  std::string ext = itksys::SystemTools::GetFilenameExtension( matname );
  if  (strcmp(ext.c_str(),".csv") == 0 ) {
    typedef itk::CSVArray2DFileReader<double> ReaderType;
    typename ReaderType::Pointer reader = ReaderType::New();
    reader->SetFileName ( matname.c_str() );
    reader->SetFieldDelimiterCharacter( ',' );
    reader->SetStringDelimiterCharacter( '"' );
    reader->HasColumnHeadersOn();
    reader->HasRowHeadersOff();
    reader->UseStringDelimiterCharacterOff();
    try
    {
      reader->Update();
    }
    catch(itk::ExceptionObject& exp)
    {
    Rcpp::Rcout << "Exception caught!" << std::endl;
    Rcpp::Rcout << exp << std::endl;
    }
    typedef itk::CSVArray2DDataObject<double> DataFrameObjectType;
    DataFrameObjectType::Pointer dfo = reader->GetOutput();
    p = dfo->GetMatrix();
    return ;
  }
  else {
  typename matReaderType::Pointer matreader1 = matReaderType::New();
  matreader1->SetFileName( matname.c_str() );
  matreader1->Update();
  p=CopyImageToVnlMatrix<MatrixImageType,Scalar>( matreader1->GetOutput() );
  }
  return ;
}

template <unsigned int ImageDimension, class PixelType>
void
ConvertImageListToMatrix( std::string imagelist, std::string maskfn , std::string outname  )
{
  std::string ext = itksys::SystemTools::GetFilenameExtension( outname );
  typedef itk::Image<PixelType,ImageDimension> ImageType;
  typedef itk::Image<PixelType,2> MatrixImageType;
  typedef itk::ImageFileReader<ImageType> ReaderType;
  typename ReaderType::Pointer reader1 = ReaderType::New();
  reader1->SetFileName( maskfn );
  reader1->Update();
  unsigned long voxct=0;
  typedef itk::ImageRegionIteratorWithIndex<ImageType> Iterator;
  Iterator mIter( reader1->GetOutput(),reader1->GetOutput()->GetLargestPossibleRegion() );
  for(  mIter.GoToBegin(); !mIter.IsAtEnd(); ++mIter )
    {
      if (mIter.Get() >= 0.5) voxct++;
    }
  std::vector<std::string> image_fn_list;
  // first, count the number of files
  const unsigned int maxChar = 512;
  char lineBuffer[maxChar];
  char filenm[maxChar];
  unsigned int filecount=0;
  {
    std::ifstream inputStreamA( imagelist.c_str(), std::ios::in );
    if ( !inputStreamA.is_open() )
      {
	Rcpp::Rcout << "Can't open image list file: " << imagelist << std::endl;
	return ;
      }
	while ( !inputStreamA.eof() )
	  {
	    inputStreamA.getline( lineBuffer, maxChar, '\n' );
      	    if ( sscanf( lineBuffer, "%s ",filenm) != 1 ){
	      continue;
	    }
	    else {
	      image_fn_list.push_back(std::string(filenm));
	      filecount++;
	    }
	  }
	inputStreamA.close();
  }

      /** declare the output matrix image */
      unsigned long xsize=image_fn_list.size();
      unsigned long ysize=voxct;
      typename MatrixImageType::SizeType tilesize;
      tilesize[0]=xsize;
      tilesize[1]=ysize;

      //      Rcpp::Rcout <<" have voxct " << voxct << " and nsub " << filecount << " or " << image_fn_list.size()<< std::endl;

  if  (strcmp(ext.c_str(),".csv") == 0 ) {
    typedef itk::Array2D<double> MatrixType;
    std::vector<std::string> ColumnHeaders;

    MatrixType matrix(xsize,ysize);
    matrix.Fill(0);
    for (unsigned int j=0; j< image_fn_list.size(); j++)
    {
      typename ReaderType::Pointer reader2 = ReaderType::New();
      reader2->SetFileName( image_fn_list[j] );
      reader2->Update();
      unsigned long xx=0,yy=0,tvoxct=0;
      xx=j;
      for(  mIter.GoToBegin(); !mIter.IsAtEnd(); ++mIter )
      {
        if (mIter.Get() >= 0.5)
        {
          yy=tvoxct;
          matrix[xx][yy]=reader2->GetOutput()->GetPixel(mIter.GetIndex());
	  if ( j == 0 )
	    {
 	    std::string colname=std::string("V")+sccan_to_string<unsigned long>(tvoxct);
            ColumnHeaders.push_back( colname );
	    }
          tvoxct++;
        }
      }
    }
    // write out the array2D object
    typedef itk::CSVNumericObjectFileWriter<double> WriterType;
    WriterType::Pointer writer = WriterType::New();
    writer->SetFileName( outname );
    writer->SetInput( &matrix );
    writer->SetColumnHeaders( ColumnHeaders );
    try
    {
      writer->Write();
    }
    catch (itk::ExceptionObject& exp)
    {
      Rcpp::Rcout << "Exception caught!" << std::endl;
      Rcpp::Rcout << exp << std::endl;
      return ;
    }
    return;
  }
  else {
      typename MatrixImageType::RegionType region;
      region.SetSize( tilesize );
      typename MatrixImageType::Pointer matimage=MatrixImageType::New();
      matimage->SetLargestPossibleRegion( region );
      matimage->SetBufferedRegion( region );
      typename MatrixImageType::DirectionType mdir;  mdir.Fill(0); mdir[0][0]=1; mdir[1][1]=1;
      typename MatrixImageType::SpacingType mspc;  mspc.Fill(1);
      typename MatrixImageType::PointType morg;  morg.Fill(0);
      matimage->SetSpacing( mspc );
      matimage->SetDirection(mdir);
      matimage->SetOrigin( morg );
      matimage->Allocate();
      for (unsigned int j=0; j< image_fn_list.size(); j++)
	{
	  typename ReaderType::Pointer reader2 = ReaderType::New();
	  reader2->SetFileName( image_fn_list[j] );
	  reader2->Update();
	  unsigned long xx=0,yy=0,tvoxct=0;
	  xx=j;
	  typename MatrixImageType::IndexType mind;
	  for(  mIter.GoToBegin(); !mIter.IsAtEnd(); ++mIter )
	    {
	      if (mIter.Get() >= 0.5)
		{
		  yy=tvoxct;
		  mind[0]=xx;
		  mind[1]=yy;
		  matimage->SetPixel(mind,reader2->GetOutput()->GetPixel(mIter.GetIndex()));
		  tvoxct++;
		}
	    }
	}

      typedef itk::ImageFileWriter<MatrixImageType> WriterType;
      typename WriterType::Pointer writer = WriterType::New();
      writer->SetFileName( outname );
      writer->SetInput( matimage );
      writer->Update();

  }
  return ;
}


template < class PixelType>
int
ConvertTimeSeriesImageToMatrix( std::string imagefn, std::string maskfn , std::string outname  , double space_smoother , double time_smoother )
{
  const unsigned int ImageDimension=4;
  typedef itk::Vector<PixelType,ImageDimension>         VectorType;
  typedef itk::Image<VectorType,ImageDimension>     FieldType;
  typedef itk::Image<PixelType,ImageDimension> ImageType;
  typedef itk::Image<PixelType,ImageDimension-1> OutImageType;
  typedef typename OutImageType::IndexType OutIndexType;
  typedef itk::ImageFileReader<ImageType> readertype;
  typedef itk::ImageFileWriter<ImageType> writertype;
  typedef typename ImageType::IndexType IndexType;
  typedef typename ImageType::SizeType SizeType;
  typedef typename ImageType::SpacingType SpacingType;
  typedef itk::ImageRegionIteratorWithIndex<ImageType> Iterator;

  typedef double Scalar;
  std::string ext = itksys::SystemTools::GetFilenameExtension( outname );
  if  (strcmp(ext.c_str(),".csv") != 0 ) {
    Rcpp::Rcout << " must use .csv as output file extension "<<std::endl;
    return EXIT_FAILURE;
  }
  typename ImageType::Pointer image1=NULL;
  typename OutImageType::Pointer mask=NULL;
  Rcpp::Rcout << " imagefn " << imagefn << std::endl;
  if (imagefn.length() > 3)   SCCANReadImage<ImageType>(image1, imagefn.c_str());
  else {Rcpp::Rcout<< " cannot read image " << imagefn << std::endl; return 1; }

  if ( space_smoother > 0 ) {
    typename ImageType::SpacingType spacing=image1->GetSpacing();
    typename ImageType::SpacingType spacing2=image1->GetSpacing();
    // basically, don't do any dim-4 smoothing 
    spacing2[3]=sqrt(spacing[0]*spacing[0]+spacing[1]*spacing[1]+spacing[2]*spacing[2])*1.e6;
    image1->SetSpacing(spacing2);
    typedef itk::DiscreteGaussianImageFilter<ImageType, ImageType> dgf;
    typename dgf::Pointer filter = dgf::New();
    filter->SetVariance(space_smoother);
    filter->SetUseImageSpacingOn();
    filter->SetMaximumError(.01f);
    filter->SetInput(image1);
    filter->Update();
    image1=filter->GetOutput();
    image1->SetSpacing(spacing);
  }

  if ( time_smoother > 0 ) {
    typename ImageType::SpacingType spacing=image1->GetSpacing();
    typename ImageType::SpacingType spacing2=image1->GetSpacing();
    // basically, don't do any dim-4 smoothing 
    double bigspace=sqrt(spacing[0]*spacing[0]+spacing[1]*spacing[1]+spacing[2]*spacing[2])*1.e6;
    // basically no spatial smoothing 
    spacing2.Fill(bigspace);
    spacing2[3]=1;
    image1->SetSpacing(spacing2);
    typedef itk::DiscreteGaussianImageFilter<ImageType, ImageType> dgf;
    typename dgf::Pointer filter = dgf::New();
    filter->SetVariance(time_smoother);
    filter->SetUseImageSpacingOn();
    filter->SetMaximumError(.01f);
    filter->SetInput(image1);
    filter->Update();
    image1=filter->GetOutput();
    image1->SetSpacing(spacing);
  }

  if (maskfn.length() > 3)   SCCANReadImage<OutImageType>(mask, maskfn.c_str());
  else {Rcpp::Rcout<< " cannot read mask " << maskfn << std::endl; return 1; }
  unsigned int timedims=image1->GetLargestPossibleRegion().GetSize()[ImageDimension-1];
  unsigned long voxct=0;
  typedef itk::ExtractImageFilter<ImageType,OutImageType> ExtractFilterType;
  typedef itk::ImageRegionIteratorWithIndex<OutImageType> SliceIt;
  SliceIt mIter( mask,mask->GetLargestPossibleRegion() );
  for(  mIter.GoToBegin(); !mIter.IsAtEnd(); ++mIter )
    if (mIter.Get() >= 0.5) voxct++;
  Rcpp::Rcout << " timedims " << timedims << std::endl;

  typename ImageType::RegionType extractRegion = image1->GetLargestPossibleRegion();
  extractRegion.SetSize(ImageDimension-1, 0);
  unsigned int sub_vol=0;
  extractRegion.SetIndex(ImageDimension-1, sub_vol );
  typename ExtractFilterType::Pointer extractFilter = ExtractFilterType::New();
  extractFilter->SetInput( image1 );
  //    extractFilter->SetDirectionCollapseToIdentity();
  extractFilter->SetDirectionCollapseToSubmatrix();
  extractFilter->SetExtractionRegion( extractRegion );
  extractFilter->Update();
  typename OutImageType::Pointer outimage=extractFilter->GetOutput();
  outimage->FillBuffer(0);


  typedef itk::ImageRegionIteratorWithIndex<ImageType> ImageIt;
  typedef itk::ImageRegionIteratorWithIndex<OutImageType> SliceIt;

  typedef vnl_vector<Scalar>      timeVectorType;
  timeVectorType mSample(timedims,0);
  typedef itk::Array2D<double> MatrixType;
  std::vector<std::string> ColumnHeaders;
  MatrixType matrix(timedims,voxct);
  matrix.Fill(0);
  SliceIt vfIter2( outimage, outimage->GetLargestPossibleRegion() );
  voxct=0;
  for(  vfIter2.GoToBegin(); !vfIter2.IsAtEnd(); ++vfIter2 )
    {
      OutIndexType ind=vfIter2.GetIndex();
      if ( mask->GetPixel(ind) >= 0.5 ) {
      IndexType tind;
      // first collect all samples for that location
      for (unsigned int i=0; i<ImageDimension-1; i++) tind[i]=ind[i];
      for (unsigned int t=0; t<timedims; t++){
        tind[ImageDimension-1]=t;
	Scalar pix=image1->GetPixel(tind);
        mSample(t)=pix;
        matrix[t][voxct]=pix;
      }
      std::string colname=std::string("V")+sccan_to_string<unsigned int>(voxct);
      ColumnHeaders.push_back( colname );
      voxct++;
      }// check mask
    }

    typedef itk::CSVNumericObjectFileWriter<double> WriterType;
    WriterType::Pointer writer = WriterType::New();
    writer->SetFileName( outname );
    writer->SetInput( &matrix );
    writer->SetColumnHeaders( ColumnHeaders );
    try
    {
      writer->Write();
    }
    catch (itk::ExceptionObject& exp)
    {
      Rcpp::Rcout << "Exception caught!" << std::endl;
      Rcpp::Rcout << exp << std::endl;
      return EXIT_FAILURE;
    }
    Rcpp::Rcout <<" done writing " << std::endl;

}


template < class PixelType>
int
ConvertCSVVectorToImage( std::string csvfn, std::string maskfn , std::string outname , unsigned long rowOrCol )
{
  typedef PixelType Scalar;
  typedef vnl_matrix<PixelType> vMatrix;
  const unsigned int ImageDimension=3;
  typedef itk::Image<PixelType,ImageDimension> ImageType;
  /** read the  images */
  typename ImageType::Pointer mask = NULL;
  SCCANReadImage<ImageType>(mask,maskfn.c_str());
  typename ImageType::Pointer outimage = NULL;
  SCCANReadImage<ImageType>(outimage,maskfn.c_str());
  outimage->FillBuffer(0);
  typedef itk::ImageRegionIteratorWithIndex<ImageType> Iterator;
  unsigned long mct=0;
  Iterator mIter( mask,mask->GetLargestPossibleRegion() );
  for(  mIter.GoToBegin(); !mIter.IsAtEnd(); ++mIter )
    {
    if (mIter.Get() >= 0.5) mct++;
    }
  /** we refer to the two view matrices as P and Q */
  vMatrix p;
  p.fill(0);
  ReadMatrixFromCSVorImageSet<Scalar>(csvfn,p);
  if ( mct != p.rows() && mct != p.cols() ) 
  {
    Rcpp::Rcout << " csv-vec rows " << p.rows() << " cols " << p.cols() << " mask non zero elements " << mct <<  std::endl;
    // exception instead of exit
    throw std::exception() ;
  }  

  if ( mct == p.rows() ) 
  {
    if ( rowOrCol > p.cols()-1 ) 
      {
	Rcpp::Rcout <<" You are trying to select the " << rowOrCol << "th column but there are only " << p.cols() << " columns " <<std::endl;
	// exception instead of exit
	throw std::exception() ;
      }
  mct=0;
  for(  mIter.GoToBegin(); !mIter.IsAtEnd(); ++mIter )
  {
    if (mIter.Get() >= 0.5) 
    {
      PixelType val = p(mct,rowOrCol);
      outimage->SetPixel(mIter.GetIndex(),val);
      mct++;
    }
  }
  }  
  else if ( mct == p.cols()  ) // map the cols to the vector 
  {
    if ( rowOrCol > p.rows()-1 ) 
      {
	Rcpp::Rcout <<" You are trying to select the " << rowOrCol << "th row but there are only " << p.rows() << " rows " <<std::endl;
	// exception instead of exit
	throw std::exception() ;
      }
  mct=0;
  for(  mIter.GoToBegin(); !mIter.IsAtEnd(); ++mIter )
  {
    if (mIter.Get() >= 0.5) 
    {
      PixelType val = p(rowOrCol,mct);
      outimage->SetPixel(mIter.GetIndex(),val);
      mct++;
    }
  }
  }
  typedef itk::ImageFileWriter<ImageType> WriterType;
  typename WriterType::Pointer writer = WriterType::New();
  writer->SetFileName( outname );
  writer->SetInput( outimage );
  writer->Update();

}


//p.d.
template <unsigned int ImageDimension, class PixelType>
void ConvertImageVecListToProjection( std::string veclist, std::string imagelist , std::string outname , bool average  )
{
	//typedef itk::Image<PixelType,ImageDimension> ImageType;
	typedef itk::Image<PixelType,ImageDimension> ImageType;
	typedef itk::ImageFileReader<ImageType> ReaderType;

	std::vector<std::string> image_fn_list;
	std::vector<std::string> vec_fn_list;

	// first, count the number of files
	const unsigned int maxChar = 512;
	char lineBuffer[maxChar],lineBufferVec[maxChar];
	char filenm[maxChar],filenmVec[maxChar];
	unsigned int filecount=0, filecountVec=0;
	{
		std::ifstream inputStreamA( imagelist.c_str(), std::ios::in );
		if ( !inputStreamA.is_open() )
		{
			Rcpp::Rcout << "Can't open image list file: " << imagelist << std::endl;
			return;
		}
		while ( !inputStreamA.eof() )
		{
			inputStreamA.getline( lineBuffer, maxChar, '\n' );
      	    if ( sscanf( lineBuffer, "%s ",filenm) != 1 ){
				continue;
			}
			else {
				image_fn_list.push_back(std::string(filenm));
				filecount++;
			}
		}
		inputStreamA.close();
	}

	{
		std::ifstream inputStreamVec( veclist.c_str(), std::ios::in );
		if ( !inputStreamVec.is_open() )
		{
			Rcpp::Rcout << "Can't open Vec list file: " << veclist << std::endl;
			return;
		}
		while ( !inputStreamVec.eof() )
		{
			inputStreamVec.getline( lineBufferVec, maxChar, '\n' );
      	    if ( sscanf( lineBufferVec, "%s ",filenmVec) != 1 ){
				continue;
			}
			else {
				vec_fn_list.push_back(std::string(filenmVec));
				filecountVec++;
			}
		}
		inputStreamVec.close();
	}


	std::ofstream myfile;
	std::string fnmp=outname+std::string(".csv");
	myfile.open(fnmp.c_str(), std::ios::out );
	typedef itk::ImageRegionIteratorWithIndex<ImageType> Iterator;


	for (unsigned int j=0; j< image_fn_list.size(); j++)
	{
		for (unsigned int k=0; k< vec_fn_list.size(); k++) {
		  double proj=0,dotSum=0,dotCounter=0,dotTotal=0;
			typename ReaderType::Pointer reader1 = ReaderType::New();
			reader1->SetFileName( image_fn_list[j] );
			reader1->Update();
			typename ReaderType::Pointer reader2 = ReaderType::New();
			reader2->SetFileName( vec_fn_list[k] );
			reader2->Update();
			Iterator mIter( reader1->GetOutput(),reader1->GetOutput()->GetLargestPossibleRegion() );
			Iterator mIter2( reader2->GetOutput(),reader2->GetOutput()->GetLargestPossibleRegion() );

			for(  mIter.GoToBegin(),mIter2.GoToBegin(); !mIter.IsAtEnd(),!mIter2.IsAtEnd(); ++mIter,++mIter2 )
			{
				proj=mIter.Get()*mIter2.Get();
				dotSum+=proj;
				if ( mIter2.Get() > 0 ) { dotCounter+=mIter2.Get(); dotTotal+=mIter.Get()*mIter2.Get(); }
			}
			if ( average && dotCounter > 0 ) dotSum=dotTotal/dotCounter;
			if (k==vec_fn_list.size()-1)
				myfile << dotSum;
			else
				myfile << dotSum << " , ";
		}
		myfile << std::endl;
	}
	myfile.close();

}


template <unsigned int ImageDimension, class PixelType>
int SVD_One_View( itk::ants::CommandLineParser *parser, unsigned int permct , unsigned int n_evec = 2 , unsigned int robustify=0 , unsigned int p_cluster_thresh = 100, unsigned int iterct = 20 )
{
  Rcpp::Rcout << " sparse-svd "<< std::endl; // note: 2 (in options) is for svd implementation
  itk::ants::CommandLineParser::OptionType::Pointer outputOption =
    parser->GetOption( "output" );
  if( !outputOption || outputOption->GetNumberOfValues() == 0 )
    {
    Rcpp::Rcout << "Warning:  no output option set." << std::endl;
    }
  itk::ants::CommandLineParser::OptionType::Pointer option =
    parser->GetOption( "sparse-svd" );
  typedef itk::Image<PixelType, ImageDimension> ImageType;
  typedef double  Scalar;
  typedef itk::ants::antsSCCANObject<ImageType,Scalar>  SCCANType;
  typedef itk::Image<Scalar,2> MatrixImageType;
  typedef itk::ImageFileReader<ImageType> imgReaderType;
  typename SCCANType::Pointer sccanobj=SCCANType::New();
  sccanobj->SetMaximumNumberOfIterations(iterct);
  typedef typename SCCANType::MatrixType         vMatrix;
  typedef typename SCCANType::VectorType         vVector;
  typedef typename SCCANType::DiagonalMatrixType dMatrix;
  /** read the matrix images */
  /** we refer to the two view matrices as P and Q */
  std::string pmatname=std::string(option->GetParameter( 0 ));
  vMatrix p;
  ReadMatrixFromCSVorImageSet<Scalar>(pmatname,p);
  if ( robustify > 0 ) {
    p=sccanobj->RankifyMatrixColumns(p);
  }
  
  typename ImageType::Pointer mask1=NULL;
  bool have_p_mask=SCCANReadImage<ImageType>(mask1, option->GetParameter( 1 ).c_str() );
  /** the penalties define the fraction of non-zero values for each view */
  double FracNonZero1 = parser->Convert<double>( option->GetParameter( 2 ) );
  if ( FracNonZero1 < 0 )
    {
      FracNonZero1=fabs(FracNonZero1);
      sccanobj->SetKeepPositiveP(false);
    }

  /** read the nuisance matrix image */
  vMatrix r;
  if ( option->GetNumberOfParameters() > 3 ) {
  std::string nuis_img=option->GetParameter( 3 );
  if ( nuis_img.length() > 3 ) {
    Rcpp::Rcout << " nuis_img " << nuis_img << std::endl;
    ReadMatrixFromCSVorImageSet<Scalar>(nuis_img, r);
    CompareMatrixSizes<Scalar>( p,r );
    itk::ants::CommandLineParser::OptionType::Pointer partialccaOpt =
      parser->GetOption( "partial-scca-option" );
    std::string partialccaoption=std::string("PQ");
    if( partialccaOpt )
    {
      //  enum SCCANFormulationType{ PQ , PminusRQ ,  PQminusR ,  PminusRQminusR , PQR  };
      if ( partialccaOpt->GetNumberOfValues() > 0 )
        partialccaoption=parser->Convert<std::string>( partialccaOpt->GetValue() );
      Rcpp::Rcout <<" Partial SCCA option " << partialccaoption << std::endl;
      if( !partialccaoption.compare( std::string( "PQ" ) ) )
      {
        sccanobj->SetSCCANFormulation(  SCCANType::PQ );
      }
      else if( !partialccaoption.compare( std::string( "PminusRQ" ) ) )
      {
        sccanobj->SetSCCANFormulation(  SCCANType::PminusRQ );
      }
      else if( !partialccaoption.compare( std::string( "PQminusR" ) ) )
      {
        sccanobj->SetSCCANFormulation(  SCCANType::PQminusR );
      }
      else if( !partialccaoption.compare( std::string( "PminusRQminusR" ) ) )
      {
        sccanobj->SetSCCANFormulation(  SCCANType::PminusRQminusR );
      }
    }
  }
  }
  else Rcpp::Rcout << " No nuisance parameters." << std::endl;

  sccanobj->SetFractionNonZeroP(FracNonZero1);
  sccanobj->SetMinClusterSizeP( p_cluster_thresh );
  if ( robustify > 0 ) {
    p=sccanobj->RankifyMatrixColumns(p);
  }
  sccanobj->SetMatrixP( p );
  sccanobj->SetMatrixR( r );
  sccanobj->SetMaskImageP( mask1 );
  double truecorr=0;
  truecorr=sccanobj->SparseArnoldiSVD(n_evec);
  vVector w_p=sccanobj->GetVariateP(0);
  Rcpp::Rcout << " true-corr " << sccanobj->GetCanonicalCorrelations() << std::endl;

  if( outputOption )
    {
      std::string filename =  outputOption->GetValue( 0 );
      Rcpp::Rcout << " write " << filename << std::endl;
      std::string::size_type pos = filename.rfind( "." );
      std::string filepre = std::string( filename, 0, pos );
      std::string extension = std::string( filename, pos, filename.length()-1);
      if (extension==std::string(".gz")){
	  pos = filepre.rfind( "." );
	  extension = std::string( filepre, pos, filepre.length()-1 )+extension;
          filepre = std::string( filepre, 0, pos );
      }
      std::string post=std::string("View1vec");      
      WriteVariatesToSpatialImage<ImageType,Scalar>( filename, post, sccanobj->GetVariatesP() , mask1 , sccanobj->GetMatrixP() , have_p_mask );
    }

  return EXIT_SUCCESS;
}



template <unsigned int ImageDimension, class PixelType>
int SCCA_vnl( itk::ants::CommandLineParser *parser, unsigned int permct , unsigned int n_evec = 2 , unsigned int newimp = 0 , unsigned int robustify=0 , unsigned int p_cluster_thresh = 100, unsigned int q_cluster_thresh = 1 , unsigned int iterct = 20 )
{
  itk::ants::CommandLineParser::OptionType::Pointer outputOption =
    parser->GetOption( "output" );
  if( !outputOption || outputOption->GetNumberOfValues() == 0 )
    {
    Rcpp::Rcout << "Warning:  no output option set." << std::endl;
    }
  itk::ants::CommandLineParser::OptionType::Pointer option =
    parser->GetOption( "scca" );
  typedef itk::Image<PixelType, ImageDimension> ImageType;
  typedef double  Scalar;
  typedef itk::ants::antsSCCANObject<ImageType,Scalar>  SCCANType;
  typedef itk::Image<Scalar,2> MatrixImageType;
  typedef itk::ImageFileReader<ImageType> imgReaderType;
  typename SCCANType::Pointer sccanobj=SCCANType::New();
  sccanobj->SetMaximumNumberOfIterations(iterct);
  typedef typename SCCANType::MatrixType         vMatrix;
  typedef typename SCCANType::VectorType         vVector;
  typedef typename SCCANType::DiagonalMatrixType dMatrix;
  Scalar pinvtoler=1.e-6;
  /** read the matrix images */
  /** we refer to the two view matrices as P and Q */
  std::string pmatname=std::string(option->GetParameter( 0 ));
  vMatrix p;
  // Rcpp::Rcout <<" read-p "<< std::endl;
  ReadMatrixFromCSVorImageSet<Scalar>(pmatname,p);
  std::string qmatname=std::string(option->GetParameter( 1 ));
  vMatrix q;
  // Rcpp::Rcout <<" read-q "<< std::endl;
  ReadMatrixFromCSVorImageSet<Scalar>(qmatname,q);
  CompareMatrixSizes<Scalar>( p,q );

  typename ImageType::Pointer mask1=NULL;
  bool have_p_mask=SCCANReadImage<ImageType>(mask1, option->GetParameter( 2 ).c_str() );

  typename ImageType::Pointer mask2=NULL;
  bool have_q_mask=SCCANReadImage<ImageType>(mask2, option->GetParameter( 3 ).c_str() );

  /** the penalties define the fraction of non-zero values for each view */
  double FracNonZero1 = parser->Convert<double>( option->GetParameter( 4 ) );
  if ( FracNonZero1 < 0 )
    {
      FracNonZero1=fabs(FracNonZero1);
      sccanobj->SetKeepPositiveP(false);
    }
  double FracNonZero2 = parser->Convert<double>( option->GetParameter( 5 ) );
  if ( FracNonZero2 < 0 )
    {
      FracNonZero2=fabs(FracNonZero2);
      sccanobj->SetKeepPositiveQ(false);
    }

  sccanobj->SetFractionNonZeroP(FracNonZero1);
  sccanobj->SetFractionNonZeroQ(FracNonZero2);
  sccanobj->SetMinClusterSizeP( p_cluster_thresh );
  sccanobj->SetMinClusterSizeQ( q_cluster_thresh );
  if ( robustify > 0 ) {
    p=sccanobj->RankifyMatrixColumns(p);
    q=sccanobj->RankifyMatrixColumns(q);
  }
  sccanobj->SetMatrixP( p );
  sccanobj->SetMatrixQ( q );
  sccanobj->SetMaskImageP( mask1 );
  sccanobj->SetMaskImageQ( mask2 );

  double truecorr=0;
  if (newimp) truecorr=sccanobj->SparseCCA(n_evec );
  else truecorr=sccanobj->SparsePartialArnoldiCCA(n_evec );
  vVector w_p=sccanobj->GetVariateP(0);
  vVector w_q=sccanobj->GetVariateQ(0);
  Rcpp::Rcout << " true-corr " << sccanobj->GetCanonicalCorrelations() << std::endl;

  if( outputOption )
    {
      std::string filename =  outputOption->GetValue( 0 );
      Rcpp::Rcout << " write " << filename << std::endl;
      std::string::size_type pos = filename.rfind( "." );
      std::string filepre = std::string( filename, 0, pos );
      std::string extension = std::string( filename, pos, filename.length()-1);
      if (extension==std::string(".gz")){
	  pos = filepre.rfind( "." );
	  extension = std::string( filepre, pos, filepre.length()-1 )+extension;
          filepre = std::string( filepre, 0, pos );
      }
      std::string post=std::string("View1vec");
      Rcpp::Rcout << " have_p_mask " << have_p_mask << std::endl;
      WriteVariatesToSpatialImage<ImageType,Scalar>( filename, post, sccanobj->GetVariatesP() , mask1 , sccanobj->GetMatrixP() , have_p_mask );
      post=std::string("View2vec");
      WriteVariatesToSpatialImage<ImageType,Scalar>( filename, post, sccanobj->GetVariatesQ() , mask2 , sccanobj->GetMatrixQ() , have_q_mask );
    }

  /** begin permutation 1. q_pvMatrix CqqInv=vnl_svd_inverse<Scalar>(Cqq);
   q=q*CqqInv;
  sermuted ;  2. scca ;  3. test corrs and weights significance */
  if ( permct > 0 ) {
  unsigned long perm_exceed_ct=0;
  vVector w_p_signif_ct(w_p.size(),0);
  vVector w_q_signif_ct(w_q.size(),0);
  for (unsigned long pct=0; pct<=permct; pct++)
    {
      // 0. compute permutation for q ( switch around rows )
      vMatrix q_perm=PermuteMatrix<Scalar>( sccanobj->GetMatrixQ() );
      sccanobj->SetMatrixQ( q_perm );
      double permcorr=0;
      if (newimp) permcorr=sccanobj->SparseCCA(n_evec );
      else permcorr=sccanobj->SparsePartialArnoldiCCA(n_evec );
      if ( permcorr > truecorr ) perm_exceed_ct++;
      vVector w_p_perm=sccanobj->GetVariateP(0);
      vVector w_q_perm=sccanobj->GetVariateQ(0);
      for (unsigned long j=0; j<w_p.size(); j++)
	if ( w_p_perm(j) > w_p(j))
	  {
	    w_p_signif_ct(j)=w_p_signif_ct(j)++;
	  }
      for (unsigned long j=0; j<w_q.size(); j++)
	if ( w_q_perm(j) > w_q(j) )
	  {
	    w_q_signif_ct(j)=w_q_signif_ct(j)++;
	  }
      // end solve cca permutation
      Rcpp::Rcout << permcorr << " p-value " <<  (double)perm_exceed_ct/(pct+1) << " ct " << pct << " true " << truecorr << std::endl;
    }
  unsigned long psigct=0,qsigct=0;
  for (unsigned long j=0; j<w_p.size(); j++){
    if ( w_p(j) > pinvtoler ) {
      w_p_signif_ct(j)=1.0-(double)w_p_signif_ct(j)/(double)(permct);
      if ( w_p_signif_ct(j) > 0.949 ) psigct++;
    } else w_p_signif_ct(j)=0;
  }
  for (unsigned long j=0; j<w_q.size(); j++) {
    if ( w_q(j) > pinvtoler ) {
      w_q_signif_ct(j)=1.0-(double)w_q_signif_ct(j)/(double)(permct);
      if ( w_q_signif_ct(j) > 0.949 ) qsigct++;
    } else w_q_signif_ct(j)=0;
    }
  Rcpp::Rcout <<  " p-value " <<  (double)perm_exceed_ct/(permct) << " ct " << permct << std::endl;
  Rcpp::Rcout << " p-vox " <<  (double)psigct/w_p.size() << " ct " << permct << std::endl;
  Rcpp::Rcout << " q-vox " <<  (double)qsigct/w_q.size() << " ct " << permct << std::endl;

    if( outputOption )
    {
      std::string filename =  outputOption->GetValue( 0 );
      Rcpp::Rcout << " write " << filename << std::endl;
      std::string::size_type pos = filename.rfind( "." );
      std::string filepre = std::string( filename, 0, pos );
      std::string extension = std::string( filename, pos, filename.length()-1);
      if (extension==std::string(".gz")){
	  pos = filepre.rfind( "." );
	  extension = std::string( filepre, pos, filepre.length()-1 )+extension;
          filepre = std::string( filepre, 0, pos );
      }
      std::string post=std::string("View1pval");
      WriteVectorToSpatialImage<ImageType,Scalar>( filename, post, w_p_signif_ct , mask1);
      post=std::string("View2pval");
      WriteVectorToSpatialImage<ImageType,Scalar>( filename, post, w_q_signif_ct , mask2);
    }
  }
  return EXIT_SUCCESS;
}

template <unsigned int ImageDimension, class PixelType>
int mSCCA_vnl( itk::ants::CommandLineParser *parser,
	       unsigned int permct , bool run_partial_scca = false , unsigned int n_e_vecs = 3 , unsigned int newimp = 0 , unsigned int robustify=0 , unsigned int p_cluster_thresh = 100 , unsigned int q_cluster_thresh = 1  , unsigned int iterct = 20 )
{
  Rcpp::Rcout <<" Entering MSCCA --- computing " << n_e_vecs << " canonical variates by default. " << std::endl;
  itk::ants::CommandLineParser::OptionType::Pointer outputOption =
    parser->GetOption( "output" );
  if( !outputOption || outputOption->GetNumberOfValues() == 0 )
    {
    Rcpp::Rcout << "Warning:  no output option set." << std::endl;
    }
  Rcpp::Rcout << " newimp " << newimp << std::endl;
  itk::ants::CommandLineParser::OptionType::Pointer option =
    parser->GetOption( "scca" );
  typedef itk::Image<PixelType, ImageDimension> ImageType;
  typedef double  Scalar;
  typedef itk::ants::antsSCCANObject<ImageType,Scalar>  SCCANType;
  typedef itk::Image<Scalar,2> MatrixImageType;
  typedef itk::ImageFileReader<MatrixImageType> matReaderType;
  typedef itk::ImageFileReader<ImageType> imgReaderType;
  typename SCCANType::Pointer sccanobj=SCCANType::New();
  sccanobj->SetMaximumNumberOfIterations(iterct);
  typedef typename SCCANType::MatrixType         vMatrix;
  typedef typename SCCANType::VectorType         vVector;
  typedef typename SCCANType::DiagonalMatrixType dMatrix;

  /** we refer to the two view matrices as P and Q */
  typedef itk::Image<PixelType, ImageDimension> ImageType;
  typedef double  Scalar;
  typedef itk::Image<Scalar,2> MatrixImageType;
  typedef itk::ImageFileReader<MatrixImageType> matReaderType;
  typedef itk::ImageFileReader<ImageType> imgReaderType;

  /** read the matrix images */
  std::string pmatname=std::string(option->GetParameter( 0 ));
  vMatrix pin;
  ReadMatrixFromCSVorImageSet<Scalar>(pmatname,pin);
  std::string qmatname=std::string(option->GetParameter( 1 ));
  vMatrix qin;
  ReadMatrixFromCSVorImageSet<Scalar>(qmatname,qin);
  std::string rmatname=std::string(option->GetParameter( 2 ));
  vMatrix rin;
  ReadMatrixFromCSVorImageSet<Scalar>(rmatname,rin);
  CompareMatrixSizes<Scalar>( pin,qin );
  CompareMatrixSizes<Scalar>( qin,rin );
  CompareMatrixSizes<Scalar>( pin,rin );

  typename ImageType::Pointer mask1=NULL;
  bool have_p_mask=SCCANReadImage<ImageType>(mask1, option->GetParameter( 3 ).c_str() );
  typename ImageType::Pointer mask2=NULL;
  bool have_q_mask=SCCANReadImage<ImageType>(mask2, option->GetParameter( 4 ).c_str() );
  typename ImageType::Pointer mask3=NULL;
  bool have_r_mask=SCCANReadImage<ImageType>(mask3, option->GetParameter( 5 ).c_str() );

  /** the penalties define the fraction of non-zero values for each view */
  double FracNonZero1 = parser->Convert<double>( option->GetParameter( 6 ) );
  if ( FracNonZero1 < 0 )
    {
      FracNonZero1=fabs(FracNonZero1);
      sccanobj->SetKeepPositiveP(false);
    }
  double FracNonZero2 = parser->Convert<double>( option->GetParameter( 7 ) );
  if ( FracNonZero2 < 0 )
    {
      FracNonZero2=fabs(FracNonZero2);
      sccanobj->SetKeepPositiveQ(false);
    }
  double FracNonZero3 = parser->Convert<double>( option->GetParameter( 8 ) );
  if ( FracNonZero3 < 0 )
    {
      FracNonZero3=fabs(FracNonZero3);
      sccanobj->SetKeepPositiveR(false);
    }

  sccanobj->SetFractionNonZeroP(FracNonZero1);
  sccanobj->SetFractionNonZeroQ(FracNonZero2);
  sccanobj->SetFractionNonZeroR(FracNonZero3);

  for ( unsigned int leave_out=pin.rows(); leave_out <= pin.rows();  leave_out++) {
    Rcpp::Rcout << " Leaving Out " << leave_out << std::endl;
    vVector p_leave_out;
    vVector q_leave_out;
  if ( leave_out < pin.rows() ) {
    p_leave_out=pin.get_row(leave_out);
    q_leave_out=qin.get_row(leave_out);
  }
  vMatrix p=DeleteRow<MatrixImageType,Scalar>( pin , leave_out );
  vMatrix q=DeleteRow<MatrixImageType,Scalar>( qin , leave_out );
  vMatrix r=DeleteRow<MatrixImageType,Scalar>( rin , leave_out );
  sccanobj->SetMinClusterSizeP( p_cluster_thresh );
  sccanobj->SetMinClusterSizeQ( q_cluster_thresh );
  if ( robustify > 0 ) {
    p=sccanobj->RankifyMatrixColumns(p);
    q=sccanobj->RankifyMatrixColumns(q);
    r=sccanobj->RankifyMatrixColumns(r);
  }
  double truecorr=0;
  if ( run_partial_scca ){
    Rcpp::Rcout <<" begin partial PQ " << std::endl;
    typename SCCANType::Pointer sccanobjCovar=SCCANType::New();
    sccanobjCovar->SetMaximumNumberOfIterations(iterct);
    sccanobjCovar->SetMatrixP( p );
    sccanobjCovar->SetMatrixQ( q );
    sccanobjCovar->SetMatrixR( r );
    sccanobjCovar->SetMinClusterSizeP( p_cluster_thresh );
    sccanobjCovar->SetMinClusterSizeQ( q_cluster_thresh );

    itk::ants::CommandLineParser::OptionType::Pointer partialccaOpt =
      parser->GetOption( "partial-scca-option" );
    std::string partialccaoption=std::string("PQ");
    if( partialccaOpt )
    {
      //  enum SCCANFormulationType{ PQ , PminusRQ ,  PQminusR ,  PminusRQminusR , PQR  };
      if ( partialccaOpt->GetNumberOfValues() > 0 )
        partialccaoption=parser->Convert<std::string>( partialccaOpt->GetValue() );
      Rcpp::Rcout <<" Partial SCCA option " << partialccaoption << std::endl;
      if( !partialccaoption.compare( std::string( "PQ" ) ) )
      {
        sccanobjCovar->SetSCCANFormulation(  SCCANType::PQ );
      }
      else if( !partialccaoption.compare( std::string( "PminusRQ" ) ) )
      {
        sccanobjCovar->SetSCCANFormulation(  SCCANType::PminusRQ );
      }
      else if( !partialccaoption.compare( std::string( "PQminusR" ) ) )
      {
        sccanobjCovar->SetSCCANFormulation(  SCCANType::PQminusR );
      }
      else if( !partialccaoption.compare( std::string( "PminusRQminusR" ) ) )
      {
        sccanobjCovar->SetSCCANFormulation(  SCCANType::PminusRQminusR );
      }
    }
    sccanobjCovar->SetFractionNonZeroP(FracNonZero1);
    sccanobjCovar->SetKeepPositiveP( sccanobj->GetKeepPositiveP() );
    sccanobjCovar->SetFractionNonZeroQ(FracNonZero2);
    sccanobjCovar->SetKeepPositiveQ( sccanobj->GetKeepPositiveQ() );
    sccanobjCovar->SetMaskImageP( mask1 );
    sccanobjCovar->SetMaskImageQ( mask2 );
    if (newimp == 1) truecorr=sccanobjCovar->SparsePartialCCA(n_e_vecs);
    else if (newimp == 0 ) truecorr=sccanobjCovar->SparsePartialArnoldiCCA(n_e_vecs);
    //  truecorr=sccanobjCovar->RunSCCAN2multiple(n_e_vecs );
    Rcpp::Rcout << " partialed out corr " ;
    for (unsigned int ff=0; ff< sccanobjCovar->GetCanonicalCorrelations().size() ; ff++ )
      Rcpp::Rcout << " " << sccanobjCovar->GetCanonicalCorrelations()[ff];
    Rcpp::Rcout << std::endl;

  if( outputOption )
    {
      std::string filename =  outputOption->GetValue( 0 );
      std::string::size_type pos = filename.rfind( "." );
      std::string filepre = std::string( filename, 0, pos );
      std::string extension = std::string( filename, pos, filename.length()-1);
      if (extension==std::string(".gz")){
	  pos = filepre.rfind( "." );
	  extension = std::string( filepre, pos, filepre.length()-1 )+extension;
          filepre = std::string( filepre, 0, pos );
      }
      std::string post=std::string("View1vec");
      WriteVariatesToSpatialImage<ImageType,Scalar>( filename, post, sccanobjCovar->GetVariatesP() , mask1, sccanobjCovar->GetMatrixP() , have_p_mask );
      post=std::string("View2vec");
      WriteVariatesToSpatialImage<ImageType,Scalar>( filename, post, sccanobjCovar->GetVariatesQ() , mask2, sccanobjCovar->GetMatrixQ() , have_q_mask );
    }

  /** begin permutation 1. q_pvMatrix CqqInv=vnl_svd_inverse<Scalar>(Cqq);
   q=q*CqqInv;
  sermuted ;  2. scca ;  3. test corrs and weights significance */
  if ( permct > 0 ) {
  unsigned long perm_exceed_ct=0;
  vVector w_p_signif_ct(p.cols(),0);
  vVector w_q_signif_ct(q.cols(),0);
  for (unsigned long pct=0; pct<=permct; pct++)
    {
      /** both the new object and the copy object should produce the same results - verified in 1 example!*/
      //      typename SCCANType::Pointer sccanobjPerm=sccanobjCovar;
      typename SCCANType::Pointer sccanobjPerm=SCCANType::New();
      sccanobjPerm->SetMaximumNumberOfIterations(iterct);
      sccanobjPerm->SetMinClusterSizeP( p_cluster_thresh );
      sccanobjPerm->SetMinClusterSizeQ( q_cluster_thresh );
      sccanobjPerm->SetFractionNonZeroP(FracNonZero1);
      sccanobjPerm->SetKeepPositiveP( sccanobj->GetKeepPositiveP() );
      sccanobjPerm->SetKeepPositiveQ( sccanobj->GetKeepPositiveQ() );
      sccanobjPerm->SetFractionNonZeroQ(FracNonZero2);
      sccanobjPerm->SetMaskImageP( mask1 );
      sccanobjPerm->SetMaskImageQ( mask2 );
      // 0. compute permutation for q ( switch around rows )
      vMatrix p_perm=PermuteMatrix<Scalar>( sccanobjCovar->GetMatrixP() );
      vMatrix q_perm=PermuteMatrix<Scalar>( sccanobjCovar->GetMatrixQ()  );
      vMatrix r_perm=PermuteMatrix<Scalar>( r );
      sccanobjPerm->SetMatrixP( p_perm );
      sccanobjPerm->SetMatrixQ( q_perm );
      sccanobjPerm->SetMatrixR( r_perm );
      //      double permcorr=sccanobjPerm->RunSCCAN2();
      sccanobjPerm->SetSCCANFormulation( sccanobjCovar->GetSCCANFormulation() );
      sccanobjPerm->SetAlreadyWhitened( false );
      double permcorr=0;
      if ( newimp == 0 ) permcorr=sccanobjPerm->SparsePartialArnoldiCCA(n_e_vecs);
      else if ( newimp == 1 ) permcorr=sccanobjPerm->SparsePartialCCA(n_e_vecs);
      //permcorr=sccanobjPerm->RunSCCAN2multiple(n_e_vecs);//
      //else permcorr=
      Rcpp::Rcout << " partialed out corr " ;
      for (unsigned int ff=0; ff< sccanobjPerm->GetCanonicalCorrelations().size() ; ff++ )
        Rcpp::Rcout << " " << sccanobjPerm->GetCanonicalCorrelations()[ff];
      Rcpp::Rcout << std::endl;
      if ( permcorr > truecorr ) perm_exceed_ct++;
 /*
      vVector w_p_perm=sccanobjPerm->GetVariateP(0);
      vVector w_q_perm=sccanobjPerm->GetVariateQ(0);
      for (unsigned long j=0; j<p.cols(); j++)
	if ( w_p_perm(j) > sccanobjCovar->GetVariateP(0)(j))
	  {
	    w_p_signif_ct(j)=w_p_signif_ct(j)++;
	  }
      for (unsigned long j=0; j<q.cols(); j++)
	if ( w_q_perm(j) >  sccanobjCovar->GetVariateQ(0)(j) )
	  {
	    w_q_signif_ct(j)=w_q_signif_ct(j)++;
	  }
      // end solve cca permutation
      */
      Rcpp::Rcout << permcorr << " p-value " <<  (double)perm_exceed_ct/(pct+1) << " ct " << pct << " true " << truecorr << std::endl;
    }
  unsigned long psigct=0,qsigct=0;
  Scalar pinvtoler=1.e-6;
  for (unsigned long j=0; j< sccanobjCovar->GetVariateP(0).size(); j++){
    if (  sccanobjCovar->GetVariateP(0)(j) > pinvtoler ) {
      w_p_signif_ct(j)=1.0-(double)w_p_signif_ct(j)/(double)(permct);
      if ( w_p_signif_ct(j) > 0.949 ) psigct++;
    } else w_p_signif_ct(j)=0;
  }
  for (unsigned long j=0; j< sccanobjCovar->GetVariateQ(0).size(); j++) {
    if (  sccanobjCovar->GetVariateQ(0)(j) > pinvtoler ) {
      w_q_signif_ct(j)=1.0-(double)w_q_signif_ct(j)/(double)(permct);
      if ( w_q_signif_ct(j) > 0.949 ) qsigct++;
    } else w_q_signif_ct(j)=0;
    }
  Rcpp::Rcout <<  " p-value " <<  (double)perm_exceed_ct/(permct) << " ct " << permct << std::endl;
  Rcpp::Rcout << " p-vox " <<  (double)psigct/sccanobjCovar->GetVariateP(0).size() << " ct " << permct << std::endl;
  Rcpp::Rcout << " q-vox " <<  (double)qsigct/sccanobjCovar->GetVariateP(0).size() << " ct " << permct << std::endl;
  }

  // exception instead of exit
  throw std::exception() ;
 }
  Rcpp::Rcout << " VNL mSCCA " << std::endl;
  sccanobj->SetMatrixP( p );
  sccanobj->SetMatrixQ( q );
  sccanobj->SetMatrixR( r );
  sccanobj->SetMaskImageP( mask1 );
  sccanobj->SetMaskImageQ( mask2 );
  sccanobj->SetMaskImageR( mask3 );
  truecorr=sccanobj->RunSCCAN3();
  vVector w_p=sccanobj->GetPWeights();
  vVector w_q=sccanobj->GetQWeights();
  vVector w_r=sccanobj->GetRWeights();
  Rcpp::Rcout << " final correlation  " << truecorr  << std::endl;
  //  Rcpp::Rcout << " Projection-P " << p*w_p << std::endl;
  // Rcpp::Rcout << " Projection-Q " << q*w_q << std::endl;
  if ( leave_out < pin.rows() ) {
    Rcpp::Rcout << " Projection-leave-P " << dot_product(p_leave_out,w_p) << std::endl;
    Rcpp::Rcout << " Projection-leave-Q " << dot_product(q_leave_out,w_q) << std::endl;
  }
  //  Rcpp::Rcout <<  " r weights " << w_r << std::endl;
  for (unsigned long j=0; j<w_r.size(); j++) {
    if ( w_r(j) > 0)
      Rcpp::Rcout << " r-weight " << j << "," << w_r(j) << std::endl;
  }
  if( outputOption )
    {
      std::string filename =  outputOption->GetValue( 0 );
      Rcpp::Rcout << " write " << filename << std::endl;
      std::string::size_type pos = filename.rfind( "." );
      std::string filepre = std::string( filename, 0, pos );
      std::string extension = std::string( filename, pos, filename.length()-1);
      if (extension==std::string(".gz")){
	  pos = filepre.rfind( "." );
	  extension = std::string( filepre, pos, filepre.length()-1 )+extension;
          filepre = std::string( filepre, 0, pos );
      }
      std::string post=std::string("View1vec");      
      WriteVariatesToSpatialImage<ImageType,Scalar>( filename, post, sccanobj->GetVariatesP() , mask1, sccanobj->GetMatrixP() , have_p_mask);
      post=std::string("View2vec");
      WriteVariatesToSpatialImage<ImageType,Scalar>( filename, post,  sccanobj->GetVariatesQ(), mask2,  sccanobj->GetMatrixQ(), have_q_mask );
    }

  /** begin permutation 1. q_pvMatrix CqqInv=vnl_svd_inverse<Scalar>(Cqq);
   q=q*CqqInv;
  sermuted ;  2. scca ;  3. test corrs and weights significance */
  unsigned long perm_exceed_ct=0;
  if ( permct > 0 ) {
  vVector w_p_signif_ct(w_p.size(),0);
  vVector w_q_signif_ct(w_q.size(),0);
  vVector w_r_signif_ct(w_r.size(),0);
  for (unsigned long pct=0; pct<=permct; pct++)
    {
      // 0. compute permutation for q ( switch around rows )
      //Rcpp::Rcout << " dont permute q " << std::endl;
      vMatrix q_perm=PermuteMatrix<Scalar>( sccanobj->GetMatrixQ() );
      vMatrix r_perm=PermuteMatrix<Scalar>( sccanobj->GetMatrixR() );
      sccanobj->SetMatrixQ( q_perm );
      sccanobj->SetMatrixR( r_perm );
      double permcorr=sccanobj->RunSCCAN3();
      if ( permcorr > truecorr ) perm_exceed_ct++;
      vVector w_p_perm=sccanobj->GetPWeights();
      vVector w_q_perm=sccanobj->GetQWeights();
      vVector w_r_perm=sccanobj->GetRWeights();

      for (unsigned long j=0; j<w_r.size(); j++)
	if ( w_r_perm(j) > w_r(j))
	  {
	    w_r_signif_ct(j)=w_r_signif_ct(j)++;
	  }
      //      Rcpp::Rcout << " only testing correlation with biserial predictions " << std::endl;
      // end solve cca permutation
      Rcpp::Rcout << permcorr << " p-value " <<  (double)perm_exceed_ct/(pct+1) << " ct " << pct << " true " << truecorr << std::endl;
      for (unsigned long j=0; j<w_r.size(); j++) {
	if ( w_r(j) > 0)
	Rcpp::Rcout << " r entry " << j << " signif " <<  (double)w_r_signif_ct(j)/(double)(pct+1) << std::endl;
      }

    }
  }
  //  Rcpp::Rcout <<  " p-value " <<  (double)perm_exceed_ct/(permct+1) << " ct " << permct << std::endl;
  }
  return EXIT_SUCCESS;
}


int sccan( itk::ants::CommandLineParser *parser )
{
  // Define dimensionality
  typedef double PixelType;
  const unsigned int ImageDimension=3;
  typedef itk::Image<PixelType, ImageDimension> ImageType;
  typedef double  matPixelType;
  typedef itk::Image<matPixelType,2> MatrixImageType;
  typedef itk::ImageFileReader<MatrixImageType> ReaderType;

  itk::ants::CommandLineParser::OptionType::Pointer outputOption =
    parser->GetOption( "output" );
  if( !outputOption || outputOption->GetNumberOfValues() == 0 )
    {
      Rcpp::Rcout << "Warning:  no output option set." << std::endl;
    }
  unsigned int permct=0;
  itk::ants::CommandLineParser::OptionType::Pointer permoption =
    parser->GetOption( "n_permutations" );
  if( !permoption || permoption->GetNumberOfValues() == 0 )
    {
      //    Rcpp::Rcout << "Warning:  no permutation option set." << std::endl;
    }
  else permct=parser->Convert<unsigned int>( permoption->GetValue() );

  unsigned int iterct=20;
  permoption = parser->GetOption( "iterations" );
  if( permoption && permoption->GetNumberOfValues() > 0 )
    iterct=parser->Convert<unsigned int>( permoption->GetValue() );
  if (iterct < 20 ) iterct=20;

  unsigned int evec_ct=1;
  itk::ants::CommandLineParser::OptionType::Pointer evec_option =
    parser->GetOption( "n_eigenvectors" );
  if( !evec_option || evec_option->GetNumberOfValues() == 0 )
    {
      //    Rcpp::Rcout << "Warning:  no permutation option set." << std::endl;
    }
  else evec_ct=parser->Convert<unsigned int>( evec_option->GetValue() );

  unsigned int robustify=1;
  itk::ants::CommandLineParser::OptionType::Pointer robust_option =
    parser->GetOption( "robustify" );
  if( !robust_option || robust_option->GetNumberOfValues() == 0 )
    {
      //    Rcpp::Rcout << "Warning:  no permutation option set." << std::endl;
    }
  else robustify=parser->Convert<unsigned int>( robust_option->GetValue() );

  unsigned int p_cluster_thresh=1;
  itk::ants::CommandLineParser::OptionType::Pointer clust_option =
    parser->GetOption( "PClusterThresh" );
  if( !clust_option || clust_option->GetNumberOfValues() == 0 )
    {
      //    Rcpp::Rcout << "Warning:  no permutation option set." << std::endl;
    }
  else p_cluster_thresh=parser->Convert<unsigned int>( clust_option->GetValue() );

  unsigned int q_cluster_thresh=1;
  clust_option = parser->GetOption( "QClusterThresh" );
  if( !clust_option || clust_option->GetNumberOfValues() == 0 )
    {
      //    Rcpp::Rcout << "Warning:  no permutation option set." << std::endl;
    }
  else q_cluster_thresh=parser->Convert<unsigned int>( clust_option->GetValue() );

  bool eigen_imp=false;
  itk::ants::CommandLineParser::OptionType::Pointer eigen_option =
    parser->GetOption( "eigen_cca" );
  if( !eigen_option || eigen_option->GetNumberOfValues() == 0 )
    {
      //    Rcpp::Rcout << "Warning:  no permutation option set." << std::endl;
    }
  else eigen_imp=parser->Convert<bool>( eigen_option->GetValue() );

  //  operations on individual matrices
  itk::ants::CommandLineParser::OptionType::Pointer matrixOption =
    parser->GetOption( "imageset-to-matrix" );
  if( matrixOption && matrixOption->GetNumberOfValues() > 0 )
    {
      std::string outname =  outputOption->GetValue( 0 );
      std::string imagelist=matrixOption->GetParameter( 0 );
      std::string maskfn=matrixOption->GetParameter( 1 );
      ConvertImageListToMatrix<ImageDimension,double>( imagelist,  maskfn  , outname );
      return EXIT_SUCCESS;
    }

  //  operations on individual matrices
  itk::ants::CommandLineParser::OptionType::Pointer matrixOptionTimeSeries =
    parser->GetOption( "timeseriesimage-to-matrix" );
  if( matrixOptionTimeSeries && matrixOptionTimeSeries->GetNumberOfValues() > 0 )
    {
      std::string outname=outputOption->GetValue( 0 );
      std::string imagefn=matrixOptionTimeSeries->GetParameter( 0 );
      std::string maskfn=matrixOptionTimeSeries->GetParameter( 1 );
      double smoother_space=0;
      if ( matrixOptionTimeSeries->GetNumberOfParameters() > 2 )
        smoother_space=parser->Convert<double>( matrixOptionTimeSeries->GetParameter( 2 ) );
      double smoother_time=0;
      if ( matrixOptionTimeSeries->GetNumberOfParameters() > 3 )
        smoother_time=parser->Convert<double>( matrixOptionTimeSeries->GetParameter( 3 ) );
      typedef itk::Image<double,2> MyImageType;
      ConvertTimeSeriesImageToMatrix<double>( imagefn,  maskfn  , outname , smoother_space, smoother_time );
      Rcpp::Rcout <<" outname done " << outname << std::endl;
      return EXIT_SUCCESS;
    }

  //  operations on individual matrices
  itk::ants::CommandLineParser::OptionType::Pointer matrixOptionV2I =
    parser->GetOption( "vector-to-image" );
  if( matrixOptionV2I && matrixOptionV2I->GetNumberOfValues() > 0 )
    {
      std::string outname=outputOption->GetValue( 0 );
      std::string csvfn=matrixOptionV2I->GetParameter( 0 );
      std::string maskfn=matrixOptionV2I->GetParameter( 1 );
      unsigned long rowOrCol=parser->Convert<unsigned long>( matrixOptionV2I->GetParameter( 2 ) );
      ConvertCSVVectorToImage<double>( csvfn,  maskfn  , outname , rowOrCol );
      Rcpp::Rcout <<" V2I done " << outname << std::endl;
      return EXIT_SUCCESS;
    }



	//p.d.
    itk::ants::CommandLineParser::OptionType::Pointer matrixProjectionOption =
      parser->GetOption( "imageset-to-projections" );
    if( matrixProjectionOption && matrixProjectionOption->GetNumberOfValues() > 0 )
    {

		std::string outFilename =  outputOption->GetValue( 0 );
		std::string vecList=matrixProjectionOption->GetParameter( 0 );
		std::string imageList=matrixProjectionOption->GetParameter( 1 );
		bool average=parser->Convert<bool>( matrixProjectionOption->GetParameter( 2 ) );
		//Rcpp::Rcout <<"here" << outFilename << " " << vecList << " " <<imageList << std::endl;
		if ( average ) Rcpp::Rcout << " doing average instead of dot product " << std::endl;
		ConvertImageVecListToProjection<ImageDimension,double>(vecList,imageList,outFilename , average );
		return EXIT_SUCCESS;
    }

    itk::ants::CommandLineParser::OptionType::Pointer svdOption = parser->GetOption( "sparse-svd" );
    if( svdOption && svdOption->GetNumberOfValues() > 0 )
    {
      SVD_One_View<ImageDimension, double>(  parser, permct , evec_ct , robustify , p_cluster_thresh, iterct);
      return EXIT_SUCCESS;
    }

  Rcpp::Rcout <<" scca-max-iterations " << iterct << " you will assess significance with " << permct << " permutations." << std::endl;
  //  operations on pairs of matrices
  itk::ants::CommandLineParser::OptionType::Pointer matrixPairOption =
    parser->GetOption( "scca" );
  if( matrixPairOption && matrixPairOption->GetNumberOfValues() > 0 )
    {
      if( matrixPairOption && matrixPairOption->GetNumberOfParameters() < 2 )
      {
        Rcpp::Rcout << "  Incorrect number of parameters."<<  std::endl;
        return EXIT_FAILURE;
      }
      std::string initializationStrategy = matrixPairOption->GetValue();
      // call RCCA_eigen or RCCA_vnl
      if (  !initializationStrategy.compare( std::string( "two-view" ) )  )
      {
      Rcpp::Rcout << " scca 2-view "<< std::endl;
      SCCA_vnl<ImageDimension, double>( parser , permct , evec_ct, eigen_imp, robustify, p_cluster_thresh, q_cluster_thresh, iterct);
      }
      else if (  !initializationStrategy.compare( std::string("three-view") )  )
      {
      Rcpp::Rcout << " mscca 3-view "<< std::endl;
      mSCCA_vnl<ImageDimension, double>( parser, permct,  false , evec_ct, eigen_imp, robustify,  p_cluster_thresh, q_cluster_thresh, iterct);
      }
      else if ( !initializationStrategy.compare( std::string("partial") )   )
      {
      Rcpp::Rcout << " pscca "<< std::endl;
      mSCCA_vnl<ImageDimension, double>( parser, permct , true , evec_ct , eigen_imp, robustify,  p_cluster_thresh, q_cluster_thresh, iterct);
      }
      else
      {
      Rcpp::Rcout <<" unrecognized option in matrixPairOperation " << std::endl;
      return EXIT_FAILURE;
      }
      return EXIT_SUCCESS;
    }
  else {
    Rcpp::Rcout << " no option specified " << std::endl;
  }
  return EXIT_FAILURE;
}

void InitializeCommandLineOptions( itk::ants::CommandLineParser *parser )
{
  /** in this function, list all the operations you will perform */

  typedef itk::ants::CommandLineParser::OptionType OptionType;
  {
  std::string description = std::string( "Print the help menu (short version)." );
  OptionType::Pointer option = OptionType::New();
  option->SetShortName( 'h' );
  option->SetDescription( description );
  option->AddValue( std::string( "0" ) );
  parser->AddOption( option );
  }

  {
  std::string description = std::string( "Print the help menu (long version)." );
  OptionType::Pointer option = OptionType::New();
  option->SetLongName( "help" );
  option->SetDescription( description );
  option->AddValue( std::string( "0" ) );
  parser->AddOption( option );
  }

  {
  std::string description =
    std::string( "Output dependent on which option is called." );
  OptionType::Pointer option = OptionType::New();
  option->SetLongName( "output" );
  option->SetShortName( 'o' );
  option->SetUsageOption( 0, "outputImage" );
  option->SetDescription( description );
  parser->AddOption( option );
  }

  {
  std::string description =
    std::string( "Number of permutations to use in scca." );
  OptionType::Pointer option = OptionType::New();
  option->SetLongName( "n_permutations" );
  option->SetShortName( 'p' );
  option->SetUsageOption( 0, "500" );
  option->SetDescription( description );
  parser->AddOption( option );
  }

  {
  std::string description =
    std::string( "Max iterations for scca optimization (min 20)." );
  OptionType::Pointer option = OptionType::New();
  option->SetLongName( "iterations" );
  option->SetShortName( 'i' );
  option->SetUsageOption( 0, "20" );
  option->SetDescription( description );
  parser->AddOption( option );
  }


  {
  std::string description =
    std::string( "Number of permutations to use in scca." );
  OptionType::Pointer option = OptionType::New();
  option->SetLongName( "n_eigenvectors" );
  option->SetShortName( 'n' );
  option->SetUsageOption( 0, "2" );
  option->SetDescription( description );
  parser->AddOption( option );
  }

  {
  std::string description =
    std::string( "rank-based scca" );
  OptionType::Pointer option = OptionType::New();
  option->SetLongName( "robustify" );
  option->SetShortName( 'r' );
  option->SetUsageOption( 0, "0" );
  option->SetDescription( description );
  parser->AddOption( option );
  }

  {
  std::string description =
    std::string( "cluster threshold on view P" );
  OptionType::Pointer option = OptionType::New();
  option->SetLongName( "PClusterThresh" );
  option->SetUsageOption( 0, "1" );
  option->SetDescription( description );
  parser->AddOption( option );
  }
  {
  std::string description =
    std::string( "cluster threshold on view Q" );
  OptionType::Pointer option = OptionType::New();
  option->SetLongName( "QClusterThresh" );
  option->SetUsageOption( 0, "1" );
  option->SetDescription( description );
  parser->AddOption( option );
  }


  {
  std::string description =
    std::string( "Number of permutations to use in scca." );
  OptionType::Pointer option = OptionType::New();
  option->SetLongName( "eigen_cca" );
  option->SetShortName( 'e' );
  option->SetUsageOption( 0, "0" );
  option->SetDescription( description );
  parser->AddOption( option );
  }


  {
  std::string description =
    std::string( "Choices for pscca: PQ, PminusRQ, PQminusR, PminusRQminusR " );
  OptionType::Pointer option = OptionType::New();
  option->SetLongName( "partial-scca-option" );
  option->SetUsageOption( 0, "PminusRQ" );
  option->SetDescription( description );
  parser->AddOption( option );
  }

  {
  std::string description =
    std::string( "takes a list of image files names (one per line) " ) +
    std::string( "and converts it to a 2D matrix / image in binary or csv format depending on the filetype used to define the output." );
  OptionType::Pointer option = OptionType::New();
  option->SetLongName( "imageset-to-matrix" );
  option->SetUsageOption( 0, "[list.txt,mask.nii.gz]" );
  option->SetDescription( description );
  parser->AddOption( option );
  }

  {
  std::string description =
    std::string( "takes a timeseries (4D) image " ) +
    std::string( "and converts it to a 2D matrix csv format as output." );
  OptionType::Pointer option = OptionType::New();
  option->SetLongName( "timeseriesimage-to-matrix" );
  option->SetUsageOption( 0, "[four_d_image.nii.gz,three_d_mask.nii.gz, optional-spatial-smoothing-param-in-spacing-units-default-zero, optional-temporal-smoothing-param-in-time-series-units-default-zero  ]" );
  option->SetDescription( description );
  parser->AddOption( option );
  }


  {
  std::string description =
    std::string( "converts the 1st column vector in a csv file back to an image --- currently needs the csv file to have > 1 columns.  if the number of entries in the column does not equal the number of entries in the mask but the number of rows does equal the number of entries in the mask, then it will convert the row vector to an image. " );
  OptionType::Pointer option = OptionType::New();
  option->SetLongName( "vector-to-image" );
  option->SetUsageOption( 0, "[vector.csv,three_d_mask.nii.gz, which-row-or-col ]" );
  option->SetDescription( description );
  parser->AddOption( option );
  }

//p.d.
  {
    std::string description =
    std::string( "takes a list of image and projection files names (one per line) " ) +
    std::string( "and writes them to a  csv file --- basically computing X*Y (matrices)." );
    OptionType::Pointer option = OptionType::New();
    option->SetLongName( "imageset-to-projections" );
    option->SetUsageOption( 0, "[list_projections.txt,list_images.txt, bool do-average-not-real-projection ]" );
    option->SetDescription( description );
    parser->AddOption( option );
  }


  {
  std::string description =
    std::string( "Matrix-based scca operations for 2 and 3 views." ) +
    std::string( "For all these options, the FracNonZero terms set the fraction of variables to use in the estimate. E.g. if one sets 0.5 then half of the variables will have non-zero values.  If the FracNonZero is (+) then the weight vectors must be positive.  If they are negative, weights can be (+) or (-).  partial does partial scca for 2 views while partialing out the 3rd view. ");
  OptionType::Pointer option = OptionType::New();
  option->SetLongName( "scca" );
  option->SetUsageOption( 0, "two-view[matrix-view1.mhd,matrix-view2.mhd,mask1,mask2,FracNonZero1,FracNonZero2] ");
  option->SetUsageOption( 1, "three-view[matrix-view1.mhd,matrix-view2.mhd,matrix-view3.mhd,mask1,mask2,mask3,FracNonZero1,FracNonZero2,FracNonZero3]" );
  option->SetUsageOption( 2, "partial[matrix-view1.mhd,matrix-view2.mhd,matrix-view3.mhd,mask1,mask2,mask3,FracNonZero1,FracNonZero2,FracNonZero3]" );
  option->SetDescription( description );
  parser->AddOption( option );
  }


  {
    std::string description =
    std::string( "a sparse svd implementation --- will report correlation of eigenvector with original data columns averaged over columns with non-zero weights." );
    OptionType::Pointer option = OptionType::New();
    option->SetLongName( "sparse-svd" );
    option->SetUsageOption( 0, "[matrix-view1.mhd,mask1,FracNonZero1,nuisance-matrix] --- will only use view1 ... unless nuisance matrix is specified." );
    option->SetDescription( description );
    parser->AddOption( option );
  }



}


RcppExport SEXP sccan( SEXP r_args )
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

  itk::ants::CommandLineParser::Pointer parser =
    itk::ants::CommandLineParser::New();
  parser->SetCommand( argv[0] );

  std::string commandDescription =
    std::string( "A tool for sparse statistical analysis on images : " ) +
    std::string( " scca, pscca (with options), mscca.  Can also convert an imagelist/mask pair to a binary matrix image.  " );

  parser->SetCommandDescription( commandDescription );
  InitializeCommandLineOptions( parser );

  parser->Parse( argc, argv );

  // Print the entire help menu
  itk::ants::CommandLineParser::OptionType::Pointer longHelpOption =
    parser->GetOption( "help" );
  if( argc == 1 ||
    ( longHelpOption && parser->Convert<unsigned int>( longHelpOption->GetValue() ) == 1 )
      )
    {
    parser->PrintMenu( Rcpp::Rcout, 5, false );
    return Rcpp::wrap( EXIT_FAILURE );
    }

  itk::ants::CommandLineParser::OptionType::Pointer shortHelpOption =
    parser->GetOption( 'h' );
  if( argc == 1 || ( shortHelpOption &&
    parser->Convert<unsigned int>( shortHelpOption->GetValue() ) == 1 ) )
    {
    parser->PrintMenu( Rcpp::Rcout, 5, true );
    return Rcpp::wrap( EXIT_FAILURE );
    }

  // Print the long help menu for specific items
  if( longHelpOption && longHelpOption->GetNumberOfValues() > 0
    && parser->Convert<unsigned int>( longHelpOption->GetValue() ) != 0 )
    {
    itk::ants::CommandLineParser::OptionListType options =
      parser->GetOptions();
    for( unsigned int n = 0; n < longHelpOption->GetNumberOfValues(); n++ )
      {
      std::string value = longHelpOption->GetValue( n );
      itk::ants::CommandLineParser::OptionListType::const_iterator it;
      for( it = options.begin(); it != options.end(); ++it )
        {
        const char *longName = ( ( *it )->GetLongName() ).c_str();
        if( strstr( longName, value.c_str() ) == longName  )
          {
          parser->PrintMenu( Rcpp::Rcout, 5, false );
          }
        }
      }
    return Rcpp::wrap( EXIT_FAILURE );
    }


  // Call main routine
  sccan( parser );

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


  // now compute covariance matrices
  // covariance matrix --- Cov(X, Y) = E[XY] - E[X].E[Y]
  /* input matrix
ta<-matrix(c(-1,1,2,2,-2,3,1,1,4,0,3,4),nrow=3,ncol=4)
 ta<-matrix(c(-1,1,2,-2,3,1,4,0,3),nrow=3,ncol=3)

> ta
     [,1] [,2] [,3]
[1,]   -1    1    2
[2,]   -2    3    1
[3,]    4    0    3

  // cov(ta,ta)
          [,1]      [,2] [,3]
[1,] 10.333333 -4.166667  3.0
[2,] -4.166667  2.333333 -1.5
[3,]  3.000000 -1.500000  1.0

> cov(a[1,]-mean(a[1,]),a[1,]-mean(a[1,]))
[1] 10.33333
v<-a[1,]-mean(a[1,])
> v*v
[1]  1.777778  5.444444 13.444444
> sum(v*v)
[1] 20.66667
> sum(v*v)/(2)  <-  sum( v*v ) / ( n - 1 ) = covariance of two vectors ...
[1] 10.33333
  */

   /** try  q.colwise.sum() to compute mean ... */
   /** try  q.rowwise.sum() to compute mean ...

 eMatrix testM(3,3);
testM(0,0)=-1;testM(1,0)=1; testM(2,0)=2;
testM(0,1)=-2;testM(1,1)=3; testM(2,1)=1;
testM(0,2)= 4;testM(1,2)=0; testM(2,2)=3;
 p=testM;
 pMatSize[0]=3;
 pMatSize[1]=3;
   */


  /*
              //1.0/(double)q.columns(); //randgen.drand32();
  for (unsigned int it=0; it<4; it++)
  {
    //    Rcpp::Rcout << " 2norm(v0) " << v_0.two_norm() << std::endl;
    vVector v_1=(q)*v_0;
    double vnorm=v_1.two_norm();
    Rcpp::Rcout << " von " << vnorm << std::endl;
    v_0=v_1/(vnorm);
    Rcpp::Rcout << " vo " << v_0 << std::endl;
  // check if power method works ....
  vVector Xv=q*v_0;
  Scalar vdotXv = dot_product(v_0,Xv);
  Rcpp::Rcout << " vdotXv " << vdotXv << std::endl;
  vVector Xv2=Xv-v_0*vdotXv;
  // this value should be small -- i.e. v_0 is an eigenvector of X
  Rcpp::Rcout << " init eigenvector result " << Xv2.squared_magnitude() << std::endl;}
*/
  //  Rcpp::Rcout << v_0 << std::endl;
   /*

function [Up,Sp,Vp] = rank_one_svd_update( U, S, V, a, b, force_orth )
% function [Up,Sp,Vp] = rank_one_svd_update( U, S, V, a, b, force_orth )
%
% Given the SVD of
%
%   X = U*S*V'
%
% update it to be the SVD of
%
%   X + ab' = Up*Sp*Vp'
%
% that is, implement a rank-one update to the SVD of X.
%
% Depending on a,b there may be considerable structure that could
% be exploited, but which this code does not.
%
% The subspace rotations involved may not preserve orthogonality due
% to numerical round-off errors.  To compensate, you can set the
% "force_orth" flag, which will force orthogonality via a QR plus
% another SVD.  In a long loop, you may want to force orthogonality
% every so often.
%
% See Matthew Brand, "Fast low-rank modifications of the thin
% singular value decomposition".
%
% D. Wingate 8/17/2007
%

  current_rank = size( U, 2 );

  % P is an orthogonal basis of the column-space
  % of (I-UU')a, which is the component of "a" that is
  % orthogonal to U.
  m = U' * a;
  p = a - U*m;
  Ra = sqrt(p'*p);
  P = (1/Ra)*p;

  % XXX this has problems if a is already in the column space of U!
  % I don't know what to do in that case.
  if ( Ra < 1e-13 )
    fprintf('------> Whoa! No orthogonal component of m!\n');
  end;

  % Q is an orthogonal basis of the column-space
  % of (I-VV')b.
  n = V' * b;
  q = b - V*n;
  Rb = sqrt(q'*q);
  Q = (1/Rb)*q;

  if ( Rb < 1e-13 )
    fprintf('------> Whoa! No orthogonal component of n!\n');
  end;

  %
  % Diagonalize K, maintaining rank
  %

  % XXX note that this diagonal-plus-rank-one, so we should be able
  % to take advantage of the structure!
  z = zeros( size(m) );

  K = [ S z ; z' 0 ] + [ m; Ra ]*[ n; Rb ]';

  [tUp,tSp,tVp] = svds( K, current_rank );

  %
  % Now update our matrices!
  %

  Sp = tSp;

  Up = [ U P ] * tUp;
  Vp = [ V Q ] * tVp;

  % The above rotations may not preserve orthogonality, so we explicitly
  % deal with that via a QR plus another SVD.  In a long loop, you may
  % want to force orthogonality every so often.

  if ( force_orth )
    [UQ,UR] = qr( Up, 0 );
    [VQ,VR] = qr( Vp, 0 );
    [tUp,tSp,tVp] = svds( UR * Sp * VR', current_rank );
    Up = UQ * tUp;
    Vp = VQ * tVp;
    Sp = tSp;
  end;

return;
*/
