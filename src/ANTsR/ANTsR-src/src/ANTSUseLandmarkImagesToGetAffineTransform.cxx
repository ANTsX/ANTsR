
#include <Rcpp.h>

/** ANTS Landmarks used to initialize an affine transform ... */

#include "itkLandmarkBasedTransformInitializer.h"
#include "itkImage.h"
#include "itkImageIOBase.h"
#include "itkImageIOFactory.h"
#include <math.h>
#include <iostream>
#include "ReadWriteImage.h"
#include "itkTransformFileWriter.h"
// #include "ANTS_affine_registration2.h"
#include <vnl/vnl_matrix.h>
// #include <vnl/vnl_qr.h>
#include "vnl/algo/vnl_qr.h"

template<class TransformAPointer, class StringType>
void DumpTransformForANTS3D(const TransformAPointer &transform, StringType &ANTS_prefix);


template<class PointContainerType, class TransformPointerType>
void GetAffineTransformFromTwoPointSets3D(PointContainerType &fixedLandmarks, PointContainerType &movingLandmarks, TransformPointerType &transform);

//
// The test specifies a bunch of fixed and moving landmarks and test if the 
// fixed landmarks after transform by the computed transform coincides
// with the moving landmarks....

int LandmarkBasedTransformInitializer3D(int, char * argv[])
{

    typedef  float  PixelType;
    const unsigned int Dimension = 3;
    typedef itk::Image< PixelType, Dimension >  FixedImageType;
    typedef itk::Image< PixelType, Dimension >  MovingImageType;
    typedef itk::Image< PixelType, Dimension >  ImageType;
    typedef itk::ImageRegionIteratorWithIndex<ImageType> Iterator;
    ImageType::Pointer fixedimage;
    ImageType::Pointer movingimage;
    ReadImage<ImageType>(fixedimage,argv[1]);
    ReadImage<ImageType>(movingimage,argv[2]);

    bool bRigid = (strcmp(argv[3], "rigid")==0);


    /** get all of the relevant labels in the fixed image and moving image */
    typedef std::vector<PixelType>                  LabelSetType;
    LabelSetType myFixLabelSet;
    LabelSetType myMovLabelSet;
    /** count the labels in the image */
    Iterator It ( fixedimage,fixedimage->GetLargestPossibleRegion() );
    for( It.GoToBegin(); !It.IsAtEnd(); ++It )
    {
        PixelType label = It.Get();
        if( fabs(label) > 0 )
        {
            if( find( myFixLabelSet.begin(), myFixLabelSet.end(), label )
                    == myFixLabelSet.end() )
            {
                //	      Rcpp::Rcout <<" f-label " << label << std::endl;
                myFixLabelSet.push_back( label );
            }
        }
    }
    Iterator ItM ( movingimage,movingimage->GetLargestPossibleRegion() );
    for( ItM.GoToBegin(); !ItM.IsAtEnd(); ++ItM )
    {
        PixelType label = ItM.Get();
        if( fabs(label) > 0 )
        {
            if( find( myMovLabelSet.begin(), myMovLabelSet.end(), label )
                    == myMovLabelSet.end() )
            {
                //	      Rcpp::Rcout <<" m-label " << label << std::endl;
                myMovLabelSet.push_back( label );
            }
        }
    }

    std::sort(myFixLabelSet.begin(),myFixLabelSet.end());
    std::sort(myMovLabelSet.begin(),myMovLabelSet.end());

    LabelSetType::const_iterator fit;
    LabelSetType::const_iterator mit=myMovLabelSet.begin();
    for( fit = myFixLabelSet.begin(); fit != myFixLabelSet.end(); ++fit )
    {
        float fixlabel= *fit ;
        float movlabel= *mit ;
        Rcpp::Rcout << " fix-label " << fixlabel << " movlabel " << movlabel << std::endl;
        if ( movlabel != fixlabel )
        {
            Rcpp::Rcout <<" labels do not match -- exiting " << std::endl;
            throw std::exception() ;
        }
        ++mit;
    }

    // Set the transform type..
    typedef itk::VersorRigid3DTransform< double > TransformType;
    TransformType::Pointer transform = TransformType::New();
    typedef itk::LandmarkBasedTransformInitializer< TransformType,
    FixedImageType, MovingImageType > TransformInitializerType;
    TransformInitializerType::Pointer initializer = TransformInitializerType::New();

    // Set fixed and moving landmarks
    TransformInitializerType::LandmarkPointContainer fixedLandmarks;
    TransformInitializerType::LandmarkPointContainer movingLandmarks;
    TransformInitializerType::LandmarkPointType point;
    TransformInitializerType::LandmarkPointType tmp;


    // compute the CoM's of all the landmarks
    ImageType::SpacingType spacing=fixedimage->GetSpacing();
    for( fit = myFixLabelSet.begin(); fit != myFixLabelSet.end(); ++fit )
    {
        float currentlabel= *fit ;
        float totalct=0;
        TransformInitializerType::LandmarkPointType myCenterOfMass;
        myCenterOfMass.Fill(0);
        for( It.GoToBegin(); !It.IsAtEnd(); ++It )
        {
            PixelType label = It.Get();
            if(  label == currentlabel  )
            {
                totalct++;
                // compute center of mass
                ImageType::PointType point;
                fixedimage->TransformIndexToPhysicalPoint(It.GetIndex(), point);
                for (unsigned int i=0; i<spacing.Size(); i++) myCenterOfMass[i]+=point[i];
                //Rcpp::Rcout << " point " << point << std::endl;
            }
        }

        for (unsigned int i=0; i<spacing.Size(); i++) myCenterOfMass[i]/=(float)totalct;
        // Rcpp::Rcout << " pushing-fix " <<  myCenterOfMass << std::endl;
        fixedLandmarks.push_back( myCenterOfMass );

    }

    // compute the CoM's of all the landmarks
    spacing=movingimage->GetSpacing();
    for( mit = myMovLabelSet.begin(); mit != myMovLabelSet.end(); ++mit )
    {
        float currentlabel= *mit ;
        float totalct=0;
        TransformInitializerType::LandmarkPointType myCenterOfMass;
        myCenterOfMass.Fill(0);
        for( ItM.GoToBegin(); !ItM.IsAtEnd(); ++ItM )
        {
            PixelType label = ItM.Get();
            if(  label == currentlabel  )
            {
                totalct++;
                // compute center of mass
                ImageType::PointType point;
                movingimage->TransformIndexToPhysicalPoint(ItM.GetIndex(), point);
                for (unsigned int i=0; i<spacing.Size(); i++) myCenterOfMass[i]+=point[i];
            }
        }

        for (unsigned int i=0; i<spacing.Size(); i++) myCenterOfMass[i]/=(float)totalct;
        //    Rcpp::Rcout << " pushing-mov " <<  myCenterOfMass << std::endl;
        movingLandmarks.push_back( myCenterOfMass );

    }

    TransformInitializerType::PointsContainerConstIterator 
    fitr = fixedLandmarks.begin();
    TransformInitializerType::PointsContainerConstIterator 
    mitr = movingLandmarks.begin();
    while( mitr != movingLandmarks.end() )
    {
        Rcpp::Rcout << "  Fixed Landmark: " << *fitr << " Moving landmark " << *mitr << std::endl;
        ++fitr;
        ++mitr;
    }


    initializer->SetFixedLandmarks(fixedLandmarks);
    initializer->SetMovingLandmarks(movingLandmarks);

    //  initializer->SetFixedImage( fixedimage );
    //  initializer->SetMovingImage( movingimage );
    initializer->SetTransform( transform );
    initializer->InitializeTransform();

    transform->Print(Rcpp::Rcout);
    // to transform a point 
    //         transform->TransformPoint( *fitr ) << std::endl;


    // transform the transform to ANTS format
    std::string ANTS_prefix(argv[4]);


    typedef itk::MatrixOffsetTransformBase< double, 3, 3> AffineTransformType;
    AffineTransformType::Pointer aff = AffineTransformType::New();
    GetAffineTransformFromTwoPointSets3D(fixedLandmarks, movingLandmarks, aff);
    Rcpp::Rcout << "affine:" << aff;


    if (bRigid)
        DumpTransformForANTS3D(transform, ANTS_prefix);
    else
        DumpTransformForANTS3D(aff, ANTS_prefix);


    return EXIT_SUCCESS;
}
//////////
// x: fixedLandmarks
// y: movingLandmarks
// (A,t,c) : affine transform, A:3*3, t: 3*1 c: 3*1 (c is the center of all points in x)
// y-c = A*(x-c) + t;
// steps:
// 1. c = average of points of x
// 2. let y1 = y-c; x1 = x - c; x11 = [x1; 1 ... 1] // extend x11
// 3. minimize (y1-A1*x11)^2, A1 is a 3*4 matrix
// 4. A = A1(1:3, 1:3), t = A1(1:3, 4);
// step 3:
//   A11 = (y1*x11')*(x11*x11')^(-1)
// type info:
//   assume PointContainerType is std::vector
//   assume TrnasformPointerType is MatrixOffsetTransformBase

template<class PointContainerType, class TransformPointerType>
void GetAffineTransformFromTwoPointSets3D(PointContainerType &fixedLandmarks, PointContainerType &movingLandmarks, TransformPointerType &transform){

    const int Dim=3;
    int n = fixedLandmarks.size();
    vnl_matrix<double> y(Dim, n), x(Dim, n);

    for(int i=0; i<n; i++){
        for(int j=0;j<Dim;j++){
            x(j,i)=fixedLandmarks[i][j];
            y(j,i)=movingLandmarks[i][j];
        }
    }

    vnl_vector<double> c(Dim);
    for(int j=0; j<Dim; j++) c[j] = x.get_row(j).mean();

    vnl_matrix<double> y1(Dim, n), x11(Dim+1, n);
    for(int i=0; i<n; i++){
        y1.set_column(i, y.get_column(i)-c);

        vnl_vector<double> x_tmp(Dim), x1_tmp(Dim+1);
        x_tmp = x.get_column(i)-c;
        for(int j=0; j<Dim; j++) x1_tmp[j]=x_tmp[j];
        x1_tmp[Dim]=1;

        x11.set_column(i, x1_tmp);
    }

    vnl_matrix<double> A11(Dim, Dim+1);
    vnl_matrix<double> x11t = x11.transpose();
    //vnl_matrix_inverse<double> tmp(x11 * x11t); // BA -- removed this -- not used?

    vnl_svd<double> qr( x11t ); // can use vnl_qr
    A11 = qr.inverse() * (y1.transpose());
    A11 = A11.transpose();






    vnl_matrix<double> A(Dim,Dim);
    A = A11.extract(Dim, Dim, 0, 0);

    Rcpp::Rcout << "y=" << y << std::endl;
    Rcpp::Rcout << "x=" << x << std::endl;

    Rcpp::Rcout << "y1=" << y1 << std::endl;
    Rcpp::Rcout << "x11=" << x11 << std::endl;
    Rcpp::Rcout << "A11=" << A11 << std::endl;

    vnl_vector<double> t = A11.get_column(Dim);


    typedef typename TransformPointerType::ObjectType TransformType;

    typedef typename TransformType::InputPointType PointType;
    typedef typename TransformType::OutputVectorType VectorType;
    typedef typename TransformType::MatrixType MatrixType;

    PointType center;
    for(int i=0;i<Dim;i++) center[i]=c[i];

    VectorType translation;
    for(int i=0;i<Dim;i++) translation[i]=t[i];

    MatrixType matrix(A);

    transform->SetCenter(center);
    transform->SetTranslation(translation);
    transform->SetMatrix(matrix);

    return;
}

////////////////////////////////////////////////////////////////////////
//Stripped from ANTS_affine_registration2.h
template<class TransformPointerType, class StringType>
void WriteAffineTransformFile(TransformPointerType &transform, StringType filename){


    itk::TransformFileWriter::Pointer transform_writer;
    transform_writer = itk::TransformFileWriter::New();
    transform_writer->SetFileName(filename);
    transform_writer->SetInput(transform);

    try{
        transform_writer->Update();
    }
    catch( itk::ExceptionObject &err){
        Rcpp::Rcout << "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!" << std::endl
        <<"Exception in writing tranform file: " << std::endl
        << filename << std::endl;
        return;
    }


    return;
}

////////////////////////////////////////////////////////////////////////
//Stripped from ANTS_affine_registration2.h
template<class RunningAffineTransformPointerType, class AffineTransformPointerType>
inline void PostConversionInAffine(RunningAffineTransformPointerType& transform_running, AffineTransformPointerType &transform){

    typedef typename RunningAffineTransformPointerType::ObjectType RunningAffineTransformType;
    typedef typename AffineTransformPointerType::ObjectType AffineTransformType;

    transform->SetCenter(*(reinterpret_cast<typename AffineTransformType::InputPointType *>
    (const_cast<typename RunningAffineTransformType::InputPointType*> (&(transform_running->GetCenter())))));
    transform->SetTranslation(*(reinterpret_cast<typename AffineTransformType::OutputVectorType *>
    (const_cast<typename RunningAffineTransformType::OutputVectorType*> (&(transform_running->GetTranslation())))));
    transform->SetMatrix(*(reinterpret_cast<typename AffineTransformType::MatrixType *>
    (const_cast<typename RunningAffineTransformType::MatrixType*> (&(transform_running->GetMatrix())))));


    // Rcpp::Rcout << "transform_running" << transform_running << std::endl;
    // Rcpp::Rcout << "transform" << transform << std::endl;
}

template<class TransformAPointer, class StringType>
void DumpTransformForANTS3D(const TransformAPointer &transform, StringType &ANTS_prefix){
    const int ImageDimension = 3;

    // ANTS transform file type
    typedef itk::MatrixOffsetTransformBase< double, ImageDimension, ImageDimension > AffineTransformType;
    AffineTransformType::Pointer transform_ANTS = AffineTransformType::New();

    //    typedef TransformAPointer::ObjectType TransformA;

    // Rcpp::Rcout << " writing " << ANTS_prefix << " affine " << std::endl;
    // std::string ANTS_affine_filename = ANTS_prefix + std::string( "Affine.txt" );

    std::string ANTS_affine_filename = ANTS_prefix;

    Rcpp::Rcout << " writing ANTS affine file:" << ANTS_affine_filename << std::endl;
    PostConversionInAffine(transform, transform_ANTS);
    WriteAffineTransformFile(transform_ANTS, ANTS_affine_filename);

}




int LandmarkBasedTransformInitializer2D(int, char * [])
{
    Rcpp::Rcout << " not implemented " << std::endl;
    return 1;

    /*
  typedef  float PixelType;
  const unsigned int Dimension = 2;
  typedef itk::Image< PixelType, Dimension >  FixedImageType;
  typedef itk::Image< PixelType, Dimension >  MovingImageType;
  typedef itk::Image< PixelType, Dimension >  ImageType;
  typename FixedImageType::Pointer fixedimage;
  typename MovingImageType::Pointer movingimage;
  ReadImage<ImageType>(fixedimage,argv[1]);
  ReadImage<ImageType>(movingimage,argv[2]);

  // Set the transform type..
  typedef itk::Rigid2DTransform< double > TransformType;
     */

    return EXIT_SUCCESS;
}



RcppExport SEXP ANTSUseLandmarkImagesToGetAffineTransform( SEXP r_args )
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
        Rcpp::Rcout << "Usage:   " << argv[0] << " FixedImageWithLabeledLandmarks.nii.gz  MovingImageWithLabeledLandmarks.nii.gz [rigid | affine] OutAffine.txt " << std::endl;
        Rcpp::Rcout << " we expect the input images to be (1) N-ary  (2) in the same physical space as the images you want to " << std::endl;
        Rcpp::Rcout << " register and (3 ) to have the same landmark points defined within them ... " << std::endl;
        Rcpp::Rcout << " landmarks will be defined from the center of mass of the labels in the input images . " << std::endl;
        Rcpp::Rcout << " You can use ITK-snap to generate the label images. " << std::endl;
        return Rcpp::wrap( EXIT_FAILURE ) ;
    }

    // Get the image dimension
    std::string fn = std::string(argv[1]);
    itk::ImageIOBase::Pointer imageIO =
            itk::ImageIOFactory::CreateImageIO(fn.c_str(), itk::ImageIOFactory::ReadMode);
    imageIO->SetFileName(fn.c_str());
    imageIO->ReadImageInformation();

    switch ( imageIO->GetNumberOfDimensions() )
    {
    case 2:
        LandmarkBasedTransformInitializer2D(argc,argv);
        break;
    case 3:
        LandmarkBasedTransformInitializer3D(argc,argv);
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

