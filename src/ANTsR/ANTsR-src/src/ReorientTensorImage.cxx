/*=========================================================================1

  Program:   Advanced Normalization Tools
  Module:    $RCSfile: ReorientTensorImage.cxx,v $
  Language:  C++
  Date:      $Date: 2009/03/17 18:55:26 $
  Version:   $Revision: 1.2 $

  Copyright (c) ConsortiumOfANTS. All rights reserved.
  See accompanying COPYING.txt or
 http://sourceforge.net/projects/advants/files/ANTS/ANTSCopyright.txt for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/


#include <Rcpp.h>

#include "ReadWriteImage.h"
#include "itkPreservationOfPrincipalDirectionTensorReorientationImageFilter.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkWarpTensorImageMultiTransformFilter.h"
#include "itkTransformFileReader.h"
#include "itkTransformFactory.h"

typedef enum{INVALID_FILE=1, AFFINE_FILE, DEFORMATION_FILE, IMAGE_AFFINE_HEADER, IDENTITY_TRANSFORM} TRAN_FILE_TYPE;
typedef struct{
    std::string filename;
    TRAN_FILE_TYPE file_type;
    bool do_affine_inv;
} TRAN_OPT;

typedef std::vector<TRAN_OPT> TRAN_OPT_QUEUE;

static void DisplayOptQueue(const TRAN_OPT_QUEUE &opt_queue);
static void DisplayOpt(const TRAN_OPT &opt);

static TRAN_FILE_TYPE CheckFileType(const char *str){

    std::string filename = str;
    std::string::size_type pos = filename.rfind( "." );
    std::string filepre = std::string( filename, 0, pos );
    if ( pos != std::string::npos ){
        std::string extension = std::string( filename, pos, filename.length()-1);
        if (extension==std::string(".gz")){
            pos = filepre.rfind( "." );
            extension = std::string( filepre, pos, filepre.length()-1 );
        }
        if (extension==".txt") return AFFINE_FILE;
        else return DEFORMATION_FILE;
    }
    else{
        return INVALID_FILE;
    }
    return AFFINE_FILE;
}


static void FilePartsWithgz(const std::string &filename, std::string &path, std::string &name, std::string &ext){
    std::string extension;
    std::string::size_type pos = filename.rfind( "." );
    std::string filepre = std::string( filename, 0, pos );


    if ( pos != std::string::npos ){
        extension = std::string( filename, pos, filename.length()-1);
        if (extension==std::string(".gz")){
            pos = filepre.rfind( "." );
            if (pos != std::string::npos){
                extension = std::string( filepre, pos, filepre.length()-1 ) + ".gz";
                filepre = std::string(filepre, 0, pos);
            }
        }
    } else {
        extension = std::string("");
    }


    ext = extension;

    pos = filepre.rfind('/');

    if ( pos != std::string::npos ){
        path = std::string(filepre, 0, pos+1);
        name = std::string(filepre, pos+1, filepre.length()-1);
    } else {
        path = std::string("");
        name = filepre;

    }

}



static bool CheckFileExistence(const char *str){
    std::ifstream myfile(str);
    bool b = myfile.is_open();
    myfile.close();
    return b;
}

static void SetAffineInvFlag(TRAN_OPT &opt, bool &set_current_affine_inv){
    opt.do_affine_inv = set_current_affine_inv;
    if (set_current_affine_inv) set_current_affine_inv = false;
}

static bool ParseInput(int argc, char **argv, char *&moving_image_filename,
        char *&output_image_filename,
        TRAN_OPT_QUEUE &opt_queue)
{

  opt_queue.clear();
  opt_queue.reserve(argc-2);

  moving_image_filename = argv[0];
  output_image_filename = argv[1];

  int ind = 2;

  while(ind < argc)
    {

    TRAN_OPT opt;
    opt.filename = argv[ind];
    opt.file_type = CheckFileType(opt.filename.c_str());
    opt.do_affine_inv = false;
    bool set_current_affine_inv = false;

    if (strcmp(argv[ind], "-i") == 0)
      {
      Rcpp::Rcout << "ERROR - inverse transforms not yet supported\n" << std::endl;
      return false;
      }
    else
      {

      if (opt.file_type == AFFINE_FILE)
        {
        SetAffineInvFlag(opt, set_current_affine_inv);
        }
      else
        {
        if (opt.file_type == DEFORMATION_FILE && set_current_affine_inv)
          {
          Rcpp::Rcout << "Ignore inversion of non-affine file type! " << std::endl;
          Rcpp::Rcout << "opt.do_affine_inv:" << opt.do_affine_inv << std::endl;
          }
        }

      opt_queue.push_back(opt);
      DisplayOpt(opt);
      }
      ++ind;
    }
  return true;


}

void DisplayOptQueue(const TRAN_OPT_QUEUE &opt_queue){
    const int kQueueSize = opt_queue.size();
    for(int i=0; i<kQueueSize; i++){
        Rcpp::Rcout << "[" << i << "/" << kQueueSize << "]: ";
        switch(opt_queue[i].file_type){
        case AFFINE_FILE:
            Rcpp::Rcout << "AFFINE";
            break;
        case DEFORMATION_FILE:
            Rcpp::Rcout << "FIELD";
            break;
        case IDENTITY_TRANSFORM:
            Rcpp::Rcout << "IDENTITY";
            break;
        case IMAGE_AFFINE_HEADER:
            Rcpp::Rcout << "HEADER";
            break;
        default:
            Rcpp::Rcout << "Invalid Format!!!";
            break;
        }
        if (opt_queue[i].do_affine_inv) Rcpp::Rcout << "-INV";
        Rcpp::Rcout << ": " << opt_queue[i].filename << std::endl;
    }

}

void DisplayOpt(const TRAN_OPT &opt){
    switch(opt.file_type){
    case AFFINE_FILE:
        Rcpp::Rcout << "AFFINE";
        break;
    case DEFORMATION_FILE:
        Rcpp::Rcout << "FIELD";
        break;
    case IDENTITY_TRANSFORM:
        Rcpp::Rcout << "IDENTITY";
        break;
    case IMAGE_AFFINE_HEADER:
        Rcpp::Rcout << "HEADER";
        break;
    default:
        Rcpp::Rcout << "Invalid Format!!!";
        break;
    }
    if (opt.do_affine_inv) Rcpp::Rcout << "-INV";
    Rcpp::Rcout << ": " << opt.filename << std::endl;
}


template<int ImageDimension>
void ReorientTensorImage(char *moving_image_filename, char *output_image_filename, TRAN_OPT_QUEUE &opt_queue)
{


  typedef itk::DiffusionTensor3D< double >  TensorType;
  typedef itk::DiffusionTensor3D< double >  PixelType;
  typedef itk::Image<PixelType, ImageDimension> TensorImageType;
  typedef itk::Image<float, ImageDimension> ImageType;
  typedef itk::Vector<double, ImageDimension>         VectorType;
  typedef itk::Image<VectorType, ImageDimension>     DisplacementFieldType;
  typedef itk::MatrixOffsetTransformBase< double, ImageDimension, ImageDimension > AffineTransformType;
  itk::TransformFactory<AffineTransformType>::RegisterTransform();

  typedef itk::ImageFileReader<ImageType> ImageFileReaderType;
  typename TensorImageType::Pointer img_mov;

  // No reason to use log-euclidean space
  ReadTensorImage<TensorImageType>(img_mov,moving_image_filename,false);

  typename ImageType::Pointer img_ref = NULL;

  typename ImageFileReaderType::Pointer reader_img_ref = ImageFileReaderType::New();

  typedef itk::TransformFileReader TranReaderType;
  typedef itk::ImageFileReader<DisplacementFieldType> FieldReaderType;
  typename DisplacementFieldType::Pointer field=NULL;
  typename AffineTransformType::Pointer aff=NULL;

  const int kOptQueueSize = opt_queue.size();

  if (kOptQueueSize > 1)
    {
    Rcpp::Rcout << "ERROR: Only 1 input transform is permitted" << std::endl;
    return;
    }

  typedef itk::PreservationOfPrincipalDirectionTensorReorientationImageFilter<TensorImageType,DisplacementFieldType> PPDReorientType;
  typename PPDReorientType::Pointer reo = PPDReorientType::New();
  reo->SetInput( img_mov );


  const TRAN_OPT &opt = opt_queue[0];
  switch(opt.file_type)
  {

  case AFFINE_FILE:
    {
    typename TranReaderType::Pointer tran_reader = TranReaderType::New();
    tran_reader->SetFileName(opt.filename);
    tran_reader->Update();
    aff = dynamic_cast< AffineTransformType* > ((tran_reader->GetTransformList())->front().GetPointer());
    reo->SetAffineTransform( aff );

    Rcpp::Rcout << "Affine transform" << std::endl;
    break;
    }

  case DEFORMATION_FILE:
    {
    typename FieldReaderType::Pointer field_reader = FieldReaderType::New();
    field_reader->SetFileName( opt.filename );
    field_reader->Update();
    //field = field_reader->GetOutput();
    reo->SetDisplacementField( field_reader->GetOutput() );
    Rcpp::Rcout << "Warp transform" << std::endl;
    break;
    }
  default:
      Rcpp::Rcout << "Unknown file type!" << std::endl;
  }

  reo->Update();

  typename TensorImageType::Pointer img_output = reo->GetOutput();

  // No reason to use log-euclidean space here
  WriteTensorImage<TensorImageType>(img_output, output_image_filename, false);


}



RcppExport SEXP ReorientTensorImage ( SEXP r_args )
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


 if ( argc < 4 )
    { Rcpp::Rcout << "Usage: " << argv[0] << " Dimension infile.nii outfile.nii <warp.nii/affine.txt> " << std::endl;
    return Rcpp::wrap( EXIT_FAILURE ) ;
  }

  TRAN_OPT_QUEUE opt_queue;
  char *moving_image_filename = NULL;
  char *output_image_filename = NULL;

  bool is_parsing_ok = false;
  int dim = atoi(argv[1]);

  if (dim != 3)
    {
    Rcpp::Rcout << "ReorientTensorImage only supports 3D image volumes" << std::endl;
    return Rcpp::wrap( EXIT_FAILURE ) ;
    }


  is_parsing_ok = ParseInput(argc-2, argv+2, moving_image_filename, output_image_filename, opt_queue);

  if (is_parsing_ok)
    {
    Rcpp::Rcout << "moving_image_filename: " << moving_image_filename << std::endl;
    Rcpp::Rcout << "output_image_filename: " << output_image_filename << std::endl;
    DisplayOptQueue(opt_queue);

    ReorientTensorImage<3>(moving_image_filename, output_image_filename, opt_queue);

    }
  else{
    Rcpp::Rcout << "Input error!" << std::endl;
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

