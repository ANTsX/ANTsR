
#include <Rcpp.h>

#include <vector>
#include <string>
#include "itkImageFileReader.h"
#include "itkVector.h"
//#include "itkVectorImageFileReader.h"
//#include "itkVectorImageFileWriter.h"
#include "itkImageFileWriter.h"
#include "itkMatrixOffsetTransformBase.h"
#include "itkTransformFactory.h"
//#include "itkWarpImageMultiTransformFilter.h"
#include "itkDisplacementFieldFromMultiTransformFilter.h"
#include "itkTransformFileReader.h"
#include "itkTransformFileWriter.h"


typedef enum {
    INVALID_FILE = 1, AFFINE_FILE, DEFORMATION_FILE
} TRAN_FILE_TYPE;
typedef struct {
    char * filename;
    TRAN_FILE_TYPE file_type;
    bool do_affine_inv;
} TRAN_OPT;

typedef std::vector<TRAN_OPT> TRAN_OPT_QUEUE;

static TRAN_FILE_TYPE CheckFileType(char *str) {

    std::string filename = str;
    std::string::size_type pos = filename.rfind(".");
    std::string filepre = std::string(filename, 0, pos);
    if (pos != std::string::npos) {
        std::string extension = std::string(filename, pos, filename.length()
                - 1);
        if (extension == std::string(".gz")) {
            pos = filepre.rfind(".");
            extension = std::string(filepre, pos, filepre.length() - 1);
        }
        if (extension == ".txt")
            return AFFINE_FILE;
        else
            return DEFORMATION_FILE;
    } else {
        return INVALID_FILE;
    }
    return AFFINE_FILE;
}

static bool ParseInput(int argc, char **argv, char *&output_image_filename,
        char *&reference_image_filename, TRAN_OPT_QUEUE &opt_queue) {

    opt_queue.clear();
    opt_queue.reserve(argc - 2);

    output_image_filename = argv[0];

    reference_image_filename = NULL;

    int ind = 1;
    while (ind < argc) {
        if (strcmp(argv[ind], "-R") == 0) {
            ind++;
            if (ind >= argc)
                return false;
            reference_image_filename = argv[ind];
        } else if (strcmp(argv[ind], "-i") == 0) {
            ind++;
            if (ind >= argc)
                return false;
            TRAN_OPT opt;
            opt.filename = argv[ind];
            if (CheckFileType(opt.filename) != AFFINE_FILE) {
                Rcpp::Rcout << "file: " << opt.filename
                << " is not an affine .txt file. Invalid to use '-i' "
                << std::endl;
                return false;
            }
            opt.file_type = AFFINE_FILE;
            opt.do_affine_inv = true;
            opt_queue.push_back(opt);
        } else {
            TRAN_OPT opt;
            opt.filename = argv[ind];
            opt.file_type = CheckFileType(opt.filename);
            opt.do_affine_inv = false;
            opt_queue.push_back(opt);
        }
        ind++;
    }

//    if (reference_image_filename == NULL) {
//        Rcpp::Rcout << "the reference image file (-R) must be given!!!"
//        << std::endl;
//        return false;
//    }

    return true;
}

static void DisplayOptQueue(const TRAN_OPT_QUEUE &opt_queue) {
    const int kQueueSize = opt_queue.size();
    for (int i = 0; i < kQueueSize; i++) {
        Rcpp::Rcout << "[" << i << "/" << kQueueSize << "]: ";
        switch (opt_queue[i].file_type) {
        case AFFINE_FILE:
            Rcpp::Rcout << "AFFINE";
            if (opt_queue[i].do_affine_inv)
                Rcpp::Rcout << "-INV";
            break;
        case DEFORMATION_FILE:
            Rcpp::Rcout << "FIELD";
            break;
        default:
            Rcpp::Rcout << "Invalid Format!!!";
            break;
        }
        Rcpp::Rcout << ": " << opt_queue[i].filename << std::endl;
    }

}

template<int ImageDimension>
void ComposeMultiTransform(char *output_image_filename,
        char *reference_image_filename, TRAN_OPT_QUEUE &opt_queue) {

    typedef itk::Image<float, ImageDimension> ImageType;
    typedef itk::Vector<float, ImageDimension> VectorType;
    typedef itk::Image<VectorType, ImageDimension> DisplacementFieldType;
    typedef itk::MatrixOffsetTransformBase<double, ImageDimension,
    ImageDimension> AffineTransformType;
    // typedef itk::WarpImageMultiTransformFilter<ImageType,ImageType, DisplacementFieldType, AffineTransformType> WarperType;
    typedef itk::DisplacementFieldFromMultiTransformFilter<DisplacementFieldType,
    DisplacementFieldType, AffineTransformType> WarperType;

    itk::TransformFactory<AffineTransformType>::RegisterTransform();

    typedef itk::ImageFileReader<ImageType> ImageFileReaderType;
    typename ImageFileReaderType::Pointer reader_img =
        ImageFileReaderType::New();
    typename ImageType::Pointer img_ref = ImageType::New();

    typename ImageFileReaderType::Pointer reader_img_ref =
        ImageFileReaderType::New();
    if (reference_image_filename) {
        reader_img_ref->SetFileName(reference_image_filename);
        reader_img_ref->Update();
        img_ref = reader_img_ref->GetOutput();
    } else {
        Rcpp::Rcout << "the reference image file (-R) must be given!!!"
        << std::endl;
        return;
    }

    typename WarperType::Pointer warper = WarperType::New();
    // warper->SetInput(img_mov);
    // warper->SetEdgePaddingValue( 0);
    VectorType pad;
    pad.Fill(0);
    // warper->SetEdgePaddingValue(pad);


    typedef itk::TransformFileReader TranReaderType;

    typedef itk::ImageFileReader<DisplacementFieldType>
    FieldReaderType;

    const int kOptQueueSize = opt_queue.size();
    for (int i = 0; i < kOptQueueSize; i++) {
        const TRAN_OPT &opt = opt_queue[i];
        switch (opt_queue[i].file_type) {
        case AFFINE_FILE: {
            typename TranReaderType::Pointer tran_reader =
                TranReaderType::New();
            tran_reader->SetFileName(opt.filename);
            tran_reader->Update();
            typename AffineTransformType::Pointer
            aff =
                dynamic_cast<AffineTransformType*> ((tran_reader->GetTransformList())->front().GetPointer());
                if (opt_queue[i].do_affine_inv) {
                    aff->GetInverse(aff);
                }
                // Rcpp::Rcout << aff << std::endl;
                warper->PushBackAffineTransform(aff);
                break;
        }
        case DEFORMATION_FILE: {
            typename FieldReaderType::Pointer field_reader =
                FieldReaderType::New();
            field_reader->SetFileName(opt.filename);
            field_reader->Update();
            typename DisplacementFieldType::Pointer field =
                field_reader->GetOutput();
            // Rcpp::Rcout << field << std::endl;
            warper->PushBackDisplacementFieldTransform(field);
            break;
        }
        default:
            Rcpp::Rcout << "Unknown file type!" << std::endl;
        }
    }

    warper->SetOutputSize(img_ref->GetLargestPossibleRegion().GetSize());
    warper->SetOutputSpacing(img_ref->GetSpacing());
    warper->SetOutputOrigin(img_ref->GetOrigin());
    warper->SetOutputDirection(img_ref->GetDirection());

    Rcpp::Rcout << "output size: " << warper->GetOutputSize() << std::endl;
    Rcpp::Rcout << "output spacing: " << warper->GetOutputSpacing() << std::endl;

    // warper->PrintTransformList();
    warper->DetermineFirstDeformNoInterp();
    warper->Update();

    typename DisplacementFieldType::Pointer field_output =
        DisplacementFieldType::New();
    field_output = warper->GetOutput();

    std::string filePrefix = output_image_filename;
    std::string::size_type pos = filePrefix.rfind(".");
    std::string extension = std::string(filePrefix, pos, filePrefix.length()
            - 1);
    filePrefix = std::string(filePrefix, 0, pos);

    Rcpp::Rcout << "output extension is: " << extension << std::endl;

    if (extension != std::string(".mha")) {
        typedef itk::ImageFileWriter<DisplacementFieldType>
        WriterType;
        typename WriterType::Pointer writer = WriterType::New();
        writer->SetFileName(output_image_filename);
	//        writer->SetUseAvantsNamingConvention(true);
        writer->SetInput(field_output);
        writer->Update();
    } else {
        typedef itk::ImageFileWriter<DisplacementFieldType> WriterType;
        typename WriterType::Pointer writer = WriterType::New();
        writer->SetFileName(output_image_filename);
        writer->SetInput(field_output);
        writer->Update();
    }

}


template<int ImageDimension>
void ComposeMultiAffine(char *output_affine_txt,
        char *reference_affine_txt, TRAN_OPT_QUEUE &opt_queue) {

    typedef itk::Image<float, ImageDimension> ImageType;
    typedef itk::Vector<float, ImageDimension> VectorType;
    typedef itk::Image<VectorType, ImageDimension> DisplacementFieldType;
    typedef itk::MatrixOffsetTransformBase<double, ImageDimension, ImageDimension> AffineTransformType;
    typedef itk::WarpImageMultiTransformFilter<ImageType,ImageType, DisplacementFieldType, AffineTransformType> WarperType;
    // typedef itk::DisplacementFieldFromMultiTransformFilter<DisplacementFieldType,
    // DisplacementFieldType, AffineTransformType> WarperType;

    itk::TransformFactory<AffineTransformType>::RegisterTransform();

    // typedef itk::ImageFileReader<ImageType> ImageFileReaderType;
    // typename ImageFileReaderType::Pointer reader_img = ImageFileReaderType::New();
    // typename ImageType::Pointer img_ref = ImageType::New();

    // typename ImageFileReaderType::Pointer reader_img_ref = ImageFileReaderType::New();

    typename WarperType::Pointer warper = WarperType::New();
    // warper->SetInput(img_mov);
    // warper->SetEdgePaddingValue( 0);
    VectorType pad;
    pad.Fill(0);
    // warper->SetEdgePaddingValue(pad);


    typedef itk::TransformFileReader TranReaderType;

    typedef itk::ImageFileReader<DisplacementFieldType>
    FieldReaderType;

    int cnt_affine = 0;
    const int kOptQueueSize = opt_queue.size();
    for (int i = 0; i < kOptQueueSize; i++) {
        const TRAN_OPT &opt = opt_queue[i];
        switch (opt_queue[i].file_type) {
        case AFFINE_FILE: {
            typename TranReaderType::Pointer tran_reader =
                TranReaderType::New();
            tran_reader->SetFileName(opt.filename);
            tran_reader->Update();
            typename AffineTransformType::Pointer
            aff = dynamic_cast<AffineTransformType*> ((tran_reader->GetTransformList())->front().GetPointer());
            if (opt_queue[i].do_affine_inv) {
                aff->GetInverse(aff);
            }
            // Rcpp::Rcout << aff << std::endl;
            warper->PushBackAffineTransform(aff);
            cnt_affine++;
            break;
        }
        case DEFORMATION_FILE: {
            Rcpp::Rcout << "Compose affine only files: ignore "
            << opt.filename << std::endl;
            break;
        }
        default:
            Rcpp::Rcout << "Unknown file type!" << std::endl;
        }
    }

    typedef typename AffineTransformType::CenterType PointType;
    PointType aff_center;

    typename AffineTransformType::Pointer aff_ref_tmp;
    if (reference_affine_txt) {
        typename TranReaderType::Pointer tran_reader = TranReaderType::New();
        tran_reader->SetFileName(reference_affine_txt);
        tran_reader->Update();
        aff_ref_tmp = dynamic_cast<AffineTransformType*> ((tran_reader->GetTransformList())->front().GetPointer());

    } else {
        if (cnt_affine > 0) {
            Rcpp::Rcout << "the reference affine file for center is selected as the first affine!" << std::endl;
            aff_ref_tmp = ((warper->GetTransformList()).begin())->second.aex.aff;
        }
        else {
            Rcpp::Rcout << "No affine input is given. nothing to do ......" << std::endl;
            return;
        }
    }


    aff_center = aff_ref_tmp->GetCenter();
    Rcpp::Rcout << "new center is : " << aff_center << std::endl;

    // warper->PrintTransformList();

    // typename AffineTransformType::Pointer aff_output = warper->ComposeAffineOnlySequence(aff_center);
    typename AffineTransformType::Pointer aff_output = AffineTransformType::New();
    warper->ComposeAffineOnlySequence(aff_center, aff_output);
    typedef itk::TransformFileWriter TranWriterType;
    typename TranWriterType::Pointer tran_writer = TranWriterType::New();
    tran_writer->SetFileName(output_affine_txt);
    tran_writer->SetInput(aff_output);
    tran_writer->Update();

    Rcpp::Rcout << "wrote file to : " << output_affine_txt << std::endl;



}

RcppExport SEXP ComposeMultiTransform( SEXP r_args )
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

    if (argc <= 3) {
        Rcpp::Rcout
        << "ComposeMultiTransform ImageDimension output_field [-R reference_image] "
        << "{[deformation_field | [-i] affine_transform_txt ]}"
        << std::endl;
	  Rcpp::Rcout <<"  Usage has the same form as WarpImageMultiTransform " << std::endl;
	Rcpp::Rcout <<" For Example: " << std::endl;
	Rcpp::Rcout << std::endl;
	Rcpp::Rcout <<   argv[0]  << " Dimension  outwarp.nii   -R template.nii   ExistingWarp.nii  ExistingAffine.nii " << std::endl;
	Rcpp::Rcout << " or for an inverse mapping : " << std::endl;
	Rcpp::Rcout << argv[0]  << " Dimension  outwarp.nii   -R template.nii   -i ExistingAffine.nii ExistingInverseWarp.nii " << std::endl;
	Rcpp::Rcout <<" recalling that the -i option takes the inverse of the affine mapping " << std::endl;
	Rcpp::Rcout << std::endl;
	Rcpp::Rcout << "Or: to compose multiple affine text file into one: "        << std::endl;
 Rcpp::Rcout 	 << "ComposeMultiTransform ImageDimension output_affine_txt [-R reference_affine_txt] "
        << "{[-i] affine_transform_txt}" << std::endl
        << "This will be evoked if a text file is given as the second parameter. In this case "
        << "reference_affine_txt is used to define the center of the output affine.  "
        << "The default reference is the first given affine text file. "
        << "This ignores all non-txt files among the following parameters."
        << std::endl;
        return Rcpp::wrap( EXIT_SUCCESS ) ;
    }

    TRAN_OPT_QUEUE opt_queue;
    //    char *moving_image_filename = NULL;
    char *output_image_filename = NULL;
    char *reference_image_filename = NULL;

    bool is_parsing_ok = false;
    int kImageDim = atoi(argv[1]);

    is_parsing_ok = ParseInput(argc - 2, argv + 2, output_image_filename,
            reference_image_filename, opt_queue);

    if (is_parsing_ok) {

        switch (CheckFileType(output_image_filename)) {
        case DEFORMATION_FILE: {

            if (reference_image_filename == NULL) {
                Rcpp::Rcout << "the reference image file (-R) must be given!!!"
                << std::endl;
                return Rcpp::wrap( EXIT_FAILURE ) ;
            }


            Rcpp::Rcout << "output_image_filename: " << output_image_filename
            << std::endl;
            Rcpp::Rcout << "reference_image_filename: ";
            if (reference_image_filename)
                Rcpp::Rcout << reference_image_filename << std::endl;
            else
                Rcpp::Rcout << "NULL" << std::endl;
            DisplayOptQueue(opt_queue);

            switch (kImageDim) {
            case 2: {
                ComposeMultiTransform<2> (output_image_filename,
                        reference_image_filename, opt_queue);
                break;
            }
            case 3: {
                ComposeMultiTransform<3> (output_image_filename,
                        reference_image_filename, opt_queue);
                break;
            }
            }
            break;
        }

        case AFFINE_FILE: {
            Rcpp::Rcout << "output_affine_txt: " << output_image_filename
            << std::endl;
            Rcpp::Rcout << "reference_affine_txt: ";
            if (reference_image_filename)
                Rcpp::Rcout << reference_image_filename << std::endl;
            else
                Rcpp::Rcout << "NULL" << std::endl;
            DisplayOptQueue(opt_queue);

            switch (kImageDim) {
            case 2: {
                ComposeMultiAffine<2> (output_image_filename,
                        reference_image_filename, opt_queue);
                break;
            }
            case 3: {
                ComposeMultiAffine<3> (output_image_filename,
                        reference_image_filename, opt_queue);
                break;
            }
            }
            break;
        }

        default:
            Rcpp::Rcout << "Unknow output file format: " << output_image_filename << std::endl;
            break;

        }

    } else {
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

