
#include <Rcpp.h>

#include <vector>
#include <string>
#include "itkImageFileReader.h"
#include "itkVector.h"
#include "itkVariableLengthVector.h"
//#include "itkVectorImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkMatrixOffsetTransformBase.h"
#include "itkTransformFactory.h"
#include "itkWarpTensorImageMultiTransformFilter.h"
#include "itkTransformFileReader.h"
#include "itkVectorNearestNeighborInterpolateImageFunction.h"
#include "ReadWriteImage.h"
#include "itkWarpImageMultiTransformFilter.h"
#include "itkExpTensorImageFilter.h"

typedef enum{INVALID_FILE=1, AFFINE_FILE, DEFORMATION_FILE, IMAGE_AFFINE_HEADER, IDENTITY_TRANSFORM} TRAN_FILE_TYPE;
typedef struct{
    //    char *filename;
    std::string filename;
    TRAN_FILE_TYPE file_type;
    bool do_affine_inv;

    //    void SetValue(char *filename, TRAN_FILE_TYPE file_type, bool do_affine_inv){
    //        this.filename = filename;
    //        this.file_type = file_type;
    //        this.do_affine_inv = do_affine_inv;
    //    };

} TRAN_OPT;

typedef std::vector<TRAN_OPT> TRAN_OPT_QUEUE;

typedef struct{
    bool use_NN_interpolator;
    bool use_TightestBoundingBox;
    char * reference_image_filename;
    bool use_RotationHeader;
} MISC_OPT;

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

//    Rcpp::Rcout << "filename: " << filename << std::endl
//    << "path: " << path << std::endl
//    << "name: " << name << std::endl
//    << "ext: " << ext << std::endl;

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
        TRAN_OPT_QUEUE &opt_queue, MISC_OPT &misc_opt){

    opt_queue.clear();
    opt_queue.reserve(argc-2);

    misc_opt.reference_image_filename = NULL;
    misc_opt.use_NN_interpolator = false;
    misc_opt.use_TightestBoundingBox = false;
    misc_opt.use_RotationHeader = false;

    moving_image_filename = argv[0];
    output_image_filename = argv[1];

    int ind = 2;
    bool set_current_affine_inv = false;

    while(ind < argc){



        if (strcmp(argv[ind], "--use-NN")==0) {
            misc_opt.use_NN_interpolator = true;
        }
        else if (strcmp(argv[ind], "-R")==0) {
            ind++; if(ind >= argc) return false;
            misc_opt.reference_image_filename = argv[ind];
        }
        else if ((strcmp(argv[ind], "--tightest-bounding-box")==0) &&  (strcmp(argv[ind], "-R")!=0)  ) {
            misc_opt.use_TightestBoundingBox = true;
        }
        else if (strcmp(argv[ind], "--reslice-by-header")==0) {
            misc_opt.use_RotationHeader = true;
            TRAN_OPT opt;
            opt.file_type = IMAGE_AFFINE_HEADER;
            opt.do_affine_inv = false;
            opt_queue.push_back(opt);
        }
        else if (strcmp(argv[ind], "--Id")==0) {
            TRAN_OPT opt;
            opt.filename = "--Id";
            opt.do_affine_inv = false;
            opt.file_type = IDENTITY_TRANSFORM;
            opt_queue.push_back(opt);
        }
        else if (strcmp(argv[ind], "--moving-image-header")==0 || strcmp(argv[ind], "-mh") ==0){
            TRAN_OPT opt;
            opt.file_type = IMAGE_AFFINE_HEADER;
            opt.filename = moving_image_filename;
            //            opt.do_affine_inv = false;
            SetAffineInvFlag(opt, set_current_affine_inv);
            opt_queue.push_back(opt);
        }
        else if (strcmp(argv[ind], "--reference-image-header")==0 || strcmp(argv[ind], "-rh") ==0){
            if (misc_opt.reference_image_filename==NULL){
                Rcpp::Rcout << "reference image filename is not given yet. Specify it with -R before --reference-image-header / -rh." << std::endl;
                return false;
            }

            TRAN_OPT opt;
            opt.file_type = IMAGE_AFFINE_HEADER;
            opt.filename = misc_opt.reference_image_filename;
            //            opt.do_affine_inv = false;
            SetAffineInvFlag(opt, set_current_affine_inv);
            opt_queue.push_back(opt);
        }
        else if (strcmp(argv[ind], "-i")==0) {
            set_current_affine_inv = true;
        }

        else if (strcmp(argv[ind], "--ANTS-prefix")==0){
            ind++;
            std::string prefix = argv[ind];
            std::string path, name, ext;
            FilePartsWithgz(prefix, path, name, ext);
            if (ext=="") ext=".nii.gz";

            std::string deform_file_name, x_deform_name;
            deform_file_name = path+name+std::string("Warp")+ext;
            x_deform_name = path+name+std::string("Warpxvec")+ext;
            if (CheckFileExistence(x_deform_name.c_str())){
                TRAN_OPT opt;
                opt.filename = deform_file_name.c_str();
                opt.file_type = CheckFileType(opt.filename.c_str());
                opt.do_affine_inv = false;
                opt_queue.push_back(opt);
                Rcpp::Rcout << "found deformation file: " << opt.filename << std::endl;
                DisplayOpt(opt);
            }

            std::string affine_file_name;
            affine_file_name = path+name+std::string("Affine.txt");
            if (CheckFileExistence(affine_file_name.c_str())){
                TRAN_OPT opt;
                opt.filename = affine_file_name.c_str();
                opt.file_type = CheckFileType(opt.filename.c_str());
                opt.do_affine_inv = false;
                opt_queue.push_back(opt);
                Rcpp::Rcout << "found affine file: " << opt.filename << std::endl;
                DisplayOpt(opt);
            }

        }
        else if (strcmp(argv[ind], "--ANTS-prefix-invert")==0){
            ind++;
            std::string prefix = argv[ind];
            std::string path, name, ext;
            FilePartsWithgz(prefix, path, name, ext);
            if (ext=="") ext=".nii.gz";

            std::string affine_file_name;
            affine_file_name = path+name+std::string("Affine.txt");
            if (CheckFileExistence(affine_file_name.c_str())){
                TRAN_OPT opt;
                opt.filename = affine_file_name.c_str();
                opt.file_type = CheckFileType(opt.filename.c_str());
                opt.do_affine_inv = true;
                opt_queue.push_back(opt);
                Rcpp::Rcout << "found affine file: " << opt.filename << std::endl;
                DisplayOpt(opt);
            }



            std::string deform_file_name, x_deform_name;
            deform_file_name = path+name+std::string("InverseWarp.nii.gz");
            x_deform_name = path+name+std::string("InverseWarpxvec.nii.gz");
            if (CheckFileExistence(x_deform_name.c_str())){
                TRAN_OPT opt;
                opt.filename = deform_file_name.c_str();
                opt.file_type = CheckFileType(opt.filename.c_str());
                opt.do_affine_inv = false;
                opt_queue.push_back(opt);
                Rcpp::Rcout << "found deformation file: " << opt.filename << std::endl;
                DisplayOpt(opt);
            }
        }
        else{
            TRAN_OPT opt;
            opt.filename = argv[ind];
            opt.file_type = CheckFileType(opt.filename.c_str());
            opt.do_affine_inv = false;
            if (opt.file_type == AFFINE_FILE)
                SetAffineInvFlag(opt, set_current_affine_inv);
            else if (opt.file_type == DEFORMATION_FILE && set_current_affine_inv){
                Rcpp::Rcout << "Ignore inversion of non-affine file type! " << std::endl;
                Rcpp::Rcout << "opt.do_affine_inv:" << opt.do_affine_inv << std::endl;
            }

            opt_queue.push_back(opt);
            DisplayOpt(opt);
        }
        ind++;
    }

    if (misc_opt.use_RotationHeader) {
        //                if (misc_opt.reference_image_filename) {
        //                    opt_queue[0].filename = misc_opt.reference_image_filename;
        //                } else {
        opt_queue[0].filename = "--Id";
        opt_queue[0].file_type = IDENTITY_TRANSFORM;
        opt_queue[0].do_affine_inv = false;
        //                }


        //               TRAN_OPT opt;
        //               opt.file_type = IMAGE_AFFINE_HEADER;
        //               opt.filename = moving_image_filename;
        //               opt.do_affine_inv = true;
        //               opt_queue.push_back(opt);
        //
        //               Rcpp::Rcout << "Use Rotation Header!" << std::endl;

    }

    return true;
}

static void DisplayOptQueue(const TRAN_OPT_QUEUE &opt_queue){
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

static void DisplayOpt(const TRAN_OPT &opt){
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

template<class AffineTransformPointer>
void GetIdentityTransform(AffineTransformPointer &aff){
    typedef typename AffineTransformPointer::ObjectType AffineTransform;
    aff = AffineTransform::New();
    aff->SetIdentity();
}


template<class ImageTypePointer, class AffineTransformPointer>
void GetAffineTransformFromImage(const ImageTypePointer& img, AffineTransformPointer &aff){
    typedef typename ImageTypePointer::ObjectType ImageType;
    typedef typename ImageType::DirectionType DirectionType;
    typedef typename ImageType::PointType PointType;
    typedef typename ImageType::SpacingType SpacingType;
    typedef typename AffineTransformPointer::ObjectType::TranslationType VectorType;

    DirectionType direction = img->GetDirection();

    SpacingType spacing = img->GetSpacing();
    VectorType translation;
    // translation.Fill(0);
    for(unsigned int i=0; i<ImageType::GetImageDimension(); i++) translation[i]=img->GetOrigin()[i];

    aff->SetMatrix(direction);
    // aff->SetCenter(pt);
    PointType pt; pt.Fill(0);
    aff->SetOffset(translation);
    aff->SetCenter(pt);


    Rcpp::Rcout << "aff from image:" << aff << std::endl;

}


template<class WarperPointerType, class ImagePointerType, class SizeType, class PointType>
void GetLaregstSizeAfterWarp(WarperPointerType &warper, ImagePointerType &img, SizeType &largest_size, PointType &origin_warped){
    typedef typename ImagePointerType::ObjectType ImageType;
    const int ImageDimension = ImageType::GetImageDimension();

    // typedef typename ImageType::PointType PointType;
    typedef typename std::vector<PointType> PointList;

    typedef typename ImageType::IndexType IndexType;

    // PointList pts_orig;
    PointList pts_warped;

    typename ImageType::SizeType imgsz;
    imgsz = img->GetLargestPossibleRegion().GetSize();

    typename ImageType::SpacingType spacing;
    spacing = img->GetSpacing();

    pts_warped.clear();
    if (ImageDimension == 3){

        for(int i=0; i<8; i++){
            IndexType ind;
            switch (i){
            case 0: ind[0]=0; ind[1]=0; ind[2]=0; break;
            case 1: ind[0]=imgsz[0]-1; ind[1]=0; ind[2]=0; break;
            case 2: ind[0]=0; ind[1]=imgsz[1]-1; ind[2]=0; break;
            case 3: ind[0]=imgsz[0]-1; ind[1]=imgsz[1]-1; ind[2]=0; break;
            case 4: ind[0]=0; ind[1]=0; ind[2]=imgsz[2]-1; break;
            case 5: ind[0]=imgsz[0]-1; ind[1]=0; ind[2]=imgsz[2]-1; break;
            case 6: ind[0]=0; ind[1]=imgsz[1]-1; ind[2]=imgsz[2]-1; break;
            case 7: ind[0]=imgsz[0]-1; ind[1]=imgsz[1]-1; ind[2]=imgsz[2]-1; break;
            }
            PointType pt_orig, pt_warped;
            img->TransformIndexToPhysicalPoint(ind, pt_orig);
            if (warper->MultiInverseAffineOnlySinglePoint(pt_orig, pt_warped)==false){
                Rcpp::Rcout << "ERROR: outside of numeric boundary with affine transform." << std::endl;
                throw std::exception() ;
            }
            pts_warped.push_back(pt_warped);
            Rcpp::Rcout << '[' << i << ']' << ind << ',' << pt_orig << "->" << pt_warped << std::endl;
        }
    }
    else if (ImageDimension == 2) {
        for(int i=0; i<4; i++){
            IndexType ind;
            switch (i){
            case 0: ind[0]=0; ind[1]=0;  break;
            case 1: ind[0]=imgsz[0]-1; ind[1]=0;  break;
            case 2: ind[0]=0; ind[1]=imgsz[1]-1;  break;
            case 3: ind[0]=imgsz[0]-1; ind[1]=imgsz[1]-1;  break;
            }
            PointType pt_orig, pt_warped;
            img->TransformIndexToPhysicalPoint(ind, pt_orig);
            if (warper->MultiInverseAffineOnlySinglePoint(pt_orig, pt_warped)==false){
                Rcpp::Rcout << "ERROR: outside of numeric boundary with affine transform." << std::endl;
                throw std::exception() ;
            }
            pts_warped.push_back(pt_warped);
            Rcpp::Rcout << '[' << i << ']' << ind << ',' << pt_orig << "->" << pt_warped << std::endl;
        }

    }
    else {
        Rcpp::Rcout << "could not determine the dimension after warping for non 2D/3D volumes" << std::endl;
        throw std::exception() ;
    }

    PointType pt_min, pt_max;
    pt_min = pts_warped[0];
    pt_max = pts_warped[0];
    for(unsigned int k=0; k<pts_warped.size(); k++){
        for(int i=0; i<ImageDimension; i++){
            pt_min[i] = (pt_min[i]<pts_warped[k][i])?(pt_min[i]):(pts_warped[k][i]);
            pt_max[i] = (pt_max[i]>pts_warped[k][i])?(pt_max[i]):(pts_warped[k][i]);
        }
    }

    for(int i=0; i<ImageDimension; i++){
        largest_size[i] = (int) (ceil((pt_max[i]-pt_min[i]) / spacing[i])+1);
    }

    origin_warped = pt_min;
    Rcpp::Rcout << "origin_warped: " << origin_warped << std::endl;
    Rcpp::Rcout << "pt_min: " << pt_min << " pt_max:" << pt_max << " largest_size:" << largest_size << std::endl;


}


template<typename TensorImageType>
void
DirectionCorrect( typename TensorImageType::Pointer img_out, typename TensorImageType::Pointer img_mov )
{
  itk::ImageRegionIteratorWithIndex<TensorImageType> it(img_out, img_out->GetLargestPossibleRegion() );

  typename TensorImageType::DirectionType directionSwap = img_out->GetDirection() * img_mov->GetDirection();

  while( !it.IsAtEnd() )
    {
    typename TensorImageType::DirectionType::InternalMatrixType  dt;
    dt(0,0) = it.Value()[0];
    dt(0,1) = dt(1,0) = it.Value()[1];
    dt(0,2) = dt(2,0) = it.Value()[2];
    dt(1,1) = it.Value()[3];
    dt(1,2) = dt(2,1) = it.Value()[4];
    dt(2,2) = it.Value()[5];

    dt = directionSwap * dt * directionSwap.GetTranspose();

    typename TensorImageType::PixelType outDt;

    outDt[0] = dt(0,0);
    outDt[1] = dt(0,1);
    outDt[2] = dt(0,2);
    outDt[3] = dt(1,1);
    outDt[4] = dt(1,2);
    outDt[5] = dt(2,2);

    it.Set( outDt );

    ++it;
    }


}


template<int ImageDimension>
void WarpImageMultiTransform(char *moving_image_filename, char *output_image_filename,
        TRAN_OPT_QUEUE &opt_queue, MISC_OPT &misc_opt){

  //typedef itk::Vector<float,6> PixelType;
  typedef itk::SymmetricSecondRankTensor< float, 3 >  PixelType;
  typedef itk::Image<PixelType, ImageDimension> TensorImageType;
  typedef itk::Image<float, ImageDimension> ImageType;
  typedef itk::Vector<float, ImageDimension>         VectorType;
  typedef itk::Image<VectorType, ImageDimension>     DisplacementFieldType;
  typedef itk::MatrixOffsetTransformBase< double, ImageDimension, ImageDimension > AffineTransformType;
  typedef itk::WarpImageMultiTransformFilter<ImageType,ImageType, DisplacementFieldType, AffineTransformType> WarperType;

    itk::TransformFactory<AffineTransformType>::RegisterTransform();

    typedef itk::ImageFileReader<ImageType> ImageFileReaderType;
    //typename ImageFileReaderType::Pointer reader_img = ImageFileReaderType::New();
    //reader_img->SetFileName(moving_image_filename);
    //reader_img->Update();
    //typename ImageType::Pointer img_mov = ImageType::New();
    //img_mov = reader_img->GetOutput();
    typename TensorImageType::Pointer img_mov;
    ReadTensorImage<TensorImageType>(img_mov,moving_image_filename,true);

    typename ImageType::Pointer img_ref; // = ImageType::New();

    typename ImageFileReaderType::Pointer reader_img_ref = ImageFileReaderType::New();
    if (misc_opt.reference_image_filename){
        reader_img_ref->SetFileName(misc_opt.reference_image_filename);
        reader_img_ref->Update();
        img_ref = reader_img_ref->GetOutput();
    }

    typename TensorImageType::Pointer img_output = TensorImageType::New();
    img_output->SetLargestPossibleRegion( img_ref->GetLargestPossibleRegion() );
    img_output->SetBufferedRegion( img_ref->GetLargestPossibleRegion() );
    img_output->SetLargestPossibleRegion( img_ref->GetLargestPossibleRegion() );
    img_output->Allocate();
    img_output->SetSpacing(img_ref->GetSpacing());
    img_output->SetOrigin(img_ref->GetOrigin());
    img_output->SetDirection(img_ref->GetDirection());
    // else
    //    img_ref = NULL;

    for (unsigned int tensdim=0;  tensdim < 6;  tensdim++) {

      typedef itk::VectorIndexSelectionCastImageFilter<TensorImageType,ImageType> IndexSelectCasterType;
      typename IndexSelectCasterType::Pointer fieldCaster = IndexSelectCasterType::New();
      fieldCaster->SetInput( img_mov );
      fieldCaster->SetIndex( tensdim );
      fieldCaster->Update();
      typename ImageType::Pointer tenscomponent=fieldCaster->GetOutput();
      tenscomponent->SetSpacing(img_mov->GetSpacing());
      tenscomponent->SetOrigin(img_mov->GetOrigin());
      tenscomponent->SetDirection(img_mov->GetDirection());

      typename WarperType::Pointer  warper = WarperType::New();
      warper->SetInput(tenscomponent);
      //      PixelType nullPix;
      // nullPix.Fill(0);
      warper->SetEdgePaddingValue(0);



    if (misc_opt.use_NN_interpolator){
        typedef typename itk::NearestNeighborInterpolateImageFunction<ImageType, typename WarperType::CoordRepType> NNInterpolateType;
        typename NNInterpolateType::Pointer interpolator_NN = NNInterpolateType::New();
        Rcpp::Rcout << "Haha" << std::endl;
        warper->SetInterpolator(interpolator_NN);
    }

    typedef itk::TransformFileReader TranReaderType;
    typedef itk::ImageFileReader<DisplacementFieldType> FieldReaderType;



    unsigned int   transcount=0;
    const int kOptQueueSize = opt_queue.size();
    for(int i=0; i<kOptQueueSize; i++){
        const TRAN_OPT &opt = opt_queue[i];

        switch(opt.file_type){
        case AFFINE_FILE:{
            typename TranReaderType::Pointer tran_reader = TranReaderType::New();
            tran_reader->SetFileName(opt.filename);
            tran_reader->Update();
            typename AffineTransformType::Pointer aff = dynamic_cast< AffineTransformType* >
            ((tran_reader->GetTransformList())->front().GetPointer());
            if (opt.do_affine_inv) {
                typename AffineTransformType::Pointer aff_inv = AffineTransformType::New();
                aff->GetInverse(aff_inv);
                aff = aff_inv;
            }
            // Rcpp::Rcout <<" aff " << transcount <<  std::endl;
            warper->PushBackAffineTransform(aff);
            if (transcount==0){
                warper->SetOutputSize(img_mov->GetLargestPossibleRegion().GetSize());
                warper->SetOutputSpacing(img_mov->GetSpacing());
                warper->SetOutputOrigin(img_mov->GetOrigin());
                warper->SetOutputDirection(img_mov->GetDirection());
            }
            transcount++;
            break;
        }

        case IDENTITY_TRANSFORM:{
            typename AffineTransformType::Pointer aff;
            GetIdentityTransform(aff);
            // Rcpp::Rcout << " aff id" << transcount << std::endl;
            warper->PushBackAffineTransform(aff);
            transcount++;
            break;
        }

        case IMAGE_AFFINE_HEADER:{

            typename AffineTransformType::Pointer aff = AffineTransformType::New();
            typename ImageType::Pointer img_affine = ImageType::New();
            typename ImageFileReaderType::Pointer reader_image_affine = ImageFileReaderType::New();
            reader_image_affine->SetFileName(opt.filename);
            reader_image_affine->Update();
            img_affine = reader_image_affine->GetOutput();

            GetAffineTransformFromImage(img_affine, aff);

            if (opt.do_affine_inv) {
                typename AffineTransformType::Pointer aff_inv = AffineTransformType::New();
                aff->GetInverse(aff_inv);
                aff = aff_inv;
            }

            // Rcpp::Rcout <<" aff from image header " << transcount <<  std::endl;
            warper->PushBackAffineTransform(aff);

            //            if (transcount==0){
            //                warper->SetOutputSize(img_mov->GetLargestPossibleRegion().GetSize());
            //                warper->SetOutputSpacing(img_mov->GetSpacing());
            //                warper->SetOutputOrigin(img_mov->GetOrigin());
            //                warper->SetOutputDirection(img_mov->GetDirection());
            //            }

            transcount++;
            break;
        }

        case DEFORMATION_FILE:{
            typename FieldReaderType::Pointer field_reader = FieldReaderType::New();
            field_reader->SetFileName( opt.filename );
            field_reader->Update();
            typename DisplacementFieldType::Pointer field = field_reader->GetOutput();

            warper->PushBackDisplacementFieldTransform(field);
            warper->SetOutputSize(field->GetLargestPossibleRegion().GetSize());
            warper->SetOutputOrigin(field->GetOrigin());
            warper->SetOutputSpacing(field->GetSpacing());
            warper->SetOutputDirection(field->GetDirection());

            transcount++;
            break;
        }
        default:
            Rcpp::Rcout << "Unknown file type!" << std::endl;
        }
    }

    // warper->PrintTransformList();



    if (img_ref.IsNotNull()){
        warper->SetOutputSize(img_ref->GetLargestPossibleRegion().GetSize());
        warper->SetOutputSpacing(img_ref->GetSpacing());
        warper->SetOutputOrigin(img_ref->GetOrigin());
        warper->SetOutputDirection(img_ref->GetDirection());
    }
    else {
        if (misc_opt.use_TightestBoundingBox == true){
            // compute the desired spacking after inputting all the transform files using the

            typename ImageType::SizeType largest_size;
            typename ImageType::PointType origin_warped;
            GetLaregstSizeAfterWarp(warper, img_mov, largest_size, origin_warped);
            warper->SetOutputSize(largest_size);
            warper->SetOutputSpacing(img_mov->GetSpacing());
            warper->SetOutputOrigin(origin_warped);

            typename ImageType::DirectionType d;
            d.SetIdentity();
            warper->SetOutputDirection(d);
        }

    }

    //Rcpp::Rcout << "output origin: " << warper->GetOutputOrigin() << std::endl;
    //Rcpp::Rcout << "output size: " << warper->GetOutputSize() << std::endl;
    //Rcpp::Rcout << "output spacing: " << warper->GetOutputSpacing() << std::endl;
    //    Rcpp::Rcout << "output direction: " << warper->GetOutputDirection() << std::endl;

    // warper->PrintTransformList();
    warper->DetermineFirstDeformNoInterp();
    warper->Update();


    typedef itk::ImageRegionIteratorWithIndex<TensorImageType> Iterator;
    Iterator vfIter2( img_output, img_output->GetLargestPossibleRegion() );
    for(  vfIter2.GoToBegin(); !vfIter2.IsAtEnd(); ++vfIter2 )
      {
	PixelType  tens=vfIter2.Get();
	tens[tensdim]=warper->GetOutput()->GetPixel(vfIter2.GetIndex());
	vfIter2.Set(tens);
      }



    }


   DirectionCorrect<TensorImageType>(img_output, img_mov);

   WriteTensorImage<TensorImageType>(img_output, output_image_filename,true);
}



RcppExport SEXP WarpTensorImageMultiTransform( SEXP r_args )
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


    if (argc<=3){
        Rcpp::Rcout << "WarpImageMultiTransform ImageDimension moving_image output_image [-R reference_image | --tightest-bounding-box] (--reslice-by-header) [--use-NN (use Nearest Neighbor Interpolator)]"
        << "[--ANTS-prefix prefix-name | --ANTS-prefix-invert prefix-name] {[deformation_field | [-i] affine_transform_txt | --Id | [-i] --moving-image-header / -mh  | [-i] --reference-image-header / -rh]}" << std::endl
        << "Example:" << std::endl
        << "Reslice the image: WarpImageMultiTransform 3 Imov.nii Iout.nii --tightest-bounding-box --reslice-by-header" << std::endl
        << "Reslice the image to a reference image: WarpImageMultiTransform 3 Imov.nii Iout.nii -R Iref.nii --tightest-bounding-box --reslice-by-header" << std::endl
        << "Note:" << std::endl
        << "-i will use the inversion of the following affine transform." << std::endl
        << "--tightest-bounding-box will be overrided by -R reference_image if given. It computes the tightest bounding box using all the affine transformations." << std::endl
        << "--Id uses the identity transform." << std::endl
        << "--moving-image-header or -mh in short will use the orientation header of the moving image file. This is typically not used with --reslice-by-header." << std::endl
        << "--reference-image-header or -rh in short will use the orientation header of the fixed image file. This is typically not used with --reslice-by-header." << std::endl
        << "--reslice-by-header uses the orientation matrix and origin encoded in the image file header. It can be used together with -R. "
        << "This is typically not used together with any other transforms. "
        << "--reslice-by-header is equvalient to -i -mh, or -fh -i -mh if used together with -R. " << std::endl;
        Rcpp::Rcout << std::endl
        << "For ANTS users:" << std::endl
        << "To use with the deformation field and the affine transform files generated from ANTS:" << std::endl
        << "--ANTS-prefix prefix-name" << std::endl
        << "--ANTS-prefix-invert prefix-name" << std::endl
        << "Example:" << std::endl
        << "3 moving_image output_image -R reference_image --ANTS-prefix abcd.nii.gz" << std::endl
        << "Applies abcdWarpxvec.nii.gz/abcdWarpyvec.nii.gz/abcdWarpzvec.nii.gz and then abcdAffine.txt. Use this with ANTS to get the moving_image warped into the reference_image domain. " << std::endl
        << "3 reference_image output_image -R moving_image --ANTS-prefix-invert abcd.nii.gz --ANTS-invert" << std::endl
        << "Applies the inversion of abcdAffine.txt and then abcdInverseWarpxvec.nii.gz/abcdInverseWarpyvec.nii.gz/abcdInverseWarpzvec.nii.gz. Use this with ANTS to get the reference_image warped into the moving_image domain. " << std::endl
        << "Note: " << std::endl
        << "prefix name \"abcd\" without any extension will use \".nii.gz\" by default" << std::endl;

        return Rcpp::wrap( EXIT_FAILURE ) ;
    }


    TRAN_OPT_QUEUE opt_queue;
    char *moving_image_filename = NULL;
    char *output_image_filename = NULL;

    MISC_OPT misc_opt;

    bool is_parsing_ok = false;
    int kImageDim = atoi(argv[1]);


    is_parsing_ok = ParseInput(argc-2, argv+2, moving_image_filename, output_image_filename, opt_queue, misc_opt);

    if (is_parsing_ok){

        Rcpp::Rcout << "moving_image_filename: " << moving_image_filename << std::endl;
        Rcpp::Rcout << "output_image_filename: " << output_image_filename << std::endl;
        Rcpp::Rcout << "reference_image_filename: ";
        if (misc_opt.reference_image_filename) Rcpp::Rcout << misc_opt.reference_image_filename << std::endl;
        else Rcpp::Rcout << "NULL" << std::endl;
        DisplayOptQueue(opt_queue);

        switch (kImageDim){
        case 2:{
            WarpImageMultiTransform<2>(moving_image_filename, output_image_filename, opt_queue, misc_opt);
            break;
        }
        case 3:{
            WarpImageMultiTransform<3>(moving_image_filename, output_image_filename, opt_queue, misc_opt);
            break;
        }
        }



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

