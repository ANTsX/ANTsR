
#include <algorithm>
#include <vector>
#include <string>
#include <RcppANTsR.h>

#include "itkImage.h"
#include "itkMatrixOffsetTransformBase.h"
#include "itkCastImageFilter.h"
#include "vnl/vnl_matrix_fixed.h"
#include "vnl/vnl_diag_matrix.h"
#include "vnl/vnl_vector.h"
#include "vnl/vnl_det.h"
#include "vnl/vnl_inverse.h"
#include "vnl/algo/vnl_real_eigensystem.h"
#include "vnl/algo/vnl_qr.h"

#define RAS_TO_FSL 0
#define FSL_TO_RAS 1


/**
 * Get a matrix that maps points voxel coordinates to RAS coordinates
 */
template< class ImageType, class TransformMatrixType >
TransformMatrixType GetVoxelSpaceToRASPhysicalSpaceMatrix(typename ImageType::Pointer image)
  {
  // Generate intermediate terms
  vnl_matrix<double> m_dir, m_ras_matrix;
  vnl_diag_matrix<double> m_scale, m_lps_to_ras;
  vnl_vector<double> v_origin, v_ras_offset;

  // Compute the matrix
  m_dir = image->GetDirection().GetVnlMatrix();
  m_scale.set(image->GetSpacing().GetVnlVector());
  m_lps_to_ras.set(vnl_vector<double>(ImageType::ImageDimension, 1.0));
  m_lps_to_ras[0] = -1;
  m_lps_to_ras[1] = -1;
  m_ras_matrix = m_lps_to_ras * m_dir * m_scale;

  // Compute the vector
  v_origin = image->GetOrigin().GetVnlVector();
  v_ras_offset = m_lps_to_ras * v_origin;

  // Create the larger matrix
  TransformMatrixType mat;
  vnl_vector<double> vcol(ImageType::ImageDimension+1, 1.0);
  vcol.update(v_ras_offset);
  mat.SetIdentity();
  mat.GetVnlMatrix().update(m_ras_matrix);
  mat.GetVnlMatrix().set_column(ImageType::ImageDimension, vcol);

  return mat;
  }


template< class PixelType, unsigned int Dimension >
SEXP fsl2antsrTransform( SEXP r_matrix, SEXP r_reference, SEXP r_moving, short flag )
{
  typedef vnl_matrix_fixed<double, 4, 4>              MatrixType;
  typedef itk::Image<PixelType, Dimension>            ImageType;
  typedef itk::Matrix<double, 4,4>                    TransformMatrixType;

  typedef itk::AffineTransform<double, 3> AffTran;

  typedef typename ImageType::Pointer     ImagePointerType;

  typedef itk::Transform<double,3,3>                    TransformBaseType;
  typedef typename TransformBaseType::Pointer           TransformBasePointerType;

  ImagePointerType ref = Rcpp::as<ImagePointerType>( r_reference );
  ImagePointerType mov = Rcpp::as<ImagePointerType>( r_moving );

  MatrixType m_fsl, m_spcref, m_spcmov, m_swpref, m_swpmov, mat, m_ref, m_mov;

  Rcpp::NumericMatrix matrix(r_matrix);
  for ( unsigned int i=0; i<matrix.nrow(); i++)
    for ( unsigned int j=0; j<matrix.ncol(); j++)
      m_fsl(i,j) = matrix(i,j);

  // Set the ref/mov matrices
  m_ref = GetVoxelSpaceToRASPhysicalSpaceMatrix<ImageType, TransformMatrixType>( ref ).GetVnlMatrix();
  m_mov = GetVoxelSpaceToRASPhysicalSpaceMatrix<ImageType, TransformMatrixType>( mov ).GetVnlMatrix();

  // Set the swap matrices
  m_swpref.set_identity();
  if(vnl_det(m_ref) > 0)
    {
    m_swpref(0,0) = -1.0;
    m_swpref(0,3) = (ref->GetBufferedRegion().GetSize(0) - 1) * ref->GetSpacing()[0];
    }

  m_swpmov.set_identity();
  if(vnl_det(m_mov) > 0)
    {
    m_swpmov(0,0) = -1.0;
    m_swpmov(0,3) = (mov->GetBufferedRegion().GetSize(0) - 1) * mov->GetSpacing()[0];
    }

  // Set the spacing matrices
  m_spcref.set_identity();
  m_spcmov.set_identity();
  for(size_t i = 0; i < 3; i++)
    {
    m_spcref(i,i) = ref->GetSpacing()[i];
    m_spcmov(i,i) = mov->GetSpacing()[i];
    }

  // Compute the output matrix
  //if (flag == FSL_TO_RAS)
    mat =
    m_mov * vnl_inverse(m_spcmov) * m_swpmov *
    vnl_inverse(m_fsl) *
    m_swpref * m_spcref * vnl_inverse(m_ref);

  // Add access to this
  // NOTE: m_fsl is really m_ras here
  //if (flag == RAS_TO_FSL)
  //  mat =
  //   vnl_inverse(vnl_inverse(m_swpmov) * m_spcmov* vnl_inverse(m_mov) *
  //  m_fsl *
  //  m_ref*vnl_inverse(m_spcref)*vnl_inverse(m_swpref));

  ///////////////

  // Flip the entries that must be flipped
  mat(2,0) *= -1; mat(2,1) *= -1;
  mat(0,2) *= -1; mat(1,2) *= -1;
  mat(0,3) *= -1; mat(1,3) *= -1;

  // Create an ITK affine transform
  AffTran::Pointer atran = AffTran::New();

  // Populate its matrix
  AffTran::MatrixType amat = atran->GetMatrix();
  AffTran::OffsetType aoff = atran->GetOffset();

  for(size_t r = 0; r < 3; r++)
    {
    for(size_t c = 0; c < 3; c++)
      {
      amat(r,c) = mat(r,c);
      }
    aoff[r] = mat(r,3);
    }

  atran->SetMatrix(amat);
  atran->SetOffset(aoff);

  TransformBasePointerType itkTransform = dynamic_cast<TransformBaseType*>( atran.GetPointer() );
  return Rcpp::wrap( itkTransform );
}

RcppExport SEXP fsl2antsrTransform( SEXP r_matrix, SEXP r_reference, SEXP r_moving, SEXP r_flag )
{
try
{
  Rcpp::S4 reference( r_reference );
  //Rcpp::S4 moving( r_moving );
  std::string pixeltype = Rcpp::as<std::string>(reference.slot("pixeltype"));
  unsigned int dimension = Rcpp::as<unsigned int>(reference.slot("dimension"));
  short flag = Rcpp::as<short>(r_flag);

  if ( dimension != 3 )
    {
    Rcpp::stop("Only 3D transforms are supported");
    }

  if ( pixeltype == "double" )
    {
    return ( fsl2antsrTransform<double,3>(r_matrix, r_reference, r_moving, flag) );
    }
  else if ( pixeltype == "float" )
    {
    return( fsl2antsrTransform<float,3>(r_matrix, r_reference, r_moving, flag) );
    }
  else if ( pixeltype == "unsigned int" )
    {
    return( fsl2antsrTransform<unsigned int,3>(r_matrix, r_reference, r_moving, flag) );
    }
  else if ( pixeltype == "unsigned char" )
    {
    return( fsl2antsrTransform<unsigned char,3>(r_matrix, r_reference, r_moving, flag) );
    }
  else
    {
    Rcpp::stop("Unsupported pixel type");
    }

  // never reached
  return( Rcpp::wrap(NA_REAL) );

}
catch( itk::ExceptionObject & err )
  {
  Rcpp::Rcout << "ITK ExceptionObject caught !" << std::endl;
  Rcpp::Rcout << err << std::endl;
  Rcpp::stop("ITK exception caught");
  }
catch( const std::exception& exc )
  {
  forward_exception_to_r( exc ) ;
  }
catch(...)
  {
	Rcpp::stop("c++ exception (unknown reason)");
  }
return Rcpp::wrap(NA_REAL); //not reached
}
