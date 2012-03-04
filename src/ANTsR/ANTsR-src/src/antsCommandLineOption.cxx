/*=========================================================================

  Program:   Advanced Normalization Tools
  Module:    $RCSfile: antsCommandLineOption.cxx,v $
  Language:  C++
  Date:      $Date: 2009/01/22 22:48:30 $
  Version:   $Revision: 1.1 $

  Copyright (c) ConsortiumOfANTS. All rights reserved.
  See accompanying COPYING.txt or
 http://sourceforge.net/projects/advants/files/ANTS/ANTSCopyright.txt for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "antsCommandLineOption.h"

namespace itk
{
namespace ants
{

CommandLineOption
::CommandLineOption() : m_ShortName( '\0' ),
                        m_LongName( "" ),
                        m_Description( "" )
{
  this->m_Values.clear();
  this->m_UsageOptions.clear();
}

void
CommandLineOption
::AddValue( std::string value, char leftDelimiter, char rightDelimiter )
{
  std::string::size_type leftDelimiterPos = value.find( leftDelimiter );
  std::string::size_type rightDelimiterPos = value.find( rightDelimiter );

  if( leftDelimiterPos == std::string::npos ||
    rightDelimiterPos == std::string::npos )
    {
    this->m_Values.push_front( value );

    ValueStackType parameters;
    this->m_Parameters.push_front( parameters );
    }
  else
    {
    ValueStackType parameters;

    this->m_Values.push_front( value.substr( 0, leftDelimiterPos ) );

    std::string::size_type leftPos = leftDelimiterPos;
    std::string::size_type rightPos = value.find( ',', leftPos+1 );
    while( rightPos != std::string::npos )
      {
      parameters.push_back( value.substr( leftPos+1, rightPos - leftPos - 1 ) );
      leftPos = rightPos;
      rightPos = value.find( ',', leftPos+1 );
      }
    rightPos = rightDelimiterPos;
    parameters.push_back( value.substr( leftPos+1, rightPos - leftPos - 1 ) );

    this->m_Parameters.push_front( parameters );
    }

  this->Modified();
}

void
CommandLineOption
::SetValue( unsigned int i, std::string value )
{
  this->m_Values[i] = value;
  this->Modified();
}

void
CommandLineOption
::SetUsageOption( unsigned int i, std::string usage )
{
  if( i >= this->m_UsageOptions.size() )
    {
    this->m_UsageOptions.resize( i + 1 );
    }
  this->m_UsageOptions[i] = usage;
  this->Modified();
}

} // end namespace ants
} // end namespace itk
