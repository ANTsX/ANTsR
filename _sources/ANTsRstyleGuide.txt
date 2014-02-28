==================================================
 Style guide for ANTsR
==================================================

This is a work in progress. This guide is based in part on Google's `R style guide <http://google-styleguide.googlecode.com/svn/trunk/google-r-style.html>`_, other style guides and conventions noted `here <http://journal.r-project.org/archive/2012-2/RJournal_2012-2_Baaaath.pdf>`_, and existing ANTsR conventions.


Summary: R Style Rules
==================================================

- File Names: end in .R
- Identifiers: variableName, functionName, kConstantName, pipeline_script_functions
- Indentation: two spaces, no tabs
- Curly Braces: first on same line, last on own line
- Assignment: use <-, not =
- Semicolons: don't use them
- Commenting Guidelines: all comments begin with # followed by a space; inline comments need two spaces before the #



Summary: R Language Rules
==================================================

- attach: avoid using it
- Functions: errors should be raised using stop()
- Objects and Methods: Never mix S3 and S4



Notation and Naming
==================================================

 
File Names
------------

File names should end in .R and, of course, be meaningful. 

GOOD::

  antsImageRead.R 

BAD::

  foo.R


Identifiers
-------------

Identifiers should be named according to the following conventions: 

- Function names wrapping ANTs functions should match the function, eg antsRegistration or ImageMath
- Function names implementing pipelines or examples should be lower case, separated by underscores, eg simple_roi_analysis
- Other function names and variable names should be in camelCase
- Constants have an initial k, then follow camelCase, eg kCamelCase

Make function names verbs and variable names nouns. 

Exception: When creating a classed object, the function name (constructor) and class should match (e.g., lm).


Syntax
========


Indentation
-------------

When indenting your code, use two spaces. Never use tabs or mix tabs and spaces.


Spacing
------------

Place spaces around all binary operators (=, +, -, <-, etc.). 
Exception: Spaces around ='s are optional when passing parameters in a function call.

Do not place a space before a comma, but always place one after a comma. 

GOOD::

  tabPrior <- table(df[df$daysFromOpt < 0, "campaignid"])
  total <- sum(x[, 1])
  total <- sum(x[1, ])

BAD::

  tabPrior <- table(df[df$daysFromOpt<0, "campaignid"])  # Needs spaces around '<'
  tabPrior <- table(df[df$daysFromOpt < 0,"campaignid"])  # Needs a space after the comma
  tabPrior<- table(df[df$daysFromOpt < 0, "campaignid"])  # Needs a space before <-
  tabPrior<-table(df[df$daysFromOpt < 0, "campaignid"])  # Needs spaces around <-
  total <- sum(x[,1])  # Needs a space after the comma
  total <- sum(x[ ,1])  # Needs a space after the comma, not before


Place a space before left parenthesis, except in a function call.

GOOD::

  if (debug)

BAD::

  if(debug)

Extra spacing (i.e., more than one space in a row) is okay if it improves alignment of equals signs or arrows (<-)::

  plot(x    = xCoord,
       y    = dataMat[, makeColName(metric, ptiles[1], "roiOpt")],
       ylim = ylim,
       xlab = "dates",
       ylab = metric,
       main = (paste(metric, " for 3 samples ", sep="")))

Do not place spaces around code in parentheses or square brackets. Exception: Always place a space after a comma.

GOOD::

  if (debug)
  x[1, ]
  
BAD::

  if ( debug )  # No spaces around debug
  x[1,]  # Needs a space after the comma 


Curly Braces
--------------

An opening curly brace should never go on its own line; a closing curly brace should always go on its own line::

  if (is.null(ylim)) {
    ylim <- c(0, 0.06)
  }

Always begin the body of a block on a new line.

BAD::

  if (is.null(ylim)) ylim <- c(0, 0.06) 
  if (is.null(ylim)) {ylim <- c(0, 0.06)}


Assignment
-------------

Use <-, not =, for assignment.

GOOD::
  
  x <- 5

BAD::

  x = 5


Exception: use = for arguments to functions, eg::
  
  function(param = defaultValue)


Semicolons
------------

Do not terminate your lines with semicolons or use semicolons to put more than one command on the same line.



Organization
=============


Commenting Guidelines
----------------------

Comment your code. Entire commented lines should begin with # and one space.

Short comments can be placed after code preceded by two spaces, #, and then one space::

  # Create histogram of frequency of campaigns by pct budget spent.
  hist(df$pctSpent,
       breaks = "scott",  # method for choosing number of buckets
       main   = "Histogram: fraction budget spent by campaignid",
       xlab   = "Fraction of budget spent",
       ylab   = "Frequency (count of campaignids)")


Function Definitions and Calls
-------------------------------

Function definitions should first list arguments without default values, followed by those with default values.

In both function definitions and function calls, multiple arguments per line are allowed; line breaks are only allowed between assignments. 

GOOD::

  PredictCTR <- function(query, property, numDays,
                         showPlot = TRUE)
BAD::
  PredictCTR <- function(query, property, numDays, showPlot =
                         TRUE)


Function Documentation
-----------------------
Functions **must** be documented with a man page, and the man page must be kept up to date.

The function documentation should use the following sections, in order::

  \name{functionName}
  \alias{functionName}
  \title {
  Function Title
  }

  \description{
    Short description of function.
  } 

  \usage{
  functionName( arg1, arg2, argWithDefault = default, \dots )
  } 

  \arguments{
    \item{arg1}{
      Input image for motion-correction. Can be a filename of a 3D image or
      an \code{antsImage} of pixeltype \code{float} and dimension 3.
    }

    % Document all args that are shown in the usage. Uncommon args abbreviated as \sQuote{\dots} can be 
    % explained in \sQuote{Details}

    % Use \code{} to highlight code
  }
  \details{} % Optional 
 
  \value{
    Describe the return value including its type. Can be omitted for functions that do not return anything.
  }
  \section{Optional Additional Sections as Needed}{
  }

  \seealso{
     \code{\link{relatedFunction}} % Optional
  } 

  \references{} % Optional 

  \author{
    Lastname AB
  }

  \examples{
  \dontrun{
    % Examples, see Rd developer guide link below
  }
  } 


For more details, see `Guidelines for Rd files <http://developer.r-project.org/Rds.html>`_. For help on the .Rd format, see `here <http://developer.r-project.org/parseRd.pdf>`_.


Language
===========

Attach
-------

The possibilities for creating errors when using attach are numerous. Avoid it.


Functions
-----------

Errors should be raised using stop(). This makes it easier to trace errors.


GOOD::

  if (x < 0) {
    stop("x must be positive")
  }

BAD::

  if (x < 0) {
    print("x must be positive")
    return(NULL)
  }


WORSE::

  if (x < 0) {
    return(NULL)
  }


WORST::

   # I'm too busy to check for errors
   
   someFunction(x) # seg faults if x < 0


Objects and Methods
-------------------------

Avoid mixing S3 and S4: S4 methods ignore S3 inheritance and vice-versa.

