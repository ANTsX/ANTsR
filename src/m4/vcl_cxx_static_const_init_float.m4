AC_DEFUN([VCL_CXX_STATIC_CONST_INIT_FLOAT],
[AC_MSG_CHECKING(whether the C++ compiler allows initialization of static const floats)
AC_LANG_SAVE
AC_LANG_CPLUSPLUS
AC_TRY_RUN([
class A {
 public:
  static const float x = 27.0f;
  static const double y = 27.0;
};
int main() { return A::x == 27.0f && A::y == 27.0 ? 0 : 1; }
],[VCL_STATIC_CONST_INIT_FLOAT=0;AC_MSG_RESULT(no)],[VCL_STATIC_CONST_INIT_FLOAT=1;AC_MSG_RESULT(yes)],)
export VCL_STATIC_CONST_INIT_FLOAT
AC_LANG_RESTORE])
