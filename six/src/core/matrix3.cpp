#include "core.h"
namespace six {
  const Matrix3 Matrix3::Zero = Matrix3();
  const Matrix3 Matrix3::Identity = Matrix3(1.f, 0.f, 0.f, 0.f, 1.f, 0.f, 0.f, 0.f, 1.f);

  Matrix3 operator * (f32 val, const Matrix3& mat) {
    Matrix3 r;
    for(int i = 0; i != 9; ++i)
      r.m[i] = val*mat.m[i];
    return r;
  }
  Vector3f operator * (const Vector3f& vec, const Matrix3& mat) {
    Vector3f r;
    for(int i = 0; i != 3; ++i)
        r[i] = vec[0]*mat.mm[0][i] + vec[1]*mat.mm[1][i] + vec[2]*mat.mm[2][i];
    return r;
  }

  void Matrix3::swap(Matrix3& mat) {
    for(int i = 0; i != 9; ++i) 
      SWAP(m[i], mat[i]);
  }
}