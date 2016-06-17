#include "core.h"
namespace six {
  const Matrix4 Matrix4::Zero = Matrix4();
  const Matrix4 Matrix4::Identity = Matrix4(
    1.f, 0.f, 0.f, 0.f,
    0.f, 1.f, 0.f, 0.f,
    0.f, 0.f, 1.f, 0.f,
    0.f, 0.f, 0.f, 1.f);

  Matrix4 operator * (float val, const Matrix4& mat) {
    Matrix4 r;
    for(int i = 0; i != 16; ++i)
      r.m[i] = val*mat.m[i];
    return r;
  }
  Vector3f operator * (const Vector3f& vec, const Matrix4& mat) {
    Vector3f r;
    for(int i = 0; i != 3; ++i)
      r[i] = vec[0]*mat.mm[0][i] + vec[1]*mat.mm[1][i] + vec[2]*mat.mm[2][i];
    return r;
  }

  void Matrix4::swap(Matrix4& mat) {
    for(int i = 0; i != 16; ++i)
      SWAP(m[i], mat[i]);
  }

}