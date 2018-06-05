#include "core.h"
namespace six {
  const Matrix4 Matrix4::Zero = Matrix4();
  const Matrix4 Matrix4::Identity = Matrix4(
    1.f, 0.f, 0.f, 0.f,
    0.f, 1.f, 0.f, 0.f,
    0.f, 0.f, 1.f, 0.f,
    0.f, 0.f, 0.f, 1.f);

  Matrix4 operator * (f32 val, const Matrix4& mat) {
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
  
  Matrix4 Matrix4::inverse() {
    f32 m00 = mm[0][0], m01 = mm[0][1], m02 = mm[0][2], m03 = mm[0][3];
    f32 m10 = mm[1][0], m11 = mm[1][1], m12 = mm[1][2], m13 = mm[1][3];
    f32 m20 = mm[2][0], m21 = mm[2][1], m22 = mm[2][2], m23 = mm[2][3];
    f32 m30 = mm[3][0], m31 = mm[3][1], m32 = mm[3][2], m33 = mm[3][3];

    f32 v0 = m20 * m31 - m21 * m30;
    f32 v1 = m20 * m32 - m22 * m30;
    f32 v2 = m20 * m33 - m23 * m30;
    f32 v3 = m21 * m32 - m22 * m31;
    f32 v4 = m21 * m33 - m23 * m31;
    f32 v5 = m22 * m33 - m23 * m32;

    f32 t00 = + (v5 * m11 - v4 * m12 + v3 * m13);
    f32 t10 = - (v5 * m10 - v2 * m12 + v1 * m13);
    f32 t20 = + (v4 * m10 - v2 * m11 + v0 * m13);
    f32 t30 = - (v3 * m10 - v1 * m11 + v0 * m12);

    f32 invDet = 1 / (t00 * m00 + t10 * m01 + t20 * m02 + t30 * m03);

    f32 d00 = t00 * invDet;
    f32 d10 = t10 * invDet;
    f32 d20 = t20 * invDet;
    f32 d30 = t30 * invDet;

    f32 d01 = - (v5 * m01 - v4 * m02 + v3 * m03) * invDet;
    f32 d11 = + (v5 * m00 - v2 * m02 + v1 * m03) * invDet;
    f32 d21 = - (v4 * m00 - v2 * m01 + v0 * m03) * invDet;
    f32 d31 = + (v3 * m00 - v1 * m01 + v0 * m02) * invDet;

    v0 = m10 * m31 - m11 * m30;
    v1 = m10 * m32 - m12 * m30;
    v2 = m10 * m33 - m13 * m30;
    v3 = m11 * m32 - m12 * m31;
    v4 = m11 * m33 - m13 * m31;
    v5 = m12 * m33 - m13 * m32;

    f32 d02 = + (v5 * m01 - v4 * m02 + v3 * m03) * invDet;
    f32 d12 = - (v5 * m00 - v2 * m02 + v1 * m03) * invDet;
    f32 d22 = + (v4 * m00 - v2 * m01 + v0 * m03) * invDet;
    f32 d32 = - (v3 * m00 - v1 * m01 + v0 * m02) * invDet;

    v0 = m21 * m10 - m20 * m11;
    v1 = m22 * m10 - m20 * m12;
    v2 = m23 * m10 - m20 * m13;
    v3 = m22 * m11 - m21 * m12;
    v4 = m23 * m11 - m21 * m13;
    v5 = m23 * m12 - m22 * m13;

    f32 d03 = - (v5 * m01 - v4 * m02 + v3 * m03) * invDet;
    f32 d13 = + (v5 * m00 - v2 * m02 + v1 * m03) * invDet;
    f32 d23 = - (v4 * m00 - v2 * m01 + v0 * m03) * invDet;
    f32 d33 = + (v3 * m00 - v1 * m01 + v0 * m02) * invDet;

    return Matrix4(
      d00, d01, d02, d03,
      d10, d11, d12, d13,
      d20, d21, d22, d23,
      d30, d31, d32, d33);
  }

}