#ifndef __SIX_MATRIX4_H_INCLUDE__
#define __SIX_MATRIX4_H_INCLUDE__

namespace six {
  enum Matrix4_Index_T {
    I00 = 0, I01 = 1, I02 = 2, I03 = 3,
    I10 = 4, I11 = 5, I12 = 6, I13 = 7,
    I20 = 8, I21 = 9, I22 = 10, I23 = 11,
    I30 = 12, I31 = 13, I32 = 14, I33 = 15,
  };
  class Matrix4 {
  public:
    static const Matrix4 Zero;
    static const Matrix4 Identity;
    union {
      float mm[4][4];
      float m[16];
    };

    Matrix4() {memset(m, 0, sizeof(*this));}
    explicit Matrix4(float val[16]) {memcpy(m, val, sizeof(*this));}
    Matrix4(float m00, float m01, float m02, float m03,
      float m10, float m11, float m12, float m13,
      float m20, float m21, float m22, float m23,
      float m30, float m31, float m32, float m33) {
        m[I00] = m00; m[I01] = m01; m[I02] = m02; m[I03] = m03;
        m[I10] = m10; m[I11] = m11; m[I12] = m12; m[I13] = m13;
        m[I20] = m20; m[I21] = m21; m[I22] = m22; m[I23] = m23;
        m[I30] = m30; m[I31] = m31; m[I32] = m32; m[I33] = m33;
    }
    Matrix4(const Matrix4& mat) {memcpy(m, mat.m, sizeof(*this));}

    float operator[] (unsigned char i) const {return m[i];}
    float& operator[] (unsigned char i) {return m[i];}
    const Matrix4& operator = (const Matrix4& mat) {memcpy(m, mat.m, sizeof(*this)); return *this;}
    inline bool operator == (const Matrix4& mat) const;
    bool operator != (const Matrix4& mat) const {return !operator ==(mat);}

    inline Matrix4 operator + (const Matrix4& mat) const;
    const Matrix4& operator += (const Matrix4& mat) {return (*this = *this+mat);}
    inline Matrix4 operator - (const Matrix4& mat) const;
    const Matrix4& operator -= (const Matrix4& mat) {return (*this = *this-mat);}
    inline Matrix4 operator * (float val)const;
    const Matrix4& operator *= (float val) {return (*this = *this*val);}
    inline Vector3f operator * (const Vector3f& vec);
    inline Matrix4 operator * (const Matrix4& mat);
    const Matrix4& operator *= (const Matrix4& mat) {return (*this = *this*mat);}

    friend Matrix4 operator * (float val, const Matrix4& mat);
    friend Vector3f operator * (const Vector3f& vec, const Matrix4& mat);
    void swap(Matrix4& mat);

    void makeIdentity() {memset(m, 0, sizeof(*this));}
    void transpose() {
      float t;
      t = m[I01]; m[I01] = m[I10]; m[I10] = t;
      t = m[I02]; m[I02] = m[I20]; m[I20] = t;
      t = m[I03]; m[I03] = m[I30]; m[I30] = t;
      t = m[I12]; m[I12] = m[I21]; m[I21] = t;
      t = m[I13]; m[I13] = m[I31]; m[I31] = t;
      t = m[I23]; m[I23] = m[I32]; m[I32] = t;
    };
  };

  inline bool Matrix4::operator == (const Matrix4& mat) const {
    for(int i = 0; i != 16; ++i)
      if(!F_EQUAL(m[i], mat.m[i]))
        return false;
    return true;
  }

  inline Matrix4 Matrix4::operator + (const Matrix4& mat) const {
    Matrix4 r;
    for(int i = 0; i != 16; ++i)
      r.m[i] = m[i]+mat.m[i];
    return r;
  }

  inline Matrix4 Matrix4::operator - (const Matrix4& mat) const {
    Matrix4 r;
    for(int i = 0; i != 16; ++i)
      r.m[i] = m[i]-mat.m[i];
    return r;
  }

  inline Matrix4 Matrix4::operator * (float val)const {
    Matrix4 r;
    for(int i = 0; i != 16; ++i)
      r.m[i] = val*m[i];
    return r;
  }
  inline Vector3f Matrix4::operator * (const Vector3f& vec) {
    Vector3f r;
    for(int i = 0; i != 3; ++i)
      r[i] = mm[i][0]*vec[0] + mm[i][1]*vec[1] + mm[i][2]*vec[2];
    return r;
  }
  inline Matrix4 Matrix4::operator * (const Matrix4& mat) {
    Matrix4 r;
    for(int i = 0; i != 4; ++i)
      for(int j = 0; j != 4; j++)
        r.mm[i][j] = mm[i][0]*mat.mm[0][j] + mm[i][1]*mat.mm[1][j] + mm[i][2]*mat.mm[2][j] + mm[i][3]*mat.mm[3][j];
    return r;
  }
}

#endif //__SIX_MATRIX4_H_INCLUDE__
