#ifndef __SIX_MATRIX3_H_INCLUDE__
#define __SIX_MATRIX3_H_INCLUDE__

namespace six {
  class Matrix3 {
  public:
    static const Matrix3 Zero;
    static const Matrix3 Identity;
		enum index_t {
			I00 = 0, I01 = 1, I02 = 2,
			I10 = 3, I11 = 4, I12 = 5,
			I20 = 6, I21 = 7, I22 = 8,
		};
    union {
      f32 mm[3][3];
      f32 m[9];
    };

    Matrix3() {MEMSET(m, 0, sizeof(*this));}
    explicit Matrix3(f32 val[9]) {MEMCPY(m, val, sizeof(*this));}
    Matrix3(f32 m00, f32 m01, f32 m02, f32 m10, f32 m11, f32 m12, f32 m20, f32 m21, f32 m22) : m[I00](m00), m[I01](m01), m[I02](m02), m[I10](m10), m[I11](m11), m[I12](m12), m[I20](m20), m[I21](m21), m[I22](m22) {}
    Matrix3(const Matrix3& mat) {MEMCPY(m, mat.m, sizeof(*this));}

    f32 operator[] (u8 i) {ASSERT(i<9); return m[i];}
    f32& operator[] (u8 i) {ASSERT(i<9); return m[i];}
    const Matrix3& operator = (const Matrix3& mat) {MEMCPY(m, mat.m, sizeof(*this)); return *this;}
    inline bool operator == (const Matrix3& mat) const;
    bool operator != (const Matrix3& mat) const {return !operator ==(mat);}

    inline Matrix3 operator + (const Matrix3& mat) const;
    const Matrix3& operator += (const Matrix3& mat) {return (*this = *this+mat);}
    inline Matrix3 operator - (const Matrix3& mat) const;
    const Matrix3& operator -= (const Matrix3& mat) {return (*this = *this-mat);}
    inline Matrix3 operator * (f32 val)const;
    const Matrix3& operator *= (f32 val) {return (*this = *this*val);}
    inline Matrix3 operator * (const Vector3f& vec);
    const Matrix3& operator *= (const Vector3f& vec) {return (*this = *this*vec);}
    inline Matrix3 operator * (const Matrix3& mat);
    const Matrix3& operator *= (const Matrix3& mat) {return (*this = *this*mat);}

    inline friend Matrix3 operator + (f32 val, const Matrix3& mat) const;
    inline friend Matrix3 operator - (f32 val, const Matrix3& mat) const;
    inline friend Matrix3 operator * (f32 val, const Matrix3& mat) const;
    inline friend Matrix3 operator * (const Vector3f& vec, const Matrix3& mat) const;

    bool isNan() {return Math::isNan(x) || Math::isNan(y) || Math::isNan(z);}
    bool empty() const {return F_ZERO(x*x+y*y+z*z);}
    void clear() {x=0.f; y=0.f; z=0.f;}
    void setZero() {clear();}
    void set(f32 fx, f32 fy, f32 fz) {x = fx; y = fy; z = fz;}
    void set(const Matrix3& mat) {x = mat.x; y = mat.y; z = mat.z;}
    void swap(Matrix3& mat) {for(int i = 0; i != 9; ++i) SWAP(m[i], mat[i]);}

    Matrix3 normalizeEx() const {Matrix3 mat = *this; mat.normalize(); return mat;}
    void length(f32 len) {normalize(); *this *= len;}
    f32 length() const {return empty() ? 0.f : Math::sqrt(lengthSqr());}
    f32 lengthSqr() const {return x*x + y*y + z*z;}
    f32 distance(const Matrix3& mat) const {return (*this - mat).length();}
    f32 distanceSqr(const Matrix3& mat) const {return (*this - mat).lengthSqr();}
    void makeFloor(const Matrix3& mat) {if(mat.x < x) x = mat.x; if(mat.y < y) y = mat.y; if(mat.z < z) z = mat.z;}
    void makeCeil(const Matrix3& mat) {if(mat.x > x) x = mat.x; if(mat.y > y) y = mat.y; if(mat.z > z) z = mat.z;}
    f32 dot(const Matrix3& mat) const {return x*mat.x + y*mat.y + z*mat.z;}
    Matrix3 cross(const Matrix3& mat) const {return Matrix3(y*mat.z - z*mat.y, z*mat.x - x*mat.z, x*mat.y - y*mat.x);}

    Matrix3 reflect(const Matrix3& normal) const {return Matrix3(*this - (2.f*dot(normal)*normal));}

    f32 normalize();
    Matrix3 perpendicular() const; //´¹Ö±
    void resize(f32 len);
    //Quaternion getRotationTo(const Matrix3& mat, const Matrix3& fallbackAxis = Matrix3::Zero);
    f32 angleBetween(const Matrix3& mat);
  };

  inline bool Matrix3::operator == (const Matrix3& mat) const {
    for(int i = 0; i != 9; ++i) 
      if(!F_EQUAL(m[i], mat.m[i]))
        return false;
    return true;
  }

  inline Matrix3 Matrix3::operator + (const Matrix3& mat) const{
    Matrix3 r;
    for(int i = 0; i != 9; ++i)
      r.m[i] = m[i]+mat.m[i];
    return r;
  }

  inline Matrix3 Matrix3::operator - (const Matrix3& mat) const{
    Matrix3 r;
    for(int i = 0; i != 9; ++i) 
      r.m[i] = m[i]-mat.m[i];
    return r;
  }

  inline Matrix3 Matrix3::operator * (f32 val)const {
    Matrix3 r;
    for(int i = 0; i != 9; ++i)
      r.m[i] = val*m[i];
    return r;
  }
  inline Matrix3 Matrix3::operator * (const Vector3f& vec) {
    Matrix3 r;
    for(int i = 0; i != 3; ++i)
      for(int j = 0; j != 3; j++)
        r.mm[i][j] = mm[i][0]*vec[0] + mm[i][1]*vec[1] + mm[i][2]*vec[2];
    return r;
  }
  inline Matrix3 Matrix3::operator * (const Matrix3& mat) {
    Matrix3 r;
    for(int i = 0; i != 3; ++i)
      for(int j = 0; j != 3; j++)
        r.mm[i][j] = mm[i][0]*mat.mm[0][j] + mm[i][1]*mat.mm[1][j] + mm[i][2]*mat.mm[2][j];
    return r;
  }

  inline Matrix3 Matrix3::operator + (f32 val, const Matrix3& mat) const {
    Matrix3 r;
    for(int i = 0; i != 9; ++i)
      r.m[i] = val+m[i];
    return r;
  }
  inline Matrix3 Matrix3::operator - (f32 val, const Matrix3& mat) const {
    Matrix3 r;
    for(int i = 0; i != 9; ++i)
      r.m[i] = val-m[i];
    return r;
  }
  inline Matrix3 Matrix3::operator * (f32 val, const Matrix3& mat) const {

  }
  inline Matrix3 Matrix3::operator * (const Vector3f& vec, const Matrix3& mat) const {

  }
}

#endif //__SIX_MATRIX3_H_INCLUDE__
