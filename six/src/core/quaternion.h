#ifndef __SIX_QUATERNION_H_INCLUDE__
#define __SIX_QUATERNION_H_INCLUDE__

namespace six {
  class Quaternion {
  public:
    union {
      struct {
        f32 x;
        f32 y;
        f32 z;
        f32 w;
      };
      f32 value[4];
    };
    Quaternion() : x(0.f), y(0.f), z(0.f), w(1.f) {}
    Quaternion(f32 fx, f32 fy, f32 fz, f32 fw) : x(fx), y(fy), z(fz), w(fw) {}
    Quaternion(const Quaternion& q) : x(q.x), y(q.y), z(q.z), w(q.w) {}
    Quaternion(f32 angle, const Vector3f& axis) {fromeAngleAxis(angle, axis);}
    Quaternion(const vector3f& eluer) {setEulerByDegree3(eluer.x, eluer.y, eluer.z);}
    Quaternion(const Vector3f& xAxis, const Vector3f& yAxis, const Vector3f& zAxis) {fromAxes(xAxis, yAxis, zAxis);}

    f32 operator[] (u8 i) {ASSERT(i<4); return value[i];}
    bool operator == (const Quaternion& q) const {return F_EQUAL(x, q.x) && F_EQUAL(y, q.y) && F_EQUAL(z, q.z) && F_EQUAL(w, q.w);}
    bool operator != (const Quaternion& q) const {return !operator ==(q);}

    Quaternion operator + (const Quaternion& q) const{return Quaternion(x+q.x, y+q.y, z+q.z, w+q.w);}
    Quaternion& operator += (const Quaternion& q) {return (*this = *this+q);}
    Quaternion operator - (const Quaternion& q) const{return Quaternion(x-q.x, y-q.y, z-q.z, w-q.z);}
    Quaternion& operator -= (const Quaternion& q) {return (*this = *this-q);}
    Quaternion operator * (f32 val)const {return Quaternion(x*val, y*val, z*val, w*val);}
    Quaternion& operator *= (f32 val) {return (*this = *this*val);}
    Quaternion operator * (const Quaternion& vec)const {return Quaternion(x*vec.x, y*vec.y, z*vec.z);}
    Quaternion& operator *= (const Quaternion& vec) {return (*this = *this*vec);}
    Quaternion operator / (f32 val)const {ASSERT(!F_ZERO(val)); return Quaternion(x/val, y/val, z/val);}
    Quaternion& operator /= (f32 val) {return (*this = *this/val);}
    Quaternion operator / (const Quaternion& vec)const {ASSERT(!F_ZERO(vec.x) && !F_ZERO(vec.y) && !F_ZERO(vec.z)); return Quaternion(x/vec.x, y/vec.y, z/vec.z);}
    Quaternion& operator /= (const Quaternion& vec) {return (*this = *this/vec);}

    void swap(Quaternion& q) {SWAP(x, q.x); SWAP(y, q.y); SWAP(z, q.z); SWAP(w, q.w);}
    //Quaternion(const Matrix3& m){}
  }
}

#endif //__SIX_QUATERNION_H_INCLUDE__
