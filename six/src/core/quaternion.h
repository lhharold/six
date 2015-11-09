#ifndef __SIX_QUATERNION_H_INCLUDE__
#define __SIX_QUATERNION_H_INCLUDE__

namespace six {
  class Quaternion {
  public:
    static const Quaternion Zero;
    static const Quaternion Identity;
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
    Quaternion(f32 angle, const Vector3f& axis) {fromAngleAxis(angle, axis);}
    //Quaternion(const Vector3f& eluer) {setEulerByDegree(eluer.x, eluer.y, eluer.z);}
    //Quaternion(const Vector3f& xAxis, const Vector3f& yAxis, const Vector3f& zAxis) {fromAxes(xAxis, yAxis, zAxis);}
    //Quaternion(const Matrix3& m){}

    f32 operator[] (u8 i) {ASSERT(i<4); return value[i];}
    bool operator == (const Quaternion& q) const {return F_EQUAL(x, q.x) && F_EQUAL(y, q.y) && F_EQUAL(z, q.z) && F_EQUAL(w, q.w);}
    bool operator != (const Quaternion& q) const {return !operator ==(q);}

    Quaternion operator + (const Quaternion& q) const{return Quaternion(x+q.x, y+q.y, z+q.z, w+q.w);}
    const Quaternion& operator += (const Quaternion& q) {return (*this = *this+q);}
    Quaternion operator - (const Quaternion& q) const{return Quaternion(x-q.x, y-q.y, z-q.z, w-q.z);}
    const Quaternion& operator -= (const Quaternion& q) {return (*this = *this-q);}
    Quaternion operator * (f32 val)const {return Quaternion(x*val, y*val, z*val, w*val);}
    const Quaternion& operator *= (f32 val) {return (*this = *this*val);}
    Quaternion operator * (const Quaternion& vec) {
      const float T0 = (z - y) * (vec.y - vec.z); //z*vy - z*vz - y*vy + y*vz
      const float T1 = (w + x) * (vec.w + vec.x); //w*vw + w*vx + x*vw + x*vx
      const float T2 = (w - x) * (vec.y + vec.z); //w*vy + w*vz - x*vy - x*vz
      const float T3 = (y + z) * (vec.w - vec.x); //y*vw - y*vx + z*vw - z*vx
      const float T4 = (z - x) * (vec.x - vec.y); //z*vx - z*vy - x*vx + x*vy
      const float T5 = (z + x) * (vec.x + vec.y); //z*vx + z*vy + x*vx + x*vy
      const float T6 = (w + y) * (vec.w - vec.z); //w*vw - w*vz + y*vw - y*vz
      const float T7 = (w - y) * (vec.w + vec.z); //w*vw + w*vz - y*vw - y*vz
      const float T8 = T5 + T6 + T7; //z*vx + z*vy + x*vx + x*vy + 2*w*vw - 2y*vz
      const float T9 = 0.5f * (T4 + T8); //z*vx + x*vy + w*vw - y*vz 

      x = T1 + T9 - T8; //w*vx + x*vw + y*vz - z*vy
      y = T2 + T9 - T7; //w*vy + y*vw - x*vz + z*vx
      z = T3 + T9 - T6; //w*vz + z*vw + x*vy - y*vx
      w = T0 + T9 - T5; //w*vw - z*vz - y*vy  - x*vx
    }
    const Quaternion& operator *= (const Quaternion& vec) {return (*this = *this*vec);}

    const Quaternion& mult(const Quaternion& q) {operator*(q); return *this;}
		bool isNan() const {return Math::isNan(x) || Math::isNan(y) || Math::isNan(z) || Math::isNan(w);}
    bool empty() const {return F_ZERO(x*x+y*y+z*z+w*w);}
    void clear() {x=0.f; y=0.f; z=0.f; w=0.f;}
    void setZero() {clear();}
    void set(f32 fx, f32 fy, f32 fz, f32 fw) {x = fx; y = fy; z=fz; w=fw;}
    void set(const Quaternion& q) {x=q.x; y=q.y; z=q.z; w=q.w;}
    void swap(Quaternion& q) {SWAP(x, q.x); SWAP(y, q.y); SWAP(z, q.z); SWAP(w, q.w);}

    Quaternion normalizeEx() const {Quaternion vec = *this; vec.normalize(); return vec;}
    void length(f32 len) {normalize(); *this *= len;}
    f32 length() const {return empty() ? 0.f : Math::sqrt(lengthSqr());}
    f32 lengthSqr() const {return x*x + y*y + z*z + w*w;}

    f32 normalize() {
      f32 len = length();
      if(len > F_PRECISION) {
        f32 invLen = 1.f / len;
        x *= invLen;
        y *= invLen;
        z *= invLen;
        w *= invLen;
      }
      return len;
    }
    void fromAngleAxis(f32 angle, const Vector3f& axis) {
      f32 halfAngle = 0.5f * angle;
      f32 s = Math::sin(halfAngle);
      x = s*axis.x;
      y = s*axis.y;
      z = s*axis.z;
      w = Math::cos(halfAngle);
      normalize();
    }
    void toAngleAxis(f32& angle, Vector3f& axis) {
      f32 sqrLen = x*x+y*y+z*z;
      if (sqrLen > 0.f) {
        angle = 2.f*Math::acos(w);
        f32 invLen = Math::invsqrt(sqrLen);
        axis.x = x*invLen;
        axis.y = y*invLen;
        axis.z = z*invLen;
      } else {
        angle = 0.f;
        axis.x = 1.f;
        axis.y = 1.f;
        axis.z = 1.f;
      }
    }
    //void fromAxes(const Vector3f& xAxis, const Vector3f& yAxis, const Vector3f& zAxis) { 
    //}
    //void setEulerByDegree();
  };
}

#endif //__SIX_QUATERNION_H_INCLUDE__
