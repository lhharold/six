#ifndef __SIX_VECTOR4_H_INCLUDE__
#define __SIX_VECTOR4_H_INCLUDE__

namespace six {
  class Vector4f {
  public:
    static const Vector4f Zero;
    static const Vector4f One;
    union {
      struct {
        f32 x;
        f32 y;
        f32 z;
        f32 w;
      };
      f32 value[4];
    };
    Vector4f() : x(0.f), y(0.f), z(0.f), w(0.f) {}
    explicit Vector4f(f32 val[4]) : x(val[0]), y(val[1]), z(val[2]), w(val[3]) {}
    Vector4f(f32 fx, f32 fy, f32 fz, f32 fw) : x(fx), y(fy), z(fz), w(fw) {}
    Vector4f(const Vector4f& vec) : x(vec.x), y(vec.y), z(vec.z), w(vec.w) {}

    inline f32 operator[] (u8 i) const {ASSERT(i <4); return value[i];}
    inline f32& operator[] (u8 i) {ASSERT(i <4); return value[i];}
    const Vector4f& operator = (const Vector4f& vec) {x = vec.x; y = vec.y; z = vec.z; w = vec.w; return *this;}
    bool operator == (const Vector4f& vec) const {return F_EQUAL(x, vec.x) && F_EQUAL(y, vec.y) && F_EQUAL(z, vec.z) && F_EQUAL(w, vec.w);}
    bool operator != (const Vector4f& vec) const {return !operator ==(vec);}
		bool operator <= (const Vector4f& vec) const {return x<=vec.x && y<=vec.y && z<=vec.z && w<=vec.w;}
		bool operator >= (const Vector4f& vec) const {return x>=vec.x && y>=vec.y && z>=vec.z && w>=vec.w;}
		bool operator < (const Vector4f& vec) const {return x<vec.x && y<vec.y && z<vec.z && w<vec.w;}
		bool operator > (const Vector4f& vec) const {return x>vec.x && y>vec.y && z>vec.z && w>vec.w;}

    Vector4f operator + (const Vector4f& vec) const{return Vector4f(x+vec.x, y+vec.y, z+vec.z, w+vec.w);}
    const Vector4f& operator += (const Vector4f& vec) {return (*this = *this+vec);}
    Vector4f operator - (const Vector4f& vec) const{return Vector4f(x-vec.x, y-vec.y, z-vec.z, w-vec.w);}
    const Vector4f& operator -= (const Vector4f& vec) {return (*this = *this-vec);}
    Vector4f operator * (f32 val)const {return Vector4f(x*val, y*val, z*val, w*val);}
    const Vector4f& operator *= (f32 val) {return (*this = *this*val);}
    Vector4f operator * (const Vector4f& vec)const {return Vector4f(x*vec.x, y*vec.y, z*vec.z, w*vec.w);}
    const Vector4f& operator *= (const Vector4f& vec) {return (*this = *this*vec);}
    Vector4f operator / (f32 val)const {ASSERT(!F_ZERO(val)); return Vector4f(x/val, y/val, z/val, w/val);}
    const Vector4f& operator /= (f32 val) {return (*this = *this/val);}
    Vector4f operator / (const Vector4f& vec)const {ASSERT(!F_ZERO(vec.x) && !F_ZERO(vec.y) && !F_ZERO(vec.z) && !F_ZERO(vec.w)); return Vector4f(x/vec.x, y/vec.y, z/vec.z, w/vec.w);}
    const Vector4f& operator /= (const Vector4f& vec) {return (*this = *this/vec);}
    Vector4f operator - () const {return Vector4f(-x, -y, -z, -w);}

    friend Vector4f operator + (f32 val, const Vector4f& vec){return Vector4f(val+vec.x, val+vec.y, val+vec.z, val+vec.w);}
    friend Vector4f operator - (f32 val, const Vector4f& vec){return Vector4f(val-vec.x, val-vec.y, val-vec.z, val-vec.w);}
    friend Vector4f operator * (f32 val, const Vector4f& vec){return Vector4f(val*vec.x, val*vec.y, val*vec.z, val*vec.w);}
    friend Vector4f operator / (f32 val, const Vector4f& vec){ASSERT(!F_ZERO(vec.x) && !F_ZERO(vec.y) && !F_ZERO(vec.z) && !F_ZERO(vec.w)); return Vector4f(val/vec.x, val/vec.y, val/vec.z, val/vec.w);}

    bool isNan() {return Math::isNan(x) || Math::isNan(y) || Math::isNan(z) || Math::isNan(w);}
    bool empty() const {return F_ZERO(x*x+y*y+z*z+w*w);}
    void clear() {x=0.f; y=0.f; z=0.f; w=0.f;}
    void setZero() {clear();}
    void set(f32 fx, f32 fy, f32 fz, f32 fw) {x = fx; y = fy; z = fz; w = fw;}
    void set(const Vector4f& vec) {x = vec.x; y = vec.y; z = vec.z; w = vec.w;}
    void swap(Vector4f& vec) {SWAP(x, vec.x); SWAP(y, vec.y); SWAP(z, vec.z); SWAP(w, vec.w);}

    Vector4f normalizeEx() const {Vector4f vec = *this; vec.normalize(); return vec;}
    void length(f32 len) {normalize(); *this *= len;}
    f32 length() const {return empty() ? 0.f : Math::sqrt(lengthSqr());}
    f32 lengthSqr() const {return x*x + y*y + z*z + w*w;}
    f32 distance(const Vector4f& vec) const {return (*this - vec).length();}
    f32 distanceSqr(const Vector4f& vec) const {return (*this - vec).lengthSqr();}
    void makeFloor(const Vector4f& vec) {if(vec.x < x) x = vec.x; if(vec.y < y) y = vec.y; if(vec.z < z) z = vec.z; if(vec.w < w) w = vec.w;}
    void makeCeil(const Vector4f& vec) {if(vec.x > x) x = vec.x; if(vec.y > y) y = vec.y; if(vec.z > z) z = vec.z; if(vec.w > w) w = vec.w;}
    f32 dot(const Vector4f& vec) const {return x*vec.x + y*vec.y + z*vec.z + w*vec.w;}

    f32 normalize();
  };
}

#endif //__SIX_VECTOR4_H_INCLUDE__