#ifndef __SIX_VECTOR3_H_INCLUDE__
#define __SIX_VECTOR3_H_INCLUDE__

namespace six {
  class Vector3f {
  public:
    static const Vector3f Zero;
    static const Vector3f One;
    static const Vector3f UnitX;
    static const Vector3f UnitY;
    static const Vector3f UnitZ;
    static const Vector3f NegativeUnitX;
    static const Vector3f NegativeUnitY;
    static const Vector3f NegativeUnitZ;
    union {
      struct {
        f32 x;
        f32 y;
        f32 z;
      };
      f32 value[3];
    };

    Vector3f() : x(0.f), y(0.f), z(0.f) {}
    explicit Vector3f(f32 val[3]) : x(val[0]), y(val[1]), z(val[2]) {}
    Vector3f(f32 fx, f32 fy, f32 fz) : x(fx), y(fy), z(fz) {}
    Vector3f(const Vector3f& vec) : x(vec.x), y(vec.y), z(vec.z) {}

    f32 operator[] (u8 i) {ASSERT(i<3); return value[i];}
    const Vector3f& operator = (const Vector3f& vec) {x = vec.x; y = vec.y; z = vec.z; return *this;}
    bool operator == (const Vector3f& vec) const {return F_EQUAL(x, vec.x) && F_EQUAL(y, vec.y) && F_EQUAL(z, vec.z);}
    bool operator != (const Vector3f& vec) const {return !operator ==(vec);}
		bool operator <= (const Vector3f& vec) const {return x<=vec.x && y<=vec.y && z<=vec.z;}
		bool operator >= (const Vector3f& vec) const {return x>=vec.x && y>=vec.y && z>=vec.z;}
		bool operator < (const Vector3f& vec) const {return x<vec.x && y<vec.y && z<vec.z;}
		bool operator > (const Vector3f& vec) const {return x>vec.x && y>vec.y && z>vec.z;}

    Vector3f operator + (const Vector3f& vec) const{return Vector3f(x+vec.x, y+vec.y, z+vec.z);}
    const Vector3f& operator += (const Vector3f& vec) {return (*this = *this+vec);}
    Vector3f operator - (const Vector3f& vec) const{return Vector3f(x-vec.x, y-vec.y, z-vec.z);}
    const Vector3f& operator -= (const Vector3f& vec) {return (*this = *this-vec);}
    Vector3f operator * (f32 val)const {return Vector3f(x*val, y*val, z*val);}
    const Vector3f& operator *= (f32 val) {return (*this = *this*val);}
    Vector3f operator * (const Vector3f& vec)const {return Vector3f(x*vec.x, y*vec.y, z*vec.z);}
    const Vector3f& operator *= (const Vector3f& vec) {return (*this = *this*vec);}
    Vector3f operator / (f32 val)const {ASSERT(!F_ZERO(val)); return Vector3f(x/val, y/val, z/val);}
    const Vector3f& operator /= (f32 val) {return (*this = *this/val);}
    Vector3f operator / (const Vector3f& vec)const {ASSERT(!F_ZERO(vec.x) && !F_ZERO(vec.y) && !F_ZERO(vec.z)); return Vector3f(x/vec.x, y/vec.y, z/vec.z);}
    const Vector3f& operator /= (const Vector3f& vec) {return (*this = *this/vec);}
    Vector3f operator - () const {return Vector3f(-x, -y, -z);}

    friend Vector3f operator + (f32 val, const Vector3f& vec){return Vector3f(val+vec.x, val+vec.y, val+vec.z);}
    friend Vector3f operator - (f32 val, const Vector3f& vec){return Vector3f(val-vec.x, val-vec.y, val-vec.z);}
    friend Vector3f operator * (f32 val, const Vector3f& vec){return Vector3f(val*vec.x, val*vec.y, val*vec.z);}
    friend Vector3f operator / (f32 val, const Vector3f& vec){ASSERT(!F_ZERO(vec.x) && !F_ZERO(vec.y) && !F_ZERO(vec.z)); return Vector3f(val/vec.x, val/vec.y, val/vec.z);}

    bool isNan() {return Math::isNan(x) || Math::isNan(y) || Math::isNan(z);}
    bool empty() const {return F_ZERO(x*x+y*y+z*z);}
    void clear() {x=0.f; y=0.f; z=0.f;}
    void setZero() {clear();}
    void set(f32 fx, f32 fy, f32 fz) {x = fx; y = fy; z = fz;}
    void set(const Vector3f& vec) {x = vec.x; y = vec.y; z = vec.z;}
    void swap(Vector3f& vec) {SWAP(x, vec.x); SWAP(y, vec.y); SWAP(z, vec.z);}

    Vector3f normalizeEx() const {Vector3f vec = *this; vec.normalize(); return vec;}
    void length(f32 len) {normalize(); *this *= len;}
    f32 length() const {return empty() ? 0.f : Math::sqrt(lengthSqr());}
    f32 lengthSqr() const {return x*x + y*y + z*z;}
    f32 distance(const Vector3f& vec) const {return (*this - vec).length();}
    f32 distanceSqr(const Vector3f& vec) const {return (*this - vec).lengthSqr();}
    void makeFloor(const Vector3f& vec) {if(vec.x < x) x = vec.x; if(vec.y < y) y = vec.y; if(vec.z < z) z = vec.z;}
    void makeCeil(const Vector3f& vec) {if(vec.x > x) x = vec.x; if(vec.y > y) y = vec.y; if(vec.z > z) z = vec.z;}
    f32 dot(const Vector3f& vec) const {return x*vec.x + y*vec.y + z*vec.z;}
    Vector3f cross(const Vector3f& vec) const {return Vector3f(y*vec.z - z*vec.y, z*vec.x - x*vec.z, x*vec.y - y*vec.x);}

    Vector3f reflect(const Vector3f& normal) const {return Vector3f(*this - (2.f*dot(normal)*normal));}

    f32 normalize();
    Vector3f perpendicular() const; //´¹Ö±
    void resize(f32 len);
    Quaternion getRotationTo(const Vector3f& vec, const Vector3f& fallbackAxis = Vector3f::Zero);
    f32 angleBetween(const Vector3f& vec);
  };
}

#endif //__SIX_VECTOR3_H_INCLUDE__
