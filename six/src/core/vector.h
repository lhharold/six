#ifndef __SIX_VECTOR_H_INCLUDE__
#define __SIX_VECTOR_H_INCLUDE__

namespace six {
  class Vector2f {
  public:
    static const Vector2f Zero = Vector2f();
    static const Vector2f One = Vector2f(1.f, 1.f);
    static const Vector2f UnitX = Vector2f(1.f, 0.f);
    static const Vector2f UnitY = Vector2f(0.f, 1.f);
    static const Vector2f NegativeUnitX = Vector2f(-1.f, 0.f);
    static const Vector2f NegativeUnitY = Vector2f(0.f, -1.f);
    union {
      struct {
        f32 x;
        f32 y;
      };
      struct {
        f32 start;
        f32 end;
      };
      f32 value[2];
    };

    Vector2f() : x(0.f), y(0.f) {}
    explicit Vector2f(f32 val[2]) : x(val[0]), y(val[1]) {}
    Vector2f(f32 fx, f32 fy) : x(fx), y(fy) {}
    Vector2f(const Vector2f& vec) : x(vec.x), y(vec.y) {}

    f32 operator[] (u8 i) {ASSERT(i>=0 && i <2); return value[i];}
    const Vector2f& operator = (const Vector2f& vec) {x = vec.x; y = vec.y; return *this;}
    bool operator == (const Vector2f& vec) const {return F_EQUAL(x, vec.x) && F_EQUAL(y, vec.y);}
    bool operator != (const Vector2f& vec) const {return !operator ==(vec);}
		bool operator <= (const Vector2f& vec) const {return x<=vec.x && y<=vec.y;}
		bool operator >= (const Vector2f& vec) const {return x>=vec.x && y>=vec.y;}
		bool operator < (const Vector2f& vec) const {return x<vec.x && y<vec.y;}
		bool operator > (const Vector2f& vec) const {return x>vec.x && y>vec.y;}

    Vector2f operator + (const Vector2f& vec) const{return Vector2f(x+vec.x, y+vec.y);}
    Vector2f& operator += (const Vector2f& vec) {return (*this = *this+vec);}
    Vector2f operator - (const Vector2f& vec) const{return Vector2f(x-vec.x, y-vec.y);}
    Vector2f& operator -= (const Vector2f& clr) {return (*this = *this-clr);}
    Vector2f operator * (f32 val)const {return Vector2f(x*val, y*val);}
    Vector2f& operator *= (f32 val) {return (*this = *this*val);}
    Vector2f operator * (const Vector2f& vec)const {return Vector2f(x*vec.x, y*vec.y);}
    Vector2f& operator *= (const Vector2f& vec) {return (*this = *this*vec);}
    Vector2f operator / (f32 val)const {ASSERT(!F_ZERO(val)); return Vector2f(x/val, y/val);}
    Vector2f& operator /= (f32 val) {return (*this = *this/val);}
    Vector2f operator / (const Vector2f& vec)const {ASSERT(!F_ZERO(vec.x) && !F_ZERO(vec.y)); return Vector2f(x/vec.x, y/vec.y);}
    Vector2f& operator /= (const Vector2f& vec) {return (*this = *this/vec);}
    Vector2f operator - () const {return Vector2f(-x, -y);}

    bool isNan() {return Math::isNan(x) || Math::isNan(y);}
    bool empty() const {return F_ZERO(x*x+y*y);}
    void clear() const {x=0.f; y=0.f;}
    void setZero() const {clear();}
    void set(f32 fx, f32 fy) {x = fx; y = fy;}
    void set(const Vector2f& vec) {x = vec.x; y = vec.y;}
    void swap(Vector2f& vec) {SWAP(x, vec.x); SWAP(y, vec.y);}

    Vector2f normalizeEx() const {Vector2f vec = *this; vec.normalize(); return vec;}
    void length(f32 len) {normalize(); *this *= len;}
    f32 length() const {return empty() ? 0.f : Math::sqrt(lengthSqr());}
    f32 lengthSqr() const {return x*x + y*y;}
    f32 distance(const Vector2f& vec) const {return (*this - vec).length();}
    f32 distanceSqr(const Vector2f& vec) const {return (*this - vec).lengthSqr();}
    void makeFloor(const Vector2f& vec) {if(vec.x < x) x = vec.x; if(vec.y < y) y = vec.y;}
    void makeCeil(const Vector2f& vec) {if(vec.x > x) x = vec.x; if(vec.y > y) y = vec.y;}
    f32 dot(const Vector2f& vec) const {return x*vec.x + y*vec.y;}
    f32 cross(const Vector2f& vec) const {return x*vec.y - y*vec.x;}

    Vector2f perpendicular() const {return Vector2f(-y, x);} //´¹Ö±
    Vector2f reflect(const Vector2f& normal) const {return Vector2f(*this - (2.f*dot(normal)*normal));} //·´Éä

    inline f32 normalize();
    inline void resize(f32 len);
  };
  
  inline f32 Vector2f::normalize() {
    f32 len = length();
    if(len > F_PRECISION) {
      f32 invLen = 1.f / len;
      x *= invLen;
      y *= invLen;
    }
    return len;
  }

  inline void Vector2f::resize(f32 len) {
    if(empty())
      return;
    if(F_ZERO(len))
      return clear();
		f32 distsqr = x*x + y*y;
    f32 r = len*Math::invsqrt(distsqr);
		x = x*r;
		y = y*r;
  }

  class Vector3f {
  public:
    static const Vector3f Zero = Vector3f();
    static const Vector3f One = Vector3f(1.f, 1.f, 1.f);
    static const Vector3f UnitX = Vector3f(1.f, 0.f, 0.f);
    static const Vector3f UnitY = Vector3f(0.f, 1.f, 0.f);
    static const Vector3f UnitZ = Vector3f(0.f, 0.f, 1.f);
    static const Vector3f NegativeUnitX = Vector3f(-1.f, 0.f, 0.f);
    static const Vector3f NegativeUnitY = Vector3f(0.f, -1.f, 0.f);
    static const Vector3f NegativeUnitZ = Vector3f(0.f, 0.f, -1.f);
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

    f32 operator[] (u8 i) {ASSERT(i>=0 && i <3); return value[i];}
    const Vector3f& operator = (const Vector3f& vec) {x = vec.x; y = vec.y; z = vec.z; return *this;}
    bool operator == (const Vector3f& vec) const {return F_EQUAL(x, vec.x) && F_EQUAL(y, vec.y) && F_EQUAL(z, vec.z);;}
    bool operator != (const Vector3f& vec) const {return !operator ==(vec);}
		bool operator <= (const Vector3f& vec) const {return x<=vec.x && y<=vec.y && z<=vec.z;}
		bool operator >= (const Vector3f& vec) const {return x>=vec.x && y>=vec.y && z>=vec.z;}
		bool operator < (const Vector3f& vec) const {return x<vec.x && y<vec.y && z<vec.z;}
		bool operator > (const Vector3f& vec) const {return x>vec.x && y>vec.y && z>vec.z;}

    Vector3f operator + (const Vector3f& vec) const{return Vector3f(x+vec.x, y+vec.y, z+vec.z);}
    Vector3f& operator += (const Vector3f& vec) {return (*this = *this+vec);}
    Vector3f operator - (const Vector3f& vec) const{return Vector3f(x-vec.x, y-vec.y, z-vec.z);}
    Vector3f& operator -= (const Vector3f& clr) {return (*this = *this-clr);}
    Vector3f operator * (f32 val)const {return Vector3f(x*val, y*val, z*val);}
    Vector3f& operator *= (f32 val) {return (*this = *this*val);}
    Vector3f operator * (const Vector3f& vec)const {return Vector3f(x*vec.x, y*vec.y, z*vec.z);}
    Vector3f& operator *= (const Vector3f& vec) {return (*this = *this*vec);}
    Vector3f operator / (f32 val)const {ASSERT(!F_ZERO(val)); return Vector3f(x/val, y/val, z/val);}
    Vector3f& operator /= (f32 val) {return (*this = *this/val);}
    Vector3f operator / (const Vector3f& vec)const {ASSERT(!F_ZERO(vec.x) && !F_ZERO(vec.y) && !F_ZERO(vec.z)); return Vector3f(x/vec.x, y/vec.y, z/vec.z);}
    Vector3f& operator /= (const Vector3f& vec) {return (*this = *this/vec);}
    Vector3f operator - () const {return Vector3f(-x, -y, -z);}

    bool isNan() {return Math::isNan(x) || Math::isNan(y) || Math::isNan(z);}
    bool empty() const {return F_ZERO(x*x+y*y+z*z);}
    void clear() const {x=0.f; y=0.f; z=0.f;}
    void setZero() const {clear();}
    void set(f32 fx, f32 fy, f32 fz) {x = fx; y = fy; z = fz;}
    void set(const Vector3f& vec) {x = vec.x; y = vec.y; z = vec.z}
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

    inline f32 normalize();
    inline Vector3f perpendicular() const; //´¹Ö±
    inline void resize(f32 len);
    //inline Quaternion getRotationTo(const Vector3f& vec, const Vector3f& fallbackAxis = Vector3f::Zero);
    inline void angleBetween(const Vector3f& vec);
  };

  inline f32 Vector3f::normalize() {
    f32 len = length();
    if(len > F_PRECISION) {
      f32 invLen = 1.f / len;
      x *= invLen;
      y *= invLen;
      z *= invLen;
    }
    return len;
  }
  inline void Vector3f::resize(f32 len) {
    if(empty())
      return;
    if(F_ZERO(len))
      return clear();
		f32 distsqr = x*x + y*y + z*z;
    f32 r = len*Math::invsqrt(distsqr);
		x = x*r;
		y = y*r;
    z = z*r;
  }
  inline Vector3f Vector3f::perpendicular() {
    static const f32 fSquareZero = f32(F_PRECISION * F_PRECISION);
    Vector3f perp = cross(UnitX);
    if(perp.lengthSqr() < fSquareZero )
      perp = cross(UnitY);
    perp.normalize();
    return perp;
  }
#if 0
  inline Quaternion getRotationTo(const Vector3f& vec, const Vector3f& fallbackAxis = Vector3f::Zero) {
  }
  inline f32 Vector3f::angleBetween(const Vector3f& vec) {
			f32 lenProduct = length() * vec.length();
			if(lenProduct < F_PRECISION)
				lenProduct = F_PRECISION;
			f32 f = dot(vec) / lenProduct;
			f = CLAMP(f, -1.f, 1.f);
			return Math::acos(f);
  }
#endif
}

#endif //__SIX_VECTOR_H_INCLUDE__