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
    bool operator == (const Vector2f& vec) const {return FEQUAL(x, vec.x) && FEQUAL(y, vec.y);}
    bool operator != (const Vector2f& vec) const {return !operator ==(vec);}
		bool operator<=(const Vector2f& vec) const { return x<=vec.x && y<=vec.y; };
		bool operator>=(const Vector2f& vec) const { return x>=vec.x && y>=vec.y; };
		bool operator<(const Vector2f& vec) const { return x<vec.x && y<vec.y; };
		bool operator>(const Vector2f& vec) const { return x>vec.x && y>vec.y; };

    Vector2f operator + (const Vector2f& vec) const{return Vector2f(x+vec.x, y+vec.y);}
    Vector2f& operator += (const Vector2f& vec) {return (*this = *this+vec);}
    Vector2f operator - (const Vector2f& vec) const{return Vector2f(x-vec.x, y-vec.y);}
    Vector2f& operator -= (const Vector2f& clr) {return (*this = *this-clr);}
    Vector2f operator * (f32 val)const {return Vector2f(x*val, y*val);}
    Vector2f& operator *= (f32 val) {return (*this = *this*val);}
    Vector2f operator * (const Vector2f& vec)const {return Vector2f(x*vec.x, y*vec.y);}
    Vector2f& operator *= (const Vector2f& vec) {return (*this = *this*vec);}
    Vector2f operator / (f32 val)const {ASSERT(!FZERO(val)); return Vector2f(x/val, y/val);}
    Vector2f& operator /= (f32 val) {return (*this = *this/val);}
    Vector2f operator / (const Vector2f& vec)const {ASSERT(!FZERO(vec.x) && !FZERO(vec.y)); return Vector2f(x/vec.x, y/vec.y);}
    Vector2f& operator /= (const Vector2f& vec) {return (*this = *this/vec);}
    Vector2f operator - () const {return Vector2f(-x, -y);}

    bool empty() const {return FZERO(x*x+y*y);}
    void clear() const {x=0.f; y=0.f;}
    void setZero() const {clear();}
    void set(f32 fx, f32 fy) {x = fx; y = fy;}
    void set(const Vector2f& other) {x = other.x; y = other.y;}

    inline const Vector2f& normalize();
    inline void resize(f32 len);
    void length(f32 len) {normalize(); *this *= len;}
    f32 length() {return empty() ? 0.f : fastSqrt(lengthSqr());}
    f32 lengthSqr() {return x*x + y*y;}
    void min(const Vector2f& vec) {if(vec.x < x) x = vec.x; if(vec.y < y) y = vec.y;}
    void max(const Vector2f& vec) {if(vec.x > x) x = vec.x; if(vec.y > y) y = vec.y;}
    f32 dot(const Vector2f& vec) {return x*vec.x + y*vec.y;}
    f32 cross(const Vector2f& vec) {return x*vec.y - y*vec.x;}
    Vector2f perpendicular() {return Vector2f(-y, x);}
  };
  
  inline const Vector2f& Vector2f::normalize() {
    ASSERT(!empty());
    f32 inv_dist = fastInvSqrt(x*x + y*y);
    x *= inv_dist;
    y *= inv_dist;
    return *this;
  }

  inline void Vector2f::resize(f32 len) {
    if(empty())
      return;
    if(FZERO(len))
      return clear();
		f32 distsqr = x*x + y*y;
		float r = len*fastInvSqrt(distsqr);
		x = x*r;
		y = y*r;
  }

  class Vector3f {
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

    const Vector2f& operator = (const Vector2f& vec) {x = vec.x; y = vec.y; return *this;}
    bool operator == (const Vector2f& vec) const {return FEQUAL(x, vec.x) && FEQUAL(y, vec.y);}
    bool operator != (const Vector2f& vec) const {return !operator ==(vec);}
    f32 operator[] (u8 i) {return value[i];}
    Vector2f operator + (const Vector2f& vec) const{return Vector2f(x+vec.x, y+vec.y);}
    Vector2f& operator += (const Vector2f& vec) {return (*this = *this+vec);}
    Vector2f operator - (const Vector2f& vec) const{return Vector2f(x-vec.x, y-vec.y);}
    Vector2f& operator -= (const Vector2f& clr) {return (*this = *this-clr);}
    Vector2f operator * (f32 val)const {return Vector2f(x*val, y*val);}
    Vector2f& operator *= (f32 val) {return (*this = *this*val);}
    Vector2f operator * (const Vector2f& vec)const {return Vector2f(x*vec.x, y*vec.y);}
    Vector2f& operator *= (const Vector2f& vec) {return (*this = *this*vec);}
  };
}

#endif //__SIX_VECTOR_H_INCLUDE__