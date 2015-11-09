#ifndef __SIX_VECTOR2_H_INCLUDE__
#define __SIX_VECTOR2_H_INCLUDE__

namespace six {
  class Vector2f {
  public:
    static const Vector2f Zero;
    static const Vector2f One;
    static const Vector2f UnitX;
    static const Vector2f UnitY;
    static const Vector2f NegativeUnitX;
    static const Vector2f NegativeUnitY;
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

    f32 operator[] (u8 i) const {ASSERT(i <2); return value[i];}
    f32& operator[] (u8 i) {ASSERT(i <2); return value[i];}
    const Vector2f& operator = (const Vector2f& vec) {x = vec.x; y = vec.y; return *this;}
    bool operator == (const Vector2f& vec) const {return F_EQUAL(x, vec.x) && F_EQUAL(y, vec.y);}
    bool operator != (const Vector2f& vec) const {return !operator ==(vec);}
		bool operator <= (const Vector2f& vec) const {return x<=vec.x && y<=vec.y;}
		bool operator >= (const Vector2f& vec) const {return x>=vec.x && y>=vec.y;}
		bool operator < (const Vector2f& vec) const {return x<vec.x && y<vec.y;}
		bool operator > (const Vector2f& vec) const {return x>vec.x && y>vec.y;}

    Vector2f operator + (const Vector2f& vec) const{return Vector2f(x+vec.x, y+vec.y);}
    const Vector2f& operator += (const Vector2f& vec) {return (*this = *this+vec);}
    Vector2f operator - (const Vector2f& vec) const{return Vector2f(x-vec.x, y-vec.y);}
    const Vector2f& operator -= (const Vector2f& vec) {return (*this = *this-vec);}
    Vector2f operator * (f32 val)const {return Vector2f(x*val, y*val);}
    const Vector2f& operator *= (f32 val) {return (*this = *this*val);}
    Vector2f operator * (const Vector2f& vec)const {return Vector2f(x*vec.x, y*vec.y);}
    const Vector2f& operator *= (const Vector2f& vec) {return (*this = *this*vec);}
    Vector2f operator / (f32 val)const {ASSERT(!F_ZERO(val)); return Vector2f(x/val, y/val);}
    const Vector2f& operator /= (f32 val) {return (*this = *this/val);}
    Vector2f operator / (const Vector2f& vec)const {ASSERT(!F_ZERO(vec.x) && !F_ZERO(vec.y)); return Vector2f(x/vec.x, y/vec.y);}
    const Vector2f& operator /= (const Vector2f& vec) {return (*this = *this/vec);}
    Vector2f operator - () const {return Vector2f(-x, -y);}

    friend Vector2f operator + (f32 val, const Vector2f& vec){return Vector2f(val+vec.x, val+vec.y);}
    friend Vector2f operator - (f32 val, const Vector2f& vec){return Vector2f(val-vec.x, val-vec.y);}
    friend Vector2f operator * (f32 val, const Vector2f& vec){return Vector2f(val*vec.x, val*vec.y);}
    friend Vector2f operator / (f32 val, const Vector2f& vec){ASSERT(!F_ZERO(vec.x) && !F_ZERO(vec.y)); return Vector2f(val/vec.x, val/vec.y);}

    bool isNan() {return Math::isNan(x) || Math::isNan(y);}
    bool empty() const {return F_ZERO(x*x+y*y);}
    void clear() {x=0.f; y=0.f;}
    void setZero() {clear();}
    void set(f32 fx, f32 fy) {x = fx; y = fy;}
    void set(const Vector2f& vec) {x = vec.x; y = vec.y;}
    void swap(Vector2f& vec) {SWAP(x, vec.x); SWAP(y, vec.y);}

    f32 normalize();
    void resize(f32 len);

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

    Vector2f perpendicular() const {return Vector2f(-y, x);} //¥π÷±
    Vector2f reflect(const Vector2f& normal) const { return Vector2f(*this - (2.f*dot(normal)*normal));} //∑¥…‰
  };
}

#endif //__SIX_VECTOR2_H_INCLUDE__
