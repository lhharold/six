#include "core.h"
namespace six {
  const Vector2f Vector2f::Zero = Vector2f();
  const Vector2f Vector2f::One = Vector2f(1.f, 1.f);
  const Vector2f Vector2f::UnitX = Vector2f(1.f, 0.f);
  const Vector2f Vector2f::UnitY = Vector2f(0.f, 1.f);
  const Vector2f Vector2f::NegativeUnitX = Vector2f(-1.f, 0.f);
  const Vector2f Vector2f::NegativeUnitY = Vector2f(0.f, -1.f);

  f32 Vector2f::normalize() {
    f32 len = length();
    if(len > F_PRECISION) {
      f32 invLen = 1.f / len;
      x *= invLen;
      y *= invLen;
    }
    return len;
  }

  void Vector2f::resize(f32 len) {
    if(empty())
      return;
    if(F_ZERO(len))
      return clear();
		f32 distsqr = x*x + y*y;
    f32 r = len*Math::invsqrt(distsqr);
		x = x*r;
		y = y*r;
  }
}