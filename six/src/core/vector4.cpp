#include "core.h"
namespace six {
  const Vector4f Vector4f::Zero = Vector4f();
  const Vector4f Vector4f::One = Vector4f(1.f, 1.f, 1.f, 1.f);

  f32 Vector4f::normalize() {
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
}