#include "core.h"
namespace six {
  const Quaternion Quaternion::Zero = Quaternion(0.f, 0.f, 0.f, 0.f);
  const Quaternion Quaternion::Identity = Quaternion();

  Quaternion::Quaternion(f32 angle, const Vector3f& axis) {
    fromAngleAxis(angle, axis);
  }
  f32 Quaternion::normalize() {
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
  void Quaternion::fromAngleAxis(f32 angle, const Vector3f& axis) {
    f32 halfAngle = 0.5f * angle;
    f32 s = Math::sin(halfAngle);
    x = s*axis.x;
    y = s*axis.y;
    z = s*axis.z;
    w = Math::cos(halfAngle);
    normalize();
  }
  void Quaternion::toAngleAxis(f32& angle, Vector3f& axis) {
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
}