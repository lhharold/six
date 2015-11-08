#include "core.h"
namespace six {
  const Vector3f Vector3f::Zero = Vector3f();
  const Vector3f Vector3f::One = Vector3f(1.f, 1.f, 1.f);
  const Vector3f Vector3f::UnitX = Vector3f(1.f, 0.f, 0.f);
  const Vector3f Vector3f::UnitY = Vector3f(0.f, 1.f, 0.f);
  const Vector3f Vector3f::UnitZ = Vector3f(0.f, 0.f, 1.f);
  const Vector3f Vector3f::NegativeUnitX = Vector3f(-1.f, 0.f, 0.f);
  const Vector3f Vector3f::NegativeUnitY = Vector3f(0.f, -1.f, 0.f);
  const Vector3f Vector3f::NegativeUnitZ = Vector3f(0.f, 0.f, -1.f);


  f32 Vector3f::normalize() {
    f32 len = length();
    if(len > F_PRECISION) {
      f32 invLen = 1.f / len;
      x *= invLen;
      y *= invLen;
      z *= invLen;
    }
    return len;
  }
  void Vector3f::resize(f32 len) {
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
  Vector3f Vector3f::perpendicular() const {
    static const f32 fSquareZero = f32(F_PRECISION * F_PRECISION);
    Vector3f perp = cross(UnitX);
    if(perp.lengthSqr() < fSquareZero )
      perp = cross(UnitY);
    perp.normalize();
    return perp;
  }
  //Quaternion Vector3f::getRotationTo(const Vector3f& dest, const Vector3f& fallbackAxis = Vector3f::Zero) {
  //  Quaternion q;
  //  Vector3f v0 = *this;
  //  Vector3f v1 = dest;
  //  v0.normalize();
  //  v1.normalize();

  //  f32 d = v0.dot(v1);
  //  if (d >= 1.f) {
  //    return Quaternion::Identity;
  //  }
  //  if (d < (F_PRECISION - 1.f)) {
  //    if (fallbackAxis != Vector3f::Zero) {
  //      q.fromAngleAxis(Math::piDegree, fallbackAxis);
  //    } else {
  //      Vector3f axis = Vector3f::UnitX.cross(*this);
  //      if (axis.empty()) // pick another if colinear
  //        axis = Vector3f::UnitY.cross(*this);
  //      axis.normalize();
  //      q.fromAngleAxis(Math::piDegree, axis);
  //    }
  //  } else {
  //    f32 s = Math::sqrt((1.f+d)*2.f);
  //    f32 invs = 1.f / s;
  //    Vector3f c = v0.cross(v1);
  //    q.x = c.x * invs;
  //    q.y = c.y * invs;
  //    q.z = c.z * invs;
  //    q.w = s * 0.5f;
  //    q.normalize();
  //  }
  //  return q;
  //}
 f32 Vector3f::angleBetween(const Vector3f& vec) {
    f32 lenProduct = length() * vec.length();
    if(lenProduct < F_PRECISION)
      lenProduct = F_PRECISION;
    f32 f = dot(vec) / lenProduct;
    f = CLAMP(f, -1.f, 1.f);
    return Math::acos(f);
  }
}