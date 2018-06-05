#include "core.h"
namespace six {
  const f32 Math::f_precision = 1e06f;
  const f32 Math::pi = 4.f*atan(1.f);
  const f32 Math::invpi = 1.f/Math::pi;
  const f32 Math::inv180 = 1.f/180.f;
  const f32 Math::rad2deg = 180.f*Math::invpi;
  const f32 Math::deg2rad = Math::pi*Math::inv180;
  const f32 Math::piDegree = Math::Rad2Deg(Math::pi);
}
