#include "core.h"
namespace six {
  const Matrix3 Matrix3::Zero = Matrix3();
  const Matrix3 Matrix3::Identity = Matrix3(1.f, 0.f, 0.f, 0.f, 1.f, 0.f, 0.f, 0.f, 1.f);
}