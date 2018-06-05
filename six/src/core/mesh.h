#ifndef __SIX_MESH_H_INCLUDE__
#define __SIX_MESH_H_INCLUDE__

#include "util.h"

namespace six {
  class aabbox3f {
  public:
  };
  class Mesh {
  DECLARE_STATIC_LOG();
  public:
    Mesh();
    virtual ~Mesh();
  protected:
    VertexData mVertexData;
    IndexData mIndexData;
    Material mMtl;
  };
}

#endif //__SIX_MESH_H_INCLUDE__