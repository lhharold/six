#include "core.h"
namespace six {
  DEFINE_STATIC_LOG(Mesh);
  Mesh::Mesh() {
    mVertexData.pointData.malloc(3 * sizeof(Vector3f));
    mVertexData.pointData.setValue(0, Vector3f(0, 0, 0));
    mVertexData.pointData.setValue(1, Vector3f(1, 0, 0));
    mVertexData.pointData.setValue(2, Vector3f(0.5, 1, 0));
    mIndexData.indexData.malloc(3 * sizeof(u32));
    mIndexData.indexData.setValue(0, 0);
    mIndexData.indexData.setValue(0, 1);
    mIndexData.indexData.setValue(0, 2);
    mIndexData.faceNum = 1;
  }
  Mesh::~Mesh() {
    mVertexData.pointData.free();
    mIndexData.indexData.free();
  }
}