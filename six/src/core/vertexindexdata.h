#ifndef __SIX_VERTEXINDEXDATA_H_INCLUDE__
#define __SIX_VERTEXINDEXDATA_H_INCLUDE__

namespace six {
  template<u32 SIZE> class _Data {
  public:
    void malloc(u32 num) {
      SAFE_DEL(data);
      data = NEW u8(num * sizeof(u8));
    }
    void free() {
      SAFE_DEL(data);
    }
    u8* getData() {return data;}
    u32 getCount() {return num;}
    template<class T> const T& getValue(u32 index) const {
      return *(T*)util::stepPointer(data, index*SIZE);
    }
    template<class T> void setValue(u32 index, const T& val) {
      MEMCPY(util::stepPointer(data, index*SIZE), &val, SIZE);
    }
  private:
    u8* data;
    u32 num;
  };

  typedef _Data<sizeof(Vector3f)> VEC3Data;
  typedef _Data<sizeof(Vector2f)> VEC2Data;
  typedef _Data<sizeof(Color4f)>  CLR4Data;
  typedef _Data<sizeof(u8)>       U8Data;
  typedef _Data<sizeof(u16)>      U16Data;
  typedef _Data<sizeof(u32)>      U32Data;
  typedef _Data<sizeof(f32)>      F32Data;

  class VertexData {
    DECLARE_STATIC_LOG();
  public:
    VertexData();
    ~VertexData();
    VEC3Data pointData;
    VEC3Data normalData;
    VEC3Data tangentData;
    VEC3Data binormalData;
    CLR4Data colorData;
    VEC2Data texUVData;
    U8Data boneIndexData;
    F32Data boneWeightData;
  };

  class IndexData {
    DECLARE_STATIC_LOG();
  public:
    IndexData();
    ~IndexData();

    U32Data indexData;
    u32 faceNum;
    u32 vbo;
    //aabbox3f aabb;
  };
}

#endif //__SIX_VERTEXINDEXDATA_H_INCLUDE__