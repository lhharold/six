#ifndef __SIX_MATERIAL_H_INCLUDE__
#define __SIX_MATERIAL_H_INCLUDE__

namespace six {
  class Material {
  DECLARE_STATIC_LOG();
  public:
    Material();
    ~Material();

  private:
    static const u32 MAX_TEX_NUM = 8;
    Texture mTexs[MAX_TEX_NUM];
  };
}

#endif //__SIX_MATERIAL_H_INCLUDE__