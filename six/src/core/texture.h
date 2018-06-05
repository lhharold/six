#ifndef __SIX_TEXTURE_H_INCLUDE__
#define __SIX_TEXTURE_H_INCLUDE__

namespace six {
  class Texture {
  DECLARE_STATIC_LOG();
  public:
    Texture();
    ~Texture();
  private:
    String mFullName;
    u32 mWidth;
    u32 mHeight;
  };
}

#endif //__SIX_TEXTURE_H_INCLUDE__