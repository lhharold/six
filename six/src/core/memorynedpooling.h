#ifndef __SIX_MEMORYNEDPOOLING_H_INCLUDE__
#define __SIX_MEMORYNEDPOOLING_H_INCLUDE__

namespace six {
  class NedPooling {
  public:
    static void* allocBytes(u32 count, const char* file, int line, const char* func);
    static void deallocBytes(void* ptr);
    static void* allocBytesAligned(u32 align, u32 count, const char* file, int line, const char* func);
    static void deallocBytesAligned(u32 align, void* ptr);
  };

  class NedPoolingPolicy {
  public:
    static inline void* allocBytes(u32 count, const char* file = NULL, int line = 0, const char* func = NULL) {
      return NedPooling::allocBytes(count, file, line, func);
    }
    static inline void deallocBytes(void* ptr) {
      NedPooling::deallocBytes(ptr);
    }
    static inline u32 getMaxAllocSize() {return U32_MAX_VALUE;}
  private:
    NedPoolingPolicy() {}
  };

  template <u32 Alignment = 0>
  class NedPoolingAlignedPolicy {
  public:
    typedef int IsValidAlignment[Alignment <= 128 && ((Alignment & (Alignment-1)) == 0) ? +1 : -1];

    static inline void* allocateBytes(u32 count, const char* file = 0, int line = 0, const char* func = 0) {
      return NedPooling::allocBytesAligned(Alignment, count, file, line, func);
    }
    static inline void deallocateBytes(void* ptr) {
      NedPooling::deallocBytesAligned(Alignment, ptr);
    }
    static inline u32 getMaxAllocSize() {return U32_MAX_VALUE;}
  private:
    NedPoolingAlignedPolicy() {}
  };
}

#endif //__SIX_MEMORYNEDPOOLING_H_INCLUDE__