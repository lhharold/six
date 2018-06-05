#ifndef __SIX_COMMON_H_INCLUDE__
#define __SIX_COMMON_H_INCLUDE__

namespace six {

    enum FrameBufferType {
        FBT_COLOUR  = 0x1,
        FBT_DEPTH   = 0x2,
        FBT_STENCIL = 0x4
    };
}

#endif //__SIX_COMMON_H_INCLUDE__