#ifndef __SIX_RENDERWINDOW_H_INCLUDE__
#define __SIX_RENDERWINDOW_H_INCLUDE__

#include "rendertarget.h"

namespace six {
	class RenderWindow : public RenderTarget {
	public:
		RenderWindow(const char* name) : RenderTarget(name), mFullScreen(false), mLeft(-1), mTop(-1) {}
		virtual ~RenderWindow() {}

    virtual void create(const char* name, u32 width, u32 height, bool fullScreen) = 0;
  protected:
    bool mFullScreen;
    int mLeft;
    int mTop;
	};
}

#endif //__SIX_RENDERWINDOW_H_INCLUDE__
