#ifndef __SIX_RENDERWINDOW_H_INCLUDE__
#define __SIX_RENDERWINDOW_H_INCLUDE__

#include "rendertarget.h"

namespace six {
	class RenderWindow : public RenderTarget {
	public:
		RenderWindow();
		virtual ~RenderWindow() {}

    virtual void create(const char* name, u32 width, u32 height, bool fullScreen) = 0;
    virtual void destroy() = 0;

    virtual void setFullscreen(bool fullscreen, u32 width, u32 height) {}
    bool isFullscreen() {return mFullScreen;}
    void setDeactivatedOnFocusChange(bool deactive) {mAutoDeactivatedOnFocusChange = deactive;}
    bool isDeactivatedOnFocusChange() const {return mAutoDeactivatedOnFocusChange;}
    virtual void windowMovedOrResized() {}
  protected:
    bool mFullScreen;
    int mLeft;
    int mTop;
    bool mAutoDeactivatedOnFocusChange;
	};
}

#endif //__SIX_RENDERWINDOW_H_INCLUDE__
