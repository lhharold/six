#ifndef __SIX_RENDERWINDOW_H_INCLUDE__
#define __SIX_RENDERWINDOW_H_INCLUDE__

#include "rendertarget.h"

namespace six {
	class RenderWindow : public RenderTarget {
	public:
		RenderWindow(const char* name) : RenderTarget(name) {}
		virtual ~RenderWindow() {}
	};
}

#endif //__SIX_RENDERWINDOW_H_INCLUDE__