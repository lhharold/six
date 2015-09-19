#ifndef __SIX_RENDERSYSTEM_H_INCLUDE__
#define __SIX_RENDERSYSTEM_H_INCLUDE__

#include "renderwindow.h"

namespace six {

	enum RenderSystemType {
		RST_GL = 0,
		RST_GLES,
		RST_GLES20,
	};

	class RenderSystem {
	public:
		RenderSystem();
		virtual ~RenderSystem();

		virtual RenderWindow* startup(bool autoWindow, const char* windowName) = 0;
		virtual RenderWindow* createWindow(const char* windowName, u32 width, u32 height, bool fullScreen) = 0;
		virtual void shutdown() = 0;
	};
}
#endif //__SIX_RENDERSYSTEM_H_INCLUDE__