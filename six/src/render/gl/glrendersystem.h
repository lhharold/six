#ifndef __SIX_GLRENDERSYSTEM_H_INCLUDE__
#define __SIX_GLRENDERSYSTEM_H_INCLUDE__

#include "rendersystem.h"
#include "glsupport.h"

namespace six {
	class GLRenderSystem : public RenderSystem {
	public:
		GLRenderSystem();
		virtual ~GLRenderSystem();

		virtual RenderWindow* startup(bool autoWindow, const char* windowName);
		virtual RenderWindow* createWindow(const char* windowName, u32 width, u32 height, bool fullScreen);
		virtual void shutdown();
	protected:
		GLSupport mGLSupport;
	};
}
#endif //__SIX_GLRENDERSYSTEM_H_INCLUDE__