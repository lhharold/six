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
		void initialiseContext(RenderWindow* window);
		GLSupport mGLSupport;
		GLContext* mMainContext;
		GLContext* mActiveContext;

		union {
			struct {
				u32 mGLInitialised : 1;
			};
			u32 mFlags;
		}
	};
}
#endif //__SIX_GLRENDERSYSTEM_H_INCLUDE__