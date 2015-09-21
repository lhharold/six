#ifndef __SIX_GLWIN32SUPPORT_H_INCLUDE__
#define __SIX_GLWIN32SUPPORT_H_INCLUDE__

#include "gl/glsupport.h"

namespace six {

	class GLWin32Support : public GLSupport {
	public:
		GLWin32Support();
		virtual ~GLWin32Support();

		virtual GLRenderWindow* createWindow(GLRenderSystem* renderSystem, bool autoWindow, const char* windowName);
		virtual GLRenderWindow* newWindow(const char* windowName, u32 width, u32 height, bool fullScreen);

		virtual void startup();
	};

	inline GLSupport* getGLSupport() {
		return NEW GLWin32Support();
	}

}

#endif //__SIX_GLWIN32SUPPORT_H_INCLUDE__