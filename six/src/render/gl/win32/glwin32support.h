#ifndef __SIX_GLWIN32SUPPORT_H_INCLUDE__
#define __SIX_GLWIN32SUPPORT_H_INCLUDE__

#include "gl/glsupport.h"

namespace six {
  class Win32Window;
	class GLWin32Support : public GLSupport {
	public:
		GLWin32Support();
		virtual ~GLWin32Support();

		virtual RenderWindow* createWindow(GLRenderSystem* renderSystem, bool autoWindow, const char* windowName);
		virtual RenderWindow* newWindow(const char* windowName, u32 width, u32 height, bool fullScreen);

		virtual void start();
		virtual void stop();
	protected:
		void initializeWGL();
		static LRESULT CALLBACK dummyWndProc(HWND hwnd, UINT umsg, WPARAM wp, LPARAM lp);

		Win32Window* mInitWindow;
	};

	inline GLSupport* getGLSupport() {
		return NEW GLWin32Support();
	}

}

#endif //__SIX_GLWIN32SUPPORT_H_INCLUDE__