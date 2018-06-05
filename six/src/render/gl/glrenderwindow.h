#ifndef __SIX_GLRENDERWINDOW_H_INCLUDE__
#define __SIX_GLRENDERWINDOW_H_INCLUDE__

#include "renderwindow.h"
#include "glcontext.h"

namespace six {
	class GLRenderWindow : public RenderWindow {
	public:
		GLRenderWindow(const char* name) : RenderWindow(name) {}
		virtual ~GLRenderWindow() {}

		virtual GLContext* getContext() {return &mGLContext;}
	protected:
		GLContext mGLContext;
	};
}
#endif //__SIX_GLRENDERWINDOW_H_INCLUDE__