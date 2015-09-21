#include "core.h"
#include "glwin32support.h"

namespace six {
	GLWin32Support::GLWin32Support(){

	}
	GLWin32Support::~GLWin32Support() {

	}
	GLRenderWindow* GLWin32Support::createWindow(GLRenderSystem* renderSystem, bool autoWindow, const char* windowName){
		return NULL;
	}
	GLRenderWindow* GLWin32Support::newWindow(const char* windowName, u32 width, u32 height, bool fullScreen) {
		return NULL;
	}
	void GLWin32Support::startup() {

	}
}