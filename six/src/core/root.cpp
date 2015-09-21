#include "core.h"
#include "gl/glrendersystem.h"

namespace six {

	Root::Root() 
		: mRender(NULL)
		, mAutoWindow(NULL)
	{
		mRender = _createRenderSystem();
	}

	Root::~Root() {
	}

	RenderWindow* Root::startup(bool autoWindow, const char* windowName) {
		mAutoWindow = mRender->startup(autoWindow, windowName);
		return mAutoWindow;
	}

	RenderWindow* Root::createWindow(const char* name, s32 width, s32 height, bool fullScreen) {
		return mRender->createWindow(name, width, height, fullScreen);
	}

	void Root::shutdown() {
		if(mAutoWindow)
			mAutoWindow = mAutoWindow;
	}

	RenderSystem* Root::_createRenderSystem() {
		RenderSystem* renderSystem = NULL;
#if RENDER_SYS == RENDER_SYS_GL
		return NEW GLRenderSystem();
#else
#endif
		ASSERT(renderSystem);
		return renderSystem;
	}
}
