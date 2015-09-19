#include "core.h"

namespace six {

	Root::Root(RenderSystemType type) 
		: mRender(NULL)
		, mAutoWindow(NULL)
	{
		mRender = _createRenderSystem(type);
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

	RenderSystem* Root::_createRenderSystem(RenderSystemType type) {
		switch (type) {
			case RST_GL:
				return NEW GLRenderSystem();
				break;
			case RST_GLES:
				ASSERT(0);
				break;
			case RST_GLES20:
				ASSERT(0);
				break;
			default:
				ASSERT(0);
		}
		return NULL;
	}
}
