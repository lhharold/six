#ifndef __SIX_ROOT_H_INCLUDE__
#define __SIX_ROOT_H_INCLUDE__

#include "rendersystem.h"

namespace six {
	class Root {
	public:
		Root();
		~Root();
		RenderWindow* startup(bool autoWindow, const char* windowName = "Auto Window");
		RenderWindow* createWindow(const char* name, s32 width, s32 height, bool fullScreen = false);
		void shutdown();
	protected:
		RenderSystem* _createRenderSystem();

		RenderSystem* mRender;
		RenderWindow* mAutoWindow;
	};
}

#endif