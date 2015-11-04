#include "core.h"
#include "glrendersystem.h"
#include "glrenderwindow.h"

#if RENDER_SYS == RENDER_SYS_GL
	#pragma comment(lib, "OpenGL32.lib")
	#if OS_PLATFORM == OS_PLATFORM_WIN32
		#include "gl/win32/glwin32support.h"
	#endif
#endif


namespace six {
	GLRenderSystem::GLRenderSystem() 
		: mFlags(0) 
		, mMainContext(NULL)
		, mActiveContext(NULL)
	{
		mGLSupport = getGLSupport();
	}

	GLRenderSystem::~GLRenderSystem() {
	}

	RenderWindow* GLRenderSystem::startup(bool autoWindow, const char* windowName) {
		mGLSupport->start();
		GLRenderWindow* window = mGLSupport->createWindow(this, autoWindow, windowName);

		RenderSystem::startup(autoWindow, windowName);
		return (RenderWindow*)window;
	}

	RenderWindow* GLRenderSystem::createWindow(const char* windowName, u32 width, u32 height, bool fullScreen) {
		if(mRenderTargets.find(windowName) != mRenderTargets.end()) {
			ASSERT(0 && "GLRenderSystem::createWindow - Window with name already exists");
			return NULL;
		}
		GLRenderWindow* window = mGLSupport->newWindow(windowName, width, height, fullScreen);
		RenderWindow* baseWindow = (RenderWindow*)window;
		attachRenderTarget(baseWindow);

		if(!mGLInitialised) {
			initialiseContext(window);
		}
		return baseWindow;
	}

	void GLRenderSystem::shutdown() {
		RenderSystem::shutdown();

		mGLSupport->stop();
		mFlags = 0;
	}

	//////////////////////////////////////////////////////////////////////////
	void GLRenderSystem::initialiseContext(GLRenderWindow* window) {
		mMainContext = window->getContext();
		mActiveContext = mMainContext;
		if(mActiveContext)
			mActiveContext->active();
		mGLSupport->initialiseExtensions();
	}
}