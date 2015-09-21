#include "core.h"
#include "glrendersystem.h"

namespace six {
	GLRenderSystem::GLRenderSystem() 
		: mFlags(0) 
		, mMainContext(NULL)
		, mActiveContext(NULL)
	{
	}

	GLRenderSystem::~GLRenderSystem() {
	}

	RenderWindow* GLRenderSystem::startup(bool autoWindow, const char* windowName) {
		mGLSupport.startup();
		RenderWindow* autoWindow = mGLSupport.createWindow(autoWindow, this, windowName);

		RenderWindow::startup(autoWindow, windowName);
		return autoWindow;
	}

	RenderWindow* GLRenderSystem::createWindow(const char* windowName, u32 width, u32 height, bool fullScreen) {
		if(mRenderTargets.find(windowName) != mRenderTargets.end()) {
			ASSERT(0 && "GLRenderSystem::createWindow - Window with name already exists");
			return NULL;
		}
		RenderWindow* window = mGLSupport.createWindow(windowName, width, height, fullScreen);
		attachRenderTarget(*window);

		if(!mGLInitialised) {
			initialiseContext(window);

			mGLSupport.
		}
		return window;
	}

	void GLRenderSystem::shutdown() {

	}

	//////////////////////////////////////////////////////////////////////////
	void GLRenderSystem::initialiseContext(RenderWindow* window) {
		mMainContext = window->getContext();
		mActiveContext = mMainContext;
		if(mActiveContext)
			mActiveContext->active();
		mGLSupport.initialiseExtensions();
	}
}