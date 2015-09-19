#include "core.h"
#include "glrendersystem.h"

namespace six {
	GLRenderSystem::GLRenderSystem() {

	}

	GLRenderSystem::~GLRenderSystem() {

	}

	RenderWindow* GLRenderSystem::startup(bool autoWindow, const char* windowName) {
		return NULL;
	}

	RenderWindow* GLRenderSystem::createWindow(const char* windowName, u32 width, u32 height, bool fullScreen) {
		return NULL;
	}

	void GLRenderSystem::shutdown() {

	}
}