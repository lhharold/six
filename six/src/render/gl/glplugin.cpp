#include "glcore.h"
#include "glrendersystem.h"
#include "glplugin.h"

namespace six {
	const char* sPluginName = "GL RenderSystem";
	GLPlugin::GLPlugin() : mRenderSystem(NULL) {

	}
	const char* GLPlugin::getName() const {
		return sPluginName;
	}
	void GLPlugin::install() {
		mRenderSystem = NEW GLRenderSystem();
		Root::get().setRenderSystem(mRenderSystem);
	}
	void GLPlugin::initialise() {
	}
	void GLPlugin::shutdown() {
	}
	void GLPlugin::uninstall() {
		SAFE_DEL(mRenderSystem);
	}
}