#include "core.h"
#include "gl/glrendersystem.h"

namespace six {
	template<> Root* Singleton<Root>::sInstance = NULL;
	Root::Root() 
		: mRender(NULL)
		, mAutoWindow(NULL)
		, flag(0)
	{
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
	void Root::installPlugin(Plugin* plugin) {
		mPlugins.push_back(plugin);
		plugin->install();

		if(mInit) {
			plugin->initialise();
		}
	}
	void Root::unstallPlugin(Plugin* plugin) {
		PluginStaticList::iterator i = std::find(mPlugins.begin(), mPlugins.end(), plugin);
		if(i != mPlugins.end()) {
			if(mInit)
				plugin->shutdown();
			plugin->uninstall();
			mPlugins.erase(i);
		}
	}
	void Root::setRenderSystem(RenderSystem* renderSystem) {
		if(mRender && mRender != renderSystem) {
			mRender->shutdown();
		}
		mRender = renderSystem;
	}
	RenderSystem* Root::getRenderSystem() {
		return mRender;
	}
}
