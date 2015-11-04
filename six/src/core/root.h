#ifndef __SIX_ROOT_H_INCLUDE__
#define __SIX_ROOT_H_INCLUDE__

#include "rendersystem.h"

namespace six {
	class Root : public Singleton<Root> {
	public:
		Root();
		~Root();
		RenderWindow* startup(bool autoWindow, const char* windowName = "Auto Window");
		RenderWindow* createWindow(const char* name, s32 width, s32 height, bool fullScreen = false);
		void shutdown();

		void installPlugin(Plugin* plugin);
		void unstallPlugin(Plugin* plugin);

		void setRenderSystem(RenderSystem* renderSystem);
		RenderSystem* getRenderSystem();
	protected:
		RenderSystem* mRender;
		RenderWindow* mAutoWindow;

		typedef Vector<Plugin*> PluginStaticList;
		PluginStaticList mPlugins;

		union {
			struct {
				u32 mInit : 1;
			};
			u32 flag;
		};
	};
}

#endif