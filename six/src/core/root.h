#ifndef __SIX_ROOT_H_INCLUDE__
#define __SIX_ROOT_H_INCLUDE__

#include "rendersystem.h"
#include "framelistener.h"

namespace six {
  enum FrameEventTime {
    FET_NORMAL,
    FET_START,
    FET_RENDER,
    FET_END,
    FET_COUNT,
  };
  class StaticPluginLoader;
	class Root : public Singleton<Root> {
	public:
		Root();
		~Root();
		RenderWindow* startup(bool autoWindow, const char* windowName = "Auto Window");
		RenderWindow* createWindow(const char* name, s32 width, s32 height, bool fullScreen = false);
    void run();
    bool render();
		void shutdown();

		void installPlugin(Plugin* plugin);
		void unstallPlugin(Plugin* plugin);

		void setRenderSystem(RenderSystem* renderSystem);
		RenderSystem* getRenderSystem();

    void clearEventTimes();
    void getFrameEvent(FrameEventTime type, FrameEvent& evt);
    f32 calcEventTime(u64 t, FrameEventTime type);

    Timer* getTimer() {return mTimer;}
    SceneManager* createSceneManager();
	protected:
    bool _frameStart();
    bool _frameStart(const FrameEvent& evt);
    bool _frameRender();
    bool _frameRender(const FrameEvent& evt);
    bool _frameEnd();
    bool _frameEnd(const FrameEvent& evt);
    bool _updateAllRenderTargets();

		RenderSystem* mRender;
		RenderWindow* mAutoWindow;

		typedef Vector<Plugin*> PluginStaticList;
		PluginStaticList mPlugins;

    Set<FrameListener*> mFrameListener;
    Set<FrameListener*> mRemoveFrameListner;

		typedef Deque<u64> EventTimesQueue;
    EventTimesQueue mEventTimes[FET_COUNT];

    Timer* mTimer;
    f32 mAvgFrameTime;
    f32 mFrameTime;
    u32 mFrameCount;
#ifdef STATIC_LIB
    StaticPluginLoader* mStaticPluginLoader;
#endif

		union {
			struct {
				u32 mInit : 1;
        u32 mRunning : 1;
			};
			u32 flag;
		};
	};
}

#endif