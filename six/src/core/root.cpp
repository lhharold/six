#include "core.h"
#include "gl/glrendersystem.h"
#include "staticpluginloader.h"
#include "windowevent.h"

namespace six {
	template<> Root* Singleton<Root>::sInstance = NULL;
	Root::Root() 
		: mRender(NULL)
		, mAutoWindow(NULL)
		, flag(0)
    , mFrameTime(0.f)
    , mAvgFrameTime(0.f)
    , mFrameCount(0)
	{
#ifdef STATIC_LIB
    mStaticPluginLoader = NEW StaticPluginLoader();
    mStaticPluginLoader->load();
#endif
    mTimer = NEW Timer();
	}
	Root::~Root() {
#ifdef STATIC_LIB
    mStaticPluginLoader->unload();
    SAFE_DEL(mStaticPluginLoader);
#endif
    SAFE_DEL(mTimer);
	}
	RenderWindow* Root::startup(bool autoWindow, const char* windowName) {
		mAutoWindow = mRender->startup(autoWindow, windowName);
    mTimer->reset();
		return mAutoWindow;
	}
	RenderWindow* Root::createWindow(const char* name, s32 width, s32 height, bool fullScreen) {
		return mRender->createWindow(name, width, height, fullScreen);
	}
  void Root::startRun() {
    ASSERT(mRender != NULL);
    //mRender->_initRenderTargets();
    clearEventTimes();
    mRunning = false;
    while(!mRunning) {
      WindowEvent::messagePump();
      if (!render())
        break;
    }
  }
  bool Root::render() {
    if(!_frameStart()) 
      return false;
    if(!_updateAllRenderTargets())
      return false;
    return _frameEnd();
  }
  void Root::clearEventTimes() {
    for(int i = 0; i != FET_COUNT; ++i)
      mEventTimes[i].clear();
  }
  f32 Root::calcEventTime(u64 t, FrameEventTime type) {
    EventTimesQueue& times = mEventTimes[type];
    times.push_back(t);
    if(times.size() <= 1)
      return 0;
    u64 eventThresholdTime = (u64)(mAvgFrameTime * 1000.f);
    EventTimesQueue::iterator i = times.begin(), iend = times.end()-2;
    while(i != iend) {
      if((t - *i) > eventThresholdTime)
        ++i;
      else
        break;
    }
    times.erase(times.begin(), i);
    f32 deltaTime = (f32)(times.back() - times.front());
    ASSERT(times.size() > 1);
    return deltaTime / (times.size()-1)*1000;
  }
  void Root::getFrameEvent(FrameEventTime type, FrameEvent& evt) {
    u64 now = mTimer->getMilliseconds();
		evt.timeSinceLastEvent = calcEventTime(now, FET_NORMAL);
		evt.timeSinceLastFrame = calcEventTime(now, type);
  }
  bool Root::_frameStart() {
    FrameEvent evt;
    getFrameEvent(FET_START, evt);
    return _frameStart(evt);
  }
  bool Root::_frameStart(const FrameEvent& evt) {
    for(Set<FrameListener*>::iterator i = mRemoveFrameListner.begin(), iend = mRemoveFrameListner.end(); i != iend; ++i) {
      mFrameListener.erase(*i);
    }
    mRemoveFrameListner.clear();
    for(Set<FrameListener*>::iterator i = mFrameListener.begin(), iend = mFrameListener.end(); i != iend; ++i) {
      if(!(*i)->frameStart(evt)) {
        return false;
      }
    }
    return true;
  }
  bool Root::_frameRender() {
    FrameEvent evt;
    getFrameEvent(FET_RENDER, evt);
    return _frameRender(evt);
  }
  bool Root::_frameRender(const FrameEvent& evt) {
    ++mFrameCount;
    for(Set<FrameListener*>::iterator i = mRemoveFrameListner.begin(), iend = mRemoveFrameListner.end(); i != iend; ++i) {
      mFrameListener.erase(*i);
    }
    mRemoveFrameListner.clear();
    for(Set<FrameListener*>::iterator i = mFrameListener.begin(), iend = mFrameListener.end(); i != iend; ++i) {
      if(!(*i)->frameRender(evt)) {
        return false;
      }
    }
    return true;
  }
  bool Root::_frameEnd() {
    FrameEvent evt;
    getFrameEvent(FET_END, evt);
    return _frameEnd(evt);
  }
  bool Root::_frameEnd(const FrameEvent& evt) {
    for(Set<FrameListener*>::iterator i = mRemoveFrameListner.begin(), iend = mRemoveFrameListner.end(); i != iend; ++i) {
      mFrameListener.erase(*i);
    }
    mRemoveFrameListner.clear();
    bool ret = true;
    for(Set<FrameListener*>::iterator i = mFrameListener.begin(), iend = mFrameListener.end(); i != iend; ++i) {
      if(!(*i)->frameEnd(evt)) {
        ret = false;
        break;
      }
    }
    //clear buffers add code here
    return ret;
  }
  bool Root::_updateAllRenderTargets() {
    mRender->update();
    bool ret = _frameRender();
    mRender->swapBuffer();
    return ret;
  }
	void Root::shutdown() {
		if(mAutoWindow)
			mAutoWindow = mAutoWindow;
	}
	void Root::installPlugin(Plugin* plugin) {
		mPlugins.push_back(plugin);
		plugin->install();

		if(!mInit) {
			plugin->initialise();
      mInit = true;
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
