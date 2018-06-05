#ifndef __SIX_RESOURCE_H_INCLUDE__
#define __SIX_RESOURCE_H_INCLUDE__

namespace six {
  class ManualLoader;
  class ResourceManager;
  class Resource : public StringInterface {
  public:
    class Listener {
      virtual ~Listener() {}
			virtual void backgroundLoadingComplete(Resource*) {}
			virtual void backgroundPreparingComplete(Resource*) {}
			virtual void loadingComplete(Resource*) {}
			virtual void preparingComplete(Resource*) {}
			virtual void unloadingComplete(Resource*) {}
    };

    enum LoadingState {
      LOADSTATE_UNLOADED,     // Not loaded
      LOADSTATE_LOADING,      // Loading is in progress
      LOADSTATE_LOADED,       // Fully loaded
      LOADSTATE_UNLOADING,    // Currently unloading
      LOADSTATE_PREPARED,     // Fully prepared
      LOADSTATE_PREPARING     // Preparing is in progress
    };
  protected:
		ResourceManager* mCreator;
    String mName;
    u64 mHandle;
    Atomic<LoadingState> mLoadingState;
		volatile bool mIsBackgroundLoaded;
    u32 mSize;
		bool mIsManual;
		String mOrigin;
		ManualLoader* mLoader;
		size_t mStateCount;
		typedef List<Listener*> ListenerList;
		ListenerList mListenerList;
		_MUTEX(mListenerListMutex);
  };
  class ManualLoader {
  public:
    virtual ~ManualLoader() {}
		virtual void prepareResource(Resource* resource) {}
		virtual void loadResource(Resource* resource) = 0;
  };
}

#endif //__SIX_RESOURCE_H_INCLUDE__