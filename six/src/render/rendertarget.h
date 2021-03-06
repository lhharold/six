#ifndef __SIX_RENDERTARGET_H_INCLUDE__
#define __SIX_RENDERTARGET_H_INCLUDE__

#ifndef NUM_RENDERTARGET_GROUPS
	#define NUM_RENDERTARGET_GROUPS 10
	#define DEFAULT_RENDERTARGET_GROUP 4
	#define REND_TO_TEX_RENDERTARGET_GROUP 2
#endif

namespace six {

	class RenderTarget {
    struct FrameStats {
      f32 lastFPS;
      f32 avgFPS;
      f32 bestFPS;
      f32 worstFPS;
      u64 bestFrameTime;
      u64 worstFrameTime;
      u32 triangleCount;
      u32 batchCount;
    };
	public:
		RenderTarget();
    virtual ~RenderTarget();
		const char* getName() const {return mName.c_str();}
		virtual u32 getWidth(void) const {return mWidth;}
		virtual u32 getHeight(void) const {return mHeight;}
		virtual void setActive(bool active) {mActive = active;}
		virtual bool isActive() const {return mActive;}
    virtual void getUserData(const char* name, void* data) {}
		virtual void setPriority(u8 priority) {mPriority = priority;}
		virtual u8 getPriority() const {return mPriority;}
		virtual bool isPrimary() const {return false;}
		virtual void setAutoUpdate(bool autoUpdate) {mAutoUpdate = autoUpdate;}
		virtual bool isAutoUpdate()const {return mAutoUpdate;}
		virtual void update(bool swapbuffers = true);
		virtual void resetStatistics();
		virtual const FrameStats& getStatistics() const {return mStats;}
		virtual void swapBuffers() {}
		virtual bool requiresTextureFlipping() = 0;
    virtual bool isHardwareGammaEnabled() const { return mHwGamma; }
		virtual Viewport* addViewport(Camera* cam, int zOrder = 0, f32 left = 0.f, f32 top = 0.f, f32 width = 1.f, f32 height = 1.f);
#if 0
		virtual u32 getColorDepth(void) const {return mColorDepth;}
		//virtual void copyContentsToMemory()
		//virtual void writeContentsToFile();
		//virtual const char* writeContentsToTimestampedFile()
#endif
		virtual u32 getViewportNum() const;
		virtual Viewport* getViewport(u32 index);
		virtual void removeViewport(int zOrder);
		virtual void removeAllViewport();
		virtual void addListener(RenderTargetListener* listener);
		virtual void removeListener(RenderTargetListener* listener);
		virtual void removeAllListener();
		virtual void _beginUpdate();
		virtual void _updateViewport(int zOrder, bool updateStatistics = true);
		virtual void _updateViewport(Viewport* viewport, bool updateStatistics = true);
		virtual void _updateAutoUpdateViewports(bool updateStatistics = true);
		virtual void _endUpdate();
	protected:
		void updateStats();
		virtual void firePreUpdate();
		virtual void firePostUpdate();
		virtual void fireViewportPreUpdate(Viewport* vp);
		virtual void fireViewportPostUpdate(Viewport* vp);
		virtual void fireViewportAdded(Viewport* vp);
		virtual void fireViewportRemoved(Viewport* vp);

    union {
      struct {
        u32 mActive : 1;
        u32 mAutoUpdate : 1;
        u32 mHwGamma : 1;
      };
      u32 flags;
    };

		String mName;
		u32 mWidth;
		u32 mHeight;
		u8 mPriority;
		FrameStats mStats;
		u32 mFrameCount;
		u64 mLastSecond;
		u64 mLastTime;
    Timer* mTimer;

		typedef Map<int, Viewport*> ViewportList;
		ViewportList mViewportList;
#if 0
		u32 mColorDepth;
		bool mIsDepthBuffered;
#endif

		

		typedef Vector<RenderTargetListener*> RenderTargetListenerList;
		RenderTargetListenerList mListeners;
	};

}
#endif //__SIX_RENDERTARGET_H_INCLUDE__
