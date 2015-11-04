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
            u32 bestFrameTime;
            u32 worstFrameTime;
            u32 triangleCount;
            u32 batchCount;
        };
	public:
		RenderTarget(const char* name);
		virtual ~RenderTarget();
		const char* getName() const {return mName.c_str();}
		virtual u32 getWidth(void) const {return mWidth;}
		virtual u32 getHeight(void) const {return mHeight;}
		virtual u32 getColourDepth(void) const {return mColorDepth;}
		virtual void update(bool swapbuffers = true);
		virtual void swapBuffers() {}
		virtual Viewport* addViewport(Camera* cam, int zOrder = 0, f32 left = 0.f, f32 top = 0.f, f32 width = 1.f, f32 height = 1.f);
		virtual u32 getViewportNum() const;
		virtual Viewport* getViewport(u32 index);
		virtual void removeViewport(int zOrder);
		virtual void removeAllViewport();
		virtual const FrameStats& getStatistics() const {return mStats;}
		virtual void resetStatistics();
		virtual void addListener(RenderTargetListener* listener);
		virtual void removeListener(RenderTargetListener* listener);
		virtual void removeAllListener();
		virtual void setPriority(u8 priority) {mPriority = priority};
		virtual u8 getPriority() {return mPriority;}
		virtual void setActive(bool active) {mActive = active;}
		virtual bool isActive() const {return mActive;}
		virtual void setAutoUpdate(bool autoUpdate) {mAutoUpdate = autoUpdate;}
		virtual void isAutoUpdate()const {return mAutoUpdate;}
		//virtual void copyContentsToMemory()
		//virtual void writeContentsToFile();
		//virtual const char* writeContentsToTimestampedFile()
		virtual bool requireTextureFlipping() = 0;
		virtual bool isPrimary() const {return false;}

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

		String mName;
		u8 mPriority;
		u32 mWidth;
		u32 mHeight;
		u32 mColorDepth;
		bool mIsDepthBuffered;
		FrameStats mStats;

		u32 mLastSecond;
		u32 mLastTime;
		u32 mFrameCount;
		bool mActive;
		bool mAutoUpdate;
		
		typedef Map<int, Viewport*> ViewportList;
		ViewportList mViewportList;

		typedef Vector<RenderTargetListener*> RenderTargetListenerList;
		RenderTargetListenerList mListeners;
	};

}
#endif //__SIX_RENDERTARGET_H_INCLUDE__