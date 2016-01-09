#ifndef __SIX_VIEWPORT_H_INCLUDE__
#define __SIX_VIEWPORT_H_INCLUDE__

namespace six {

  class Camera;
  class RenderTarget;
	class Viewport {
	public:
		class Listener {
		public:
			virtual ~Listener() {}
			virtual void viewportCameraChanged(Viewport* viewport) {}
			virtual void viewportDimensionsChanged(Viewport* viewport) {}
			virtual void viewportDestroyed(Viewport* viewport) {}
		};
		Viewport(Camera* camera, RenderTarget* target, f32 left, f32 top, f32 width, f32 height, int zOrder);
		virtual ~Viewport();
		void updateDimensions();
    RenderTarget* getTarget() const {return mTarget;}
		void update();
		u32 getRenderFacesNum() const;
		u32 getRenderBatchesNum() const;
    bool isUpdated() const {return mUpdated;}
    void clearUpdatedFlag() {mUpdated = false;}
		void clear(u32 buffers = FBT_COLOUR | FBT_DEPTH, const Color& color = Color::Black, f32 depth = 1.f, u16 stencil = 0);
    Camera* getCamera() const {return mCamera;}
		void setCamera(Camera* cam);
    int getZOrder() const {return mZOrder;}
    f32 getLeft() const {return mLeft;}
    f32 getTop() const {return mTop;}
    f32 getWidth() const {return mWidth;}
    f32 getHeight() const {return mHeight;}
    int getRealLeft() const {return mRealLeft;}
    int getRealTop() const {return mRealTop;}
    int getRealWidth() const {return mRealWidth;}
    int getRealHeight() const {return mRealHeight;}
		void setDimensions(f32 left, f32 top, f32 width, f32 height);
		void geRealDimensions(int& left, int& top, int& width, int& height);
    void setBackgroundColor(const Color& color) { mBackgroundColor = color;}
    const Color& getBackgroundColor() const {return mBackgroundColor;}
    void setClearEveryFrame(bool clear, u32 buffers = FBT_COLOUR | FBT_DEPTH) {mClearEveryFrame = clear; mClearBuffers = buffers;}
    bool getClearEveryFrame() const {return mClearEveryFrame;}
    u32 getClearBuffers() const {return mClearBuffers;}
    void setAutoUpdated(bool autoupdate) {mIsAutoUpdate = autoupdate;}
    bool isAutoUpdate() const {return mIsAutoUpdate;}
    void setOverlaysEnabled(bool enabled) {mShowOverlays = enabled;}
    bool getOverlaysEnabled() const {return mShowOverlays;}
    void setSkyEnabled(bool enabled) {mShowSky = enabled;}
    bool getSkyEnabled() const {return mShowSky;}
    void setShadowsEnabled(bool enabled) {mShowShadows = enabled;}
    bool getShadowsEnabled() const {return mShowShadows;}
		void setVisibilityMask(u32 mask) { mVisibilityMask = mask;}
		u32 getVisibilityMask() const { return mVisibilityMask;}
    void setDepthClear(f32 depthClear) {mDepthClearValue = depthClear;}
    f32 getDepthClear() {return mDepthClearValue;}

		void pointOrientedToScreen(const Vector2f& v, int orientationMode, Vector2f& outv);
		void pointOrientedToScreen(f32 orientedX, f32 orientedY, int orientationMode, f32& screenX, f32& screenY);
  protected:
		Camera* mCamera;
		RenderTarget* mTarget;
		f32 mLeft, mTop, mWidth, mHeight;
		int mZOrder;
    f32 mDepthClearValue;
		Color mBackgroundColor;
		int mRealLeft, mRealTop, mRealWidth, mRealHeight;
		u32 mClearBuffers;
		u32 mVisibilityMask;
    union {
      struct {
        u32 mIsAutoUpdate : 1;
        u32 mClearEveryFrame : 1;
        u32 mUpdated : 1;
        u32 mShowOverlays : 1;
        u32 mShowSky : 1;
        u32 mShowShadows : 1;
      };
      u32 flags;
    };
		typedef Vector<Listener*> ListenerList;
		ListenerList mListeners;
	};
}
#endif //__SIX_RENDERVIEWPORT_H_INCLUDE__

#if 0
		void setOrientationMode(OrientationMode orientationMode, bool setDefault = true);
		OrientationMode getOrientationMode() const;
		static void setDefaultOrientationMode(OrientationMode orientationMode);
		static OrientationMode getDefaultOrientationMode();
		void setMaterialScheme(const String& schemeName) { mMaterialScheme = schemeName; }
		const String& getMaterialScheme() const { return mMaterialScheme; }
		virtual void setRenderQueueInvocationSequenceName(const String& sequenceName);
		virtual const String& getRenderQueueInvocationSequenceName() const;
		RenderQueueInvocationSequence* _getRenderQueueInvocationSequnce();
	protected:
		String mRQSequenceName;
		RenderQueueInvocationSequence* mRQSequence;
		String mMaterialSchemeName;
		OrientationMode mOrientationMode;
		static OrientationMode mDefaultOrientationMode;
#endif