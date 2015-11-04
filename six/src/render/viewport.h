#ifndef __SIX_VIEWPORT_H_INCLUDE__
#define __SIX_VIEWPORT_H_INCLUDE__

namespace six {

	class Viewport {
	public:
		Viewport(Camera* camera, RenderTarget* target, f32 left, f32 top, f32 width, f32 height, int zOrder);
		virtual ~Viewport();
		void _updateDimensions();
		void update();
		void clear(u32 buffers = FBT_COLOUR | FBT_DEPTH, const Color& color = Color::Black, f32 depth = 1.f, u16 stencil = 0);
		RenderTarget getTarget() const;
		Camera* getCamera() const;
		void setCamera(Camera* cam);
		int getZOrder() const;
		f32 getLeft() const;
		f32 getTop() const;
		f32 getWidth() const;
		f32 getHeight() const;
		int getRealLeft() const;
		int getRealTop() const;
		int getRealWidth() const;
		int getRealHeight() const;
		void setDimensions(f32 left, f32 top, f32 width, f32 height);
		void geRealDimensions(int& left, int& top, int& width, int& height);
		void setOrientationMode(OrientationMode orientationMode, bool setDefault = true);
		OrientationMode getOrientationMode() const;
		static void setDefaultOrientationMode(OrientationMode orientationMode);
		static OrientationMode getDefaultOrientationMode();
		void setBackgroundColor(const Color& color);
		const Color& getBackgroundColor() const;
		void setClearEveryFrame(bool clear, u32 buffers = FBT_COLOUR | FBT_DEPTH);
		bool getClearEveryFrame() const;
		u32 getClearBuffers() const;
		void setAutoUpdated(bool autoupdate);
		bool isAutoUpdated() const;
		void setMaterialScheme(const String& schemeName) { mMaterialScheme = schemeName; }
		const String& getMaterialScheme() const { return mMaterialScheme; }
		bool _isUpdated() const;
		void _clearUpdatedFlag();
		u32 _getNumRenderFaces() const;
		u32 _getNumRenderBatches() const;
		void setOverlaysEnabled(bool enabled);
		bool getOverlaysEnabled() const;
		void setSkyEnabled(bool enabled);
		bool getSkyEnabled() const;
		void setShadowsEnabled(bool enabled);
		bool getShadowsEnabled() const;
		void setVisibilityMask(u32 mask) { mVisibilityMask = mask; }
		u32 getVisibilityMask() const { return mvisibilityMask; }
		virtual void setRenderQueueInvocationSequenceName(const String& sequenceName);
		virtual const String& getRenderQueueInvocationSequenceName() const;
		RenderQueueInvocationSequence* _getRenderQueueInvocationSequnce();
		void pointOrientedToScreen(const Vector2& v, int orientationMode, Vector2& outv);
		void pointOrientedToScreen(f32 orientedX, f32 orientedY, int orientationMode, f32& screenX, f32& screenY);
	protected:
		Camera* camera;
		RenderTarget* mTarget;
		f32 mLeft, mTop, mWidth, mHeight;
		int mRealLeft, mRealTop, mRealWidth, mRealHeight;
		int mZOrder;
		Color mBackgroundColor;
		u32 mClearBuffers;
		bool mClearEveryFrame;
		bool mUpdated;
		bool mShowOverlays;
		bool mShowSky;
		bool mShowShadows;
		u32 mVisibilityMask;
		String mRQSequenceName;
		RenderQueueInvocationSequence* mRQSequence;
		String mMaterialSchemeName;
		OrientationMode mOrientationMode;
		static OrientationMode mDefaultOrientationMode;
		bool mIsAutoUpdated;
	};
}
#endif //__SIX_RENDERVIEWPORT_H_INCLUDE__