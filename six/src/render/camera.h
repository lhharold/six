#ifndef __SIX_CAMERA_H_INCLUDE__
#define __SIX_CAMERA_H_INCLUDE__

namespace six {
  enum ProjectionType {
    PT_ORTHOGRAPHIC = 0,
    PT_PERSPECTIVE
  };

  class Viewport;
  class SceneManager;
	class Camera {
	public:
		class Listener {
		public:
			Listener() {}
			virtual ~Listener() {}
			virtual void cameraPreRenderScene(Camera* cam) { (void)cam; }
			virtual void cameraPostRenderScene(Camera* cam) { (void)cam; }
			virtual void cameraDestroyed(Camera* cam) { (void)cam; }
		};
    Camera(SceneManager* sm);
    ~Camera();

    void notifyViewport(Viewport* viewport) {mViewport = viewport;}
    Viewport* getViewport() {return mViewport;}
    bool getAutoAspectRatio() {return mAutoAspectRatio;}
    void setAutoAspectRatio(bool autoRatio) {mAutoAspectRatio = autoRatio;}
    u32 getRenderFacesNum() {return mVisFacesLastRender;}
    u32 getRenderBatchesNum() {return mVisBatchesLastRender;}
    void renderScene(Viewport* viewport, bool includeOverlays);
  protected:
    Viewport* mViewport;
    SceneManager* mSceneMgr;
    u32 mVisFacesLastRender;
    u32 mVisBatchesLastRender;
    f32 mPixelDisplayRatio;
    union {
      struct {
        u32 mAutoAspectRatio : 1;
      };
      u32 flags;
    };
		typedef Vector<Listener*> ListenerList;
		ListenerList mListeners;

    //virtual 
  public:
    virtual void setAspectRatio(f32 ratio) {mAspectRatio = ratio;}
    virtual f32 getAspectRatio() {return mAspectRatio;}
  protected:
    f32 mAspectRatio;
    ProjectionType mProjType;
    f32 mFOVy;
		mutable f32 mLeft, mRight, mTop, mBottom;

    Matrix4 mProjMatrix;
	};
}
#endif //__SIX_CAMERA_H_INCLUDE__
