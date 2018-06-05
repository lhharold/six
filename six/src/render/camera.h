#ifndef __SIX_CAMERA_H_INCLUDE__
#define __SIX_CAMERA_H_INCLUDE__

#include "frustum.h"
namespace six {
  class Viewport;
  class SceneManager;
  class Camera : public Frustum {
  DECLARE_STATIC_LOG();
	public:
		class Listener {
		public:
			Listener() {}
			virtual ~Listener() {}
			virtual void cameraPreRenderScene(Camera* cam) { (void)cam; }
			virtual void cameraPostRenderScene(Camera* cam) { (void)cam; }
			virtual void cameraDestroyed(Camera* cam) { (void)cam; }
		};
    Camera(const char* name, SceneManager* sm);
    virtual ~Camera();

    void notifyViewport(Viewport* viewport) {mViewport = viewport;}
    Viewport* getViewport() {return mViewport;}
    bool getAutoAspectRatio() {return mAutoAspectRatio;}
    void setAutoAspectRatio(bool autoRatio) {mAutoAspectRatio = autoRatio;}
    u32 getRenderFacesNum() {return mVisFacesLastRender;}
    u32 getRenderBatchesNum() {return mVisBatchesLastRender;}
    void renderScene(Viewport* viewport, bool includeOverlays);

    void makeViewMatrix(const Vector3f& pos, const Vector3f& dir, const Vector3f& up);
    void setProjection(f32 fovy, f32 aspect, f32 nearClip, f32 farClip);
    void setOrtho(f32 left, f32 right, f32 bottom, f32 top, f32 nearClip, f32 farClip);
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
	};
}
#endif //__SIX_CAMERA_H_INCLUDE__
