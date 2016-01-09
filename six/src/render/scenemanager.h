#ifndef __SIX_SCENEMANAGER_H_INCLUDE__
#define __SIX_SCENEMANAGER_H_INCLUDE__

namespace six {
  class Camera;
	class SceneManager {
	public:
    SceneManager();
    ~SceneManager();

    virtual void renderScene(Camera* camera, Viewport* vp, bool includeOverlays);
    virtual Camera* createCamera();
	};
}
#endif //__SIX_SCENEMANAGER_H_INCLUDE__
