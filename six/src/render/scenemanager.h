#ifndef __SIX_SCENEMANAGER_H_INCLUDE__
#define __SIX_SCENEMANAGER_H_INCLUDE__

namespace six {
  class Camera;
	class SceneManager {
  DECLARE_STATIC_LOG();
	public:
    SceneManager();
    virtual ~SceneManager();
    void update();

    virtual void renderScene(Camera* camera, Viewport* vp, bool includeOverlays);
    virtual Camera* createCamera(const char* name);
    virtual void clearScene();
	};
}
#endif //__SIX_SCENEMANAGER_H_INCLUDE__
