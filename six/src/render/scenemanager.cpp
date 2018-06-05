#include "core.h"
#include "camera.h"
#include "scenemanager.h"
namespace six {
  DEFINE_STATIC_LOG(SceneManager);
  SceneManager::SceneManager() {

  }
  SceneManager::~SceneManager() {

  }
  void SceneManager::update() {
  }
  void SceneManager::renderScene(Camera* camera, Viewport* vp, bool includeOverlays) {
    if(vp->getClearEveryFrame()) {
      RenderSystem* renderSystem = Root::get().getRenderSystem();
      renderSystem->setViewport(vp);
      renderSystem->clearFrameBuffer(vp->getClearBuffers(), vp->getBackgroundColor(), vp->getDepthClear());

    }
  }
  Camera* SceneManager::createCamera(const char* name) {
    return NEW Camera(name, this);
  }
  void SceneManager::clearScene() {
    //destory all static geometry
    //destory all entity;
    //remove all scene node children relation
    //detach all entity reation
    //delete all scene node;
    //destory all animation
    //clear render queue
  }
}
