#include "core.h"
#include "scenemanager.h"
#include "camera.h"
namespace six {
  SceneManager::SceneManager() {

  }
  SceneManager::~SceneManager() {

  }
  void SceneManager::renderScene(Camera* camera, Viewport* vp, bool includeOverlays) {

    if(vp->getClearEveryFrame()) {
      RenderSystem* renderSystem = Root::get().getRenderSystem();
      renderSystem->setViewport(vp);
      renderSystem->clearFrameBuffer(vp->getClearBuffers(), vp->getBackgroundColor(), vp->getDepthClear());
    }
  }
  Camera* SceneManager::createCamera() {
    return NEW Camera(this);
  }
}
