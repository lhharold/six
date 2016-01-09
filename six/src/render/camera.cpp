#include "core.h"
#include "camera.h"
#include "viewport.h"
#include "scenemanager.h"

namespace six {
  Camera::Camera(SceneManager* sm) : mSceneMgr(sm) {

  }
  Camera::~Camera() {

  }

  void Camera::renderScene(Viewport* viewport, bool includeOverlays) {
		if (mProjType == PT_PERSPECTIVE) {
			mPixelDisplayRatio = (2.f * Math::tan(mFOVy * 0.5f)) / viewport->getRealHeight();
		} else {
			mPixelDisplayRatio = (mTop - mBottom) / viewport->getRealHeight();
		}
		for (ListenerList::iterator i = mListeners.begin(); i != mListeners.end(); ++i) {
			(*i)->cameraPreRenderScene(this);
		}
		mSceneMgr->renderScene(this, viewport, includeOverlays);

		for (ListenerList::iterator i = mListeners.begin(); i != mListeners.end(); ++i) {
			(*i)->cameraPostRenderScene(this);
		}
  }
}
