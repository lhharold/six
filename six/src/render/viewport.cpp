#include "core.h"
#include "viewport.h"
#include "rendertarget.h"
#include "camera.h"
#include "root.h"

namespace six {
  Viewport::Viewport(Camera* camera, RenderTarget* target, f32 left, f32 top, f32 width, f32 height, int zOrder) 
    : flags(-1)
    , mCamera(camera)
    , mTarget(target)
    , mLeft(left) , mTop(top) , mWidth(width) , mHeight(height)
    , mZOrder(zOrder)
    , mBackgroundColor(Color4f::Black)
    , mDepthClearValue(1.f)
    , mClearEveryFrame(true)
    , mClearBuffers(FBT_COLOUR | FBT_DEPTH)
    , mVisibilityMask(-1)
  {
    mUpdated = false;
    updateDimensions();
    if(camera)
      camera->notifyViewport(this);
  }
  Viewport::~Viewport() {
		for (ListenerList::iterator i = mListeners.begin(); i != mListeners.end(); ++i) {
			(*i)->viewportDestroyed(this);
		}

		RenderSystem* rs = Root::get().getRenderSystem();
		if (rs && (rs->getViewport() == this)) {
			rs->setViewport(NULL);
		}
  }
  void Viewport::updateDimensions() {
    f32 height = (f32)mTarget->getHeight();
    f32 width = (f32)mTarget->getWidth();

    mRealLeft = (int)(mLeft * width);
    mRealTop = (int)(mTop * height);
    mRealWidth = (int)(mWidth * width);
    mRealHeight = (int)(mHeight * height);
    if (mCamera) {
      if (mCamera->getAutoAspectRatio())
        mCamera->setAspectRatio((f32)mRealWidth / (f32)mRealHeight);
    }
    mUpdated = true;
		for (ListenerList::iterator i = mListeners.begin(); i != mListeners.end(); ++i) {
			(*i)->viewportDimensionsChanged(this);
		}
  }
  void Viewport::update() {
    if(mCamera) {
      mCamera->renderScene(this, mShowOverlays);
    }
  }
  void Viewport::clear(u32 buffers/* = FBT_COLOUR | FBT_DEPTH*/, const Color& color/* = Color::Black*/, f32 depth/* = 1.f*/, u16 stencil/*  = 0*/) {
		RenderSystem* rs = Root::get().getRenderSystem();
		if (rs) {
			Viewport* currentvp = rs->getViewport();
      if (currentvp && currentvp == this) {
				rs->clearFrameBuffer(buffers, color, depth, stencil);
      } else if (currentvp) {
				rs->setViewport(this);
				rs->clearFrameBuffer(buffers, color, depth, stencil);
				rs->setViewport(currentvp);
			}
		}
  }

  u32 Viewport::getRenderFacesNum() const {
    if(mCamera)
      return mCamera->getRenderFacesNum();
    return 0;
  }
  u32 Viewport::getRenderBatchesNum() const {
    if(mCamera)
      return mCamera->getRenderBatchesNum();
    return 0;
  }
  void Viewport::setCamera(Camera* camera) {
		if(mCamera) {
			if(mCamera->getViewport() == this) {
				mCamera->notifyViewport(NULL);
			}
		}
		mCamera = camera;
		if (camera) {
			if (camera->getAutoAspectRatio()) {
				camera->setAspectRatio((f32)mRealWidth / (f32)mRealWidth);
			}
			camera->notifyViewport(this);
		}

		for (ListenerList::iterator i = mListeners.begin(); i != mListeners.end(); ++i) {
			(*i)->viewportCameraChanged(this);
		}
  }
  void Viewport::setDimensions(f32 left, f32 top, f32 width, f32 height) {
    mLeft = left;
    mTop = top;
    mWidth = width;
    mHeight = height;
    updateDimensions();
  }
  void Viewport::geRealDimensions(int& left, int& top, int& width, int& height) {
    left = mRealLeft;
    top = mRealTop;
    width = mRealWidth;
    height = mRealHeight;
  }
  void Viewport::pointOrientedToScreen(const Vector2f& v, int orientationMode, Vector2f& outv) {
    pointOrientedToScreen(v.x, v.y, orientationMode, outv.x, outv.y);
  }
  void Viewport::pointOrientedToScreen(f32 orientedX, f32 orientedY, int orientationMode, f32& screenX, f32& screenY) {
    f32 orX = orientedX;
    f32 orY = orientedY;
    switch (orientationMode) {
        case 1:
          screenX = orY;
          screenY = 1.f - orX;
          break;
        case 2:
          screenX = 1.f - orX;
          screenY = 1.f - orY;
          break;
        case 3:
          screenX = 1.f - orY;
          screenY = orX;
          break;
        default:
          screenX = orX;
          screenY = orY;
          break;
    }
  }
}
