#include "core.h"
#include "viewport.h"
#include "rendertarget.h"
#include "camera.h"

namespace six {
  Viewport::Viewport(Camera* camera, RenderTarget* target, f32 left, f32 top, f32 width, f32 height, int zOrder) 
    : mCamera(camera)
    , mTarget(target)
    , mLeft(left)
    , mTop(top)
    , mWidth(width)
    , mWidth(height)
    , mZOrder(zOrder)
    , mBackgroundColor(Color4f::Black)
    , mDepthClearValue(1.f)
    , mClearEveryFrame(true)
    , mClearBuffers(FBT_COLOUR | FBT_DEPTH)
  {

  }
  Viewport::~Viewport() {

  }
  void Viewport::updateDimensions() {

  }
  void Viewport::update() {

  }

  u32 Viewport::getRenderFacesNum() const {
    return 0;
  }
  u32 Viewport::getRenderBatchesNum() const {
    return 0;
  }
}
