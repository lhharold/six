#include "core.h"
#include "camera.h"
#include "viewport.h"
#include "scenemanager.h"

namespace six {
  DEFINE_STATIC_LOG(Camera);
  Camera::Camera(const char* name, SceneManager* sm) : Frustum(name), mSceneMgr(sm) {
  }
  Camera::~Camera() {
  }

  void Camera::renderScene(Viewport* viewport, bool includeOverlays) {
#if 0
    if (mProjType == PT_PERSPECTIVE) {
      mPixelDisplayRatio = (2.f * Math::tan(mFOVy * 0.5f)) / viewport->getRealHeight();
    } else {
      mPixelDisplayRatio = (mTop - mBottom) / viewport->getRealHeight();
    }
#endif
    for (ListenerList::iterator i = mListeners.begin(); i != mListeners.end(); ++i) {
      (*i)->cameraPreRenderScene(this);
    }
    mSceneMgr->renderScene(this, viewport, includeOverlays);

    for (ListenerList::iterator i = mListeners.begin(); i != mListeners.end(); ++i) {
      (*i)->cameraPostRenderScene(this);
    }
  }

  void Camera::makeViewMatrix(const Vector3f& pos, const Vector3f& dir, const Vector3f& up) {
#if 0
    Vector3f forward = dir.normalizeEx();
    Vector3f right = forward.cross(up);
    Vector3f Up = right.cross(forward);
    right.normalize();
    Up.normalize();

    mViewM[I00] = right.x;
    mViewM[I10] = right.y;
    mViewM[I20] = right.z;
    mViewM[I30] = 0.f;

    mViewM[I01] = Up.x;
    mViewM[I11] = Up.y;
    mViewM[I21] = Up.z;
    mViewM[I31] = 0.f;

    mViewM[I02] = forward.x;
    mViewM[I12] = forward.y;
    mViewM[I22] = -forward.z;
    mViewM[I32] = 0.f;
    mViewM[I30] = pos.dot(right);
    mViewM[I31] = pos.dot(Up);
    mViewM[I32] = pos.dot(forward);
    mViewM[I33] = 1.f;
#endif
  }

  void Camera::setProjection(f32 fovy, f32 aspect, f32 nearClip, f32 farClip) {
#if 0
    mProjType = PT_PERSPECTIVE;
#if 0
    f32 tangent = Math::tan(Math::Deg2Rad(fovy*0.5));
    f32 height = nearClip * tangent;
    f32 width = height * aspect;

    f32 l = -width;
    f32 r = width;
    f32 b = -height;
    f32 t = height;
    f32 n = nearClip;
    f32 f = farClip;

    mProjM.makeIdentity();

    mProjM[0]  = 2 * n / (r - l);
    mProjM[2]  = (r + l) / (r - l);
    mProjM[5]  = 2 * n / (t - b);
    mProjM[6]  = (t + b) / (t - b);
    mProjM[10] = -(f + n) / (f - n);
    mProjM[11] = -(2 * f * n) / (f - n);
    mProjM[14] = -1;
    mProjM[15] = 0;

    mProjM.transpose();
#else
    f32 sine, cotangent, deltaZ;
    const f32 radians = Math::Deg2Rad(fovy*0.5f);
    deltaZ = farClip - nearClip;
    sine = sinf(radians);
    if ((deltaZ == 0) || (sine == 0) || (aspect == 0))
      return;

    cotangent = cosf(radians) / sine;

    mProjM.makeIdentity();
    mProjM[I00] = cotangent/aspect;
    mProjM[I11] = cotangent;
    mProjM[I22] = -(farClip+nearClip)/deltaZ;

    mProjM[I23] = -1;
    mProjM[I32] = -2.f*nearClip*farClip/deltaZ;
    mProjM[I33] = 0;

#endif
    mMVP.makeIdentity();
    mMVP = mViewM * mProjM;
#endif
  }

  void Camera::setOrtho(f32 left, f32 right, f32 bottom, f32 top, f32 nearClip, f32 farClip) {
#if 0
    mProjType = PT_ORTHOGRAPHIC;
    f32 originWidth  = right  - left;
    f32 originHeight = top - bottom;

    const f32 l2r_inv = 1.f/originWidth;
    const f32 b2t_inv = 1.f/originWidth;
    const f32 n2f_inv = 1.f/(farClip-nearClip);

    f32 tx = -(right+left)*l2r_inv;
    f32 ty = -(top+bottom)*b2t_inv;
    f32 tz = -(farClip+nearClip)*n2f_inv;

    Matrix4& m = mProjM;
    m[I00] = 2.f*l2r_inv;  m[I01] = 0.f;		 m[I02] = 0.f;		    m[I03] = 0.f;
    m[I10] = 0.f;		   m[I11] = 2.f*b2t_inv; m[I12] = 0.f;		    m[I13] = 0.f;
    m[I20] = 0.f;		   m[I21] = 0.f;		 m[I22] = -2.f*n2f_inv; m[I23] = 0.f;
    m[I30] = tx;		   m[I31] = ty;			 m[I32] = tz;		    m[I33] = 1.f;

    mMVP.makeIdentity();
    mMVP = mProjM * mViewM;
#endif
  }
}