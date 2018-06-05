#include "core.h"
#include "frustum.h"

namespace six {
  static const f32 INFINITE_FAR_PLANE_ADJUST = 0.00001f;
  Frustum::Frustum(const char* name)
    : mProjDirty(true)
    , mViewDirty(true)
    , mCursomProjMatrix(false)
    , mProjType(PT_PERSPECTIVE)
    , mFovy(45)
    , mFar(100000.f)
    , mNear(100.f)
    , mAspect(4.f/3.f)
    , mOrthoHeight(1000.f)
  {
    updateView();
    updateFrustum();
  }
  Frustum::~Frustum() {
  }
  void Frustum::calcProjectionParameters(f32& left, f32& right, f32& bottom, f32 top) {
    if(mCursomProjMatrix) {
			Matrix4 invProj = mProjMatrix.inverse();
			Vector3f topLeft(-0.5f, 0.5f, 0.f);
			Vector3f bottomRight(0.5f, -0.5f, 0.f);

			topLeft = invProj * topLeft;
			bottomRight = invProj * bottomRight;

			left = topLeft.x;
			top = topLeft.y;
			right = bottomRight.x;
			bottom = bottomRight.y;
    } else {
      if(mProjType == PT_PERSPECTIVE) {
				f32 thetaY (mFovy * 0.5f);
        f32 tanThetaY = Math::tan(thetaY);
				f32 tanThetaX = tanThetaY * mAspect;

				f32 half_w = tanThetaX * mNear;
				f32 half_h = tanThetaY * mNear;

				left   = - half_w;
				right  = + half_w;
				bottom = - half_h;
				top    = + half_h;
      } else if(mProjType == PT_ORTHOGRAPHIC) {
				f32 half_w = getOrthoWindowWidth() * 0.5f;
				f32 half_h = getOrthoWindowHeight() * 0.5f;

				left   = - half_w;
				right  = + half_w;
				bottom = - half_h;
				top    = + half_h;
      }
    }
  }
  void Frustum::updateFrustum() {
    if(!mProjDirty)
      return;
		f32 left = 0.f, right = 0.f, bottom = 0.f, top = 0.f;
    calcProjectionParameters(left, right, bottom, top);
    if(!mCursomProjMatrix) {
			f32 inv_w = 1.f / (right - left);
			f32 inv_h = 1.f / (top - bottom);
			f32 inv_d = 1.f / (mFar - mNear);
			if (mProjType == PT_PERSPECTIVE) {
				f32 A = 2 * mNear * inv_w;
				f32 B = 2 * mNear * inv_h;
				f32 C = (right + left) * inv_w;
				f32 D = (top + bottom) * inv_h;
				f32 q, qn;
				if (F_ZERO(mFar)) {
					q = INFINITE_FAR_PLANE_ADJUST - 1;
					qn = mNear * (INFINITE_FAR_PLANE_ADJUST - 2);
				} else {
					q = - (mFar + mNear) * inv_d;
					qn = -2 * (mFar * mNear) * inv_d;
				}
				// which depth range [-1,1], right-handed rules
				//
				// [ A   0   C   0  ]
				// [ 0   B   D   0  ]
				// [ 0   0   q   qn ]
				// [ 0   0   -1  0  ]
				//
				// A = 2 * near / (right - left)
				// B = 2 * near / (top - bottom)
				// C = (right + left) / (right - left)
				// D = (top + bottom) / (top - bottom)
				// q = - (far + near) / (far - near)
				// qn = - 2 * (far * near) / (far - near)
				mProjMatrix = Matrix4::Zero;
				mProjMatrix[I00] = A;
				mProjMatrix[I02] = C;
				mProjMatrix[I11] = B;
				mProjMatrix[I12] = D;
				mProjMatrix[I22] = q;
				mProjMatrix[I23] = qn;
				mProjMatrix[I32] = -1;
			} // perspective
      else if(mProjType == PT_ORTHOGRAPHIC) {
				f32 A = 2 * inv_w;
				f32 B = 2 * inv_h;
				f32 C = - (right + left) * inv_w;
				f32 D = - (top + bottom) * inv_h;
				f32 q, qn;
				if (F_ZERO(mFar)) {
					q = - INFINITE_FAR_PLANE_ADJUST / mNear;
					qn = - INFINITE_FAR_PLANE_ADJUST - 1;
				} else {
					q = - 2 * inv_d;
					qn = - (mFar + mNear)  * inv_d;
				}
				// NB: This creates 'uniform' orthographic projection matrix,
				// which depth range [-1,1], right-handed rules
				// [ A   0   0   C  ]
				// [ 0   B   0   D  ]
				// [ 0   0   q   qn ]
				// [ 0   0   0   1  ]
				// A = 2 * / (right - left)
				// B = 2 * / (top - bottom)
				// C = - (right + left) / (right - left)
				// D = - (top + bottom) / (top - bottom)
				// q = - 2 / (far - near)
				// qn = - (far + near) / (far - near)
				mProjMatrix = Matrix4::Zero;
				mProjMatrix[I00] = A;
				mProjMatrix[I03] = C;
				mProjMatrix[I11] = B;
				mProjMatrix[I13] = D;
				mProjMatrix[I22] = q;
				mProjMatrix[I23] = qn;
				mProjMatrix[I33] = 1;
      } // ortho
    } //!mCustomProjMatrix

    mProjDirty = false;
  }

  void Frustum::calcViewMatrixRelative(const Vector3f& relPos, Matrix4& matToUpdate) {
  }
  void Frustum::updateView() {
    if(!mViewDirty)
      return;
  }

}