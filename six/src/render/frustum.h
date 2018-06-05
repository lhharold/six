#ifndef __SIX_FRUSTUM_H_INCLUDE__
#define __SIX_FRUSTUM_H_INCLUDE__

namespace six {
  enum ProjectionType {
    PT_ORTHOGRAPHIC = 0,
    PT_PERSPECTIVE
  };
  enum FrustumPlane {
    FRUSTUM_PLANE_NEAR   = 0,
    FRUSTUM_PLANE_FAR    = 1,
    FRUSTUM_PLANE_LEFT   = 2,
    FRUSTUM_PLANE_RIGHT  = 3,
    FRUSTUM_PLANE_TOP    = 4,
    FRUSTUM_PLANE_BOTTOM = 5
  };
	class Frustum {
	public:
    Frustum(const char* name);
    virtual ~Frustum();

    void invalidateFrustum() {mProjDirty = true;}

    void calcProjectionParameters(f32& left, f32& right, f32& bottom, f32 top);
    virtual void updateFrustum();
		virtual void calcViewMatrixRelative(const Vector3f& relPos, Matrix4& matToUpdate);
    virtual void updateView(void);

    virtual void setFOVy(f32 fovy) {mFovy = fovy; invalidateFrustum();}
    virtual f32 getFOVy(void) const {return mFovy;}
    virtual void setNearClipDistance(f32 nearDist) {ASSERT(nearDist > 0); mNear = nearDist; invalidateFrustum();}
    virtual f32 getNearClipDistance(void) const {return mNear;}
    virtual void setFarClipDistance(f32 farDist) {mFar = farDist; invalidateFrustum();}
    virtual f32 getFarClipDistance(void) const {return mFar;}
    virtual void setAspectRatio(f32 ratio) {mAspect = ratio; invalidateFrustum();}
    virtual f32 getAspectRatio(void) const {return mAspect;}
    virtual void setProjectionType(ProjectionType pt) {mProjType = pt; invalidateFrustum();}
    virtual ProjectionType getProjectionType(void) const {return mProjType;}

    virtual const Matrix4& getProjectionMatrix(void) {updateFrustum(); return mProjMatrix;}
    virtual const Matrix4& getViewMatrix(void) {updateView(); return mViewMatrix;}

    virtual void setOrthoWindow(f32 w, f32 h) {mOrthoHeight = h; mAspect = w / h; invalidateFrustum();}
    virtual void setOrthoWindowHeight(f32 h) {mOrthoHeight = h; invalidateFrustum();}
    virtual void setOrthoWindowWidth(f32 w) {mOrthoHeight = w / mAspect; invalidateFrustum();}
    virtual f32 getOrthoWindowHeight() const {return mOrthoHeight;}
    virtual f32 getOrthoWindowWidth() const {return mOrthoHeight * mAspect;}
  protected:
    mutable Matrix4 mProjMatrix;
    mutable Matrix4 mViewMatrix;
    f32 mFovy;
    f32 mNear;
    f32 mFar;
    f32 mAspect;
    f32 mOrthoHeight;
    ProjectionType mProjType;
    union {
      struct {
        u32 mProjDirty : 1;
        u32 mViewDirty : 1;
        u32 mCursomProjMatrix : 1;
      };
      u32 flags;
    };
	};
}
#endif //__SIX_FRUSTUM_H_INCLUDE__
