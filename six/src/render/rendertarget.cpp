#include "core.h"
#include "root.h"

namespace six {
	RenderTarget::RenderTarget()
		: mPriority(DEFAULT_RENDERTARGET_GROUP)
    , flags(0)
	{
    mAutoUpdate = true;
    mTimer = Root::get().getTimer();
		resetStatistics();
	}

	RenderTarget::~RenderTarget() {
		for(ViewportList::iterator i = mViewportList.begin(), n = mViewportList.end(); i != n; ++i) {
			fireViewportRemoved(i->second);
			SAFE_DEL(i->second);
		}
	}
	void RenderTarget::update(bool swapbuffers /* = true */) {
		_beginUpdate();
		_updateAutoUpdateViewports(true);
		_endUpdate();

		if(swapbuffers)
			swapBuffers();
	}
	Viewport* RenderTarget::addViewport(Camera* cam, int zOrder /* = 0 */, f32 left /* = 0.f */, f32 top /* = 0.f */, f32 width /* = 1.f */, f32 height /* = 1.f */) {
		ViewportList::iterator i = mViewportList.find(zOrder);
		if(i != mViewportList.end()) {
			ASSERT(0 && "RenderTarget::addViewport - Cant create another viewport with the same Z-Order");
		}
		Viewport* viewport = NEW Viewport(cam, this, left, top, width, height, zOrder);
		mViewportList[zOrder] = viewport;
		fireViewportAdded(viewport);
		return viewport;
	}

	u32 RenderTarget::getViewportNum() const {
		return (u32)mViewportList.size();
	}

	Viewport* RenderTarget::getViewport(u32 index) {
		ASSERT(index < mViewportList.size() && "RenderTarget::getViewport - index out of bounds");
		ViewportList::iterator i = mViewportList.begin();
		while(index--)
			i++;
		return i->second;
	}
	void RenderTarget::removeViewport(int zOrder) {
		ViewportList::iterator i = mViewportList.find(zOrder);
		if(i != mViewportList.end()) {
			fireViewportRemoved(i->second);
			SAFE_DEL(i->second);
			mViewportList.erase(i);
		}
	}
	void RenderTarget::removeAllViewport() {
		for(ViewportList::iterator i = mViewportList.begin(), n = mViewportList.end(); i != n; ++i) {
			fireViewportRemoved(i->second);
			SAFE_DEL(i->second);
		}
		mViewportList.clear();
	}
	void RenderTarget::resetStatistics() {
        mStats.avgFPS = 0.0;
        mStats.bestFPS = 0.0;
        mStats.lastFPS = 0.0;
        mStats.worstFPS = 999.0;
        mStats.triangleCount = 0;
        mStats.batchCount = 0;
        mStats.bestFrameTime = 999999;
        mStats.worstFrameTime = 0;

        mLastTime = mTimer->getMilliseconds();
        mLastSecond = mLastTime;
        mFrameCount = 0;
	}
	void RenderTarget::addListener(RenderTargetListener* listener) {
		mListeners.push_back(listener);
	}
	void RenderTarget::removeListener(RenderTargetListener* listener) {
		RenderTargetListenerList::iterator i = std::find(mListeners.begin(), mListeners.end(), listener);
		if(i != mListeners.end()) {
			mListeners.erase(i);
		}
	}
	void RenderTarget::removeAllListener() {
		mListeners.clear();
	}
	void RenderTarget::_beginUpdate() {
		firePreUpdate();
		mStats.triangleCount = 0;
		mStats.batchCount = 0;
	}
	void RenderTarget::_updateViewport(int zOrder, bool updateStatistics /* = true */) {
		ViewportList::iterator i = mViewportList.find(zOrder);
		if(i != mViewportList.end()) {
			_updateViewport(i->second, updateStatistics);
		} else {
			ASSERT(0 && "RenderTarget::_updateViewport - dont find zOrder viewport.");
		}
	}
	void RenderTarget::_updateViewport(Viewport* viewport, bool updateStatistics /* = true */) {
		ASSERT(viewport->getTarget() == this && "RenderTarget::_updateViewport - the requested viewport is not bound to this RenderTarget!");
		fireViewportPreUpdate(viewport);
		viewport->update();
		if(updateStatistics) {
			mStats.triangleCount += viewport->getRenderFacesNum();
			mStats.batchCount += viewport->getRenderBatchesNum();
		}
		fireViewportPostUpdate(viewport);
	}
	void RenderTarget::_updateAutoUpdateViewports(bool updateStatistics /* = true */) {
		for(ViewportList::iterator i = mViewportList.begin(), n = mViewportList.end(); i != n; ++i) {
			Viewport* vp = i->second;
			if(vp->isAutoUpdate()) {
				_updateViewport(vp, updateStatistics);
			}
		}
	}
	void RenderTarget::_endUpdate() {
		firePostUpdate();
		updateStats();
	}
  void RenderTarget::updateStats(void) {
    ++mFrameCount;
    u64 thisTime = mTimer->getMilliseconds();
    u64 frameTime = thisTime - mLastTime;
    mLastTime = thisTime;
    mStats.bestFrameTime = MIN(mStats.bestFrameTime, frameTime);
    mStats.worstFrameTime = MAX(mStats.worstFrameTime, frameTime);
    if (thisTime - mLastSecond > 1000) { 
      mStats.lastFPS = (f32)mFrameCount / (f32)(thisTime - mLastSecond) * 1000.0f;
      if (mStats.avgFPS == 0)
        mStats.avgFPS = mStats.lastFPS;
      else
        mStats.avgFPS = (mStats.avgFPS + mStats.lastFPS) / 2; // not strictly correct, but good enough
      mStats.bestFPS = MAX(mStats.bestFPS, mStats.lastFPS);
      mStats.worstFPS = MIN(mStats.worstFPS, mStats.lastFPS);
      mLastSecond = thisTime ;
      mFrameCount  = 0;
    }
  }
	void RenderTarget::firePreUpdate() {
		RenderTargetEvent evt;
		evt.source = this;
		for(RenderTargetListenerList::iterator i = mListeners.begin(), n = mListeners.end(); i != n; ++i) {
			(*i)->preRenderTargetUpdate(evt);
		}
	}
	void RenderTarget::fireViewportPreUpdate(Viewport* vp) {
		RenderTargetViewportEvent evt;
		evt.source = vp;
		for(RenderTargetListenerList::iterator i = mListeners.begin(), n = mListeners.end(); i != n; ++i) {
			(*i)->preViewportUpdate(evt);
		}
	}
	void RenderTarget::fireViewportPostUpdate(Viewport* vp) {
		RenderTargetViewportEvent evt;
		evt.source = vp;
		for(RenderTargetListenerList::iterator i = mListeners.begin(), n = mListeners.end(); i != n; ++i) {
			(*i)->postViewportUpdate(evt);
		}
	}
	void RenderTarget::fireViewportAdded(Viewport* vp) {
		RenderTargetViewportEvent evt;
		evt.source = vp;
		for(RenderTargetListenerList::iterator i = mListeners.begin(), n = mListeners.end(); i != n; ++i) {
			(*i)->viewportAdded(evt);
		}
	}
	void RenderTarget::fireViewportRemoved(Viewport* vp) {
		RenderTargetViewportEvent evt;
		evt.source = vp;
		for(RenderTargetListenerList::iterator i = mListeners.begin(), n = mListeners.end(); i != n; ++i) {
			(*i)->viewportRemoved(evt);
		}
	}
	void RenderTarget::firePostUpdate() {
		RenderTargetEvent evt;
		evt.source = this;
		for(RenderTargetListenerList::iterator i = mListeners.begin(), n = mListeners.end(); i != n; ++i) {
			(*i)->postRenderTargetUpdate(evt);
		}
	}
}
