#include "core.h"

namespace six {
  RenderSystem::RenderSystem() : mActiveRenderTarget(NULL) , mActiveViewport(NULL) {
	}

	RenderSystem::~RenderSystem() {
	}

	void RenderSystem::attachRenderTarget(RenderTarget& target) {
		ASSERT(target.getPriority() < NUM_RENDERTARGET_GROUPS);
		mRenderTargets.insert(std::pair<String, RenderTarget*>(target.getName(), &target));
    mPriorityRenderTargets.insert(std::pair<u8, RenderTarget*>(target.getPriority(), &target));
	}

  void RenderSystem::update(bool swapBuf) {
    for(RenderTargetPriorityMap::iterator i = mPriorityRenderTargets.begin(), n = mPriorityRenderTargets.end(); i != n; ++i) {
      if(i->second->isActive() && i->second->isAutoUpdate()) {
        i->second->update(swapBuf);
      }
    }
  }
  void RenderSystem::swapBuffer() {
    for(RenderTargetPriorityMap::iterator i = mPriorityRenderTargets.begin(), n = mPriorityRenderTargets.end(); i != n; ++i) {
      if(i->second->isActive() && i->second->isAutoUpdate()) {
        i->second->swapBuffers();
      }
    }
  }
}