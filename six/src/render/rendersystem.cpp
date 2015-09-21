#include "core.h"

namespace six {
	RenderSystem::RenderSystem() {
	}

	RenderSystem::~RenderSystem() {
	}

	void RenderSystem::attachRenderTarget(RenderTarget& target) {
        mRenderTargets.insert(target.getName(), &target);
	}
}