#include "core.h"

namespace six {
	RenderSystem::RenderSystem() {
	}

	RenderSystem::~RenderSystem() {
	}

	void RenderSystem::attachRenderTarget(const RenderTarget* target) {
		ASSERT(target);
		mRenderTargets.insert(std::pair<String, const RenderTarget*>(target->getName(), target));
	}
}