#ifndef __SIX_RENDERSYSTEM_H_INCLUDE__
#define __SIX_RENDERSYSTEM_H_INCLUDE__

#include "renderwindow.h"

namespace six {

	class RenderSystem {
	public:
		RenderSystem();
		virtual ~RenderSystem();

    virtual RenderWindow* startup(bool autoWindow, const char* windowName) {return NULL;}
		virtual RenderWindow* createWindow(const char* windowName, u32 width, u32 height, bool fullScreen) = 0;
		virtual void shutdown() = 0;
		
		virtual void attachRenderTarget(const RenderTarget* target);
	protected:
		typedef Map<String, const RenderTarget*> RenderTargetMap;
		RenderTargetMap mRenderTargets;
	};
}
#endif //__SIX_RENDERSYSTEM_H_INCLUDE__