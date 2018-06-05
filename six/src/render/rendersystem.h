#ifndef __SIX_RENDERSYSTEM_H_INCLUDE__
#define __SIX_RENDERSYSTEM_H_INCLUDE__

#include "renderwindow.h"

namespace six {
  class Viewport;
	class RenderSystem {
	public:
		RenderSystem();
		virtual ~RenderSystem();

    virtual RenderWindow* startup(bool autoWindow, const char* windowName) {return NULL;}
		virtual RenderWindow* createWindow(const char* windowName, u32 width, u32 height, bool fullScreen) = 0;
		virtual void shutdown() = 0;
    virtual void update(bool swapBuf = true);
    virtual void swapBuffer();
		
		virtual void attachRenderTarget(RenderTarget& target);
    virtual Viewport* getViewport() {return mActiveViewport;}
    virtual void setViewport(Viewport* viewport) = 0;
    virtual void clearFrameBuffer(u32 buffers, const Color& color = Color::Black, f32 depth = 1.f, u16 stencil = 0) = 0;
    virtual void _setColourBufferWriteEnabled(bool r, bool g, bool b, bool a) = 0;
    virtual void setRenderTarget(RenderTarget* target) = 0;

    virtual void setVertexData(void* data, u32 stride, const int* offsets, int vertexDeclare, u32 vbo = 0) = 0;
    virtual void useMaterial(Material* mtl) = 0;
    virtual void renderIndies() = 0;
	protected:
		typedef Map<String, RenderTarget*> RenderTargetMap;
		RenderTargetMap mRenderTargets;
		typedef Mutimap<u8, RenderTarget*> RenderTargetPriorityMap;
    RenderTargetPriorityMap mPriorityRenderTargets;
    RenderTarget* mActiveRenderTarget;
    Viewport* mActiveViewport;
	};
}
#endif //__SIX_RENDERSYSTEM_H_INCLUDE__